import Lithe
import Lithe.FFI.Exports
import Lithe.Storage.Sqlite
import SQLite
import CrafterLean
import CrafterLean.Action
import CrafterLean.FFI.Exports

open Lithe
open Crafter

structure CreateRequest where
  seed : Option UInt64 := none
  config : Option Lean.Json := none
  deriving Lean.FromJson

structure StepRequest where
  action : String
  deriving Lean.FromJson

structure FrameResponse where
  id : String
  frame : String
  deriving Lean.ToJson

structure WSActionMsg where
  type : String
  action : String
  deriving Lean.FromJson

structure WSFrameMsg where
  type : String
  frame : String
  deriving Lean.ToJson

structure Session where
  handle : UInt64
  lastSeen : Nat
  startedAt : Nat
  steps : Nat
  achievements : Std.HashMap String Unit

private def sessionTtlMs : Nat := 10 * 60 * 1000
private def cleanupIntervalMs : Nat := 60 * 1000

initialize sessionRef : IO.Ref (Std.HashMap String Session) ←
  IO.mkRef (Std.HashMap.emptyWithCapacity)

initialize nextSessionIdRef : IO.Ref Nat ← IO.mkRef 1

private def nextSessionId : IO String := do
  let n ← nextSessionIdRef.get
  nextSessionIdRef.set (n + 1)
  pure (toString n)

private def nowNanos : IO Nat :=
  IO.monoNanosNow

private def liftIO (io : IO α) : ExceptT HttpError IO α :=
  ExceptT.mk (io.map Except.ok)

private def crafterDbPath : IO System.FilePath := do
  match (← IO.getEnv "CRAFTER_DB_PATH") with
  | some path => pure path
  | none => pure "crafter.sqlite"

private def initCrafterDb : IO (Option Lithe.Storage.Sqlite.SqliteDb) := do
  try
    let path ← crafterDbPath
    let db ← Lithe.Storage.Sqlite.«open» { path := path }
    Lithe.Storage.Sqlite.exec db (String.intercalate "\n"
      [ "CREATE TABLE IF NOT EXISTS session_results ("
      , "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
      , "  session_id TEXT NOT NULL,"
      , "  started_at INTEGER NOT NULL,"
      , "  ended_at INTEGER NOT NULL,"
      , "  duration_ms INTEGER NOT NULL,"
      , "  steps INTEGER NOT NULL,"
      , "  achievements_count INTEGER NOT NULL,"
      , "  achievements_json TEXT NOT NULL,"
      , "  end_reason TEXT NOT NULL"
      , ");"
      , "CREATE INDEX IF NOT EXISTS idx_session_results_achievements"
      , "  ON session_results (achievements_count DESC);"
      , "CREATE INDEX IF NOT EXISTS idx_session_results_duration"
      , "  ON session_results (duration_ms DESC);"
      ])
    pure (some db)
  catch e =>
    IO.eprintln s!"Crafter: sqlite disabled ({e})"
    pure none

initialize crafterDbRef : IO.Ref (Option Lithe.Storage.Sqlite.SqliteDb) ← do
  IO.mkRef (← initCrafterDb)

private def parseAction (input : String) : Option Action :=
  let token := input.trimAscii.toString.toLower
  match token with
  | "noop" | "n" => some Action.Noop
  | "left" | "a" | "moveleft" => some Action.MoveLeft
  | "right" | "d" | "moveright" => some Action.MoveRight
  | "up" | "w" | "moveup" => some Action.MoveUp
  | "down" | "s" | "movedown" => some Action.MoveDown
  | "do" | "e" | "space" => some Action.Do
  | "sleep" => some Action.Sleep
  | "place_stone" => some Action.PlaceStone
  | "place_table" => some Action.PlaceTable
  | "place_furnace" => some Action.PlaceFurnace
  | "place_plant" => some Action.PlacePlant
  | "make_wood_pickaxe" => some Action.MakeWoodPickaxe
  | "make_stone_pickaxe" => some Action.MakeStonePickaxe
  | "make_iron_pickaxe" => some Action.MakeIronPickaxe
  | "make_wood_sword" => some Action.MakeWoodSword
  | "make_stone_sword" => some Action.MakeStoneSword
  | "make_iron_sword" => some Action.MakeIronSword
  | "make_diamond_pickaxe" => some Action.MakeDiamondPickaxe
  | "make_diamond_sword" => some Action.MakeDiamondSword
  | "make_iron_armor" => some Action.MakeIronArmor
  | "make_diamond_armor" => some Action.MakeDiamondArmor
  | "make_bow" => some Action.MakeBow
  | "make_arrow" => some Action.MakeArrow
  | "shoot_arrow" => some Action.ShootArrow
  | "drink_red" => some Action.DrinkPotionRed
  | "drink_green" => some Action.DrinkPotionGreen
  | "drink_blue" => some Action.DrinkPotionBlue
  | "drink_pink" => some Action.DrinkPotionPink
  | "drink_cyan" => some Action.DrinkPotionCyan
  | "drink_yellow" => some Action.DrinkPotionYellow
  | _ =>
      match token.toNat? with
      | some n => Action.fromUInt32? (UInt32.ofNat n)
      | none => none

private def actionCode? (input : String) : Option UInt32 :=
  (parseAction input).map Action.toUInt32

private def parseCreateRequest (ctx : RequestCtx) : ExceptT HttpError IO (Option CreateRequest) := do
  let body ← ctx.req.readBodyAll
  if body.isEmpty then
    return none
  let s ←
    match bytesToString? body with
    | some v => pure v
    | none => throw (HttpError.badRequest "invalid utf-8 body")
  let j ←
    match Lean.Json.parse s with
    | .ok v => pure v
    | .error err => throw (HttpError.badRequest err)
  match Lean.fromJson? (α := CreateRequest) j with
  | .ok v => return some v
  | .error err => throw (HttpError.badRequest err)

private def touchSession (id : String) (sess : Session) : IO Session := do
  let now ← nowNanos
  let sess' := { sess with lastSeen := now }
  sessionRef.modify (fun m => m.insert id sess')
  pure sess'

private def updateSession (id : String) (f : Session → Session) : IO Unit := do
  sessionRef.modify (fun m =>
    match m.get? id with
    | none => m
    | some sess => m.insert id (f sess)
  )

private def isHeadingLine (line : String) : Bool :=
  line.startsWith "===" || line.startsWith "---"

private def extractAchievements (frame : String) : Array String :=
  let lines := frame.splitOn "\n"
  let rec loop (rest : List String) (inSection : Bool) (acc : Array String) : Array String :=
    match rest with
    | [] => acc
    | line :: tail =>
        let trimmed := line.trimAscii.toString
        let lower := trimmed.toLower
        if !inSection then
          if lower.contains "achievement" then
            loop tail true acc
          else
            loop tail false acc
        else
          if trimmed.isEmpty || isHeadingLine trimmed then
            loop tail false acc
          else
            loop tail true (acc.push trimmed)
  loop lines false #[]

private def updateAchievements (sess : Session) (frame : String) : Session :=
  let found := extractAchievements frame
  let updated := found.foldl (fun acc item => acc.insert item ()) sess.achievements
  { sess with achievements := updated }

private def recordFrame (id : String) (frame : String) : IO Unit :=
  updateSession id (fun sess => updateAchievements sess frame)

private def recordActionFrame (id : String) (frame : String) : IO Unit :=
  updateSession id (fun sess =>
    let stepped := { sess with steps := sess.steps + 1 }
    updateAchievements stepped frame
  )

private def storeSessionResult (id : String) (sess : Session) (reason : String) : IO Unit := do
  let db? ← crafterDbRef.get
  match db? with
  | none => pure ()
  | some db =>
      let endedAt ← nowNanos
      let durationMs := (endedAt - sess.startedAt) / 1_000_000
      let achievements := sess.achievements.toList.map (fun (k, _) => k)
      let achievementsJson := Lean.Json.compress (Lean.toJson achievements)
      let stmt ← Lithe.Storage.Sqlite.prepare db
        "INSERT INTO session_results (session_id, started_at, ended_at, duration_ms, steps, achievements_count, achievements_json, end_reason) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      SQLite.Stmt.bindText stmt 1 id
      SQLite.Stmt.bindInt64 stmt 2 (Int64.ofNat sess.startedAt)
      SQLite.Stmt.bindInt64 stmt 3 (Int64.ofNat endedAt)
      SQLite.Stmt.bindInt64 stmt 4 (Int64.ofNat durationMs)
      SQLite.Stmt.bindInt64 stmt 5 (Int64.ofNat sess.steps)
      SQLite.Stmt.bindInt64 stmt 6 (Int64.ofNat achievements.length)
      SQLite.Stmt.bindText stmt 7 achievementsJson
      SQLite.Stmt.bindText stmt 8 reason
      stmt.exec

private def getSession (id : String) : ExceptT HttpError IO Session := do
  let m ← sessionRef.get
  match m.get? id with
  | some sess =>
      let sess' ← touchSession id sess
      pure sess'
  | none => throw { status := 404, code := "session_not_found", message := s!"unknown session '{id}'" }

private def putSession (id : String) (sess : Session) : IO Unit :=
  sessionRef.modify (fun m => m.insert id sess)

private def freeSession (id : String) : IO Unit := do
  let m ← sessionRef.get
  match m.get? id with
  | none => pure ()
  | some sess =>
      Crafter.FFI.crafter_lean_free sess.handle
      sessionRef.modify (fun m => m.erase id)

private def renderFrame (handle : UInt64) : ExceptT HttpError IO String := do
  let bytes ← Crafter.FFI.crafter_lean_render handle
  match bytesToString? bytes with
  | some s => pure s
  | none => throw { status := 500, code := "render_invalid_utf8", message := "render output invalid utf-8" }

private def stepAndRender (id : String) (handle : UInt64) (action : UInt32) : ExceptT HttpError IO String := do
  let _ ← Crafter.FFI.crafter_lean_step handle action
  let frame ← renderFrame handle
  liftIO (recordActionFrame id frame)
  pure frame

private def makeFrameResponse (id : String) (frame : String) : Response :=
  Response.jsonOf ({ id := id, frame := frame } : FrameResponse)

private def createSession (req? : Option CreateRequest) : ExceptT HttpError IO (String × String) := do
  let seed ←
    match req?.bind (fun r => r.seed) with
    | some s => pure s
    | none => do
        let n ← nowNanos
        pure (UInt64.ofNat n)
  let cfgBytes :=
    match req?.bind (fun r => r.config) with
    | some cfg => stringToBytes (Lean.Json.compress cfg)
    | none => ByteArray.empty
  let handle ← Crafter.FFI.crafter_lean_new cfgBytes seed
  if handle == 0 then
    throw { status := 500, code := "crafter_init_failed", message := "failed to create session" }
  let id ← nextSessionId
  let now ← nowNanos
  putSession id
    { handle := handle
    , lastSeen := now
    , startedAt := now
    , steps := 0
    , achievements := Std.HashMap.emptyWithCapacity
    }
  let frame ← renderFrame handle
  liftIO (recordFrame id frame)
  pure (id, frame)

private def resetSession (id : String) : ExceptT HttpError IO String := do
  let sess ← getSession id
  liftIO (storeSessionResult id sess "reset")
  Crafter.FFI.crafter_lean_free sess.handle
  let seed := UInt64.ofNat (← nowNanos)
  let handle ← Crafter.FFI.crafter_lean_new ByteArray.empty seed
  if handle == 0 then
    throw { status := 500, code := "crafter_init_failed", message := "failed to reset session" }
  let now ← nowNanos
  putSession id
    { handle := handle
    , lastSeen := now
    , startedAt := now
    , steps := 0
    , achievements := Std.HashMap.emptyWithCapacity
    }
  let frame ← renderFrame handle
  liftIO (recordFrame id frame)
  pure frame

private def closeSession (id : String) : ExceptT HttpError IO Unit := do
  let m ← sessionRef.get
  match m.get? id with
  | none => pure ()
  | some sess =>
      liftIO (storeSessionResult id sess "close")
      freeSession id

private def createHandler : Handler :=
  fun ctx => do
    let req? ← parseCreateRequest ctx
    let (id, frame) ← createSession req?
    pure (makeFrameResponse id frame)

private def renderHandler : Handler :=
  fun ctx => do
    let id ← (Path.param "id" String) ctx
    let sess ← getSession id
    let frame ← renderFrame sess.handle
    liftIO (recordFrame id frame)
    pure (makeFrameResponse id frame)

private def stepHandler : Handler :=
  fun ctx => do
    let id ← (Path.param "id" String) ctx
    let req ← (JsonBody StepRequest) ctx
    let action? := actionCode? req.action
    match action? with
    | none => throw (HttpError.badRequest s!"unknown action '{req.action}'")
    | some action =>
        let sess ← getSession id
        let frame ← stepAndRender id sess.handle action
        pure (makeFrameResponse id frame)

private def resetHandler : Handler :=
  fun ctx => do
    let id ← (Path.param "id" String) ctx
    let frame ← resetSession id
    pure (makeFrameResponse id frame)

private def closeHandler : Handler :=
  fun ctx => do
    let id ← (Path.param "id" String) ctx
    closeSession id
    pure (Response.json (Lean.Json.mkObj [ ("ok", true) ]))

private def parseActionMessage (s : String) : Option String :=
  match Lean.Json.parse s with
  | .error _ => none
  | .ok j =>
      match Lean.fromJson? (α := WSActionMsg) j with
      | .ok msg => if msg.type == "action" then some msg.action else none
      | .error _ => none

private def sendFrame (conn : WSConnection) (frame : String) : ExceptT HttpError IO Unit := do
  let msg : WSFrameMsg := { type := "frame", frame := frame }
  let payload := Lean.Json.compress (Lean.toJson msg)
  let _ ← WSConnection.sendText conn payload
  pure ()

partial def wsLoop (conn : WSConnection) (id : String) : ExceptT HttpError IO Unit := do
  let msg? ← WSConnection.receive conn
  match msg? with
  | none => pure ()
  | some msg =>
      match msg.kind with
      | .text =>
          let payload := bytesToString msg.data
          let actionRaw := (parseActionMessage payload).getD payload
          match actionCode? actionRaw with
          | none => wsLoop conn id
          | some code =>
              let sess ← getSession id
              let frame ← stepAndRender id sess.handle code
              sendFrame conn frame
              wsLoop conn id
      | .binary =>
          let payload := bytesToString msg.data
          let actionRaw := (parseActionMessage payload).getD payload
          match actionCode? actionRaw with
          | none => wsLoop conn id
          | some code =>
              let sess ← getSession id
              let frame ← stepAndRender id sess.handle code
              sendFrame conn frame
              wsLoop conn id
      | .ping =>
          let _ ← WSConnection.send conn (WSMessage.pong msg.data)
          wsLoop conn id
      | .pong => wsLoop conn id
      | .close =>
          let _ ← WSConnection.send conn (WSMessage.close)
          WSConnection.close conn

private def wsHandler : WSHandler :=
  fun conn ctx => do
    let id ← (Path.param "id" String) ctx
    let sess ← getSession id
    let frame ← renderFrame sess.handle
    liftIO (recordFrame id frame)
    sendFrame conn frame
    wsLoop conn id

private def responseWithType (contentType : String) (body : String) : Response :=
  { status := Status.ok
  , headers := #[ ("content-type", contentType) ]
  , body := stringToBytes body
  , bodyStream := none
  }

private def indexHtml : String :=
  String.intercalate "\n"
    [ "<!doctype html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <meta charset=\"utf-8\" />"
    , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
    , "  <title>Crafter Web</title>"
    , "  <link rel=\"stylesheet\" href=\"/styles.css\" />"
    , "</head>"
    , "<body>"
    , "  <div id=\"app\">"
    , "    <header>Crafter Web</header>"
    , "    <main>"
    , "      <section class=\"screen\">"
    , "        <pre id=\"screen\">loading...</pre>"
    , "      </section>"
    , "      <aside class=\"panel\">"
    , "        <div class=\"status\">"
    , "          <div>Session: <span id=\"session\">-</span></div>"
    , "          <div>Status: <span id=\"status\">booting</span></div>"
    , "        </div>"
    , "        <div class=\"controls\">"
    , "          <h3>Move</h3>"
    , "          <div class=\"grid\">"
    , "            <button data-action=\"moveup\">W</button>"
    , "            <button data-action=\"moveleft\">A</button>"
    , "            <button data-action=\"movedown\">S</button>"
    , "            <button data-action=\"moveright\">D</button>"
    , "          </div>"
    , "          <button class=\"wide\" data-action=\"do\">Do (E/Space)</button>"
    , "          <button class=\"wide\" data-action=\"sleep\">Sleep</button>"
    , "          <div class=\"row\">"
    , "            <button id=\"reset\" class=\"ghost\">Reset</button>"
    , "            <button id=\"close\" class=\"ghost\">Close</button>"
    , "          </div>"
    , "        </div>"
    , "        <div class=\"hint\">Use WASD or arrow keys. E/Space = Do.</div>"
    , "      </aside>"
    , "    </main>"
    , "  </div>"
    , "  <script src=\"/app.js\"></script>"
    , "</body>"
    , "</html>"
    ]

private def appJs : String :=
  String.intercalate "\n"
    [ "const screen = document.getElementById('screen');"
    , "const statusEl = document.getElementById('status');"
    , "const sessionEl = document.getElementById('session');"
    , "const resetBtn = document.getElementById('reset');"
    , "const closeBtn = document.getElementById('close');"
    , "let sessionId = null;"
    , "let socket = null;"
    , "const keyMap = {"
    , "  ArrowUp: 'moveup', ArrowDown: 'movedown', ArrowLeft: 'moveleft', ArrowRight: 'moveright',"
    , "  w: 'moveup', a: 'moveleft', s: 'movedown', d: 'moveright',"
    , "  W: 'moveup', A: 'moveleft', S: 'movedown', D: 'moveright',"
    , "  e: 'do', E: 'do', ' ': 'do'"
    , "};"
    , "function setStatus(text) { statusEl.textContent = text; }"
    , "function setSession(id) { sessionEl.textContent = id; }"
    , "function setFrame(frame) { screen.textContent = frame; }"
    , "async function createSession() {"
    , "  const res = await fetch('/api/sessions', { method: 'POST' });"
    , "  if (!res.ok) { setStatus('failed'); return; }"
    , "  const data = await res.json();"
    , "  sessionId = data.id;"
    , "  setSession(sessionId);"
    , "  if (data.frame) setFrame(data.frame);"
    , "  connectWS();"
    , "}"
    , "function wsUrl(id) {"
    , "  const proto = location.protocol === 'https:' ? 'wss' : 'ws';"
    , "  return `${proto}://${location.host}/api/sessions/${id}/stream`;"
    , "}"
    , "function connectWS() {"
    , "  if (!sessionId) return;"
    , "  if (socket) socket.close();"
    , "  socket = new WebSocket(wsUrl(sessionId));"
    , "  setStatus('connecting');"
    , "  socket.onopen = () => setStatus('live');"
    , "  socket.onclose = () => setStatus('disconnected');"
    , "  socket.onerror = () => setStatus('error');"
    , "  socket.onmessage = (ev) => {"
    , "    try {"
    , "      const msg = JSON.parse(ev.data);"
    , "      if (msg.type === 'frame' && msg.frame) {"
    , "        setFrame(msg.frame);"
    , "        return;"
    , "      }"
    , "    } catch (_) {}"
    , "    setFrame(ev.data);"
    , "  };"
    , "}"
    , "async function stepAction(action) {"
    , "  if (!sessionId) return;"
    , "  const res = await fetch(`/api/sessions/${sessionId}/step`, {"
    , "    method: 'POST',"
    , "    headers: { 'content-type': 'application/json' },"
    , "    body: JSON.stringify({ action })"
    , "  });"
    , "  if (!res.ok) return;"
    , "  const data = await res.json();"
    , "  if (data.frame) setFrame(data.frame);"
    , "}"
    , "function sendAction(action) {"
    , "  if (socket && socket.readyState === WebSocket.OPEN) {"
    , "    socket.send(JSON.stringify({ type: 'action', action }));"
    , "  } else {"
    , "    stepAction(action);"
    , "  }"
    , "}"
    , "document.addEventListener('keydown', (ev) => {"
    , "  const action = keyMap[ev.key];"
    , "  if (action) {"
    , "    ev.preventDefault();"
    , "    sendAction(action);"
    , "  }"
    , "});"
    , "document.querySelectorAll('[data-action]').forEach(btn => {"
    , "  btn.addEventListener('click', () => sendAction(btn.dataset.action));"
    , "});"
    , "resetBtn.addEventListener('click', async () => {"
    , "  if (!sessionId) return;"
    , "  const res = await fetch(`/api/sessions/${sessionId}/reset`, { method: 'POST' });"
    , "  if (!res.ok) return;"
    , "  const data = await res.json();"
    , "  if (data.frame) setFrame(data.frame);"
    , "});"
    , "closeBtn.addEventListener('click', async () => {"
    , "  if (!sessionId) return;"
    , "  await fetch(`/api/sessions/${sessionId}`, { method: 'DELETE' });"
    , "  if (socket) socket.close();"
    , "  setStatus('closed');"
    , "});"
    , "createSession();"
    ]

private def stylesCss : String :=
  String.intercalate "\n"
    [ ":root {"
    , "  --bg: #0b0e11;"
    , "  --panel: #12161b;"
    , "  --accent: #2dd4bf;"
    , "  --text: #e2e8f0;"
    , "  --muted: #94a3b8;"
    , "  --border: #1f2937;"
    , "  font-family: 'IBM Plex Mono', ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', monospace;"
    , "}"
    , "* { box-sizing: border-box; }"
    , "body { margin: 0; background: var(--bg); color: var(--text); }"
    , "#app { max-width: 1100px; margin: 0 auto; padding: 24px; }"
    , "header { font-size: 24px; margin-bottom: 16px; letter-spacing: 0.08em; text-transform: uppercase; }"
    , "main { display: grid; grid-template-columns: 2fr 1fr; gap: 16px; }"
    , ".screen { background: var(--panel); border: 1px solid var(--border); padding: 16px; }"
    , "#screen { margin: 0; white-space: pre; font-size: 14px; line-height: 1.2; }"
    , ".panel { background: var(--panel); border: 1px solid var(--border); padding: 16px; display: flex; flex-direction: column; gap: 16px; }"
    , ".status { font-size: 12px; color: var(--muted); }"
    , ".controls h3 { margin: 0 0 8px 0; font-size: 12px; text-transform: uppercase; }"
    , ".grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 8px; margin-bottom: 8px; }"
    , ".row { display: flex; gap: 8px; }"
    , "button { padding: 8px 10px; border-radius: 6px; border: 1px solid var(--border); background: #0f172a; color: var(--text); cursor: pointer; }"
    , "button:hover { border-color: var(--accent); color: var(--accent); }"
    , ".wide { width: 100%; margin-bottom: 8px; }"
    , ".ghost { flex: 1; background: transparent; }"
    , ".hint { font-size: 11px; color: var(--muted); }"
    , "@media (max-width: 900px) {"
    , "  main { grid-template-columns: 1fr; }"
    , "}"
    ]

private def indexHandler : Handler :=
  fun _ => pure (responseWithType "text/html; charset=utf-8" indexHtml)

private def appJsHandler : Handler :=
  fun _ => pure (responseWithType "application/javascript; charset=utf-8" appJs)

private def stylesHandler : Handler :=
  fun _ => pure (responseWithType "text/css; charset=utf-8" stylesCss)

private def crafterApp : App :=
  App.empty
  |>.useAll (Lithe.defaultStack)
  |>.get "/" indexHandler
  |>.get "/app.js" appJsHandler
  |>.get "/styles.css" stylesHandler
  |>.post "/api/sessions" createHandler
  |>.get "/api/sessions/:id/frame" renderHandler
  |>.post "/api/sessions/:id/step" stepHandler
  |>.post "/api/sessions/:id/reset" resetHandler
  |>.delete "/api/sessions/:id" closeHandler
  |>.ws "/api/sessions/:id/stream" wsHandler

private partial def cleanupLoop : IO Unit := do
  let ttlNanos := sessionTtlMs * 1_000_000
  let now ← nowNanos
  let m ← sessionRef.get
  let expired := m.fold (init := #[]) (fun acc k v =>
    if now > v.lastSeen && now - v.lastSeen > ttlNanos then
      acc.push k
    else
      acc
  )
  for id in expired do
    match m.get? id with
    | some sess => storeSessionResult id sess "expired"
    | none => pure ()
    freeSession id
  IO.sleep (UInt32.ofNat cleanupIntervalMs)
  cleanupLoop

initialize crafterAppRegistry : Unit ← do
  Lithe.registerApp "crafter" (pure crafterApp)
  let _ ← IO.asTask cleanupLoop
  pure ()

@[export crafter_new_app]
def crafter_new_app : IO UInt64 := do
  let router := App.toRouter crafterApp
  Lithe.newInstance router

@[export crafter_handle]
def crafter_handle (app : UInt64) (reqBytes : ByteArray) : IO ByteArray :=
  Lithe.lithe_handle app reqBytes

@[export crafter_free_app]
def crafter_free_app (app : UInt64) : IO Unit :=
  Lithe.freeInstance app

@[export lithe_new_app_named]
def lithe_new_app_named (nameBytes : ByteArray) : IO UInt64 := do
  let name ←
    match bytesToString? nameBytes with
    | some s => pure s
    | none => throw (IO.userError "invalid app name")
  let app ← Lithe.getApp name
  let router := App.toRouter app
  Lithe.newInstance router app.state
