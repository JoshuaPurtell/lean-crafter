# Crafter Web App Sketch (Railway-ready)

## Goal
Deploy a minimal web app that hosts the Crafter game (Lean backend) with a browser UI.
Use Lithe for HTTP/WebSocket, and a tiny HTML/JS frontend for rendering and input.

## High-level architecture
1. **Lean backend (CrafterLean)**: session creation, step, render (ASCII).
2. **Lithe web app**: HTTP API + WebSocket stream + static UI.
3. **Frontend**: simple HTML/JS, renders ASCII in `<pre>`, sends actions via WebSocket.
4. **Railway deployment**: single service, `lithe-shim` binary serving HTTP.

```
Browser
  |-- GET /                     -> HTML + JS
  |-- POST /api/sessions         -> new session
  |-- WS  /api/sessions/:id/stream -> frames + actions
  |-- DELETE /api/sessions/:id   -> free session
  |
Lithe app (Lean) -> CrafterLean FFI (session store)
```

## Minimal feature set (MVP)
- **Sessions**
  - create, step, render, reset, delete
- **Transport**
  - WebSocket for frames + actions
- **UI**
  - ASCII render in `<pre>`
  - keyboard controls (WASD + arrows + space)
- **Operational**
  - timeouts, session cleanup, simple rate limiting

## API sketch

### Create session
```
POST /api/sessions
Body: { "seed": 123, "config": {...optional...} }
Response: { "id": "abc123" }
```

### Step action (WebSocket message)
```
{ "type": "action", "action": "MoveUp" }
```

### Stream frames (WebSocket)
```
WS /api/sessions/:id/stream

// Server -> client
{ "type": "frame", "frame": "...", "tick": 42, "health": 8 }

// Client -> server
{ "type": "action", "action": "MoveUp" }
```

### Delete session
```
DELETE /api/sessions/:id
Response: { "ok": true }
```

## UI sketch

### Layout
```
┌───────────────────────────────────────────┐
│ Crafter [Lean]                            │
├───────────────────────────────────────────┤
│ <pre id="frame">ASCII map...              │
│                                           │
│                                           │
├───────────────────────────────────────────┤
│ Status: HP 9 Food 8 Drink 7 Energy 6      │
│ Actions: WASD / arrows / space            │
└───────────────────────────────────────────┘
```

### Frontend behaviors
- On load: `POST /api/sessions` then open WebSocket.
- On keydown: send `{type:"action"}` over WebSocket.
- Render frame on each `frame` message.
- Reconnect if WebSocket drops.

## Implementation notes (Lean/Lithe)

### Lean session storage
- Reuse CrafterLean FFI session store (`crafter_lean_new/step/render/free`).
- Map session IDs to handles in Lithe app state.
- Add TTL cleanup (e.g., 10 min idle).

### WebSocket handler
- Upgrade to WebSocket on `/api/sessions/:id/stream`.
- Send `frame` messages with JSON payload.
- Receive `action` messages and step session.

### Action mapping
Map browser key -> `Action`:
```
ArrowUp/W -> MoveUp
ArrowDown/S -> MoveDown
ArrowLeft/A -> MoveLeft
ArrowRight/D -> MoveRight
Space -> Do
```

## Railway deployment

### Build artifacts
- Build `lithe-shim` binary
- Serve HTTP on `$PORT`

### Required env
- `PORT` (Railway sets automatically)
- Optional: `CRAFTER_CONFIG` (serialized config)

### Process
```
./lithe-shim --app crafter
```

## Next implementation steps
1. Create `examples/crafter_web/` in `lithe/`:
   - `CrafterWeb.lean` (Lithe app + API + WebSocket)
   - `static/index.html` + `static/app.js` + `static/style.css`
2. Wire `lithe-shim` to use the Crafter app registry name.
3. Add simple session registry + TTL cleanup.
4. Add action mapping and WebSocket streaming.
5. Add Railway `Procfile` or `railway.toml` (optional).

## Stretch goals
- Canvas rendering (tile sprites)
- Replay/recording controls
- Mobile-friendly controls
