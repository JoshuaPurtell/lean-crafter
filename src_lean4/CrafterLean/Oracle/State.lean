import CrafterLean.Config
import CrafterLean.Entity
import CrafterLean.Types.Coord
import CrafterLean.Types.ChaCha8
import CrafterLean.Types.Grid
import CrafterLean.World.Types
import CrafterLean.WorldGen.Generator

namespace Crafter

structure OracleState where
  config : SessionConfig
  rng : ChaCha8Rng
  step : Nat
  episode : Nat
  world : World
  objects : Array (UInt32 × GameObject)
  playerId : UInt32

namespace OracleState

@[inline] def worldSize (cfg : SessionConfig) : Nat × Nat := cfg.worldSize

@[inline] def initialPlayerPos (cfg : SessionConfig) : Coord :=
  let (w, h) := cfg.worldSize
  let x := (w / 2)
  let y := (h / 2)
  { x := Int.ofNat x, y := Int.ofNat y }

@[inline] def newWorld (cfg : SessionConfig) (seed : UInt64) : World :=
  let (w, h) := cfg.worldSize
  { materials := Grid.init w h Material.Grass, daylight := 1.0, rngSeed := seed }

@[inline] def new (cfg : SessionConfig) (seed : UInt64) : OracleState :=
  let (world, objects, playerId) := WorldGen.generate cfg seed
  { config := cfg, rng := ChaCha8Rng.seedFromU64 seed, step := 0, episode := 1, world := world, objects := objects, playerId := playerId }

@[inline] def getPlayer? (s : OracleState) : Option Player :=
  s.objects.foldl (init := none) fun acc entry =>
    match acc with
    | some _ => acc
    | none =>
        let (id, obj) := entry
        if id == s.playerId then
          match obj with
          | GameObject.Player p => some p
          | _ => none
        else
          none

@[inline] def getPlayerOrDefault (s : OracleState) : Player :=
  s.getPlayer?.getD (Player.default (initialPlayerPos s.config))

@[inline] def setPlayer (s : OracleState) (p : Player) : OracleState :=
  let objs := s.objects.map fun entry =>
    let (id, obj) := entry
    if id == s.playerId then
      (id, GameObject.Player p)
    else
      (id, obj)
  { s with objects := objs }

@[inline] def getObject? (s : OracleState) (id : UInt32) : Option GameObject :=
  s.objects.foldl (init := none) fun acc entry =>
    match acc with
    | some _ => acc
    | none =>
        let (oid, obj) := entry
        if oid == id then some obj else none

@[inline] def getObjectAt? (s : OracleState) (pos : Coord) : Option GameObject :=
  s.objects.foldl (init := none) fun acc entry =>
    match acc with
    | some _ => acc
    | none =>
        let (_, obj) := entry
        if GameObject.position obj == pos then some obj else none

@[inline] def getObjectIdAt? (s : OracleState) (pos : Coord) : Option UInt32 :=
  s.objects.foldl (init := none) fun acc entry =>
    match acc with
    | some _ => acc
    | none =>
        let (oid, obj) := entry
        if GameObject.position obj == pos then some oid else none

@[inline] def isWalkable (s : OracleState) (pos : Coord) : Bool :=
  match s.world.getMaterial pos with
  | none => false
  | some m =>
      if !m.isWalkable then
        false
      else
        match s.getObjectAt? pos with
        | none => true
        | some obj => !GameObject.isBlocking obj

@[inline] def nextObjectId (s : OracleState) : UInt32 :=
  let maxId := s.objects.foldl (init := 0) fun acc entry =>
    let (oid, _) := entry
    Nat.max acc oid.toNat
  UInt32.ofNat (maxId + 1)

@[inline] def addObject (s : OracleState) (obj : GameObject) : OracleState × UInt32 :=
  let id := s.nextObjectId
  let objs := s.objects.push (id, obj)
  ({ s with objects := objs }, id)

@[inline] def removeObject (s : OracleState) (id : UInt32) : OracleState × Option GameObject :=
  let (objs, removed) := s.objects.foldl (init := (#[], none)) fun acc entry =>
    let (arr, rem) := acc
    let (oid, obj) := entry
    if oid == id then
      (arr, some obj)
    else
      (arr.push entry, rem)
  ({ s with objects := objs }, removed)

@[inline] def moveObject (s : OracleState) (id : UInt32) (pos : Coord) : OracleState :=
  let objs := s.objects.map fun entry =>
    let (oid, obj) := entry
    if oid == id then
      (oid, GameObject.setPosition obj pos)
    else
      (oid, obj)
  { s with objects := objs }

@[inline] def updateObject (s : OracleState) (id : UInt32) (obj : GameObject) : OracleState :=
  let objs := s.objects.map fun entry =>
    let (oid, existing) := entry
    if oid == id then
      (oid, obj)
    else
      (oid, existing)
  { s with objects := objs }

end OracleState

end Crafter
