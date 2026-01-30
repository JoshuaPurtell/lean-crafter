import CrafterLean.Serialize
import CrafterLean.World.Material
import CrafterLean.Entity
import CrafterLean.Oracle.State
import CrafterLean.Oracle.Step
import CrafterLean.Types.ChaCha8

namespace Crafter

open Lean

structure WorldSaveData where
  area : Nat × Nat
  materials : Array Material
  objects : Array (UInt32 × GameObject)
  playerId : UInt32
  daylight : Float
  rngSeed : UInt64

structure SaveData where
  version : UInt32
  timestamp : UInt64
  name : Option String
  config : SessionConfig
  episode : UInt32
  step : UInt64
  rngState : Array UInt8
  world : WorldSaveData

instance : ToJson WorldSaveData where
  toJson w :=
    Lean.Json.mkObj
      [ ("area", Lean.toJson w.area)
      , ("materials", Lean.toJson w.materials)
      , ("objects", Lean.toJson w.objects)
      , ("player_id", Lean.toJson w.playerId)
      , ("daylight", Lean.toJson w.daylight)
      , ("rng_seed", Lean.toJson w.rngSeed)
      ]

instance : FromJson WorldSaveData where
  fromJson? j := do
    let o <- j.getObj?
    let area <- getFieldAs o "area"
    let materials <- getFieldAs o "materials"
    let objects <- getFieldOr o "objects" (#[] : Array (UInt32 × GameObject))
    let playerId <- getFieldOr o "player_id" (0 : UInt32)
    let daylight <- getFieldAs o "daylight"
    let rngSeed <- getFieldAs o "rng_seed"
    return { area := area, materials := materials, objects := objects, playerId := playerId, daylight := daylight, rngSeed := rngSeed }

instance : ToJson SaveData where
  toJson s :=
    Lean.Json.mkObj
      [ ("version", Lean.toJson s.version)
      , ("timestamp", Lean.toJson s.timestamp)
      , ("name", Lean.toJson s.name)
      , ("config", Lean.toJson s.config)
      , ("episode", Lean.toJson s.episode)
      , ("step", Lean.toJson s.step)
      , ("rng_state", Lean.toJson s.rngState)
      , ("world", Lean.toJson s.world)
      ]

instance : FromJson SaveData where
  fromJson? j := do
    let o <- j.getObj?
    let version <- getFieldAs o "version"
    let timestamp <- getFieldOr o "timestamp" (0 : UInt64)
    let name <- getFieldOr o "name" (none : Option String)
    let config <- getFieldAs o "config"
    let episode <- getFieldOr o "episode" (1 : UInt32)
    let step <- getFieldOr o "step" (0 : UInt64)
    let rngState <- getFieldOr o "rng_state" (Array.replicate 32 (0 : UInt8))
    let world <- getFieldAs o "world"
    return { version := version, timestamp := timestamp, name := name, config := config, episode := episode, step := step, rngState := rngState, world := world }

namespace SaveLoad

def currentVersion : UInt32 := 1

@[inline] def defaultPlayerId : UInt32 :=
  UInt32.ofNatLT 1 (by decide)

@[inline] def rngStateBytes (_state : UInt64) : Array UInt8 :=
  Array.replicate 32 (0 : UInt8)

@[inline] def materialsFromWorld (w : World) : Array Material :=
  w.materials.data

@[inline] def playerObject (p : Player) : GameObject :=
  GameObject.Player p

@[inline] def selectPlayerFromObjects (playerId : UInt32) (objects : Array (UInt32 × GameObject)) : Option Player :=
  objects.foldl (init := none) fun acc entry =>
    match acc with
    | some _ => acc
    | none =>
      let (id, obj) := entry
      if id == playerId then
        match obj with
        | GameObject.Player p => some p
        | _ => none
      else
        none

@[inline] def fromState (s : OracleState) : SaveData :=
  let (w, h) := s.config.worldSize
  let playerId := s.playerId
  let objects : Array (UInt32 × GameObject) :=
    if s.objects.isEmpty then
      let player := OracleState.getPlayerOrDefault s
      #[(playerId, playerObject player)]
    else
      s.objects
  let world : WorldSaveData :=
    { area := (w, h)
      materials := materialsFromWorld s.world
      objects := objects
      playerId := playerId
      daylight := s.world.daylight
      rngSeed := s.world.rngSeed }
  { version := currentVersion
    timestamp := 0
    name := none
    config := s.config
    episode := UInt32.ofNat s.episode
    step := UInt64.ofNat s.step
    rngState := rngStateBytes 0
    world := world }

@[inline] def toState (ss : SaveData) : Except String OracleState := do
  if ss.version != currentVersion then
    throw s!"unsupported save version {ss.version}"
  let (w, h) := ss.world.area
  let cfg := { ss.config with worldSize := (w, h) }
  let playerOpt := selectPlayerFromObjects ss.world.playerId ss.world.objects
  let player := playerOpt.getD (Player.default (OracleState.initialPlayerPos cfg))
  let objects :=
    match playerOpt with
    | some _ => ss.world.objects
    | none => ss.world.objects.push (ss.world.playerId, GameObject.Player player)
  let expected := w * h
  let data :=
    if ss.world.materials.size == expected then
      ss.world.materials
    else
      Array.replicate expected Material.Grass
  let world : World :=
    { materials := { width := w, height := h, data := data }
      daylight := ss.world.daylight
      rngSeed := ss.world.rngSeed }
  let seed := cfg.seed.getD 0
  let rng := ChaCha8Rng.seedFromU64 (seed + ss.step)
  return {
    config := cfg,
    rng := rng,
    step := UInt64.toNat ss.step,
    episode := UInt32.toNat ss.episode,
    world := world,
    objects := objects,
    playerId := ss.world.playerId
  }

@[inline] def save (s : OracleState) : ByteArray :=
  encodeToJson (fromState s)

@[inline] def load (b : ByteArray) : Except String OracleState := do
  let ss <- decodeFromJson (α := SaveData) b
  toState ss

end SaveLoad

end Crafter
