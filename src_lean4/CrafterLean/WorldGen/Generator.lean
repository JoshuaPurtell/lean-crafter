import CrafterLean.Config
import CrafterLean.Entity
import CrafterLean.Inventory
import CrafterLean.World.Material
import CrafterLean.World.Types
import CrafterLean.Types.RNG
import CrafterLean.Types.Grid
import CrafterLean.Types.ChaCha8
import CrafterLean.Craftax.Mobs
import CrafterLean.WorldGen.OpenSimplex

namespace Crafter

namespace WorldGen

open OpenSimplex

set_option maxHeartbeats 800000

structure GenState where
  rng : ChaCha8Rng
  world : World
  objects : Array (UInt32 × GameObject)
  nextId : UInt32
  tunnels : Grid Bool

@[inline] def floatMin (a b : Float) : Float := if a <= b then a else b

@[inline] def scaledThreshold (base : Float) (mult : Float) : Float :=
  let mult := if mult < 0.0 then 0.0 else mult
  let prob := floatMin (base * mult) 1.0
  1.0 - prob

@[inline] def scaledProbability (base : Float) (mult : Float) : Float :=
  let mult := if mult < 0.0 then 0.0 else mult
  floatMin (base * mult) 1.0

@[inline] def rngNextFloat64 (s : GenState) : GenState × Float :=
  let (rng', value) := s.rng.nextFloat
  ({ s with rng := rng' }, value)

@[inline] def rngNextFloat32 (s : GenState) : GenState × Float :=
  let (rng', value) := s.rng.nextFloat32
  ({ s with rng := rng' }, value)

@[inline] def simplex3 (simp : OpenSimplex.PermutationTable) (x y z : Float) (sizes : Array (Float × Float)) (normalize : Bool) : Float :=
  let (sum, total) := sizes.foldl (init := (0.0, 0.0)) fun acc entry =>
    let (accum, tot) := acc
    let (size, weight) := entry
    let n := OpenSimplex.openSimplex3 { x := x / size, y := y / size, z := z } simp
    (accum + weight * n, tot + weight)
  if normalize && total > 0.0 then sum / total else sum

@[inline] def simplex3Single (simp : OpenSimplex.PermutationTable) (x y z size : Float) : Float :=
  OpenSimplex.openSimplex3 { x := x / size, y := y / size, z := z } simp

@[inline] def placeObject (s : GenState) (obj : GameObject) : GenState :=
  let id := s.nextId
  { s with objects := s.objects.push (id, obj), nextId := id + 1 }

def generate (cfg : SessionConfig) (seed : UInt64) : World × Array (UInt32 × GameObject) × UInt32 :=
  let (w, h) := cfg.worldSize
  let playerPos : Coord := { x := Int.ofNat (w / 2), y := Int.ofNat (h / 2) }
  let simplex := OpenSimplex.PermutationTable.new (UInt32.ofNat seed.toNat)
  let world := { materials := Grid.init w h Material.Grass, daylight := 0.5, rngSeed := seed }
  let tunnels := Grid.init w h false
  let state0 : GenState :=
    { rng := ChaCha8Rng.seedFromU64 seed
      , world := world
      , objects := #[]
      , nextId := UInt32.ofNat 1
      , tunnels := tunnels }

  let distFromPlayer := fun (x y : Float) =>
    let dx := x - Float.ofInt playerPos.x
    let dy := y - Float.ofInt playerPos.y
    Float.sqrt (dx * dx + dy * dy)

  let generateMountainMaterial : Float → Float → Float → GenState → Material × Bool × GenState :=
    fun x y mountain s =>
      if simplex3Single simplex x y 6.0 7.0 > 0.15 && mountain > 0.3 then
        (Material.Path, false, s)
      else if simplex3Single simplex (2.0 * x) (y / 5.0) 7.0 3.0 > 0.4 then
        (Material.Path, true, s)
      else if simplex3Single simplex (x / 5.0) (2.0 * y) 7.0 3.0 > 0.4 then
        (Material.Path, true, s)
      else
        let coalThreshold := scaledThreshold 0.30 cfg.coalDensity
        let (s, rollCoal) := rngNextFloat64 s
        if simplex3Single simplex x y 1.0 8.0 > 0.0 && rollCoal > coalThreshold then
          (Material.Coal, false, s)
        else
          let ironThreshold := scaledThreshold 0.30 cfg.ironDensity
          let (s, rollIron) := rngNextFloat64 s
          if simplex3Single simplex x y 2.0 6.0 > 0.3 && rollIron > ironThreshold then
            (Material.Iron, false, s)
          else
            let diamondThreshold := scaledThreshold 0.016 cfg.diamondDensity
            let (s, rollDiamond) := rngNextFloat64 s
            if mountain > 0.18 && rollDiamond > diamondThreshold then
              (Material.Diamond, false, s)
            else if mountain > 0.3 && simplex3Single simplex x y 6.0 5.0 > 0.35 then
              (Material.Lava, false, s)
            else
              (Material.Stone, false, s)

  let generateGrasslandMaterial : Float → Float → GenState → Material × GenState :=
    fun x y s =>
      let treeThreshold := scaledThreshold 0.2 cfg.treeDensity
      let (s, rollTree) := rngNextFloat64 s
      if simplex3Single simplex x y 5.0 7.0 > 0.0 && rollTree > treeThreshold then
        (Material.Tree, s)
      else
        (Material.Grass, s)

  let getTerrainMaterial : Float → Float → GenState → Material × Bool × GenState :=
    fun x y s =>
      let dist := distFromPlayer x y
      let start0 := 4.0 - dist
      let start1 := start0 + 2.0 * simplex3Single simplex x y 8.0 3.0
      let start := 1.0 / (1.0 + Float.exp (-start1))
      let water0 := simplex3 simplex x y 3.0 #[(15.0, 1.0), (5.0, 0.15)] false + 0.1
      let water := water0 - 2.0 * start
      let mountain0 := simplex3 simplex x y 0.0 #[(15.0, 1.0), (5.0, 0.3)] true
      let mountain := mountain0 - 4.0 * start - 0.3 * water
      if start > 0.5 then
        (Material.Grass, false, s)
      else if mountain > 0.15 then
        generateMountainMaterial x y mountain s
      else if water > 0.25 && water <= 0.35 && simplex3Single simplex x y 4.0 9.0 > -0.2 then
        (Material.Sand, false, s)
      else if water > 0.3 then
        (Material.Water, false, s)
      else
        let (mat, s) := generateGrasslandMaterial x y s
        (mat, false, s)

  let rec loopY (y : Nat) (s : GenState) : GenState :=
    if y >= h then
      s
    else
      let rec loopX (x : Nat) (s : GenState) : GenState :=
        if x >= w then
          s
        else
          let fx := Float.ofNat x
          let fy := Float.ofNat y
          let (mat, isTunnel, s) := getTerrainMaterial fx fy s
          let world := { s.world with materials := s.world.materials.set x y mat }
          let tunnels := s.tunnels.set x y isTunnel
          loopX (x + 1) { s with world := world, tunnels := tunnels }
      loopY (y + 1) (loopX 0 s)

  let state1 := loopY 0 state0

  let player := Player.default playerPos
  let playerId := state1.nextId
  let state2 := { state1 with objects := state1.objects.push (playerId, GameObject.Player player), nextId := playerId + 1 }

  let rec spawnY (y : Nat) (s : GenState) : GenState :=
    if y >= h then
      s
    else
      let rec spawnX (x : Nat) (s : GenState) : GenState :=
        if x >= w then
          s
        else
          let pos : Coord := { x := Int.ofNat x, y := Int.ofNat y }
          let dist := distFromPlayer (Float.ofNat x) (Float.ofNat y)
          let mat := s.world.getMaterial pos |>.getD Material.Water
          if !mat.isWalkable then
            spawnX (x + 1) s
          else
            let cowThreshold := scaledThreshold 0.015 cfg.cowDensity
            let (s, rollCow) := rngNextFloat64 s
            let s :=
              if mat == Material.Grass && dist > 3.0 && rollCow > cowThreshold then
                let cow := Cow.withHealth pos (UInt8.ofNat cfg.cowHealth)
                placeObject s (GameObject.Cow cow)
              else
                s
            let zombieThreshold := scaledThreshold 0.007 cfg.zombieDensity
            let (s, rollZombie) := rngNextFloat64 s
            let s :=
              if dist > 10.0 && rollZombie > zombieThreshold then
                let zombie := Zombie.new pos (UInt8.ofNat cfg.zombieHealth)
                placeObject s (GameObject.Zombie zombie)
              else
                s
            let skeletonThreshold := scaledThreshold 0.05 cfg.skeletonDensity
            let (s, rollSkeleton) := rngNextFloat64 s
            let s :=
              if mat == Material.Path && (s.tunnels.get? x y).getD false && rollSkeleton > skeletonThreshold then
                let skeleton := Skeleton.new pos (UInt8.ofNat cfg.skeletonHealth)
                placeObject s (GameObject.Skeleton skeleton)
              else
                s
            spawnX (x + 1) s
      spawnY (y + 1) (spawnX 0 s)

  let state3 := spawnY 0 state2

  let state4 :=
    if cfg.craftax.enabled && cfg.craftax.worldgenEnabled then
      Id.run do
        let mut s := state3
        for y in [0:h] do
          for x in [0:w] do
            let pos : Coord := { x := Int.ofNat x, y := Int.ofNat y }
            let dist := distFromPlayer (Float.ofNat x) (Float.ofNat y)
            let mat := s.world.getMaterial pos |>.getD Material.Water
            if cfg.craftax.itemsEnabled && mat == Material.Stone then
              let (s', rollSapphire) := rngNextFloat32 s
              s := s'
              if rollSapphire < scaledProbability 0.004 cfg.craftax.spawn.sapphireDensity then
                let world := { s.world with materials := s.world.materials.set x y Material.Sapphire }
                s := { s with world := world }
              else
                let (s', rollRuby) := rngNextFloat32 s
                s := s'
                if rollRuby < scaledProbability 0.003 cfg.craftax.spawn.rubyDensity then
                  let world := { s.world with materials := s.world.materials.set x y Material.Ruby }
                  s := { s with world := world }
            if cfg.craftax.itemsEnabled && cfg.craftax.chestsEnabled && dist > 6.0
              && (mat == Material.Grass || mat == Material.Path) then
              let (s', rollChest) := rngNextFloat32 s
              s := s'
              if rollChest < scaledProbability 0.002 cfg.craftax.spawn.chestDensity then
                let world := { s.world with materials := s.world.materials.set x y Material.Chest }
                s := { s with world := world }
            if cfg.craftax.mobsEnabled then
              if mat == Material.Grass && dist > 4.0 then
                let (s', rollSnail) := rngNextFloat32 s
                s := s'
                if rollSnail < scaledProbability 0.01 cfg.craftax.spawn.snailDensity then
                  let stats := craftaxStats CraftaxMobKind.Snail
                  let mob := CraftaxMob.new CraftaxMobKind.Snail pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
              if mat == Material.Path && (s.tunnels.get? x y).getD false then
                let (s', rollBat) := rngNextFloat32 s
                s := s'
                if rollBat < scaledProbability 0.02 cfg.craftax.spawn.batDensity then
                  let stats := craftaxStats CraftaxMobKind.Bat
                  let mob := CraftaxMob.new CraftaxMobKind.Bat pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
              if dist > 10.0 && mat.isWalkable then
                let (s', rollOrc) := rngNextFloat32 s
                s := s'
                if rollOrc < scaledProbability 0.004 cfg.craftax.spawn.orcSoldierDensity then
                  let stats := craftaxStats CraftaxMobKind.OrcSoldier
                  let mob := CraftaxMob.new CraftaxMobKind.OrcSoldier pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
                let (s', rollMage) := rngNextFloat32 s
                s := s'
                if rollMage < scaledProbability 0.003 cfg.craftax.spawn.orcMageDensity then
                  let stats := craftaxStats CraftaxMobKind.OrcMage
                  let mob := CraftaxMob.new CraftaxMobKind.OrcMage pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
                let (s', rollKnight) := rngNextFloat32 s
                s := s'
                if rollKnight < scaledProbability 0.003 cfg.craftax.spawn.knightDensity then
                  let stats := craftaxStats CraftaxMobKind.Knight
                  let mob := CraftaxMob.new CraftaxMobKind.Knight pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
                let (s', rollArcher) := rngNextFloat32 s
                s := s'
                if rollArcher < scaledProbability 0.003 cfg.craftax.spawn.knightArcherDensity then
                  let stats := craftaxStats CraftaxMobKind.KnightArcher
                  let mob := CraftaxMob.new CraftaxMobKind.KnightArcher pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
                let (s', rollTroll) := rngNextFloat32 s
                s := s'
                if mat == Material.Lava || rollTroll < scaledProbability 0.002 cfg.craftax.spawn.trollDensity then
                  let stats := craftaxStats CraftaxMobKind.Troll
                  let mob := CraftaxMob.new CraftaxMobKind.Troll pos stats.health
                  s := placeObject s (GameObject.CraftaxMob mob)
        return s
    else
      state3

  (state4.world, state4.objects, playerId)

end WorldGen

end Crafter
