import Std.Tactic
import CrafterLean.Action
import CrafterLean.Achievement
import CrafterLean.Config
import CrafterLean.Craftax.Loot
import CrafterLean.Craftax.Mobs
import CrafterLean.Entity
import CrafterLean.Inventory
import CrafterLean.Oracle.SaveLoad
import CrafterLean.Oracle.State
import CrafterLean.Oracle.Step
import CrafterLean.Serialize
import CrafterLean.Types.ChaCha8
import CrafterLean.World.Material
import CrafterLean.World.Types
import CrafterLean.WorldGen.Generator

namespace Crafter

namespace PropertiesTests

open Oracle

@[inline] def seeds (n : Nat) : List Nat :=
  List.range n

@[inline] def cfgBase : SessionConfig :=
  SessionConfig.default

@[inline] def cfgCraftax : SessionConfig :=
  { SessionConfig.default with craftax := { CraftaxConfig.default with enabled := true, xpEnabled := true, achievementsEnabled := true, itemsEnabled := true, combatEnabled := true } }

@[inline] def mkState (cfg : SessionConfig) (seed : UInt64) : OracleState :=
  OracleState.new cfg seed

@[inline] def potionCount (loot : CraftaxLoot.ChestLoot) : Nat :=
  loot.potionRed.toNat
    + loot.potionGreen.toNat
    + loot.potionBlue.toNat
    + loot.potionPink.toNat
    + loot.potionCyan.toNat
    + loot.potionYellow.toNat

@[inline] def gemCount (loot : CraftaxLoot.ChestLoot) : Nat :=
  loot.sapphire.toNat + loot.ruby.toNat

@[inline] def boolImp (a b : Bool) : Bool :=
  (!a) || b

@[inline] def growN : Nat → Plant → Plant
  | 0, p => p
  | Nat.succ n, p => growN n (p.grow)

@[inline] def findObj? (objs : Array (UInt32 × GameObject)) (p : UInt32 × GameObject → Bool) : Option (UInt32 × GameObject) :=
  objs.toList.find? p

def prop01_loot_invariants : Bool :=
  let cfgAll : CraftaxLootConfig :=
    { potionDropChance := 1.0, arrowDropChance := 1.0, gemDropChance := 1.0 }
  let cfgNone : CraftaxLootConfig :=
    { potionDropChance := 0.0, arrowDropChance := 0.0, gemDropChance := 0.0 }
  let checkOne (seed : Nat) : Bool :=
    let (_, loot) := CraftaxLoot.rollChestLoot (ChaCha8Rng.seedFromU64 seed.toUInt64) cfgAll
    let arrows := loot.arrows.toNat
    let potions := potionCount loot
    let gems := gemCount loot
    let coal := loot.coal.toNat
    let iron := loot.iron.toNat
    let diamond := loot.diamond.toNat
    let gemOk := (gems == 0) || (gems == 1 && loot.sapphire.toNat + loot.ruby.toNat == 1)
    let arrowsOk := arrows == 0 || (arrows >= 2 && arrows <= 6)
    let potionsOk := potions <= 1
    let coalOk := coal == 0 || (coal >= 1 && coal <= 2)
    let ironOk := iron == 0 || (iron >= 1 && iron <= 2)
    let diamondOk := diamond == 0 || diamond == 1
    let ( _, loot0) := CraftaxLoot.rollChestLoot (ChaCha8Rng.seedFromU64 (seed + 1).toUInt64) cfgNone
    let potions0 := potionCount loot0
    let gems0 := gemCount loot0
    (potionsOk && arrowsOk && gemOk)
      && (coalOk && ironOk && diamondOk)
      && (potions0 == 0)
      && (loot0.arrows.toNat == 0)
      && (gems0 == 0)
  (seeds 20).all checkOne

def prop02_xp_invariants : Bool :=
  let cfg := cfgCraftax
  let s0 := mkState cfg 7
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with xp := 9, level := 0, statPoints := 0 } }
  let s1 := OracleState.setPlayer s0 p0
  let s2 := grantXp s1 1
  let p2 := OracleState.getPlayerOrDefault s2
  let monotone :=
    p2.inventory.xp.toNat >= p0.inventory.xp.toNat
      && p2.inventory.level.toNat >= p0.inventory.level.toNat
      && p2.inventory.statPoints.toNat >= p0.inventory.statPoints.toNat
  let leveled := p2.inventory.level.toNat == 1 && p2.inventory.statPoints.toNat == 1
  let achOk := p2.achievements.gainXp.toNat == 1 && p2.achievements.reachLevel.toNat == 1
  let sNo := OracleState.setPlayer s0 { p0 with inventory := { p0.inventory with xp := 5, level := 0, statPoints := 0 } }
  let sNo := grantXp sNo 4
  let pNo := OracleState.getPlayerOrDefault sNo
  let noLevel := pNo.inventory.level.toNat == 0 && pNo.inventory.statPoints.toNat == 0
  let cfgNoAch := { cfg with craftax := { cfg.craftax with achievementsEnabled := false } }
  let sNoAch := mkState cfgNoAch 7
  let pNoAch := OracleState.getPlayerOrDefault sNoAch
  let sNoAch := OracleState.setPlayer sNoAch { pNoAch with inventory := { pNoAch.inventory with xp := 9, level := 0, statPoints := 0 } }
  let sNoAch := grantXp sNoAch 1
  let pNoAch2 := OracleState.getPlayerOrDefault sNoAch
  let achDisabled :=
    pNoAch2.achievements.gainXp == pNoAch.achievements.gainXp
      && pNoAch2.achievements.reachLevel == pNoAch.achievements.reachLevel
  let cfgOff := { cfg with craftax := { cfg.craftax with enabled := false } }
  let s3 := mkState cfgOff 7
  let p3 := OracleState.getPlayerOrDefault s3
  let s4 := grantXp s3 5
  let p4 := OracleState.getPlayerOrDefault s4
  monotone && leveled && achOk && noLevel && achDisabled && (p4.inventory.xp == p3.inventory.xp)

def prop03_damage_reduction : Bool :=
  let cfg := { cfgBase with healthEnabled := true }
  let s0 := mkState cfg 1
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with health := 9 } }
  let s1 := OracleState.setPlayer s0 p0
  let s2 := applyPlayerDamageWithReduction s1 DamageSource.Zombie 2.0 1.0 5.0
  let p2 := OracleState.getPlayerOrDefault s2
  let damage0 := 2.0 * 1.0
  let clamped := clampFloat 5.0 0.0 0.9
  let damage := damage0 * (1.0 - clamped)
  let rounded := floatToNatRound damage
  let finalDamage := if rounded == 0 && damage > 0.0 then 1 else rounded
  let s3 := applyPlayerDamageWithReduction s1 DamageSource.Zombie 0.1 1.0 0.0
  let p3 := OracleState.getPlayerOrDefault s3
  let minDamageOk := p3.inventory.health.toNat + 1 == p0.inventory.health.toNat
  let clampOk := clamped >= 0.0 && clamped <= 0.9
  (p2.inventory.health.toNat + finalDamage) == p0.inventory.health.toNat && minDamageOk && clampOk

def prop04_arrow_interactions : Bool :=
  let cfg := { cfgBase with worldSize := (6, 6) }
  let s0 := mkState cfg 1
  let p := OracleState.getPlayerOrDefault s0
  let tablePos := Coord.add p.pos 1 0
  let arrowPos := Coord.add tablePos (-1) 0
  let arrow := Arrow.withStats arrowPos (1, 0) ProjectileKind.Arrow 1 DamageSource.PlayerArrow
  let s1 := { s0 with world := s0.world.setMaterial tablePos Material.Table }
  let (s2, arrowId) := s1.addObject (GameObject.Arrow arrow)
  let s3 := processArrows s2
  let tableOk := s3.world.getMaterial tablePos == some Material.Path
  let arrowGone := s3.getObject? arrowId |>.isNone
  let edge := { x := 0, y := 0 }
  let outArrow := Arrow.withStats edge (-1, 0) ProjectileKind.Arrow 1 DamageSource.Arrow
  let (s4, outId) := s3.addObject (GameObject.Arrow outArrow)
  let s5 := processArrows s4
  let outGone := s5.getObject? outId |>.isNone
  let waterPos := { x := 2, y := 2 }
  let waterArrow := Arrow.withStats { x := 1, y := 2 } (1, 0) ProjectileKind.Arrow 1 DamageSource.Arrow
  let s6 := { s5 with world := s5.world.setMaterial waterPos Material.Water }
  let (s7, waterId) := s6.addObject (GameObject.Arrow waterArrow)
  let s8 := processArrows s7
  let waterPass :=
    match s8.getObject? waterId with
    | some (GameObject.Arrow a) => a.pos == waterPos
    | _ => false
  tableOk && arrowGone && outGone && waterPass

def prop05_mob_ai_bounds : Bool :=
  let s0 := mkState cfgBase 3
  let (_, dir) := randomDirection s0
  let dirOk := dir == (0, 1) || dir == (0, -1) || dir == (1, 0) || dir == (-1, 0)
  let skel := Skeleton.new { x := 0, y := 0 } 3
  let skelOk := (Skeleton.resetReload skel).reload.toNat == 4
  let zombie := Zombie.new { x := 0, y := 0 } 3
  let cooldownOk := zombie.cooldown.toNat <= 5
  dirOk && skelOk && cooldownOk

def prop06_spawn_despawn : Bool :=
  let cfg := { cfgBase with cowDespawnRate := 1.0, zombieDespawnRate := 1.0 }
  let s0 := mkState cfg 1
  let p := OracleState.getPlayerOrDefault s0
  let far := Coord.add p.pos 40 0
  let near := Coord.add p.pos 5 0
  let cowFar := Cow.new far
  let cowNear := Cow.new near
  let (s1, idFar) := s0.addObject (GameObject.Cow cowFar)
  let (s2, idNear) := s1.addObject (GameObject.Cow cowNear)
  let s3 := spawnDespawnMobs s2
  let farRemoved := (s3.getObject? idFar).isNone
  let nearKept := (s3.getObject? idNear).isSome
  farRemoved && nearKept

def prop07_terrain_interactions : Bool :=
  let cfg := cfgBase
  let s0 := mkState cfg 5
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with wood := 0, stone := 0, drink := 0 }, thirstCounter := 5.0 }
  let s1 := OracleState.setPlayer s0 p0
  let s2 := interactWithTerrain s1 p0.pos Material.Water p0
  let p2 := OracleState.getPlayerOrDefault s2
  let waterOk := p2.thirstCounter == 0.0 && p2.inventory.drink.toNat <= 1
  let s3 := interactWithTerrain s1 p0.pos Material.Stone p0
  let p3 := OracleState.getPlayerOrDefault s3
  let stoneOk := p3.inventory.stone == p0.inventory.stone
  let pPick := { p0 with inventory := { p0.inventory with woodPickaxe := 1 } }
  let sPick := OracleState.setPlayer s0 pPick
  let sPick := interactWithTerrain sPick pPick.pos Material.Stone pPick
  let pPick' := OracleState.getPlayerOrDefault sPick
  let minedOk := pPick'.inventory.stone.toNat == 1 && sPick.world.getMaterial pPick.pos == some Material.Path
  let sGrass := interactWithTerrain s1 p0.pos Material.Grass p0
  let pGrass := OracleState.getPlayerOrDefault sGrass
  let saplingOk := pGrass.inventory.sapling.toNat <= 1
  waterOk && stoneOk && minedOk && saplingOk

def prop08_plants : Bool :=
  let p := Plant.new { x := 0, y := 0 }
  let p' := p.grow
  let monotone := p'.grown.toNat >= p.grown.toNat
  let p300 := growN 300 p
  let p350 := growN 350 p
  let ripe := p300.isRipe && p350.isRipe
  let capped := p350.grown.toNat <= 300
  monotone && ripe && capped

def prop09_rng_ranges : Bool :=
  let rng := ChaCha8Rng.seedFromU64 1
  let (rng, v0) := rng.nextNat 0
  let (_, v1) := rng.nextNat 10
  let (_, f) := rng.nextFloat32
  v0 == 0 && v1 < 10 && f >= 0.0 && f < 1.0

def prop10_serialization_roundtrip : Bool :=
  let cfg := cfgBase
  let s0 := mkState cfg 9
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with wood := 7, stone := 2, iron := 1 }, pos := { x := 3, y := 4 } }
  let s0 := OracleState.setPlayer s0 p0
  match SaveLoad.load (SaveLoad.save s0) with
  | Except.error _ => false
  | Except.ok s1 =>
      let w0 := s0.world.materials
      let w1 := s1.world.materials
      let p1 := OracleState.getPlayerOrDefault s1
      let invOk :=
        p1.inventory.wood == p0.inventory.wood
          && p1.inventory.stone == p0.inventory.stone
          && p1.inventory.iron == p0.inventory.iron
      w0.width == w1.width && w0.height == w1.height && p0.pos == p1.pos && invOk

def prop11_inventory : Bool :=
  let inv := Inventory.default
  let inv1 := inv.addWood 250
  let inv2 := inv.addWood 10
  let capOk := inv1.wood.toNat <= Inventory.maxValue && inv2.wood.toNat <= Inventory.maxValue
  let subOk := (Inventory.subOrZero 0 3) == 0
  let inv3 := { inv with woodPickaxe := 1, stoneSword := 1, armorHelmet := 1, armorChestplate := 1 }
  let tiersOk := inv3.bestPickaxeTier.toNat == 1 && inv3.bestSwordTier.toNat == 2
  let armor := inv3.armorReduction
  let invWood := { inv with woodSword := 1 }
  let invStone := { inv with stoneSword := 1 }
  let invIron := { inv with ironSword := 1 }
  let invDiamond := { inv with diamondSword := 1 }
  let dmgOk :=
    inv.attackDamage.toNat == 1
      && invWood.attackDamage.toNat == 2
      && invStone.attackDamage.toNat == 3
      && invIron.attackDamage.toNat == 5
      && invDiamond.attackDamage.toNat == 8
  let maxU32 := UInt32.ofNat 0xFFFF_FFFF
  let invXp := inv.addXp maxU32
  let xpOk := invXp.xp == maxU32
  capOk && subOk && tiersOk && armor >= 0.0 && armor <= 0.4 && dmgOk && xpOk

def prop12_crafting : Bool :=
  let cfg := cfgBase
  let s0 := mkState cfg 2
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with wood := 1 } }
  let s1 := OracleState.setPlayer s0 p0
  let sNoTable := processCraftWoodPickaxe s1
  let pNo := OracleState.getPlayerOrDefault sNoTable
  let noCraft := pNo.inventory.wood == p0.inventory.wood && pNo.inventory.woodPickaxe == p0.inventory.woodPickaxe
  let tablePos := Coord.add p0.pos 1 0
  let sTable := { s1 with world := s1.world.setMaterial tablePos Material.Table }
  let sYes := processCraftWoodPickaxe sTable
  let pYes := OracleState.getPlayerOrDefault sYes
  let didCraft := pYes.inventory.wood.toNat == 0 && pYes.inventory.woodPickaxe.toNat == 1
  noCraft && didCraft

def prop13_action_invariants : Bool :=
  let cfg := { cfgCraftax with worldSize := (8, 8) }
  let s0 := mkState cfg 3
  let p0 := OracleState.getPlayerOrDefault s0
  let blockPos := Coord.add p0.pos 1 0
  let frontPos := Coord.add p0.pos 0 1
  let world1 := s0.world.setMaterial blockPos Material.Tree
  let world1 := world1.setMaterial frontPos Material.Path
  let s1 := { s0 with world := world1 }
  let s2 := applyMovement s1 1 0
  let p2 := OracleState.getPlayerOrDefault s2
  let facingOk := p2.facing == (1, 0)
  let posUnchanged := p2.pos == p0.pos
  let s3 := processPlayerAction s1 Action.Sleep
  let p3 := OracleState.getPlayerOrDefault s3
  let pBow := { p0 with inventory := { p0.inventory with bow := 1, arrows := 2 } }
  let sBow := OracleState.setPlayer s1 pBow
  let sBow := processShootArrow sBow
  let pBow' := OracleState.getPlayerOrDefault sBow
  let arrowsOk := pBow'.inventory.arrows.toNat == 1
  facingOk && posUnchanged && p3.sleeping && arrowsOk

def prop14_life_stats : Bool :=
  let p0 := Player.default { x := 0, y := 0 }
  let p0 := { p0 with inventory := { p0.inventory with health := 1, food := 1, drink := 1 }, hungerCounter := 0.9 }
  let p1 := p0.updateLifeStats true false false false 1.0 1.0
  let maxHealth := UInt8.ofNat Inventory.maxValue
  let healthOk := p1.inventory.health == maxHealth
  let p2 := p0.updateLifeStats true false false true 1.0 1.0
  let hungerOk := p2.hungerCounter == 0.0 || p2.hungerCounter == 0.9
  healthOk && hungerOk

def prop15_daylight : Bool :=
  let cfgOff := { cfgBase with dayNightCycle := false }
  let s0 := mkState cfgOff 1
  let s1 := updateDaylight s0
  let same := s1.world.daylight == s0.world.daylight
  let cfgOn := { cfgBase with dayNightCycle := true, dayCyclePeriod := 300 }
  let s2 := updateDaylight (mkState cfgOn 1)
  let dayOk := s2.world.daylight >= 0.0 && s2.world.daylight <= 1.0
  same && dayOk

def prop16_worldgen : Bool :=
  let cfg := { cfgBase with worldSize := (32, 32) }
  let (world, objects, playerId) := WorldGen.generate cfg 11
  let areaOk := world.materials.width == 32 && world.materials.height == 32
  let playerOk :=
    match findObj? objects (fun entry => entry.1 == playerId) with
    | some (_, GameObject.Player _) => true
    | _ => false
  let (world2, objects2, playerId2) := WorldGen.generate cfg 11
  let deterministic :=
    decide (world.materials.data.toList = world2.materials.data.toList) && playerId == playerId2 && objects.size == objects2.size
  let spawnWalkable :=
    match findObj? objects (fun entry => entry.1 == playerId) with
    | some (_, GameObject.Player p) =>
        match world.getMaterial p.pos with
        | some mat => mat.isWalkable
        | none => false
    | _ => false
  areaOk && playerOk && deterministic && spawnWalkable

def prop17_object_ids : Bool :=
  let s0 := mkState cfgBase 4
  let ids := s0.objects.toList.map (fun entry => entry.1)
  let unique := ids.eraseDups.length == ids.length
  let playerOk :=
    match s0.getObject? s0.playerId with
    | some (GameObject.Player _) => true
    | _ => false
  let idAt :=
    match OracleState.getPlayer? s0 with
    | some p => s0.getObjectIdAt? p.pos == some s0.playerId
    | none => false
  unique && playerOk && idAt

def prop18_movement_collision : Bool :=
  let s0 := mkState cfgBase 5
  let p0 := OracleState.getPlayerOrDefault s0
  let next := Coord.add p0.pos 1 0
  let canMove := canMoveTo s0 next
  let inBounds := s0.world.inBounds next
  let walkable :=
    match s0.world.getMaterial next with
    | some mat => mat.isWalkable
    | none => false
  let allowed := boolImp canMove (inBounds && walkable)
  let s1 := applyMovement s0 0 1
  let p1 := OracleState.getPlayerOrDefault s1
  let moved := p1.pos == Coord.add p0.pos 0 1 || p1.pos == p0.pos
  allowed && moved

def prop19_damage_sources : Bool :=
  let p0 := Player.default { x := 0, y := 0 }
  let (p1, _) := p0.applyDamage DamageSource.Zombie 1
  let (p2, _) := p0.applyDamage DamageSource.Lava 1
  p1.lastDamageSource == some DamageSource.Zombie && p2.lastDamageSource == some DamageSource.Lava

def prop20_craftax_mobs : Bool :=
  let kinds := [
    CraftaxMobKind.OrcSoldier,
    CraftaxMobKind.OrcMage,
    CraftaxMobKind.Knight,
    CraftaxMobKind.KnightArcher,
    CraftaxMobKind.Troll,
    CraftaxMobKind.Bat,
    CraftaxMobKind.Snail
  ]
  kinds.all fun k =>
    let stats := craftaxStats k
    let mob := CraftaxMob.new k { x := 0, y := 0 } stats.health
    let hostileOk := boolImp mob.isHostile (stats.meleeDamage.toNat > 0 || stats.rangedDamage.toNat > 0)
    let passiveOk := boolImp mob.isPassive (stats.meleeDamage.toNat == 0 && stats.rangedDamage.toNat == 0)
    hostileOk && passiveOk

def prop21_plant_place : Bool :=
  let cfg := cfgBase
  let s0 := mkState cfg 6
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with sapling := 1 } }
  let s1 := OracleState.setPlayer s0 p0
  let target := Coord.add p0.pos 1 0
  let sGrass := { s1 with world := s1.world.setMaterial target Material.Grass }
  let s2 := processPlacePlant sGrass
  let planted :=
    match s2.getObjectAt? target with
    | some (GameObject.Plant _) => true
    | _ => false
  let sStone := { s1 with world := s1.world.setMaterial target Material.Stone }
  let s3 := processPlacePlant sStone
  let notPlanted :=
    match s3.getObjectAt? target with
    | some (GameObject.Plant _) => false
    | _ => true
  planted && notPlanted

def prop22_arrow_damage : Bool :=
  let cfg := { cfgCraftax with craftax := { cfgCraftax.craftax with itemsEnabled := true, combatEnabled := true } }
  let s0 := mkState cfg 7
  let p0 := OracleState.getPlayerOrDefault s0
  let p0 := { p0 with inventory := { p0.inventory with bow := 1, arrows := 1, woodSword := 1 } }
  let s1 := OracleState.setPlayer s0 p0
  let s2 := processShootArrow s1
  let arrow :=
    findObj? s2.objects (fun entry => match entry.2 with | GameObject.Arrow _ => true | _ => false)
  match arrow with
  | some (_, GameObject.Arrow a) => a.damage.toNat == 2 + p0.inventory.bestSwordTier.toNat
  | _ => false

def prop23_rng_determinism : Bool :=
  let rng1 := ChaCha8Rng.seedFromU64 42
  let rng2 := ChaCha8Rng.seedFromU64 42
  let (rng1, a1) := rng1.nextNat 100
  let (rng2, b1) := rng2.nextNat 100
  let (_, a2) := rng1.nextNat 100
  let (_, b2) := rng2.nextNat 100
  a1 == b1 && a2 == b2

def prop24_config_invariants : Bool :=
  let cfgOff := { cfgBase with craftax := { CraftaxConfig.default with enabled := false, xpEnabled := false } }
  let s0 := mkState cfgOff 1
  let p0 := OracleState.getPlayerOrDefault s0
  let s1 := grantXp s0 10
  let p1 := OracleState.getPlayerOrDefault s1
  let xpOk := p1.inventory.xp == p0.inventory.xp
  let sShoot := processShootArrow s0
  let arrowsOk := sShoot.objects.size == s0.objects.size
  let cfgNoHealth := { cfgBase with healthEnabled := false, maxSteps := none }
  let done := stepDone cfgNoHealth 0 false
  xpOk && arrowsOk && (done == false)

def prop25_observation : Bool :=
  let s0 := mkState cfgBase 3
  let obs := observe s0
  let p0 := OracleState.getPlayerOrDefault s0
  obs.playerPos == p0.pos && obs.step == s0.step

def prop26_save_load : Bool :=
  let cfg := { cfgBase with craftax := { CraftaxConfig.default with enabled := true, achievementsEnabled := true } }
  let s0 := mkState cfg 12
  match SaveLoad.load (SaveLoad.save s0) with
  | Except.error _ => false
  | Except.ok s1 =>
      let sameCfg := s1.config.craftax.enabled == s0.config.craftax.enabled
      let sameObjs := s1.objects.size == s0.objects.size
      let sameCounters := s1.step == s0.step && s1.episode == s0.episode
      let sameDaylight := s1.world.daylight == s0.world.daylight
      sameCfg && sameObjs && sameCounters && sameDaylight

example : prop01_loot_invariants = true := by native_decide
example : prop02_xp_invariants = true := by native_decide
example : prop03_damage_reduction = true := by native_decide
example : prop04_arrow_interactions = true := by native_decide
example : prop05_mob_ai_bounds = true := by native_decide
example : prop06_spawn_despawn = true := by native_decide
example : prop07_terrain_interactions = true := by native_decide
example : prop08_plants = true := by native_decide
example : prop09_rng_ranges = true := by native_decide
example : prop10_serialization_roundtrip = true := by native_decide
example : prop11_inventory = true := by native_decide
example : prop12_crafting = true := by native_decide
example : prop13_action_invariants = true := by native_decide
example : prop14_life_stats = true := by native_decide
example : prop15_daylight = true := by native_decide
example : prop16_worldgen = true := by native_decide
example : prop17_object_ids = true := by native_decide
example : prop18_movement_collision = true := by native_decide
example : prop19_damage_sources = true := by native_decide
example : prop20_craftax_mobs = true := by native_decide
example : prop21_plant_place = true := by native_decide
example : prop22_arrow_damage = true := by native_decide
example : prop23_rng_determinism = true := by native_decide
example : prop24_config_invariants = true := by native_decide
example : prop25_observation = true := by native_decide
example : prop26_save_load = true := by native_decide

end PropertiesTests

end Crafter
