import CrafterLean.Session.Types

namespace Crafter

structure CraftaxSpawnConfig where
  sapphireDensity : Float
  rubyDensity : Float
  chestDensity : Float
  orcSoldierDensity : Float
  orcMageDensity : Float
  knightDensity : Float
  knightArcherDensity : Float
  trollDensity : Float
  batDensity : Float
  snailDensity : Float
  deriving Repr

namespace CraftaxSpawnConfig

def default : CraftaxSpawnConfig :=
  {
    sapphireDensity := 1.0,
    rubyDensity := 1.0,
    chestDensity := 1.0,
    orcSoldierDensity := 1.0,
    orcMageDensity := 1.0,
    knightDensity := 1.0,
    knightArcherDensity := 1.0,
    trollDensity := 1.0,
    batDensity := 1.0,
    snailDensity := 1.0
  }

instance : Inhabited CraftaxSpawnConfig := ⟨default⟩

end CraftaxSpawnConfig

structure CraftaxLootConfig where
  potionDropChance : Float
  arrowDropChance : Float
  gemDropChance : Float
  deriving Repr

namespace CraftaxLootConfig

def default : CraftaxLootConfig :=
  {
    potionDropChance := 0.35,
    arrowDropChance := 0.5,
    gemDropChance := 0.2
  }

instance : Inhabited CraftaxLootConfig := ⟨default⟩

end CraftaxLootConfig

structure CraftaxConfig where
  enabled : Bool
  mobsEnabled : Bool
  worldgenEnabled : Bool
  itemsEnabled : Bool
  combatEnabled : Bool
  chestsEnabled : Bool
  potionsEnabled : Bool
  xpEnabled : Bool
  achievementsEnabled : Bool
  spawn : CraftaxSpawnConfig
  loot : CraftaxLootConfig
  deriving Repr

namespace CraftaxConfig

def default : CraftaxConfig :=
  {
    enabled := false,
    mobsEnabled := true,
    worldgenEnabled := true,
    itemsEnabled := true,
    combatEnabled := true,
    chestsEnabled := true,
    potionsEnabled := true,
    xpEnabled := true,
    achievementsEnabled := true,
    spawn := CraftaxSpawnConfig.default,
    loot := CraftaxLootConfig.default
  }

instance : Inhabited CraftaxConfig := ⟨default⟩

end CraftaxConfig

structure SessionConfig where
  worldSize : Nat × Nat
  seed : Option UInt64
  chunkSize : Nat × Nat
  treeDensity : Float
  coalDensity : Float
  ironDensity : Float
  diamondDensity : Float
  cowDensity : Float
  zombieDensity : Float
  skeletonDensity : Float
  zombieSpawnRate : Float
  zombieDespawnRate : Float
  cowSpawnRate : Float
  cowDespawnRate : Float
  maxSteps : Option Nat
  dayNightCycle : Bool
  dayCyclePeriod : Nat
  hungerEnabled : Bool
  hungerRate : Nat
  thirstEnabled : Bool
  thirstRate : Nat
  fatigueEnabled : Bool
  healthEnabled : Bool
  zombieDamageMult : Float
  arrowDamageMult : Float
  playerDamageMult : Float
  cowHealth : Nat
  zombieHealth : Nat
  skeletonHealth : Nat
  viewRadius : Nat
  fullWorldState : Bool
  timeMode : TimeMode
  defaultTicksPerSecond : Float
  craftax : CraftaxConfig
  deriving Repr

namespace SessionConfig

def default : SessionConfig :=
  {
    worldSize := (64, 64),
    seed := none,
    chunkSize := (12, 12),
    treeDensity := 1.0,
    coalDensity := 1.0,
    ironDensity := 1.0,
    diamondDensity := 1.0,
    cowDensity := 1.0,
    zombieDensity := 1.0,
    skeletonDensity := 1.0,
    zombieSpawnRate := 0.3,
    zombieDespawnRate := 0.4,
    cowSpawnRate := 0.01,
    cowDespawnRate := 0.01,
    maxSteps := some 10000,
    dayNightCycle := true,
    dayCyclePeriod := 300,
    hungerEnabled := true,
    hungerRate := 25,
    thirstEnabled := true,
    thirstRate := 20,
    fatigueEnabled := true,
    healthEnabled := true,
    zombieDamageMult := 1.0,
    arrowDamageMult := 1.0,
    playerDamageMult := 1.0,
    cowHealth := 3,
    zombieHealth := 5,
    skeletonHealth := 3,
    viewRadius := 4,
    fullWorldState := false,
    timeMode := TimeMode.Logical,
    defaultTicksPerSecond := 10.0,
    craftax := CraftaxConfig.default
  }

instance : Inhabited SessionConfig := ⟨default⟩

end SessionConfig

end Crafter
