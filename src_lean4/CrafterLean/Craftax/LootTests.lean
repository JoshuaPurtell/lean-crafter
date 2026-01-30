import Std.Tactic
import CrafterLean.Craftax.Loot

namespace Crafter

namespace CraftaxLootTests

@[inline] def potionCount (loot : ChestLoot) : Nat :=
  loot.potionRed.toNat
    + loot.potionGreen.toNat
    + loot.potionBlue.toNat
    + loot.potionPink.toNat
    + loot.potionCyan.toNat
    + loot.potionYellow.toNat

@[inline] def gemCount (loot : ChestLoot) : Nat :=
  loot.sapphire.toNat + loot.ruby.toNat

def testLootRanges : Bool :=
  let cfg : CraftaxLootConfig :=
    { potionDropChance := 1.0, arrowDropChance := 1.0, gemDropChance := 1.0 }
  let (_, loot) := CraftaxLoot.rollChestLoot (ChaCha8Rng.seedFromU64 7) cfg
  let arrows := loot.arrows.toNat
  let potions := potionCount loot
  let gems := gemCount loot
  decide (arrows >= 2) && decide (arrows <= 6) && (potions == 1) && (gems == 1)

def testZeroDrop : Bool :=
  let cfg : CraftaxLootConfig :=
    { potionDropChance := 0.0, arrowDropChance := 0.0, gemDropChance := 0.0 }
  let (_, loot) := CraftaxLoot.rollChestLoot (ChaCha8Rng.seedFromU64 1) cfg
  let potions := potionCount loot
  let gems := gemCount loot
  (potions == 0) && (loot.arrows.toNat == 0) && (gems == 0)

example : testLootRanges = true := by
  native_decide

example : testZeroDrop = true := by
  native_decide

end CraftaxLootTests

end Crafter
