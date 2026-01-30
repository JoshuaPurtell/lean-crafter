import CrafterLean.Config
import CrafterLean.Types.ChaCha8

namespace Crafter

structure ChestLoot where
  arrows : UInt8
  potionRed : UInt8
  potionGreen : UInt8
  potionBlue : UInt8
  potionPink : UInt8
  potionCyan : UInt8
  potionYellow : UInt8
  sapphire : UInt8
  ruby : UInt8
  coal : UInt8
  iron : UInt8
  diamond : UInt8
  deriving Repr

namespace ChestLoot

@[inline] def empty : ChestLoot :=
  { arrows := 0
    potionRed := 0
    potionGreen := 0
    potionBlue := 0
    potionPink := 0
    potionCyan := 0
    potionYellow := 0
    sapphire := 0
    ruby := 0
    coal := 0
    iron := 0
    diamond := 0 }

instance : Inhabited ChestLoot := ⟨empty⟩

end ChestLoot

inductive PotionKind where
  | Red
  | Green
  | Blue
  | Pink
  | Cyan
  | Yellow
  deriving Repr, DecidableEq, Inhabited

namespace CraftaxLoot

@[inline] def genRange (rng : ChaCha8Rng) (upperExclusive : Nat) : ChaCha8Rng × Nat :=
  rng.nextNat upperExclusive

@[inline] def genRangeInclusive (rng : ChaCha8Rng) (lo hi : Nat) : ChaCha8Rng × Nat :=
  if hi < lo then
    (rng, lo)
  else
    let span := hi - lo + 1
    let (rng, v) := rng.nextNat span
    (rng, lo + v)

@[inline] def rollChestLoot (rng : ChaCha8Rng) (config : CraftaxLootConfig) : ChaCha8Rng × ChestLoot :=
  let loot0 := ChestLoot.empty
  let (rng1, rollArrow) := rng.nextFloat32
  let (rng2, loot1) :=
    if rollArrow < config.arrowDropChance then
      let (rng2, count) := genRangeInclusive rng1 2 6
      (rng2, { loot0 with arrows := UInt8.ofNat count })
    else
      (rng1, loot0)

  let (rng3, rollPotion) := rng2.nextFloat32
  let (rng4, loot2) :=
    if rollPotion < config.potionDropChance then
      let (rng4, pick) := genRange rng3 6
      let loot :=
        match pick with
        | 0 => { loot1 with potionRed := 1 }
        | 1 => { loot1 with potionGreen := 1 }
        | 2 => { loot1 with potionBlue := 1 }
        | 3 => { loot1 with potionPink := 1 }
        | 4 => { loot1 with potionCyan := 1 }
        | _ => { loot1 with potionYellow := 1 }
      (rng4, loot)
    else
      (rng3, loot1)

  let (rng5, rollGem) := rng4.nextFloat32
  let (rng6, loot3) :=
    if rollGem < config.gemDropChance then
      let (rng6, pickGem) := rng5.nextFloat32
      let loot :=
        if pickGem < 0.5 then
          { loot2 with sapphire := 1 }
        else
          { loot2 with ruby := 1 }
      (rng6, loot)
    else
      (rng5, loot2)

  let (rng7, rollCoal) := rng6.nextFloat32
  let (rng8, loot4) :=
    if rollCoal < 0.6 then
      let (rng8, count) := genRangeInclusive rng7 1 2
      (rng8, { loot3 with coal := UInt8.ofNat count })
    else
      (rng7, loot3)

  let (rng9, rollIron) := rng8.nextFloat32
  let (rng10, loot5) :=
    if rollIron < 0.4 then
      let (rng10, count) := genRangeInclusive rng9 1 2
      (rng10, { loot4 with iron := UInt8.ofNat count })
    else
      (rng9, loot4)

  let (rng11, rollDiamond) := rng10.nextFloat32
  let loot6 :=
    if rollDiamond < 0.2 then
      { loot5 with diamond := 1 }
    else
      loot5
  (rng11, loot6)

end CraftaxLoot

end Crafter
