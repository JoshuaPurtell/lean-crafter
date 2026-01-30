namespace Crafter

structure Inventory where
  health : UInt8
  food : UInt8
  drink : UInt8
  energy : UInt8
  sapling : UInt8
  wood : UInt8
  stone : UInt8
  coal : UInt8
  iron : UInt8
  diamond : UInt8
  sapphire : UInt8
  ruby : UInt8
  woodPickaxe : UInt8
  stonePickaxe : UInt8
  ironPickaxe : UInt8
  diamondPickaxe : UInt8
  woodSword : UInt8
  stoneSword : UInt8
  ironSword : UInt8
  diamondSword : UInt8
  bow : UInt8
  arrows : UInt8
  armorHelmet : UInt8
  armorChestplate : UInt8
  armorLeggings : UInt8
  armorBoots : UInt8
  potionRed : UInt8
  potionGreen : UInt8
  potionBlue : UInt8
  potionPink : UInt8
  potionCyan : UInt8
  potionYellow : UInt8
  xp : UInt32
  level : UInt8
  statPoints : UInt8
  deriving Repr

namespace Inventory

def maxValue : Nat := 9

@[inline] def capAdd (slot amount : UInt8) : UInt8 :=
  let sum := slot.toNat + amount.toNat
  UInt8.ofNat (Nat.min sum maxValue)

@[inline] def subOrZero (slot amount : UInt8) : UInt8 :=
  if slot.toNat > amount.toNat then
    UInt8.ofNat (slot.toNat - amount.toNat)
  else
    0

@[inline] def geNat (slot : UInt8) (amount : Nat) : Bool :=
  slot.toNat >= amount

@[inline] def default : Inventory :=
  { health := UInt8.ofNat 9
    food := UInt8.ofNat 9
    drink := UInt8.ofNat 9
    energy := UInt8.ofNat 9
    sapling := 0
    wood := 0
    stone := 0
    coal := 0
    iron := 0
    diamond := 0
    sapphire := 0
    ruby := 0
    woodPickaxe := 0
    stonePickaxe := 0
    ironPickaxe := 0
    diamondPickaxe := 0
    woodSword := 0
    stoneSword := 0
    ironSword := 0
    diamondSword := 0
    bow := 0
    arrows := 0
    armorHelmet := 0
    armorChestplate := 0
    armorLeggings := 0
    armorBoots := 0
    potionRed := 0
    potionGreen := 0
    potionBlue := 0
    potionPink := 0
    potionCyan := 0
    potionYellow := 0
    xp := 0
    level := 0
    statPoints := 0 }

instance : Inhabited Inventory := ⟨default⟩

@[inline] def addWood (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with wood := capAdd inv.wood amount }

@[inline] def addStone (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with stone := capAdd inv.stone amount }

@[inline] def addCoal (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with coal := capAdd inv.coal amount }

@[inline] def addIron (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with iron := capAdd inv.iron amount }

@[inline] def addDiamond (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with diamond := capAdd inv.diamond amount }

@[inline] def addSapphire (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with sapphire := capAdd inv.sapphire amount }

@[inline] def addRuby (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with ruby := capAdd inv.ruby amount }

@[inline] def addSapling (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with sapling := capAdd inv.sapling amount }

@[inline] def addFood (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with food := capAdd inv.food amount }

@[inline] def addDrink (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with drink := capAdd inv.drink amount }

@[inline] def addEnergy (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with energy := capAdd inv.energy amount }

@[inline] def addHealth (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with health := capAdd inv.health amount }

@[inline] def addArrows (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with arrows := capAdd inv.arrows amount }

@[inline] def addPotionRed (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with potionRed := capAdd inv.potionRed amount }

@[inline] def addPotionGreen (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with potionGreen := capAdd inv.potionGreen amount }

@[inline] def addPotionBlue (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with potionBlue := capAdd inv.potionBlue amount }

@[inline] def addPotionPink (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with potionPink := capAdd inv.potionPink amount }

@[inline] def addPotionCyan (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with potionCyan := capAdd inv.potionCyan amount }

@[inline] def addPotionYellow (inv : Inventory) (amount : UInt8) : Inventory :=
  { inv with potionYellow := capAdd inv.potionYellow amount }

@[inline] def addXp (inv : Inventory) (amount : UInt32) : Inventory :=
  let maxU32 := UInt32.toNat (UInt32.ofNat 0xFFFF_FFFF)
  let sum := inv.xp.toNat + amount.toNat
  let capped := Nat.min sum maxU32
  { inv with xp := UInt32.ofNat capped }

@[inline] def takeDamage (inv : Inventory) (amount : UInt8) : Inventory × Bool :=
  if inv.health.toNat > amount.toNat then
    let h := UInt8.ofNat (inv.health.toNat - amount.toNat)
    ({ inv with health := h }, true)
  else
    ({ inv with health := 0 }, false)

@[inline] def isAlive (inv : Inventory) : Bool :=
  inv.health.toNat > 0

@[inline] def bestPickaxeTier (inv : Inventory) : UInt8 :=
  if inv.diamondPickaxe.toNat > 0 then
    4
  else if inv.ironPickaxe.toNat > 0 then
    3
  else if inv.stonePickaxe.toNat > 0 then
    2
  else if inv.woodPickaxe.toNat > 0 then
    1
  else
    0

@[inline] def bestSwordTier (inv : Inventory) : UInt8 :=
  if inv.diamondSword.toNat > 0 then
    4
  else if inv.ironSword.toNat > 0 then
    3
  else if inv.stoneSword.toNat > 0 then
    2
  else if inv.woodSword.toNat > 0 then
    1
  else
    0

@[inline] def attackDamage (inv : Inventory) : UInt8 :=
  match inv.bestSwordTier.toNat with
  | 4 => 8
  | 3 => 5
  | 2 => 3
  | 1 => 2
  | _ => 1

@[inline] def armorReduction (inv : Inventory) : Float :=
  let total := inv.armorHelmet.toNat + inv.armorChestplate.toNat + inv.armorLeggings.toNat + inv.armorBoots.toNat
  (Float.ofNat total) * 0.1

@[inline] def canCraftWoodPickaxe (inv : Inventory) : Bool :=
  geNat inv.wood 1

@[inline] def canCraftStonePickaxe (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.stone 1

@[inline] def canCraftIronPickaxe (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.coal 1 && geNat inv.iron 1

@[inline] def canCraftDiamondPickaxe (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.diamond 1

@[inline] def canCraftWoodSword (inv : Inventory) : Bool :=
  geNat inv.wood 1

@[inline] def canCraftStoneSword (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.stone 1

@[inline] def canCraftIronSword (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.coal 1 && geNat inv.iron 1

@[inline] def canCraftDiamondSword (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.diamond 2

@[inline] def canCraftBow (inv : Inventory) : Bool :=
  geNat inv.wood 2

@[inline] def canCraftArrow (inv : Inventory) : Bool :=
  geNat inv.wood 1 && geNat inv.stone 1

@[inline] def craftWoodPickaxe (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftWoodPickaxe then
    let inv' := { inv with wood := subOrZero inv.wood 1, woodPickaxe := capAdd inv.woodPickaxe 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftStonePickaxe (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftStonePickaxe then
    let inv' := { inv with wood := subOrZero inv.wood 1, stone := subOrZero inv.stone 1, stonePickaxe := capAdd inv.stonePickaxe 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftIronPickaxe (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftIronPickaxe then
    let inv' := { inv with wood := subOrZero inv.wood 1, coal := subOrZero inv.coal 1, iron := subOrZero inv.iron 1, ironPickaxe := capAdd inv.ironPickaxe 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftDiamondPickaxe (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftDiamondPickaxe then
    let inv' := { inv with wood := subOrZero inv.wood 1, diamond := subOrZero inv.diamond 1, diamondPickaxe := capAdd inv.diamondPickaxe 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftWoodSword (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftWoodSword then
    let inv' := { inv with wood := subOrZero inv.wood 1, woodSword := capAdd inv.woodSword 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftStoneSword (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftStoneSword then
    let inv' := { inv with wood := subOrZero inv.wood 1, stone := subOrZero inv.stone 1, stoneSword := capAdd inv.stoneSword 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftIronSword (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftIronSword then
    let inv' := { inv with wood := subOrZero inv.wood 1, coal := subOrZero inv.coal 1, iron := subOrZero inv.iron 1, ironSword := capAdd inv.ironSword 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftDiamondSword (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftDiamondSword then
    let inv' := { inv with wood := subOrZero inv.wood 1, diamond := subOrZero inv.diamond 2, diamondSword := capAdd inv.diamondSword 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftIronArmor (inv : Inventory) : Inventory × Bool :=
  if geNat inv.iron 3 && geNat inv.coal 3 then
    if inv.armorHelmet.toNat == 0 then
      ({ inv with armorHelmet := 1, iron := subOrZero inv.iron 3, coal := subOrZero inv.coal 3 }, true)
    else if inv.armorChestplate.toNat == 0 then
      ({ inv with armorChestplate := 1, iron := subOrZero inv.iron 3, coal := subOrZero inv.coal 3 }, true)
    else if inv.armorLeggings.toNat == 0 then
      ({ inv with armorLeggings := 1, iron := subOrZero inv.iron 3, coal := subOrZero inv.coal 3 }, true)
    else if inv.armorBoots.toNat == 0 then
      ({ inv with armorBoots := 1, iron := subOrZero inv.iron 3, coal := subOrZero inv.coal 3 }, true)
    else
      (inv, false)
  else
    (inv, false)

@[inline] def craftDiamondArmor (inv : Inventory) : Inventory × Bool :=
  if geNat inv.diamond 3 then
    if inv.armorHelmet.toNat < 2 then
      ({ inv with armorHelmet := 2, diamond := subOrZero inv.diamond 3 }, true)
    else if inv.armorChestplate.toNat < 2 then
      ({ inv with armorChestplate := 2, diamond := subOrZero inv.diamond 3 }, true)
    else if inv.armorLeggings.toNat < 2 then
      ({ inv with armorLeggings := 2, diamond := subOrZero inv.diamond 3 }, true)
    else if inv.armorBoots.toNat < 2 then
      ({ inv with armorBoots := 2, diamond := subOrZero inv.diamond 3 }, true)
    else
      (inv, false)
  else
    (inv, false)

@[inline] def craftBow (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftBow then
    let inv' := { inv with wood := subOrZero inv.wood 2, bow := capAdd inv.bow 1 }
    (inv', true)
  else
    (inv, false)

@[inline] def craftArrow (inv : Inventory) : Inventory × Bool :=
  if inv.canCraftArrow then
    let inv' := { inv with wood := subOrZero inv.wood 1, stone := subOrZero inv.stone 1, arrows := capAdd inv.arrows 3 }
    (inv', true)
  else
    (inv, false)

@[inline] def hasStone (inv : Inventory) : Bool := geNat inv.stone 1

@[inline] def hasWoodForTable (inv : Inventory) : Bool := geNat inv.wood 2

@[inline] def hasStoneForFurnace (inv : Inventory) : Bool := geNat inv.stone 4

@[inline] def hasSapling (inv : Inventory) : Bool := geNat inv.sapling 1

@[inline] def useStone (inv : Inventory) : Inventory × Bool :=
  if inv.hasStone then
    ({ inv with stone := subOrZero inv.stone 1 }, true)
  else
    (inv, false)

@[inline] def useWoodForTable (inv : Inventory) : Inventory × Bool :=
  if inv.hasWoodForTable then
    ({ inv with wood := subOrZero inv.wood 2 }, true)
  else
    (inv, false)

@[inline] def useStoneForFurnace (inv : Inventory) : Inventory × Bool :=
  if inv.hasStoneForFurnace then
    ({ inv with stone := subOrZero inv.stone 4 }, true)
  else
    (inv, false)

@[inline] def useSapling (inv : Inventory) : Inventory × Bool :=
  if inv.hasSapling then
    ({ inv with sapling := subOrZero inv.sapling 1 }, true)
  else
    (inv, false)

end Inventory

end Crafter
