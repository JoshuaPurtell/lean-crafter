namespace Crafter

inductive Action where
  | Noop
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | Do
  | Sleep
  | PlaceStone
  | PlaceTable
  | PlaceFurnace
  | PlacePlant
  | MakeWoodPickaxe
  | MakeStonePickaxe
  | MakeIronPickaxe
  | MakeWoodSword
  | MakeStoneSword
  | MakeIronSword
  | MakeDiamondPickaxe
  | MakeDiamondSword
  | MakeIronArmor
  | MakeDiamondArmor
  | MakeBow
  | MakeArrow
  | ShootArrow
  | DrinkPotionRed
  | DrinkPotionGreen
  | DrinkPotionBlue
  | DrinkPotionPink
  | DrinkPotionCyan
  | DrinkPotionYellow
  deriving Repr, DecidableEq, Inhabited

namespace Action

@[inline] def toUInt32 : Action → UInt32
  | Noop => 0
  | MoveLeft => 1
  | MoveRight => 2
  | MoveUp => 3
  | MoveDown => 4
  | Do => 5
  | Sleep => 6
  | PlaceStone => 7
  | PlaceTable => 8
  | PlaceFurnace => 9
  | PlacePlant => 10
  | MakeWoodPickaxe => 11
  | MakeStonePickaxe => 12
  | MakeIronPickaxe => 13
  | MakeWoodSword => 14
  | MakeStoneSword => 15
  | MakeIronSword => 16
  | MakeDiamondPickaxe => 17
  | MakeDiamondSword => 18
  | MakeIronArmor => 19
  | MakeDiamondArmor => 20
  | MakeBow => 21
  | MakeArrow => 22
  | ShootArrow => 23
  | DrinkPotionRed => 24
  | DrinkPotionGreen => 25
  | DrinkPotionBlue => 26
  | DrinkPotionPink => 27
  | DrinkPotionCyan => 28
  | DrinkPotionYellow => 29

@[inline] def fromUInt32? : UInt32 → Option Action
  | 0 => some Noop
  | 1 => some MoveLeft
  | 2 => some MoveRight
  | 3 => some MoveUp
  | 4 => some MoveDown
  | 5 => some Do
  | 6 => some Sleep
  | 7 => some PlaceStone
  | 8 => some PlaceTable
  | 9 => some PlaceFurnace
  | 10 => some PlacePlant
  | 11 => some MakeWoodPickaxe
  | 12 => some MakeStonePickaxe
  | 13 => some MakeIronPickaxe
  | 14 => some MakeWoodSword
  | 15 => some MakeStoneSword
  | 16 => some MakeIronSword
  | 17 => some MakeDiamondPickaxe
  | 18 => some MakeDiamondSword
  | 19 => some MakeIronArmor
  | 20 => some MakeDiamondArmor
  | 21 => some MakeBow
  | 22 => some MakeArrow
  | 23 => some ShootArrow
  | 24 => some DrinkPotionRed
  | 25 => some DrinkPotionGreen
  | 26 => some DrinkPotionBlue
  | 27 => some DrinkPotionPink
  | 28 => some DrinkPotionCyan
  | 29 => some DrinkPotionYellow
  | _ => none

@[inline] def movementDelta? : Action → Option (Int × Int)
  | MoveLeft => some (-1, 0)
  | MoveRight => some (1, 0)
  | MoveUp => some (0, -1)
  | MoveDown => some (0, 1)
  | _ => none

@[inline] def isMovement (a : Action) : Bool :=
  a.movementDelta?.isSome

@[inline] def isCrafting : Action → Bool
  | MakeWoodPickaxe
  | MakeStonePickaxe
  | MakeIronPickaxe
  | MakeWoodSword
  | MakeStoneSword
  | MakeIronSword
  | MakeDiamondPickaxe
  | MakeDiamondSword
  | MakeIronArmor
  | MakeDiamondArmor
  | MakeBow
  | MakeArrow => true
  | _ => false

@[inline] def isPlacement : Action → Bool
  | PlaceStone
  | PlaceTable
  | PlaceFurnace
  | PlacePlant => true
  | _ => false

@[inline] def all : Array Action :=
  #[
    Noop,
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
    Do,
    Sleep,
    PlaceStone,
    PlaceTable,
    PlaceFurnace,
    PlacePlant,
    MakeWoodPickaxe,
    MakeStonePickaxe,
    MakeIronPickaxe,
    MakeWoodSword,
    MakeStoneSword,
    MakeIronSword,
    MakeDiamondPickaxe,
    MakeDiamondSword,
    MakeIronArmor,
    MakeDiamondArmor,
    MakeBow,
    MakeArrow,
    ShootArrow,
    DrinkPotionRed,
    DrinkPotionGreen,
    DrinkPotionBlue,
    DrinkPotionPink,
    DrinkPotionCyan,
    DrinkPotionYellow
  ]

end Action

end Crafter
