namespace Crafter

inductive Material where
  | Water
  | Grass
  | Stone
  | Path
  | Sand
  | Tree
  | Lava
  | Coal
  | Iron
  | Diamond
  | Table
  | Furnace
  | Sapphire
  | Ruby
  | Chest
  deriving Repr, DecidableEq, Inhabited

namespace Material

@[inline] def isWalkable : Material → Bool
  | Grass | Path | Sand | Lava => true
  | _ => false

@[inline] def isObstacle (m : Material) : Bool :=
  !m.isWalkable

@[inline] def isMinable : Material → Bool
  | Tree | Stone | Coal | Iron | Diamond | Sapphire | Ruby => true
  | _ => false

@[inline] def isDeadly : Material → Bool
  | Lava => true
  | _ => false

@[inline] def providesWater : Material → Bool
  | Water => true
  | _ => false

@[inline] def requiredPickaxeTier : Material → Option UInt8
  | Stone | Coal => some (UInt8.ofNat 1)
  | Iron => some (UInt8.ofNat 2)
  | Diamond => some (UInt8.ofNat 3)
  | Sapphire | Ruby => some (UInt8.ofNat 4)
  | _ => none

@[inline] def minedReplacement : Material → Material
  | Tree => Grass
  | Stone | Coal | Iron | Diamond | Sapphire | Ruby => Path
  | Table | Furnace => Grass
  | m => m

@[inline] def toString : Material → String
  | Water => "Water"
  | Grass => "Grass"
  | Stone => "Stone"
  | Path => "Path"
  | Sand => "Sand"
  | Tree => "Tree"
  | Lava => "Lava"
  | Coal => "Coal"
  | Iron => "Iron"
  | Diamond => "Diamond"
  | Table => "Table"
  | Furnace => "Furnace"
  | Sapphire => "Sapphire"
  | Ruby => "Ruby"
  | Chest => "Chest"

@[inline] def fromString? : String → Option Material
  | "Water" => some Water
  | "Grass" => some Grass
  | "Stone" => some Stone
  | "Path" => some Path
  | "Sand" => some Sand
  | "Tree" => some Tree
  | "Lava" => some Lava
  | "Coal" => some Coal
  | "Iron" => some Iron
  | "Diamond" => some Diamond
  | "Table" => some Table
  | "Furnace" => some Furnace
  | "Sapphire" => some Sapphire
  | "Ruby" => some Ruby
  | "Chest" => some Chest
  | _ => none

@[inline] def displayChar : Material → Char
  | Water => '~'
  | Grass => '.'
  | Stone => '#'
  | Path => '_'
  | Sand => ':'
  | Tree => 'T'
  | Lava => '%'
  | Coal => 'c'
  | Iron => 'i'
  | Diamond => 'd'
  | Table => '+'
  | Furnace => 'f'
  | Sapphire => 's'
  | Ruby => 'r'
  | Chest => 'H'

end Material

end Crafter
