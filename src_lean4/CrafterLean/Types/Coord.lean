namespace Crafter

structure Coord where
  x : Int
  y : Int
  deriving Repr, DecidableEq, Inhabited

namespace Coord

@[inline] def add (a : Coord) (dx dy : Int) : Coord :=
  { x := a.x + dx, y := a.y + dy }

@[inline] def toNat? (c : Coord) : Option (Nat × Nat) :=
  if c.x >= 0 ∧ c.y >= 0 then
    some (Int.toNat c.x, Int.toNat c.y)
  else
    none

end Coord

end Crafter
