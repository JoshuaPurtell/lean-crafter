import Std
import CrafterLean.Types.Coord

namespace Crafter

structure Grid (α : Type) where
  width : Nat
  height : Nat
  data : Array α
  deriving Repr

namespace Grid

@[inline] def init (w h : Nat) (init : α) : Grid α :=
  { width := w, height := h, data := Array.replicate (w * h) init }

@[inline] def inBounds (g : Grid α) (x y : Nat) : Bool :=
  x < g.width && y < g.height

@[inline] def index? (g : Grid α) (x y : Nat) : Option Nat :=
  if g.inBounds x y then
    some (y * g.width + x)
  else
    none

@[inline] def arrayGet? (a : Array α) (idx : Nat) : Option α :=
  if h : idx < a.size then
    some (a.getInternal idx h)
  else
    none

@[inline] def get? (g : Grid α) (x y : Nat) : Option α :=
  match g.index? x y with
  | some idx => arrayGet? g.data idx
  | none => none

@[inline] def set (g : Grid α) (x y : Nat) (value : α) : Grid α :=
  match g.index? x y with
  | some idx => { g with data := g.data.set! idx value }
  | none => g

@[inline] def getCoord? (g : Grid α) (c : Coord) : Option α :=
  match Coord.toNat? c with
  | some (x, y) => g.get? x y
  | none => none

@[inline] def setCoord (g : Grid α) (c : Coord) (value : α) : Grid α :=
  match Coord.toNat? c with
  | some (x, y) => g.set x y value
  | none => g

end Grid

end Crafter
