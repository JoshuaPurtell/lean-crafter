import Std
import CrafterLean.Types.RNG

namespace Crafter

namespace OpenSimplex

structure XorShiftRng where
  x : UInt32
  y : UInt32
  z : UInt32
  w : UInt32
  deriving Repr

namespace XorShiftRng

@[inline] def fromSeed (seed : UInt32) : XorShiftRng :=
  { x := 1, y := seed, z := seed, w := seed }

@[inline] def nextU32 (rng : XorShiftRng) : XorShiftRng × UInt32 :=
  let x := rng.x
  let t := UInt32.xor x (UInt32.shiftLeft x 11)
  let x' := rng.y
  let y' := rng.z
  let z' := rng.w
  let w' := UInt32.xor rng.w (UInt32.shiftRight rng.w 19)
  let w'' := UInt32.xor w' (UInt32.xor t (UInt32.shiftRight t 8))
  ({ x := x', y := y', z := z', w := w'' }, w'')

end XorShiftRng

@[inline] def floorInt (x : Float) : Int :=
  let f := Float.floor x
  if f >= 0.0 then
    Int.ofNat (UInt64.toNat (Float.toUInt64 f))
  else
    let n := UInt64.toNat (Float.toUInt64 (-f))
    (- (Int.ofNat n))

@[inline] def mod256 (x : Int) : Nat :=
  Int.toNat (Int.emod x 256)

structure PermutationTable where
  values : Array UInt8
  deriving Repr

namespace PermutationTable

@[inline] partial def genIndex (rng : XorShiftRng) (ubound : UInt32) : XorShiftRng × UInt32 :=
  if ubound == 0 then
    (rng, 0)
  else
    let range : UInt64 := UInt64.ofNat ubound.toNat
    let unsignedMax : UInt64 := UInt64.ofNat 4294967295
    let intsToReject : UInt64 := (unsignedMax - range + 1) % range
    let zone : UInt64 := unsignedMax - intsToReject
    let rec loop (r : XorShiftRng) : XorShiftRng × UInt32 :=
      let (r', v) := r.nextU32
      let prod : UInt64 := (UInt64.ofNat v.toNat) * range
      let hi : UInt64 := UInt64.shiftRight prod 32
      let lo : UInt64 := UInt64.land prod (UInt64.ofNat 4294967295)
      if lo <= zone then
        (r', UInt32.ofNat hi.toNat)
      else
        loop r'
    loop rng

@[inline] def shuffle (values : Array UInt8) (rng : XorShiftRng) : Array UInt8 × XorShiftRng :=
  let len := values.size
  let rec loop : Nat → Array UInt8 → XorShiftRng → Array UInt8 × XorShiftRng
    | 0, arr, r => (arr, r)
    | Nat.succ i, arr, r =>
        let current := Nat.succ i
        let (r', idx) := genIndex r (UInt32.ofNat (current + 1))
        let j := idx.toNat
        let ai := arr.getD current 0
        let aj := arr.getD j 0
        let arr := arr.set! current aj
        let arr := arr.set! j ai
        loop i arr r'
  if len == 0 then
    (values, rng)
  else
    loop (len - 1) values rng

@[inline] def new (seed : UInt32) : PermutationTable :=
  let values := (List.range 256 |>.map UInt8.ofNat).toArray
  let rng := XorShiftRng.fromSeed seed
  let (values, _) := shuffle values rng
  { values := values }

@[inline] def hash3 (t : PermutationTable) (x y z : Int) : Nat :=
  let idx0 := mod256 x
  let v0 := t.values.getD idx0 0
  let idx1 := UInt8.toNat (UInt8.xor v0 (UInt8.ofNat (mod256 y)))
  let v1 := t.values.getD idx1 0
  let idx2 := UInt8.toNat (UInt8.xor v1 (UInt8.ofNat (mod256 z)))
  let v2 := t.values.getD idx2 0
  UInt8.toNat v2

end PermutationTable

structure Vec3 where
  x : Float
  y : Float
  z : Float
  deriving Repr

namespace Vec3

@[inline] def sum (v : Vec3) : Float := v.x + v.y + v.z

@[inline] def map (v : Vec3) (f : Float → Float) : Vec3 :=
  { x := f v.x, y := f v.y, z := f v.z }

@[inline] def add (a b : Vec3) : Vec3 := { x := a.x + b.x, y := a.y + b.y, z := a.z + b.z }

@[inline] def sub (a b : Vec3) : Vec3 := { x := a.x - b.x, y := a.y - b.y, z := a.z - b.z }

@[inline] def scale (v : Vec3) (s : Float) : Vec3 := { x := v.x * s, y := v.y * s, z := v.z * s }

@[inline] def dot (a b : Vec3) : Float := a.x * b.x + a.y * b.y + a.z * b.z

@[inline] def magnitudeSquared (v : Vec3) : Float := v.dot v

end Vec3

@[inline] def grad3 (index : Nat) : Vec3 :=
  let diag : Float := 0.7071067811865476 -- 1/sqrt(2)
  let diag2 : Float := 0.5773502691896258
  match index % 32 with
  | 0 | 12 => { x := diag, y := diag, z := 0.0 }
  | 1 | 13 => { x := -diag, y := diag, z := 0.0 }
  | 2 | 14 => { x := diag, y := -diag, z := 0.0 }
  | 3 | 15 => { x := -diag, y := -diag, z := 0.0 }
  | 4 | 16 => { x := diag, y := 0.0, z := diag }
  | 5 | 17 => { x := -diag, y := 0.0, z := diag }
  | 6 | 18 => { x := diag, y := 0.0, z := -diag }
  | 7 | 19 => { x := -diag, y := 0.0, z := -diag }
  | 8 | 20 => { x := 0.0, y := diag, z := diag }
  | 9 | 21 => { x := 0.0, y := -diag, z := diag }
  | 10 | 22 => { x := 0.0, y := diag, z := -diag }
  | 11 | 23 => { x := 0.0, y := -diag, z := -diag }
  | 24 => { x := diag2, y := diag2, z := diag2 }
  | 25 => { x := -diag2, y := diag2, z := diag2 }
  | 26 => { x := diag2, y := -diag2, z := diag2 }
  | 27 => { x := -diag2, y := -diag2, z := diag2 }
  | 28 => { x := diag2, y := diag2, z := -diag2 }
  | 29 => { x := -diag2, y := diag2, z := -diag2 }
  | 30 => { x := diag2, y := -diag2, z := -diag2 }
  | _ => { x := -diag2, y := -diag2, z := -diag2 }

@[inline] def openSimplex3 (point : Vec3) (table : PermutationTable) : Float :=
  let stretchConstant : Float := -1.0 / 6.0
  let squishConstant : Float := 1.0 / 3.0
  let normConstant : Float := 1.0 / 14.0

  let stretchOffset := point.sum * stretchConstant
  let stretched := point.map fun v => v + stretchOffset

  let floorX := floorInt stretched.x
  let floorY := floorInt stretched.y
  let floorZ := floorInt stretched.z
  let squishOffset := (Float.ofInt (floorX + floorY + floorZ)) * squishConstant
  let origin : Vec3 :=
    { x := Float.ofInt floorX + squishOffset
    , y := Float.ofInt floorY + squishOffset
    , z := Float.ofInt floorZ + squishOffset }

  let relCoords := Vec3.sub stretched { x := Float.ofInt floorX, y := Float.ofInt floorY, z := Float.ofInt floorZ }
  let regionSum := relCoords.sum
  let relPos := Vec3.sub point origin

  let contribute := fun (ox oy oz : Float) =>
    let vx := floorX + (if ox == 0.0 then 0 else 1)
    let vy := floorY + (if oy == 0.0 then 0 else 1)
    let vz := floorZ + (if oz == 0.0 then 0 else 1)
    let idx := table.hash3 vx vy vz
    let offsetSum := ox + oy + oz
    let dpos := Vec3.sub relPos
      { x := squishConstant * offsetSum + ox
      , y := squishConstant * offsetSum + oy
      , z := squishConstant * offsetSum + oz }
    let t := 2.0 - dpos.magnitudeSquared
    if t > 0.0 then
      let g := grad3 idx
      let t2 := t * t
      let t4 := t2 * t2
      t4 * dpos.dot g
    else
      0.0

  let value :=
    if regionSum <= 1.0 then
      contribute 0.0 0.0 0.0
        + contribute 1.0 0.0 0.0
        + contribute 0.0 1.0 0.0
        + contribute 0.0 0.0 1.0
    else if regionSum >= 2.0 then
      contribute 1.0 1.0 0.0
        + contribute 1.0 0.0 1.0
        + contribute 0.0 1.0 1.0
        + contribute 1.0 1.0 1.0
    else
      contribute 1.0 0.0 0.0
        + contribute 0.0 1.0 0.0
        + contribute 0.0 0.0 1.0
        + contribute 1.0 1.0 0.0
        + contribute 1.0 0.0 1.0
        + contribute 0.0 1.0 1.0
  value * normConstant

end OpenSimplex

end Crafter
