namespace Crafter

structure RNG where
  state : UInt64
  deriving Repr, Inhabited

namespace RNG

-- Simple LCG for deterministic, pure stepping. Replace with a stronger RNG later.
@[inline] def next (rng : RNG) : RNG × UInt64 :=
  let a : UInt64 := 6364136223846793005
  let c : UInt64 := 1442695040888963407
  let nextState := a * rng.state + c
  ({ state := nextState }, nextState)

@[inline] def nextNat (rng : RNG) (maxExclusive : Nat) : RNG × Nat :=
  if maxExclusive == 0 then
    (rng, 0)
  else
    let (rng', value) := rng.next
    (rng', (UInt64.toNat value) % maxExclusive)

@[inline] def nextFloat (rng : RNG) : RNG × Float :=
  let (rng', value) := rng.next
  let denom : Float := 18446744073709551616.0 -- 2^64
  let f := (Float.ofNat value.toNat) / denom
  (rng', f)

end RNG

end Crafter
