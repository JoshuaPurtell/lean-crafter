import Std

namespace Crafter

structure ChaCha8Rng where
  key : Array UInt32
  counter : UInt64
  stream : UInt64
  buffer : Array UInt32
  index : Nat

namespace ChaCha8Rng

@[inline] def rotl (x : UInt32) (n : Nat) : UInt32 :=
  if n == 0 then
    x
  else
    let left := UInt32.shiftLeft x (UInt32.ofNat n)
    let right := UInt32.shiftRight x (UInt32.ofNat (32 - n))
    UInt32.lor left right

@[inline] def rotr (x : UInt32) (n : Nat) : UInt32 :=
  if n == 0 then
    x
  else
    let right := UInt32.shiftRight x (UInt32.ofNat n)
    let left := UInt32.shiftLeft x (UInt32.ofNat (32 - n))
    UInt32.lor left right

@[inline] def quarterRound (arr : Array UInt32) (a b c d : Nat) : Array UInt32 :=
  let va0 := arr.getD a 0
  let vb0 := arr.getD b 0
  let vc0 := arr.getD c 0
  let vd0 := arr.getD d 0
  let va1 := va0 + vb0
  let vd1 := rotl (UInt32.xor vd0 va1) 16
  let vc1 := vc0 + vd1
  let vb1 := rotl (UInt32.xor vb0 vc1) 12
  let va2 := va1 + vb1
  let vd2 := rotl (UInt32.xor vd1 va2) 8
  let vc2 := vc1 + vd2
  let vb2 := rotl (UInt32.xor vb1 vc2) 7
  let arr := arr.set! a va2
  let arr := arr.set! b vb2
  let arr := arr.set! c vc2
  let arr := arr.set! d vd2
  arr

@[inline] def doubleRound (arr : Array UInt32) : Array UInt32 :=
  let x := quarterRound arr 0 4 8 12
  let x := quarterRound x 1 5 9 13
  let x := quarterRound x 2 6 10 14
  let x := quarterRound x 3 7 11 15
  let x := quarterRound x 0 5 10 15
  let x := quarterRound x 1 6 11 12
  let x := quarterRound x 2 7 8 13
  quarterRound x 3 4 9 14

@[inline] def u32OfU64 (x : UInt64) : UInt32 :=
  UInt32.ofNat x.toNat

@[inline] def splitCounter (x : UInt64) : UInt32 × UInt32 :=
  let lo := u32OfU64 (UInt64.land x (UInt64.ofNat 0xFFFF_FFFF))
  let hi := u32OfU64 (UInt64.shiftRight x 32)
  (lo, hi)

@[inline] def chachaBlock (key : Array UInt32) (counter stream : UInt64) : Array UInt32 :=
  let (ctrLo, ctrHi) := splitCounter counter
  let (streamLo, streamHi) := splitCounter stream
  let k0 := key.getD 0 0
  let k1 := key.getD 1 0
  let k2 := key.getD 2 0
  let k3 := key.getD 3 0
  let k4 := key.getD 4 0
  let k5 := key.getD 5 0
  let k6 := key.getD 6 0
  let k7 := key.getD 7 0
  let state : Array UInt32 := #[
    UInt32.ofNat 0x6170_7865, UInt32.ofNat 0x3320_646e, UInt32.ofNat 0x7962_2d32, UInt32.ofNat 0x6b20_6574,
    k0, k1, k2, k3,
    k4, k5, k6, k7,
    ctrLo, ctrHi, streamLo, streamHi
  ]
  let rec rounds : Nat → Array UInt32 → Array UInt32
    | 0, x => x
    | Nat.succ n, x => rounds n (doubleRound x)
  let x := rounds 4 state
  let out := (List.range 16).foldl (fun acc i => acc.set! i (acc.getD i 0 + state.getD i 0)) x
  out

@[inline] def appendBlock (arr : Array UInt32) (block : Array UInt32) : Array UInt32 :=
  (List.range 16).foldl (fun acc i => acc.push (block.getD i 0)) arr

@[inline] def refill (rng : ChaCha8Rng) : ChaCha8Rng :=
  let (buffer, ctr) := (List.range 4).foldl
    (fun acc _ =>
      let (buf, ctr) := acc
      let block := chachaBlock rng.key ctr rng.stream
      (appendBlock buf block, ctr + 1))
    (#[], rng.counter)
  { rng with buffer := buffer, index := 0, counter := ctr }

@[inline] def nextU32 (rng : ChaCha8Rng) : ChaCha8Rng × UInt32 :=
  let rng := if rng.index >= rng.buffer.size then refill rng else rng
  let value := rng.buffer.getD rng.index 0
  ({ rng with index := rng.index + 1 }, value)

@[inline] def nextU64 (rng : ChaCha8Rng) : ChaCha8Rng × UInt64 :=
  let (rng, lo) := nextU32 rng
  let (rng, hi) := nextU32 rng
  let value := UInt64.lor (UInt64.shiftLeft (UInt64.ofNat hi.toNat) 32) (UInt64.ofNat lo.toNat)
  (rng, value)

@[inline] def nextFloat (rng : ChaCha8Rng) : ChaCha8Rng × Float :=
  let (rng, value) := nextU64 rng
  let shifted := UInt64.shiftRight value 11
  let scale := 1.0 / (Float.ofNat (UInt64.toNat (UInt64.shiftLeft (UInt64.ofNat 1) 53)))
  let f := (Float.ofNat shifted.toNat) * scale
  (rng, f)

@[inline] def nextFloat32 (rng : ChaCha8Rng) : ChaCha8Rng × Float :=
  let (rng, value) := nextU32 rng
  let shifted := UInt32.shiftRight value 8
  let scale := 1.0 / (Float.ofNat (UInt32.toNat (UInt32.shiftLeft (UInt32.ofNat 1) 24)))
  let f := (Float.ofNat shifted.toNat) * scale
  (rng, f)

@[inline] partial def nextNat (rng : ChaCha8Rng) (upper : Nat) : ChaCha8Rng × Nat :=
  if upper == 0 then
    (rng, 0)
  else
    let range : UInt64 := UInt64.ofNat upper
    let unsignedMax : UInt64 := UInt64.ofNat 4294967295
    let intsToReject : UInt64 := (unsignedMax - range + 1) % range
    let zone : UInt64 := unsignedMax - intsToReject
    let rec loop (r : ChaCha8Rng) : ChaCha8Rng × Nat :=
      let (r', v) := nextU32 r
      let prod : UInt64 := (UInt64.ofNat v.toNat) * range
      let hi : UInt64 := UInt64.shiftRight prod 32
      let lo : UInt64 := UInt64.land prod (UInt64.ofNat 4294967295)
      if lo <= zone then
        (r', hi.toNat)
      else
        loop r'
    loop rng

@[inline] def pcg32 (state : UInt64) : UInt64 × UInt32 :=
  let mul : UInt64 := 6364136223846793005
  let inc : UInt64 := 11634580027462260723
  let state' := state * mul + inc
  let t := UInt64.xor (UInt64.shiftRight state' 18) state'
  let xorshifted := UInt64.shiftRight t 27
  let rot := (UInt64.shiftRight state' 59).toNat
  let x := UInt32.ofNat xorshifted.toNat
  (state', rotr x rot)

@[inline] def seedFromU64 (seed : UInt64) : ChaCha8Rng :=
  let (_state, key) := (List.range 8).foldl
    (fun acc _ =>
      let (state, key) := acc
      let (state', word) := pcg32 state
      (state', key.push word))
    (seed, #[])
  { key := key, counter := 0, stream := 0, buffer := #[], index := 64 }

end ChaCha8Rng

end Crafter
