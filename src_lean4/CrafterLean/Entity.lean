import Lean.Data.Json
import CrafterLean.World.Types

namespace Crafter

abbrev Position := Coord
abbrev Facing := Int × Int

inductive ProjectileKind where
  | Arrow
  | Fireball
  | Iceball
  deriving Repr, DecidableEq, Inhabited

structure Cow where
  pos : Position
  health : UInt8
  deriving Repr

namespace Cow

@[inline] def new (pos : Position) : Cow := { pos := pos, health := 3 }

@[inline] def withHealth (pos : Position) (health : UInt8) : Cow := { pos := pos, health := health }

@[inline] def takeDamage (c : Cow) (amount : UInt8) : Cow × Bool :=
  if c.health.toNat > amount.toNat then
    ({ c with health := UInt8.ofNat (c.health.toNat - amount.toNat) }, true)
  else
    ({ c with health := 0 }, false)

end Cow

structure Zombie where
  pos : Position
  health : UInt8
  cooldown : UInt8
  deriving Repr

namespace Zombie

@[inline] def new (pos : Position) (health : UInt8) : Zombie := { pos := pos, health := health, cooldown := 0 }

@[inline] def takeDamage (z : Zombie) (amount : UInt8) : Zombie × Bool :=
  if z.health.toNat > amount.toNat then
    ({ z with health := UInt8.ofNat (z.health.toNat - amount.toNat) }, true)
  else
    ({ z with health := 0 }, false)

end Zombie

structure Skeleton where
  pos : Position
  health : UInt8
  reload : UInt8
  deriving Repr

namespace Skeleton

@[inline] def new (pos : Position) (health : UInt8) : Skeleton := { pos := pos, health := health, reload := 0 }

@[inline] def canShoot (s : Skeleton) : Bool :=
  s.reload.toNat == 0

@[inline] def resetReload (s : Skeleton) : Skeleton :=
  { s with reload := 4 }

@[inline] def tickReload (s : Skeleton) : Skeleton :=
  if s.reload.toNat > 0 then
    { s with reload := UInt8.ofNat (s.reload.toNat - 1) }
  else
    s

@[inline] def takeDamage (s : Skeleton) (amount : UInt8) : Skeleton × Bool :=
  if s.health.toNat > amount.toNat then
    ({ s with health := UInt8.ofNat (s.health.toNat - amount.toNat) }, true)
  else
    ({ s with health := 0 }, false)

end Skeleton

structure Arrow where
  pos : Position
  facing : Facing
  kind : ProjectileKind
  damage : UInt8
  source : DamageSource
  deriving Repr

namespace Arrow

@[inline] def withStats (pos : Position) (facing : Facing) (kind : ProjectileKind) (damage : UInt8) (source : DamageSource) : Arrow :=
  { pos := pos, facing := facing, kind := kind, damage := damage, source := source }

@[inline] def nextPosition (a : Arrow) : Position :=
  { x := a.pos.x + a.facing.1, y := a.pos.y + a.facing.2 }

end Arrow

inductive CraftaxMobKind where
  | OrcSoldier
  | OrcMage
  | Knight
  | KnightArcher
  | Troll
  | Bat
  | Snail
  deriving Repr, DecidableEq, Inhabited

structure CraftaxMob where
  kind : CraftaxMobKind
  pos : Position
  health : UInt8
  cooldown : UInt8
  deriving Repr

namespace CraftaxMob

@[inline] def new (kind : CraftaxMobKind) (pos : Position) (health : UInt8) : CraftaxMob :=
  { kind := kind, pos := pos, health := health, cooldown := 0 }

@[inline] def isHostile (m : CraftaxMob) : Bool :=
  match m.kind with
  | CraftaxMobKind.OrcSoldier
  | CraftaxMobKind.OrcMage
  | CraftaxMobKind.Knight
  | CraftaxMobKind.KnightArcher
  | CraftaxMobKind.Troll => true
  | _ => false

@[inline] def isPassive (m : CraftaxMob) : Bool :=
  match m.kind with
  | CraftaxMobKind.Bat | CraftaxMobKind.Snail => true
  | _ => false

@[inline] def takeDamage (m : CraftaxMob) (amount : UInt8) : CraftaxMob × Bool :=
  if m.health.toNat > amount.toNat then
    ({ m with health := UInt8.ofNat (m.health.toNat - amount.toNat) }, true)
  else
    ({ m with health := 0 }, false)

end CraftaxMob

structure Plant where
  pos : Position
  health : UInt8
  grown : UInt16
  deriving Repr

namespace Plant

@[inline] def new (pos : Position) : Plant := { pos := pos, health := 1, grown := 0 }

@[inline] def grow (p : Plant) : Plant :=
  if p.grown < 300 then
    { p with grown := p.grown + 1 }
  else
    p

@[inline] def isRipe (p : Plant) : Bool :=
  p.grown >= 300

end Plant

inductive GameObject where
  | Player (p : Player)
  | Cow (c : Cow)
  | Zombie (z : Zombie)
  | Skeleton (s : Skeleton)
  | Arrow (a : Arrow)
  | Plant (p : Plant)
  | CraftaxMob (m : CraftaxMob)
  | Unknown (tag : String) (payload : Lean.Json)

namespace GameObject

@[inline] def position : GameObject → Coord
  | Player p => p.pos
  | Cow c => c.pos
  | Zombie z => z.pos
  | Skeleton s => s.pos
  | Arrow a => a.pos
  | Plant p => p.pos
  | CraftaxMob m => m.pos
  | Unknown _ _ => { x := 0, y := 0 }

@[inline] def setPosition (obj : GameObject) (pos : Coord) : GameObject :=
  match obj with
  | Player p => GameObject.Player { p with pos := pos }
  | Cow c => GameObject.Cow { c with pos := pos }
  | Zombie z => GameObject.Zombie { z with pos := pos }
  | Skeleton s => GameObject.Skeleton { s with pos := pos }
  | Arrow a => GameObject.Arrow { a with pos := pos }
  | Plant p => GameObject.Plant { p with pos := pos }
  | CraftaxMob m => GameObject.CraftaxMob { m with pos := pos }
  | Unknown tag payload => GameObject.Unknown tag payload

@[inline] def isBlocking : GameObject → Bool
  | Player _ => true
  | Cow _ => true
  | Zombie _ => true
  | Skeleton _ => true
  | CraftaxMob _ => true
  | _ => false

@[inline] def isHostile : GameObject → Bool
  | Zombie _ => true
  | Skeleton _ => true
  | CraftaxMob m => m.isHostile
  | _ => false

end GameObject

end Crafter
