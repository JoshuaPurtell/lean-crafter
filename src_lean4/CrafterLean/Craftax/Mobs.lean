import CrafterLean.Entity

namespace Crafter

structure CraftaxMobStats where
  health : UInt8
  meleeDamage : UInt8
  rangedDamage : UInt8
  range : Int
  cooldown : UInt8
  projectile : ProjectileKind
  deriving Repr

namespace CraftaxMobStats

@[inline] def isRanged (s : CraftaxMobStats) : Bool :=
  s.rangedDamage.toNat > 0

@[inline] def isMelee (s : CraftaxMobStats) : Bool :=
  s.meleeDamage.toNat > 0

end CraftaxMobStats

@[inline] def craftaxStats (kind : CraftaxMobKind) : CraftaxMobStats :=
  match kind with
  | CraftaxMobKind.OrcSoldier =>
      { health := 5, meleeDamage := 3, rangedDamage := 0, range := 1, cooldown := 3, projectile := ProjectileKind.Arrow }
  | CraftaxMobKind.OrcMage =>
      { health := 3, meleeDamage := 1, rangedDamage := 3, range := 6, cooldown := 4, projectile := ProjectileKind.Fireball }
  | CraftaxMobKind.Knight =>
      { health := 9, meleeDamage := 6, rangedDamage := 0, range := 1, cooldown := 2, projectile := ProjectileKind.Arrow }
  | CraftaxMobKind.KnightArcher =>
      { health := 8, meleeDamage := 2, rangedDamage := 5, range := 7, cooldown := 3, projectile := ProjectileKind.Arrow }
  | CraftaxMobKind.Troll =>
      { health := 12, meleeDamage := 6, rangedDamage := 0, range := 1, cooldown := 3, projectile := ProjectileKind.Arrow }
  | CraftaxMobKind.Bat =>
      { health := 2, meleeDamage := 0, rangedDamage := 0, range := 0, cooldown := 0, projectile := ProjectileKind.Arrow }
  | CraftaxMobKind.Snail =>
      { health := 3, meleeDamage := 0, rangedDamage := 0, range := 0, cooldown := 0, projectile := ProjectileKind.Arrow }

end Crafter
