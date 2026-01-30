import CrafterLean.Achievement
import CrafterLean.Inventory
import CrafterLean.Types.Coord
import CrafterLean.Types.Grid
import CrafterLean.World.Material

namespace Crafter

inductive Tile where
  | Empty
  | Grass
  | Wall
  deriving Repr, DecidableEq, Inhabited

namespace Tile

@[inline] def isSolid : Tile → Bool
  | Wall => true
  | _ => false

@[inline] def ofMaterial : Material → Tile
  | m => if m.isObstacle then Tile.Wall else Tile.Grass

@[inline] def toMaterial : Tile → Material
  | Empty => Material.Grass
  | Grass => Material.Grass
  | Wall => Material.Stone

end Tile

inductive DamageSource where
  | Zombie
  | Skeleton
  | Arrow
  | PlayerArrow
  | CraftaxMelee
  | CraftaxRanged
  | CraftaxMagic
  | Lava
  | Starvation
  | Thirst
  | Exhaustion
  | Unknown
  deriving Repr, DecidableEq, Inhabited

structure Player where
  pos : Coord
  facing : Int × Int
  inventory : Inventory
  achievements : Achievements
  sleeping : Bool
  hungerCounter : Float
  thirstCounter : Float
  fatigueCounter : Int
  recoverCounter : Float
  lastHealth : UInt8
  lastDamageSource : Option DamageSource
  deriving Repr

namespace Player

def default (pos : Coord) : Player :=
  { pos := pos
    facing := (0, 1)
    inventory := Inventory.default
    achievements := Achievements.default
    sleeping := false
    hungerCounter := 0.0
    thirstCounter := 0.0
    fatigueCounter := 0
    recoverCounter := 0.0
    lastHealth := UInt8.ofNat 9
    lastDamageSource := none }

@[inline] def isAlive (p : Player) : Bool :=
  p.inventory.isAlive

@[inline] def attackDamage (p : Player) : UInt8 :=
  p.inventory.attackDamage

@[inline] def takeDamage (p : Player) (amount : UInt8) : Player × Bool :=
  let (inv', alive) := p.inventory.takeDamage amount
  ({ p with inventory := inv' }, alive)

@[inline] def applyDamage (p : Player) (source : DamageSource) (amount : UInt8) : Player × Bool :=
  let p' := { p with lastDamageSource := some source }
  p'.takeDamage amount

@[inline] def startSleep (p : Player) : Player :=
  { p with sleeping := true }

@[inline] def wakeUp (p : Player) : Player :=
  if p.sleeping then
    { p with sleeping := false, achievements := { p.achievements with wakeUp := p.achievements.wakeUp + 1 } }
  else
    p

def updateLifeStats
    (p : Player)
    (hungerEnabled thirstEnabled fatigueEnabled healthEnabled : Bool)
    (hungerRate thirstRate : Float) : Player :=
  let p :=
    if hungerEnabled then
      let inc := if p.sleeping then 0.5 else 1.0
      let next := p.hungerCounter + inc
      if next >= hungerRate then
        let food := if p.inventory.food.toNat > 0 then UInt8.ofNat (p.inventory.food.toNat - 1) else p.inventory.food
        { p with hungerCounter := 0.0, inventory := { p.inventory with food := food } }
      else
        { p with hungerCounter := next }
    else
      p
  let p :=
    if thirstEnabled then
      let inc := if p.sleeping then 0.5 else 1.0
      let next := p.thirstCounter + inc
      if next >= thirstRate then
        let drink := if p.inventory.drink.toNat > 0 then UInt8.ofNat (p.inventory.drink.toNat - 1) else p.inventory.drink
        { p with thirstCounter := 0.0, inventory := { p.inventory with drink := drink } }
      else
        { p with thirstCounter := next }
    else
      p
  let p :=
    if fatigueEnabled then
      let fatigue := if p.sleeping then p.fatigueCounter - 1 else p.fatigueCounter + 1
      if fatigue > 30 then
        let energy := if p.inventory.energy.toNat > 0 then UInt8.ofNat (p.inventory.energy.toNat - 1) else p.inventory.energy
        { p with fatigueCounter := 0, inventory := { p.inventory with energy := energy } }
      else if fatigue < -10 then
        let inv' := p.inventory.addEnergy 1
        { p with fatigueCounter := 0, inventory := inv' }
      else
        { p with fatigueCounter := fatigue }
    else
      p
  if !healthEnabled then
    let maxHealth := UInt8.ofNat Inventory.maxValue
    { p with recoverCounter := 0.0, inventory := { p.inventory with health := maxHealth }, lastHealth := maxHealth }
  else
    let hasEnergy := p.inventory.energy.toNat > 0 || p.sleeping
    let depleted := p.inventory.food.toNat == 0 || p.inventory.drink.toNat == 0 || !hasEnergy
    let p :=
      if depleted then
        let next := p.recoverCounter - 1.0
        if next < -15.0 then
          let cause :=
            if p.inventory.food.toNat == 0 then
              DamageSource.Starvation
            else if p.inventory.drink.toNat == 0 then
              DamageSource.Thirst
            else
              DamageSource.Exhaustion
          let health := UInt8.ofNat (Nat.max 0 (p.inventory.health.toNat - 1))
          { p with recoverCounter := 0.0, lastDamageSource := some cause, inventory := { p.inventory with health := health } }
        else
          { p with recoverCounter := next }
      else
        let rate := if p.sleeping then 2.0 else 1.0
        let next := p.recoverCounter + rate
        if next > 25.0 then
          let inv' := p.inventory.addHealth 1
          { p with recoverCounter := 0.0, inventory := inv' }
        else
          { p with recoverCounter := next }
    let p :=
      if p.sleeping && p.inventory.health.toNat < p.lastHealth.toNat then
        { p with sleeping := false, achievements := { p.achievements with wakeUp := p.achievements.wakeUp + 1 } }
      else
        p
    { p with lastHealth := p.inventory.health }

end Player

structure World where
  materials : Grid Material
  daylight : Float
  rngSeed : UInt64
  deriving Repr

namespace World

@[inline] def inBounds (w : World) (c : Coord) : Bool :=
  match Coord.toNat? c with
  | some (x, y) => w.materials.inBounds x y
  | none => false

@[inline] def getMaterial (w : World) (c : Coord) : Option Material :=
  w.materials.getCoord? c

@[inline] def setMaterial (w : World) (c : Coord) (m : Material) : World :=
  { w with materials := w.materials.setCoord c m }

@[inline] def getTile (w : World) (c : Coord) : Option Tile :=
  w.getMaterial c |>.map Tile.ofMaterial

@[inline] def setTile (w : World) (c : Coord) (t : Tile) : World :=
  { w with materials := w.materials.setCoord c (Tile.toMaterial t) }

@[inline] def neighbors4 (c : Coord) : List Coord :=
  [Coord.add c (-1) 0, Coord.add c 1 0, Coord.add c 0 (-1), Coord.add c 0 1]

@[inline] def hasAdjacentTable (w : World) (pos : Coord) : Bool :=
  (neighbors4 pos).any (fun c => w.getMaterial c == some Material.Table)

@[inline] def hasAdjacentFurnace (w : World) (pos : Coord) : Bool :=
  (neighbors4 pos).any (fun c => w.getMaterial c == some Material.Furnace)

end World

end Crafter
