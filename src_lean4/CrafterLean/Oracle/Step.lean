import CrafterLean.Action
import CrafterLean.Achievement
import CrafterLean.Oracle.State
import CrafterLean.Inventory
import CrafterLean.Entity
import CrafterLean.Craftax.Mobs
import CrafterLean.Craftax.Loot
import CrafterLean.Types.ChaCha8
import CrafterLean.World.Material
import CrafterLean.World.Types

namespace Crafter

structure Observation where
  step : Nat
  playerPos : Coord
  daylight : Float
  deriving Repr, Inhabited

structure StepOut where
  obs : Observation
  reward : Float
  done : Bool
  info : String
  achievements : List String
  deriving Repr, Inhabited

namespace Oracle

@[inline] def facingPos (p : Player) : Coord :=
  Coord.add p.pos p.facing.1 p.facing.2

@[inline] def updatePlayer (s : OracleState) (f : Player → Player) : OracleState :=
  let p := OracleState.getPlayerOrDefault s
  OracleState.setPlayer s (f p)

@[inline] def inc32 (x : UInt32) : UInt32 :=
  x + 1

@[inline] def nextFloat (s : OracleState) : OracleState × Float :=
  let (rng', value) := s.rng.nextFloat32
  ({ s with rng := rng' }, value)

@[inline] def nextNat (s : OracleState) (upper : Nat) : OracleState × Nat :=
  let (rng', value) := s.rng.nextNat upper
  ({ s with rng := rng' }, value)

@[inline] def intSign (x : Int) : Int :=
  if x > 0 then 1 else if x < 0 then -1 else 0

@[inline] def clampFloat (x lo hi : Float) : Float :=
  if x < lo then
    lo
  else if x > hi then
    hi
  else
    x

@[inline] def floatToIntTrunc (x : Float) : Int :=
  if x >= 0.0 then
    Int.ofNat (UInt64.toNat (Float.toUInt64 x))
  else
    let n := UInt64.toNat (Float.toUInt64 (-x))
    (- (Int.ofNat n))

@[inline] def floatToNatFloor (x : Float) : Nat :=
  let f := Float.floor x
  if f <= 0.0 then
    0
  else
    UInt64.toNat (Float.toUInt64 f)

@[inline] def floatToNatRound (x : Float) : Nat :=
  let r := Float.round x
  if r <= 0.0 then
    0
  else
    UInt64.toNat (Float.toUInt64 r)

@[inline] def scaledDamage (base : UInt8) (mult : Float) : UInt8 :=
  let dmg := (Float.ofNat base.toNat) * mult
  UInt8.ofNat (floatToNatFloor dmg)

@[inline] def saturatingAddU32 (a b : UInt32) : UInt32 :=
  let maxU32 := UInt32.toNat (UInt32.ofNat 0xFFFF_FFFF)
  let sum := a.toNat + b.toNat
  UInt32.ofNat (Nat.min sum maxU32)

@[inline] def saturatingIncU8 (x : UInt8) : UInt8 :=
  if x.toNat < 255 then
    UInt8.ofNat (x.toNat + 1)
  else
    x

@[inline] def manhattan (a b : Coord) : Nat :=
  Int.natAbs (a.x - b.x) + Int.natAbs (a.y - b.y)

@[inline] def towardDirection (fromPos toPos : Coord) (longAxis : Bool) : Int × Int :=
  let dx := toPos.x - fromPos.x
  let dy := toPos.y - fromPos.y
  let distX := Int.natAbs dx
  let distY := Int.natAbs dy
  let chooseX := if longAxis then distX > distY else distX <= distY
  if chooseX then
    (intSign dx, 0)
  else
    (0, intSign dy)

@[inline] def randomDirection (s : OracleState) : OracleState × (Int × Int) :=
  let (s, idx) := nextNat s 4
  let dir :=
    match idx with
    | 0 => (0, 1)
    | 1 => (0, -1)
    | 2 => (1, 0)
    | _ => (-1, 0)
  (s, dir)

@[inline] def applyPlayerDamageWithReduction
    (s : OracleState)
    (source : DamageSource)
    (baseDamage : Float)
    (sleepingMultiplier : Float)
    (reduction : Float) : OracleState :=
  if !s.config.healthEnabled then
    s
  else
    updatePlayer s fun p =>
      let damage0 := baseDamage * sleepingMultiplier
      let clamped := clampFloat reduction 0.0 0.9
      let damage := damage0 * (1.0 - clamped)
      let rounded := floatToNatRound damage
      let finalDamage := if rounded == 0 && damage > 0.0 then 1 else rounded
      let (p', _) := p.applyDamage source (UInt8.ofNat finalDamage)
      p'

@[inline] def grantXp (s : OracleState) (amount : UInt32) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.xpEnabled then
    s
  else
    updatePlayer s fun p =>
      let inv0 := { p.inventory with xp := saturatingAddU32 p.inventory.xp amount }
      let ach0 :=
        if s.config.craftax.achievementsEnabled then
          { p.achievements with gainXp := saturatingAddU32 p.achievements.gainXp amount }
        else
          p.achievements
      let maxLevels : Nat := 256
      let rec loop : Nat → Inventory → Achievements → Inventory × Achievements
        | 0, inv, ach => (inv, ach)
        | Nat.succ n, inv, ach =>
            let nextLevel := saturatingIncU8 inv.level
            let threshold := 10 * nextLevel.toNat
            if inv.xp.toNat < threshold then
              (inv, ach)
            else
              let inv := { inv with level := nextLevel, statPoints := saturatingIncU8 inv.statPoints }
              let ach :=
                if s.config.craftax.achievementsEnabled then
                  { ach with reachLevel := inc32 ach.reachLevel }
                else
                  ach
              loop n inv ach
      let (inv', ach') := loop maxLevels inv0 ach0
      { p with inventory := inv', achievements := ach' }

@[inline] def recordCraftaxKill (s : OracleState) (kind : CraftaxMobKind) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.achievementsEnabled then
    s
  else
    updatePlayer s fun p =>
      let ach :=
        match kind with
        | CraftaxMobKind.OrcSoldier => { p.achievements with defeatOrcSoldier := inc32 p.achievements.defeatOrcSoldier }
        | CraftaxMobKind.OrcMage => { p.achievements with defeatOrcMage := inc32 p.achievements.defeatOrcMage }
        | CraftaxMobKind.Knight => { p.achievements with defeatKnight := inc32 p.achievements.defeatKnight }
        | CraftaxMobKind.KnightArcher => { p.achievements with defeatKnightArcher := inc32 p.achievements.defeatKnightArcher }
        | CraftaxMobKind.Troll => { p.achievements with defeatTroll := inc32 p.achievements.defeatTroll }
        | _ => p.achievements
      { p with achievements := ach }

@[inline] def applyChestLoot (s : OracleState) (loot : ChestLoot) : OracleState :=
  updatePlayer s fun p =>
    let inv := p.inventory
    let inv :=
      if loot.arrows.toNat > 0 then inv.addArrows loot.arrows else inv
    let inv :=
      if loot.potionRed.toNat > 0 then inv.addPotionRed loot.potionRed else inv
    let inv :=
      if loot.potionGreen.toNat > 0 then inv.addPotionGreen loot.potionGreen else inv
    let inv :=
      if loot.potionBlue.toNat > 0 then inv.addPotionBlue loot.potionBlue else inv
    let inv :=
      if loot.potionPink.toNat > 0 then inv.addPotionPink loot.potionPink else inv
    let inv :=
      if loot.potionCyan.toNat > 0 then inv.addPotionCyan loot.potionCyan else inv
    let inv :=
      if loot.potionYellow.toNat > 0 then inv.addPotionYellow loot.potionYellow else inv
    let inv :=
      if loot.sapphire.toNat > 0 then inv.addSapphire loot.sapphire else inv
    let inv :=
      if loot.ruby.toNat > 0 then inv.addRuby loot.ruby else inv
    let inv :=
      if loot.coal.toNat > 0 then inv.addCoal loot.coal else inv
    let inv :=
      if loot.iron.toNat > 0 then inv.addIron loot.iron else inv
    let inv :=
      if loot.diamond.toNat > 0 then inv.addDiamond loot.diamond else inv
    let ach0 := p.achievements
    let ach1 :=
      if s.config.craftax.achievementsEnabled && loot.sapphire.toNat > 0 then
        { ach0 with collectSapphire := saturatingAddU32 ach0.collectSapphire (UInt32.ofNat loot.sapphire.toNat) }
      else
        ach0
    let ach2 :=
      if s.config.craftax.achievementsEnabled && loot.ruby.toNat > 0 then
        { ach1 with collectRuby := saturatingAddU32 ach1.collectRuby (UInt32.ofNat loot.ruby.toNat) }
      else
        ach1
    let ach3 :=
      if loot.coal.toNat > 0 then
        { ach2 with collectCoal := saturatingAddU32 ach2.collectCoal (UInt32.ofNat loot.coal.toNat) }
      else
        ach2
    let ach4 :=
      if loot.iron.toNat > 0 then
        { ach3 with collectIron := saturatingAddU32 ach3.collectIron (UInt32.ofNat loot.iron.toNat) }
      else
        ach3
    let ach5 :=
      if loot.diamond.toNat > 0 then
        { ach4 with collectDiamond := saturatingAddU32 ach4.collectDiamond (UInt32.ofNat loot.diamond.toNat) }
      else
        ach4
    { p with inventory := inv, achievements := ach5 }

@[inline] def randomSpawnNearPlayer (s : OracleState) (playerPos : Coord) (minDist maxDist : Float) : OracleState × Option Coord :=
  if maxDist <= 0.0 || minDist < 0.0 then
    (s, none)
  else
    let tau : Float := 6.283185307179586
    let rec loop (n : Nat) (s : OracleState) : OracleState × Option Coord :=
      match n with
      | 0 => (s, none)
      | Nat.succ n =>
          let (s, rAngle) := nextFloat s
          let angle := rAngle * tau
          let (s, rDist) := nextFloat s
          let dist := minDist + rDist * (maxDist - minDist)
          let dx := floatToIntTrunc (Float.cos angle * dist)
          let dy := floatToIntTrunc (Float.sin angle * dist)
          let pos := Coord.add playerPos dx dy
          if s.world.inBounds pos then
            (s, some pos)
          else
            loop n s
    loop 6 s

@[inline] def canMoveTo (s : OracleState) (c : Coord) : Bool :=
  if s.world.inBounds c then
    OracleState.isWalkable s c
  else
    false

@[inline] def applyMovement (s : OracleState) (dx dy : Int) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  let nextPos := Coord.add player.pos dx dy
  let facing : Int × Int := (dx, dy)
  let s' := OracleState.setPlayer s { player with facing := facing }
  if canMoveTo s' nextPos then
    let s'' := OracleState.setPlayer s' { player with pos := nextPos, facing := facing }
    match s''.world.getMaterial nextPos with
    | some m =>
        if m.isDeadly && s''.config.healthEnabled then
          let p := OracleState.getPlayerOrDefault s''
          let p' := { p with inventory := { p.inventory with health := 0 }, lastDamageSource := some DamageSource.Lava }
          OracleState.setPlayer s'' p'
        else
          s''
    | none => s''
  else
    s'

@[inline] def stepDone (cfg : SessionConfig) (step : Nat) (playerAlive : Bool) : Bool :=
  if cfg.healthEnabled && !playerAlive then
    true
  else
    match cfg.maxSteps with
    | none => false
    | some maxSteps => step >= maxSteps

@[inline] def observe (s : OracleState) : Observation :=
  let player := OracleState.getPlayerOrDefault s
  { step := s.step, playerPos := player.pos, daylight := s.world.daylight }

@[inline] def updateDaylight (s : OracleState) : OracleState :=
  if s.config.dayNightCycle then
    if s.config.dayCyclePeriod == 0 then
      s
    else
      let period := s.config.dayCyclePeriod
      let phase := s.step % period
      let progress := (Float.ofNat phase) / (Float.ofNat period) + 0.3
      let piConst : Float := 3.141592653589793
      let v := Float.abs (Float.cos (piConst * progress))
      let daylight := 1.0 - (v * v * v)
      { s with world := { s.world with daylight := daylight } }
  else
    s

@[inline] def interactWithObject (s : OracleState) (objId : UInt32) (player : Player) : OracleState :=
  match s.getObject? objId with
  | none => s
  | some obj =>
      match obj with
      | GameObject.Cow cow =>
          let damage := scaledDamage player.attackDamage s.config.playerDamageMult
          let (cow', alive) := Cow.takeDamage cow damage
          if alive then
            s.updateObject objId (GameObject.Cow cow')
          else
            let (s', _) := s.removeObject objId
            updatePlayer s' fun p =>
              let inv := p.inventory.addFood 6
              let ach := { p.achievements with eatCow := inc32 p.achievements.eatCow }
              { p with inventory := inv, achievements := ach }
      | GameObject.Zombie zombie =>
          let damage := scaledDamage player.attackDamage s.config.playerDamageMult
          let (z', alive) := Zombie.takeDamage zombie damage
          if alive then
            s.updateObject objId (GameObject.Zombie z')
          else
            let (s', _) := s.removeObject objId
            let s' := updatePlayer s' fun p =>
              let ach := { p.achievements with defeatZombie := inc32 p.achievements.defeatZombie }
              { p with achievements := ach }
            grantXp s' 2
      | GameObject.Skeleton skeleton =>
          let damage := scaledDamage player.attackDamage s.config.playerDamageMult
          let (s', alive) := Skeleton.takeDamage skeleton damage
          if alive then
            s.updateObject objId (GameObject.Skeleton s')
          else
            let (s', _) := s.removeObject objId
            let s' := updatePlayer s' fun p =>
              let ach := { p.achievements with defeatSkeleton := inc32 p.achievements.defeatSkeleton }
              { p with achievements := ach }
            grantXp s' 2
      | GameObject.CraftaxMob mob =>
          if !s.config.craftax.enabled || !s.config.craftax.combatEnabled then
            s
          else
            let damage := scaledDamage player.attackDamage s.config.playerDamageMult
            let (m', alive) := CraftaxMob.takeDamage mob damage
            if alive then
              s.updateObject objId (GameObject.CraftaxMob m')
            else
              let (s', _) := s.removeObject objId
              let s' := grantXp s' 3
              recordCraftaxKill s' m'.kind
      | GameObject.Plant plant =>
          if plant.isRipe then
            let (s', _) := s.removeObject objId
            updatePlayer s' fun p =>
              let inv := p.inventory.addFood 4
              let ach := { p.achievements with eatPlant := inc32 p.achievements.eatPlant }
              { p with inventory := inv, achievements := ach }
          else
            s
      | _ => s

@[inline] def interactWithTerrain (s : OracleState) (pos : Coord) (mat : Material) (player : Player) : OracleState :=
  match mat with
  | Material.Tree =>
      let world' := s.world.setMaterial pos Material.Grass
      let s' := { s with world := world' }
      updatePlayer s' fun p =>
        let inv := p.inventory.addWood 1
        let ach := { p.achievements with collectWood := inc32 p.achievements.collectWood }
        { p with inventory := inv, achievements := ach }
  | Material.Stone =>
      if player.inventory.bestPickaxeTier.toNat >= 1 then
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        updatePlayer s' fun p =>
          let inv := p.inventory.addStone 1
          let ach := { p.achievements with collectStone := inc32 p.achievements.collectStone }
          { p with inventory := inv, achievements := ach }
      else
        s
  | Material.Coal =>
      if player.inventory.bestPickaxeTier.toNat >= 1 then
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        updatePlayer s' fun p =>
          let inv := p.inventory.addCoal 1
          let ach := { p.achievements with collectCoal := inc32 p.achievements.collectCoal }
          { p with inventory := inv, achievements := ach }
      else
        s
  | Material.Iron =>
      if player.inventory.bestPickaxeTier.toNat >= 2 then
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        updatePlayer s' fun p =>
          let inv := p.inventory.addIron 1
          let ach := { p.achievements with collectIron := inc32 p.achievements.collectIron }
          { p with inventory := inv, achievements := ach }
      else
        s
  | Material.Diamond =>
      if player.inventory.bestPickaxeTier.toNat >= 3 then
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        updatePlayer s' fun p =>
          let inv := p.inventory.addDiamond 1
          let ach := { p.achievements with collectDiamond := inc32 p.achievements.collectDiamond }
          { p with inventory := inv, achievements := ach }
      else
        s
  | Material.Sapphire =>
      if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
        s
      else if player.inventory.bestPickaxeTier.toNat >= 4 then
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        if s.config.craftax.achievementsEnabled then
          updatePlayer s' fun p =>
            let inv := p.inventory.addSapphire 1
            let ach := { p.achievements with collectSapphire := inc32 p.achievements.collectSapphire }
            { p with inventory := inv, achievements := ach }
        else
          updatePlayer s' fun p => { p with inventory := p.inventory.addSapphire 1 }
      else
        s
  | Material.Ruby =>
      if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
        s
      else if player.inventory.bestPickaxeTier.toNat >= 4 then
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        if s.config.craftax.achievementsEnabled then
          updatePlayer s' fun p =>
            let inv := p.inventory.addRuby 1
            let ach := { p.achievements with collectRuby := inc32 p.achievements.collectRuby }
            { p with inventory := inv, achievements := ach }
        else
          updatePlayer s' fun p => { p with inventory := p.inventory.addRuby 1 }
      else
        s
  | Material.Chest =>
      if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled || !s.config.craftax.chestsEnabled then
        s
      else
        let world' := s.world.setMaterial pos Material.Path
        let s' := { s with world := world' }
        let (rng', loot) := CraftaxLoot.rollChestLoot s'.rng s'.config.craftax.loot
        let s' := { s' with rng := rng' }
        let s' :=
          if s.config.craftax.achievementsEnabled then
            updatePlayer s' fun p => { p with achievements := { p.achievements with openChest := inc32 p.achievements.openChest } }
          else
            s'
        applyChestLoot s' loot
  | Material.Water =>
      updatePlayer s fun p =>
        let inv := p.inventory.addDrink 1
        let ach := { p.achievements with collectDrink := inc32 p.achievements.collectDrink }
        { p with thirstCounter := 0.0, inventory := inv, achievements := ach }
  | Material.Grass =>
      let (s', roll) := nextFloat s
      if roll < 0.1 then
        updatePlayer s' fun p =>
          let inv := p.inventory.addSapling 1
          let ach := { p.achievements with collectSapling := inc32 p.achievements.collectSapling }
          { p with inventory := inv, achievements := ach }
      else
        s'
  | _ => s

@[inline] def processPlace (s : OracleState) (mat : Material) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  let target := facingPos player
  if s.world.getMaterial target != some Material.Grass then
    s
  else if s.getObjectAt? target |>.isSome then
    s
  else
    match mat with
    | Material.Stone =>
        let (inv', ok) := player.inventory.useStone
        if ok then
          let world' := s.world.setMaterial target mat
          let s' := { s with world := world' }
          updatePlayer s' fun p => { p with inventory := inv', achievements := { p.achievements with placeStone := inc32 p.achievements.placeStone } }
        else
          s
    | Material.Table =>
        let (inv', ok) := player.inventory.useWoodForTable
        if ok then
          let world' := s.world.setMaterial target mat
          let s' := { s with world := world' }
          updatePlayer s' fun p => { p with inventory := inv', achievements := { p.achievements with placeTable := inc32 p.achievements.placeTable } }
        else
          s
    | Material.Furnace =>
        let (inv', ok) := player.inventory.useStoneForFurnace
        if ok then
          let world' := s.world.setMaterial target mat
          let s' := { s with world := world' }
          updatePlayer s' fun p => { p with inventory := inv', achievements := { p.achievements with placeFurnace := inc32 p.achievements.placeFurnace } }
        else
          s
    | _ => s

@[inline] def processPlacePlant (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  let target := facingPos player
  if s.world.getMaterial target != some Material.Grass then
    s
  else if s.getObjectAt? target |>.isSome then
    s
  else
    let (inv', ok) := player.inventory.useSapling
    if ok then
      let plant := Plant.new target
      let (s', _) := s.addObject (GameObject.Plant plant)
      updatePlayer s' fun p =>
        let ach := { p.achievements with placePlant := inc32 p.achievements.placePlant }
        { p with inventory := inv', achievements := ach }
    else
      s

@[inline] def processCraftWoodPickaxe (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if !s.world.hasAdjacentTable player.pos then
    s
  else
    let (inv', ok) := player.inventory.craftWoodPickaxe
    if ok then
      updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeWoodPickaxe := inc32 p.achievements.makeWoodPickaxe } }
    else
      s

@[inline] def processCraftStonePickaxe (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if !s.world.hasAdjacentTable player.pos then
    s
  else
    let (inv', ok) := player.inventory.craftStonePickaxe
    if ok then
      updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeStonePickaxe := inc32 p.achievements.makeStonePickaxe } }
    else
      s

@[inline] def processCraftIronPickaxe (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if !s.world.hasAdjacentTable player.pos || !s.world.hasAdjacentFurnace player.pos then
    s
  else
    let (inv', ok) := player.inventory.craftIronPickaxe
    if ok then
      updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeIronPickaxe := inc32 p.achievements.makeIronPickaxe } }
    else
      s

@[inline] def processCraftWoodSword (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if !s.world.hasAdjacentTable player.pos then
    s
  else
    let (inv', ok) := player.inventory.craftWoodSword
    if ok then
      updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeWoodSword := inc32 p.achievements.makeWoodSword } }
    else
      s

@[inline] def processCraftStoneSword (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if !s.world.hasAdjacentTable player.pos then
    s
  else
    let (inv', ok) := player.inventory.craftStoneSword
    if ok then
      updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeStoneSword := inc32 p.achievements.makeStoneSword } }
    else
      s

@[inline] def processCraftIronSword (s : OracleState) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if !s.world.hasAdjacentTable player.pos || !s.world.hasAdjacentFurnace player.pos then
    s
  else
    let (inv', ok) := player.inventory.craftIronSword
    if ok then
      updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeIronSword := inc32 p.achievements.makeIronSword } }
    else
      s

@[inline] def processCraftDiamondPickaxe (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if !s.world.hasAdjacentTable player.pos then
      s
    else
      let (inv', ok) := player.inventory.craftDiamondPickaxe
      if ok then
        if s.config.craftax.achievementsEnabled then
          updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeDiamondPickaxe := inc32 p.achievements.makeDiamondPickaxe } }
        else
          updatePlayer s fun p => { p with inventory := inv' }
      else
        s

@[inline] def processCraftDiamondSword (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if !s.world.hasAdjacentTable player.pos then
      s
    else
      let (inv', ok) := player.inventory.craftDiamondSword
      if ok then
        if s.config.craftax.achievementsEnabled then
          updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeDiamondSword := inc32 p.achievements.makeDiamondSword } }
        else
          updatePlayer s fun p => { p with inventory := inv' }
      else
        s

@[inline] def processCraftIronArmor (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if !s.world.hasAdjacentTable player.pos || !s.world.hasAdjacentFurnace player.pos then
      s
    else
      let (inv', ok) := player.inventory.craftIronArmor
      if ok then
        if s.config.craftax.achievementsEnabled then
          updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeIronArmor := inc32 p.achievements.makeIronArmor } }
        else
          updatePlayer s fun p => { p with inventory := inv' }
      else
        s

@[inline] def processCraftDiamondArmor (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if !s.world.hasAdjacentTable player.pos then
      s
    else
      let (inv', ok) := player.inventory.craftDiamondArmor
      if ok then
        if s.config.craftax.achievementsEnabled then
          updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeDiamondArmor := inc32 p.achievements.makeDiamondArmor } }
        else
          updatePlayer s fun p => { p with inventory := inv' }
      else
        s

@[inline] def processCraftBow (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if !s.world.hasAdjacentTable player.pos then
      s
    else
      let (inv', ok) := player.inventory.craftBow
      if ok then
        if s.config.craftax.achievementsEnabled then
          updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeBow := inc32 p.achievements.makeBow } }
        else
          updatePlayer s fun p => { p with inventory := inv' }
      else
        s

@[inline] def processCraftArrow (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if !s.world.hasAdjacentTable player.pos then
      s
    else
      let (inv', ok) := player.inventory.craftArrow
      if ok then
        if s.config.craftax.achievementsEnabled then
          updatePlayer s fun p => { p with inventory := inv', achievements := { p.achievements with makeArrow := inc32 p.achievements.makeArrow } }
        else
          updatePlayer s fun p => { p with inventory := inv' }
      else
        s

@[inline] def processShootArrow (s : OracleState) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled || !s.config.craftax.combatEnabled then
    s
  else
    let player := OracleState.getPlayerOrDefault s
    if player.inventory.bow.toNat == 0 || player.inventory.arrows.toNat == 0 then
      s
    else
      let target := facingPos player
      let s' := updatePlayer s fun p =>
        let arrows := if p.inventory.arrows.toNat > 0 then UInt8.ofNat (p.inventory.arrows.toNat - 1) else p.inventory.arrows
        { p with inventory := { p.inventory with arrows := arrows } }
      if OracleState.isWalkable s' target then
        let baseDamage := UInt8.ofNat (2 + player.inventory.bestSwordTier.toNat)
        let arrow := Arrow.withStats target player.facing ProjectileKind.Arrow baseDamage DamageSource.PlayerArrow
        let (s'', _) := s'.addObject (GameObject.Arrow arrow)
        s''
      else
        s'

@[inline] def processDrinkPotion (s : OracleState) (kind : PotionKind) : OracleState :=
  if !s.config.craftax.enabled || !s.config.craftax.itemsEnabled || !s.config.craftax.potionsEnabled then
    s
  else
    updatePlayer s fun p =>
      let inv := p.inventory
      let (inv', consumed) :=
        match kind with
        | PotionKind.Red =>
            if inv.potionRed.toNat > 0 then
              let inv' := { inv with potionRed := UInt8.ofNat (inv.potionRed.toNat - 1) }
              (inv'.addHealth 2, true)
            else
              (inv, false)
        | PotionKind.Green =>
            if inv.potionGreen.toNat > 0 then
              let inv' := { inv with potionGreen := UInt8.ofNat (inv.potionGreen.toNat - 1) }
              (inv'.addEnergy 2, true)
            else
              (inv, false)
        | PotionKind.Blue =>
            if inv.potionBlue.toNat > 0 then
              let inv' := { inv with potionBlue := UInt8.ofNat (inv.potionBlue.toNat - 1) }
              (inv'.addDrink 2, true)
            else
              (inv, false)
        | PotionKind.Pink =>
            if inv.potionPink.toNat > 0 then
              let inv' := { inv with potionPink := UInt8.ofNat (inv.potionPink.toNat - 1) }
              (inv'.addFood 2, true)
            else
              (inv, false)
        | PotionKind.Cyan =>
            if inv.potionCyan.toNat > 0 then
              let inv' := { inv with potionCyan := UInt8.ofNat (inv.potionCyan.toNat - 1) }
              let inv' := inv'.addHealth 1
              (inv'.addEnergy 1, true)
            else
              (inv, false)
        | PotionKind.Yellow =>
            if inv.potionYellow.toNat > 0 then
              let inv' := { inv with potionYellow := UInt8.ofNat (inv.potionYellow.toNat - 1) }
              let inv' := inv'.addFood 1
              (inv'.addDrink 1, true)
            else
              (inv, false)
      let ach :=
        if consumed && s.config.craftax.achievementsEnabled then
          { p.achievements with drinkPotion := inc32 p.achievements.drinkPotion }
        else
          p.achievements
      { p with inventory := inv', achievements := ach }

@[inline] def processPlayerAction (s : OracleState) (a : Action) : OracleState :=
  let player := OracleState.getPlayerOrDefault s
  if player.sleeping && a != Action.Noop && a != Action.Sleep then
    OracleState.setPlayer s (player.wakeUp)
  else
    match a with
    | Action.Noop => s
    | Action.MoveLeft => applyMovement s (-1) 0
    | Action.MoveRight => applyMovement s 1 0
    | Action.MoveUp => applyMovement s 0 (-1)
    | Action.MoveDown => applyMovement s 0 1
    | Action.Do =>
        let player' := OracleState.getPlayerOrDefault s
        let target := facingPos player'
        match s.getObjectIdAt? target with
        | some oid => interactWithObject s oid player'
        | none =>
            match s.world.getMaterial target with
            | some mat => interactWithTerrain s target mat player'
            | none => s
    | Action.Sleep => updatePlayer s fun p => p.startSleep
    | Action.PlaceStone => processPlace s Material.Stone
    | Action.PlaceTable => processPlace s Material.Table
    | Action.PlaceFurnace => processPlace s Material.Furnace
    | Action.PlacePlant => processPlacePlant s
    | Action.MakeWoodPickaxe => processCraftWoodPickaxe s
    | Action.MakeStonePickaxe => processCraftStonePickaxe s
    | Action.MakeIronPickaxe => processCraftIronPickaxe s
    | Action.MakeWoodSword => processCraftWoodSword s
    | Action.MakeStoneSword => processCraftStoneSword s
    | Action.MakeIronSword => processCraftIronSword s
    | Action.MakeDiamondPickaxe => processCraftDiamondPickaxe s
    | Action.MakeDiamondSword => processCraftDiamondSword s
    | Action.MakeIronArmor => processCraftIronArmor s
    | Action.MakeDiamondArmor => processCraftDiamondArmor s
    | Action.MakeBow => processCraftBow s
    | Action.MakeArrow => processCraftArrow s
    | Action.ShootArrow => processShootArrow s
    | Action.DrinkPotionRed => processDrinkPotion s PotionKind.Red
    | Action.DrinkPotionGreen => processDrinkPotion s PotionKind.Green
    | Action.DrinkPotionBlue => processDrinkPotion s PotionKind.Blue
    | Action.DrinkPotionPink => processDrinkPotion s PotionKind.Pink
    | Action.DrinkPotionCyan => processDrinkPotion s PotionKind.Cyan
    | Action.DrinkPotionYellow => processDrinkPotion s PotionKind.Yellow

@[inline] def processLifeStats (s : OracleState) : OracleState :=
  updatePlayer s fun p =>
    let p' := p.updateLifeStats s.config.hungerEnabled s.config.thirstEnabled s.config.fatigueEnabled s.config.healthEnabled (Float.ofNat s.config.hungerRate) (Float.ofNat s.config.thirstRate)
    if p'.sleeping && p'.inventory.energy.toNat >= Inventory.maxValue then
      p'.wakeUp
    else
      p'

@[inline] def processCowAi (s : OracleState) (id : UInt32) (cow : Cow) : OracleState :=
  let (s, roll) := nextFloat s
  if roll < 0.5 then
    s
  else
    let (s, dir) := randomDirection s
    let newPos := Coord.add cow.pos dir.1 dir.2
    if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
      s.moveObject id newPos
    else
      s

@[inline] def processZombieAi (s : OracleState) (id : UInt32) (zombie : Zombie) (playerPos : Coord) (playerSleeping : Bool) : OracleState :=
  let dist := manhattan zombie.pos playerPos
  let s :=
    if dist <= 8 then
      let (s, roll) := nextFloat s
      if roll < 0.9 then
        let (s, rollAxis) := nextFloat s
        let longAxis := decide (rollAxis < 0.8)
        let (dx, dy) := towardDirection zombie.pos playerPos longAxis
        let newPos := Coord.add zombie.pos dx dy
        if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
          s.moveObject id newPos
        else
          s
      else
        let (s, dir) := randomDirection s
        let newPos := Coord.add zombie.pos dir.1 dir.2
        if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
          s.moveObject id newPos
        else
          s
    else
      let (s, dir) := randomDirection s
      let newPos := Coord.add zombie.pos dir.1 dir.2
      if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
        s.moveObject id newPos
      else
        s
  let zombiePos :=
    match s.getObject? id with
    | some obj => GameObject.position obj
    | none => zombie.pos
  let distAfter := manhattan zombiePos playerPos
  let (s, cooldown) :=
    if distAfter <= 1 then
      if zombie.cooldown.toNat > 0 then
        (s, UInt8.ofNat (zombie.cooldown.toNat - 1))
      else
        let baseDamage := 2.0 * s.config.zombieDamageMult
        let sleepMult := if playerSleeping then 3.5 else 1.0
        let reduction :=
          if s.config.craftax.enabled && s.config.craftax.combatEnabled then
            (OracleState.getPlayer? s).map (fun p => p.inventory.armorReduction) |>.getD 0.0
          else
            0.0
        let s := applyPlayerDamageWithReduction s DamageSource.Zombie baseDamage sleepMult reduction
        let s := if playerSleeping then updatePlayer s fun p => p.wakeUp else s
        (s, UInt8.ofNat 5)
    else
      (s, zombie.cooldown)
  match s.getObject? id with
  | some (GameObject.Zombie z) =>
      s.updateObject id (GameObject.Zombie { z with cooldown := cooldown })
  | _ => s

@[inline] def processSkeletonAi (s : OracleState) (id : UInt32) (skeleton : Skeleton) (playerPos : Coord) : OracleState :=
  let skeleton := skeleton.tickReload
  let dist := manhattan skeleton.pos playerPos
  let (s, retreated) :=
    if dist <= 3 then
      let (s, rollAxis) := nextFloat s
      let longAxis := decide (rollAxis < 0.6)
      let (dx, dy) := towardDirection skeleton.pos playerPos longAxis
      let newPos := Coord.add skeleton.pos (-dx) (-dy)
      if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
        (s.moveObject id newPos, true)
      else
        (s, false)
    else
      (s, false)
  if retreated then
    match s.getObject? id with
    | some (GameObject.Skeleton sk) =>
        s.updateObject id (GameObject.Skeleton { sk with reload := skeleton.reload })
    | _ => s
  else
    let shootAttempt :=
      if dist <= 5 && skeleton.canShoot then
        let (s, rollShoot) := nextFloat s
        if rollShoot < 0.5 then
          let (dx, dy) := towardDirection skeleton.pos playerPos true
          let arrowPos := Coord.add skeleton.pos dx dy
          let base := 2.0 * s.config.arrowDamageMult
          let rounded := floatToNatRound base
          let dmgNat := if rounded == 0 && base > 0.0 then 1 else rounded
          let arrow := Arrow.withStats arrowPos (dx, dy) ProjectileKind.Arrow (UInt8.ofNat dmgNat) DamageSource.Arrow
          let (s, _) := s.addObject (GameObject.Arrow arrow)
          let skeleton := skeleton.resetReload
          (s, some skeleton)
        else
          (s, none)
      else
        (s, none)
    let (s, skeletonOpt) := shootAttempt
    match skeletonOpt with
    | some sk =>
        match s.getObject? id with
        | some (GameObject.Skeleton cur) =>
            s.updateObject id (GameObject.Skeleton { cur with reload := sk.reload })
        | _ => s
    | none =>
        let chaseAttempt :=
          if dist <= 8 then
            let (s, rollChase) := nextFloat s
            if rollChase < 0.3 then
              let (s, rollAxis) := nextFloat s
              let longAxis := decide (rollAxis < 0.6)
              let (dx, dy) := towardDirection skeleton.pos playerPos longAxis
              let newPos := Coord.add skeleton.pos dx dy
              let s :=
                if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
                  s.moveObject id newPos
                else
                  s
              (s, true)
            else
              (s, false)
          else
            (s, false)
        let (s, chased) := chaseAttempt
        let s :=
          if chased then
            s
          else
            let (s, rollIdle) := nextFloat s
            if rollIdle < 0.2 then
              let (s, dir) := randomDirection s
              let newPos := Coord.add skeleton.pos dir.1 dir.2
              if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
                s.moveObject id newPos
              else
                s
            else
              s
        match s.getObject? id with
        | some (GameObject.Skeleton cur) =>
            s.updateObject id (GameObject.Skeleton { cur with reload := skeleton.reload })
        | _ => s

@[inline] def processCraftaxMobAi (s : OracleState) (id : UInt32) (mob : CraftaxMob) (playerPos : Coord) (playerSleeping : Bool) : OracleState :=
  let stats := craftaxStats mob.kind
  let dist := manhattan mob.pos playerPos
  let combatEnabled := s.config.craftax.combatEnabled
  let mob0 :=
    if mob.cooldown.toNat > 0 then
      { mob with cooldown := UInt8.ofNat (mob.cooldown.toNat - 1) }
    else
      mob
  let (s, mob) :=
    if mob0.isPassive then
      let moveChance :=
        match mob0.kind with
        | CraftaxMobKind.Bat => 0.6
        | CraftaxMobKind.Snail => 0.3
        | _ => 0.4
      let (s, roll) := nextFloat s
      if roll < moveChance then
        let (s, dir) := randomDirection s
        let newPos := Coord.add mob0.pos dir.1 dir.2
        let walkable :=
          match mob0.kind with
          | CraftaxMobKind.Bat =>
              s.world.inBounds newPos && (s.getObjectAt? newPos |>.isNone)
          | _ =>
              OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone)
        if walkable then
          (s.moveObject id newPos, mob0)
        else
          (s, mob0)
      else
        (s, mob0)
    else
      let (s, mob, attacked) :=
        if combatEnabled && stats.isRanged && decide (dist <= Int.toNat stats.range) && mob0.cooldown.toNat == 0 then
          let (dx, dy) := towardDirection mob0.pos playerPos true
          let arrowPos := Coord.add mob0.pos dx dy
          if s.world.inBounds arrowPos then
            let source :=
              match stats.projectile with
              | ProjectileKind.Arrow => DamageSource.CraftaxRanged
              | ProjectileKind.Fireball | ProjectileKind.Iceball => DamageSource.CraftaxMagic
            let arrow := Arrow.withStats arrowPos (dx, dy) stats.projectile stats.rangedDamage source
            let (s', _) := s.addObject (GameObject.Arrow arrow)
            (s', { mob0 with cooldown := stats.cooldown }, true)
          else
            (s, mob0, false)
        else
          (s, mob0, false)
      let (s, mob, attacked) :=
        if combatEnabled && stats.isMelee && decide (dist <= 1) && mob.cooldown.toNat == 0 then
          let reduction :=
            if s.config.craftax.enabled && s.config.craftax.combatEnabled then
              (OracleState.getPlayer? s).map (fun p => p.inventory.armorReduction) |>.getD 0.0
            else
              0.0
          let sleepMult := if playerSleeping then 3.5 else 1.0
          let s := applyPlayerDamageWithReduction s DamageSource.CraftaxMelee (Float.ofNat stats.meleeDamage.toNat) sleepMult reduction
          let s := if playerSleeping then updatePlayer s fun p => p.wakeUp else s
          (s, { mob with cooldown := stats.cooldown }, true)
        else
          (s, mob, attacked)
      if attacked then
        (s, mob)
      else
        let flee := stats.isRanged && decide (dist <= 2)
        let (s, moveToward) :=
          if dist <= 8 then
            let (s, roll) := nextFloat s
            (s, decide (roll < 0.6))
          else
            (s, false)
        let (s, moveRandom) :=
          let (s, roll) := nextFloat s
          (s, decide (roll < 0.2))
        let (s, dir) :=
          if moveRandom then
            randomDirection s
          else
            (s, (0, 0))
        let (dx, dy) :=
          if flee then
            let (dx, dy) := towardDirection mob.pos playerPos true
            (-dx, -dy)
          else if moveToward then
            towardDirection mob.pos playerPos true
          else if moveRandom then
            dir
          else
            (0, 0)
        let s :=
          if dx != 0 || dy != 0 then
            let newPos := Coord.add mob.pos dx dy
            if OracleState.isWalkable s newPos && (s.getObjectAt? newPos |>.isNone) then
              s.moveObject id newPos
            else
              s
          else
            s
        (s, mob)
  match s.getObject? id with
  | some (GameObject.CraftaxMob cur) =>
      s.updateObject id (GameObject.CraftaxMob { cur with cooldown := mob.cooldown })
  | _ => s

@[inline] def processMobs (s : OracleState) : OracleState :=
  let playerPos := (OracleState.getPlayer? s).map (fun p => p.pos)
  let playerSleeping := (OracleState.getPlayer? s).map (fun p => p.sleeping) |>.getD false
  let mobIds := s.objects.foldl (init := #[]) fun acc entry =>
    let (oid, obj) := entry
    match obj with
    | GameObject.Cow _ | GameObject.Zombie _ | GameObject.Skeleton _ | GameObject.CraftaxMob _ => acc.push oid
    | _ => acc
  mobIds.foldl (init := s) fun acc oid =>
    match acc.getObject? oid with
    | some (GameObject.Cow cow) => processCowAi acc oid cow
    | some (GameObject.Zombie zombie) =>
        match playerPos with
        | some pos => processZombieAi acc oid zombie pos playerSleeping
        | none => acc
    | some (GameObject.Skeleton skel) =>
        match playerPos with
        | some pos => processSkeletonAi acc oid skel pos
        | none => acc
    | some (GameObject.CraftaxMob mob) =>
        if !acc.config.craftax.enabled || !acc.config.craftax.mobsEnabled then
          acc
        else
          match playerPos with
          | some pos => processCraftaxMobAi acc oid mob pos playerSleeping
          | none => acc
    | _ => acc

@[inline] def processArrows (s : OracleState) : OracleState :=
  let arrowIds := s.objects.foldl (init := #[]) fun acc entry =>
    let (oid, obj) := entry
    match obj with
    | GameObject.Arrow _ => acc.push oid
    | _ => acc
  arrowIds.foldl (init := s) fun acc oid =>
    match acc.getObject? oid with
    | some (GameObject.Arrow arrow) =>
        let nextPos := arrow.nextPosition
        match OracleState.getPlayer? acc with
        | some player =>
            if nextPos == player.pos then
              let reduction :=
                if acc.config.craftax.enabled && acc.config.craftax.combatEnabled then
                  player.inventory.armorReduction
                else
                  0.0
              let acc := applyPlayerDamageWithReduction acc arrow.source (Float.ofNat arrow.damage.toNat) 1.0 reduction
              let acc := if player.sleeping then updatePlayer acc fun p => p.wakeUp else acc
              let (acc, _) := acc.removeObject oid
              acc
            else
              let acc :=
                match acc.getObjectIdAt? nextPos with
                | some targetId =>
                    let (acc, removeTarget, grantXpAmount, craftaxKill) :=
                      match acc.getObject? targetId with
                      | some (GameObject.Cow cow) =>
                          let (cow', alive) := Cow.takeDamage cow arrow.damage
                          if alive then
                            (acc.updateObject targetId (GameObject.Cow cow'), false, none, none)
                          else
                            (acc, true, none, none)
                      | some (GameObject.Zombie zombie) =>
                          let (z', alive) := Zombie.takeDamage zombie arrow.damage
                          if alive then
                            (acc.updateObject targetId (GameObject.Zombie z'), false, none, none)
                          else
                            let grant := if arrow.source == DamageSource.PlayerArrow then some 2 else none
                            (acc, true, grant, none)
                      | some (GameObject.Skeleton skel) =>
                          let (sk', alive) := Skeleton.takeDamage skel arrow.damage
                          if alive then
                            (acc.updateObject targetId (GameObject.Skeleton sk'), false, none, none)
                          else
                            let grant := if arrow.source == DamageSource.PlayerArrow then some 2 else none
                            (acc, true, grant, none)
                      | some (GameObject.Plant plant) =>
                          if plant.health.toNat > arrow.damage.toNat then
                            let plant' := { plant with health := UInt8.ofNat (plant.health.toNat - arrow.damage.toNat) }
                            (acc.updateObject targetId (GameObject.Plant plant'), false, none, none)
                          else
                            (acc, true, none, none)
                      | some (GameObject.CraftaxMob mob) =>
                          let (m', alive) := CraftaxMob.takeDamage mob arrow.damage
                          if alive then
                            (acc.updateObject targetId (GameObject.CraftaxMob m'), false, none, none)
                          else
                            let grant := if arrow.source == DamageSource.PlayerArrow then some 3 else none
                            let kill := if arrow.source == DamageSource.PlayerArrow then some mob.kind else none
                            (acc, true, grant, kill)
                      | _ => (acc, false, none, none)
                    let acc :=
                      if removeTarget then
                        let (acc', _) := acc.removeObject targetId
                        acc'
                      else
                        acc
                    let (acc, _) := acc.removeObject oid
                    let acc :=
                      match grantXpAmount with
                      | some amount => grantXp acc (UInt32.ofNat amount)
                      | none => acc
                    match craftaxKill with
                    | some kind => recordCraftaxKill acc kind
                    | none => acc
                | none =>
                    if !acc.world.inBounds nextPos then
                      let (acc, _) := acc.removeObject oid
                      acc
                    else
                      match acc.world.getMaterial nextPos with
                      | some mat =>
                          if mat == Material.Table || mat == Material.Furnace then
                            let world' := acc.world.setMaterial nextPos Material.Path
                            let acc := { acc with world := world' }
                            let (acc, _) := acc.removeObject oid
                            acc
                          else
                            let canPass := mat.isWalkable || mat == Material.Water || mat == Material.Lava
                            if !canPass then
                              let (acc, _) := acc.removeObject oid
                              acc
                            else
                              acc.moveObject oid nextPos
                      | none =>
                          acc.moveObject oid nextPos
              acc
        | none =>
            if !acc.world.inBounds nextPos then
              let (acc, _) := acc.removeObject oid
              acc
            else
              acc.moveObject oid nextPos
    | _ => acc

@[inline] def spawnDespawnMobs (s : OracleState) : OracleState :=
  match OracleState.getPlayer? s with
  | none => s
  | some p =>
      let playerPos := p.pos
      let (s, toRemove) := s.objects.foldl (init := (s, #[])) fun acc entry =>
        let (s, ids) := acc
        let (oid, obj) := entry
        let pos := GameObject.position obj
        let dist := manhattan pos playerPos
        if dist > 30 then
          match obj with
          | GameObject.Cow _ =>
              let (s, roll) := nextFloat s
              if roll < s.config.cowDespawnRate then
                (s, ids.push oid)
              else
                (s, ids)
          | GameObject.Zombie _ =>
              let (s, roll) := nextFloat s
              if roll < s.config.zombieDespawnRate then
                (s, ids.push oid)
              else
                (s, ids)
          | GameObject.CraftaxMob mob =>
              if s.config.craftax.enabled && s.config.craftax.mobsEnabled then
                let rate := if mob.isHostile then s.config.zombieDespawnRate else s.config.cowDespawnRate
                let (s, roll) := nextFloat s
                if roll < rate then
                  (s, ids.push oid)
                else
                  (s, ids)
              else
                (s, ids)
          | _ => (s, ids)
        else
          (s, ids)
      let s := toRemove.foldl (init := s) fun acc oid =>
        let (acc', _) := acc.removeObject oid
        acc'
      let s :=
        if s.world.daylight < 0.5 then
          let (s, roll) := nextFloat s
          if roll < s.config.zombieSpawnRate * 0.01 then
            let (s, randAngle) := nextFloat s
            let angle := randAngle * 6.283185307179586
            let (s, randDist) := nextFloat s
            let dist := 15.0 + randDist * 10.0
            let dx := floatToIntTrunc (Float.cos angle * dist)
            let dy := floatToIntTrunc (Float.sin angle * dist)
            let spawnPos := Coord.add playerPos dx dy
            if OracleState.isWalkable s spawnPos && (s.getObjectAt? spawnPos |>.isNone) then
              let zombie := Zombie.new spawnPos (UInt8.ofNat s.config.zombieHealth)
              let (s', _) := s.addObject (GameObject.Zombie zombie)
              s'
            else
              s
          else
            s
        else
          s
      let s :=
        let (s, roll) := nextFloat s
        if roll < s.config.cowSpawnRate * 0.1 then
          let (s, randAngle) := nextFloat s
          let angle := randAngle * 6.283185307179586
          let (s, randDist) := nextFloat s
          let dist := 10.0 + randDist * 15.0
          let dx := floatToIntTrunc (Float.cos angle * dist)
          let dy := floatToIntTrunc (Float.sin angle * dist)
          let spawnPos := Coord.add playerPos dx dy
          if OracleState.isWalkable s spawnPos && (s.getObjectAt? spawnPos |>.isNone) then
            let cow := Cow.withHealth spawnPos (UInt8.ofNat s.config.cowHealth)
            let (s', _) := s.addObject (GameObject.Cow cow)
            s'
          else
            s
        else
          s
      if !s.config.craftax.enabled || !s.config.craftax.mobsEnabled then
        s
      else
        let s :=
          if s.world.daylight < 0.5 then
            let hostile : List (CraftaxMobKind × Float × Float) :=
              [
                (CraftaxMobKind.OrcSoldier, 0.01, s.config.craftax.spawn.orcSoldierDensity),
                (CraftaxMobKind.OrcMage, 0.008, s.config.craftax.spawn.orcMageDensity),
                (CraftaxMobKind.Knight, 0.004, s.config.craftax.spawn.knightDensity),
                (CraftaxMobKind.KnightArcher, 0.004, s.config.craftax.spawn.knightArcherDensity),
                (CraftaxMobKind.Troll, 0.003, s.config.craftax.spawn.trollDensity)
              ]
            hostile.foldl (init := s) fun acc entry =>
              let (kind, baseRate, density) := entry
              let (acc, roll) := nextFloat acc
              if roll < baseRate * density then
                let (acc, posOpt) := randomSpawnNearPlayer acc playerPos 12.0 20.0
                match posOpt with
                | some pos =>
                    if OracleState.isWalkable acc pos && (acc.getObjectAt? pos |>.isNone) then
                      let stats := craftaxStats kind
                      let mob := CraftaxMob.new kind pos stats.health
                      let (acc', _) := acc.addObject (GameObject.CraftaxMob mob)
                      acc'
                    else
                      acc
                | none => acc
              else
                acc
          else
            s
        let s :=
          let (s, roll) := nextFloat s
          if roll < 0.02 * s.config.craftax.spawn.snailDensity then
            let (s, posOpt) := randomSpawnNearPlayer s playerPos 8.0 16.0
            match posOpt with
            | some pos =>
                if s.world.getMaterial pos == some Material.Grass && (s.getObjectAt? pos |>.isNone) then
                  let stats := craftaxStats CraftaxMobKind.Snail
                  let mob := CraftaxMob.new CraftaxMobKind.Snail pos stats.health
                  let (s', _) := s.addObject (GameObject.CraftaxMob mob)
                  s'
                else
                  s
            | none => s
          else
            s
        let (s, roll) := nextFloat s
        if roll < 0.02 * s.config.craftax.spawn.batDensity then
          let (s, posOpt) := randomSpawnNearPlayer s playerPos 8.0 16.0
          match posOpt with
          | some pos =>
              if s.world.getMaterial pos == some Material.Path && (s.getObjectAt? pos |>.isNone) then
                let stats := craftaxStats CraftaxMobKind.Bat
                let mob := CraftaxMob.new CraftaxMobKind.Bat pos stats.health
                let (s', _) := s.addObject (GameObject.CraftaxMob mob)
                s'
              else
                s
          | none => s
        else
          s

@[inline] def processPlants (s : OracleState) : OracleState :=
  let plantIds := s.objects.foldl (init := #[]) fun acc entry =>
    let (oid, obj) := entry
    match obj with
    | GameObject.Plant _ => acc.push oid
    | _ => acc
  let s := plantIds.foldl (init := s) fun acc oid =>
    match acc.getObject? oid with
    | some (GameObject.Plant plant) =>
        let directions := World.neighbors4 plant.pos
        let takeDamage := directions.any fun pos =>
          match acc.getObjectAt? pos with
          | some (GameObject.Zombie _) => true
          | some (GameObject.Skeleton _) => true
          | some (GameObject.Cow _) => true
          | some (GameObject.CraftaxMob m) => m.isHostile
          | _ => false
        let plant' :=
          if takeDamage then
            if plant.health.toNat > 1 then
              { plant with health := UInt8.ofNat (plant.health.toNat - 1) }
            else
              { plant with health := 0 }
          else
            plant.grow
        acc.updateObject oid (GameObject.Plant plant')
    | _ => acc
  let deadIds := s.objects.foldl (init := #[]) fun acc entry =>
    let (oid, obj) := entry
    match obj with
    | GameObject.Plant p => if p.health.toNat == 0 then acc.push oid else acc
    | _ => acc
  deadIds.foldl (init := s) fun acc oid =>
    let (s', _) := acc.removeObject oid
    s'

@[inline] def step (s : OracleState) (a : Action) : OracleState × StepOut :=
  let s1 := updateDaylight s
  let s2 := processPlayerAction s1 a
  let s3 := processLifeStats s2
  let s4 := processMobs s3
  let s5 := processArrows s4
  let s6 := processPlants s5
  let s7 := spawnDespawnMobs s6
  let s8 := { s7 with step := s7.step + 1 }
  let playerAlive := (OracleState.getPlayerOrDefault s8).isAlive
  let done := stepDone s8.config s8.step playerAlive
  let obs := observe s8
  let unlocked := Achievements.unlockedNames (OracleState.getPlayerOrDefault s8).achievements
  let out : StepOut := { obs := obs, reward := 0.0, done := done, info := "", achievements := unlocked }
  (s8, out)

end Oracle

end Crafter
