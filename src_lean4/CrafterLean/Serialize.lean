import Lean.Data.Json
import Lean.Data.Json.FromToJson
import CrafterLean.Achievement
import CrafterLean.Action
import CrafterLean.Config
import CrafterLean.Entity
import CrafterLean.Inventory
import CrafterLean.Types.Coord
import CrafterLean.Types.Grid
import CrafterLean.World.Material
import CrafterLean.World.Types
import CrafterLean.Oracle.State
import CrafterLean.Oracle.Step

open Lean

namespace Crafter

@[inline] def encodeJson (j : Json) : ByteArray :=
  (Json.compress j).toUTF8

@[inline] def decodeJson (b : ByteArray) : Except String Json :=
  match String.fromUTF8? b with
  | none => throw "invalid UTF-8"
  | some s => Json.parse s

@[inline] def encodeToJson [ToJson α] (a : α) : ByteArray :=
  encodeJson (toJson a)

@[inline] def decodeFromJson [FromJson α] (b : ByteArray) : Except String α := do
  let j <- decodeJson b
  fromJson? j

@[inline] def getField (o : Std.TreeMap.Raw String Json compare) (name : String) : Except String Json :=
  match o.get? name with
  | some v => return v
  | none => throw s!"missing field '{name}'"

@[inline] def getFieldAs [FromJson α] (o : Std.TreeMap.Raw String Json compare) (name : String) : Except String α := do
  let v <- getField o name
  fromJson? v

@[inline] def getFieldOr [FromJson α] (o : Std.TreeMap.Raw String Json compare) (name : String) (default : α) : Except String α := do
  match o.get? name with
  | some v => fromJson? v
  | none => return default

instance : ToJson UInt8 where
  toJson n := (n.toNat : Nat)

instance : FromJson UInt8 where
  fromJson? j := do
    let n <- Json.getNat? j
    if n <= 255 then
      return UInt8.ofNat n
    else
      throw s!"u8 out of range: {n}"

instance : ToJson UInt16 where
  toJson n := (n.toNat : Nat)

instance : FromJson UInt16 where
  fromJson? j := do
    let n <- Json.getNat? j
    if h : n < UInt16.size then
      return UInt16.ofNatLT n h
    else
      throw s!"u16 out of range: {n}"

instance : ToJson UInt32 where
  toJson n := (n.toNat : Nat)

instance : FromJson UInt32 where
  fromJson? j := do
    let n <- Json.getNat? j
    if h : n < UInt32.size then
      return UInt32.ofNatLT n h
    else
      throw s!"u32 out of range: {n}"

instance : ToJson UInt64 where
  toJson n := (n.toNat : Nat)

instance : FromJson UInt64 where
  fromJson? j := do
    let n <- Json.getNat? j
    return UInt64.ofNat n

instance : ToJson Coord where
  toJson c := Json.mkObj [("x", c.x), ("y", c.y)]

instance : FromJson Coord where
  fromJson? j := do
    let o <- j.getObj?
    let x <- getFieldAs o "x"
    let y <- getFieldAs o "y"
    return { x := x, y := y }

namespace Tile

@[inline] def toString : Tile → String
  | Tile.Empty => "empty"
  | Tile.Grass => "grass"
  | Tile.Wall => "wall"

@[inline] def fromString? : String → Option Tile
  | "empty" => some Tile.Empty
  | "grass" => some Tile.Grass
  | "wall" => some Tile.Wall
  | _ => none

end Tile

instance : ToJson Tile where
  toJson t := Tile.toString t

instance : FromJson Tile where
  fromJson? j := do
    let s <- j.getStr?
    match Tile.fromString? s with
    | some t => return t
    | none => throw s!"invalid tile '{s}'"

instance : ToJson Material where
  toJson m := Material.toString m

instance : FromJson Material where
  fromJson? j := do
    let s <- j.getStr?
    match Material.fromString? s with
    | some m => return m
    | none => throw s!"invalid material '{s}'"

@[inline] def coordToJsonTuple (c : Coord) : Json :=
  Json.arr #[toJson c.x, toJson c.y]

@[inline] def coordFromJsonTuple (j : Json) : Except String Coord := do
  let arr <- j.getArr?
  if arr.size != 2 then
    throw "coord expects 2 elements"
  let x <- fromJson? arr[0]!
  let y <- fromJson? arr[1]!
  return { x := x, y := y }

@[inline] def pairToJsonTuple (p : Int × Int) : Json :=
  Json.arr #[toJson p.1, toJson p.2]

@[inline] def pairFromJsonTuple (j : Json) : Except String (Int × Int) := do
  let arr <- j.getArr?
  if arr.size != 2 then
    throw "pair expects 2 elements"
  let a <- fromJson? arr[0]!
  let b <- fromJson? arr[1]!
  return (a, b)

@[inline] def coordFromJsonAny (j : Json) : Except String Coord := do
  match j with
  | Json.arr _ => coordFromJsonTuple j
  | _ => fromJson? j

instance : ToJson DamageSource where
  toJson
    | DamageSource.Zombie => Json.str "Zombie"
    | DamageSource.Skeleton => Json.str "Skeleton"
    | DamageSource.Arrow => Json.str "Arrow"
    | DamageSource.PlayerArrow => Json.str "PlayerArrow"
    | DamageSource.CraftaxMelee => Json.str "CraftaxMelee"
    | DamageSource.CraftaxRanged => Json.str "CraftaxRanged"
    | DamageSource.CraftaxMagic => Json.str "CraftaxMagic"
    | DamageSource.Lava => Json.str "Lava"
    | DamageSource.Starvation => Json.str "Starvation"
    | DamageSource.Thirst => Json.str "Thirst"
    | DamageSource.Exhaustion => Json.str "Exhaustion"
    | DamageSource.Unknown => Json.str "Unknown"

instance : FromJson DamageSource where
  fromJson? j := do
    let s <- j.getStr?
    match s with
    | "Zombie" | "zombie" => return DamageSource.Zombie
    | "Skeleton" | "skeleton" => return DamageSource.Skeleton
    | "Arrow" | "arrow" => return DamageSource.Arrow
    | "PlayerArrow" | "player_arrow" => return DamageSource.PlayerArrow
    | "CraftaxMelee" | "craftax_melee" => return DamageSource.CraftaxMelee
    | "CraftaxRanged" | "craftax_ranged" => return DamageSource.CraftaxRanged
    | "CraftaxMagic" | "craftax_magic" => return DamageSource.CraftaxMagic
    | "Lava" | "lava" => return DamageSource.Lava
    | "Starvation" | "starvation" => return DamageSource.Starvation
    | "Thirst" | "thirst" => return DamageSource.Thirst
    | "Exhaustion" | "exhaustion" => return DamageSource.Exhaustion
    | "Unknown" | "unknown" => return DamageSource.Unknown
    | _ => throw s!"invalid damage source '{s}'"

instance : ToJson ProjectileKind where
  toJson
    | ProjectileKind.Arrow => Json.str "Arrow"
    | ProjectileKind.Fireball => Json.str "Fireball"
    | ProjectileKind.Iceball => Json.str "Iceball"

instance : FromJson ProjectileKind where
  fromJson? j := do
    let s <- j.getStr?
    match s with
    | "Arrow" => return ProjectileKind.Arrow
    | "Fireball" => return ProjectileKind.Fireball
    | "Iceball" => return ProjectileKind.Iceball
    | _ => throw s!"invalid projectile kind '{s}'"

instance : ToJson CraftaxMobKind where
  toJson
    | CraftaxMobKind.OrcSoldier => Json.str "OrcSoldier"
    | CraftaxMobKind.OrcMage => Json.str "OrcMage"
    | CraftaxMobKind.Knight => Json.str "Knight"
    | CraftaxMobKind.KnightArcher => Json.str "KnightArcher"
    | CraftaxMobKind.Troll => Json.str "Troll"
    | CraftaxMobKind.Bat => Json.str "Bat"
    | CraftaxMobKind.Snail => Json.str "Snail"

instance : FromJson CraftaxMobKind where
  fromJson? j := do
    let s <- j.getStr?
    match s with
    | "OrcSoldier" => return CraftaxMobKind.OrcSoldier
    | "OrcMage" => return CraftaxMobKind.OrcMage
    | "Knight" => return CraftaxMobKind.Knight
    | "KnightArcher" => return CraftaxMobKind.KnightArcher
    | "Troll" => return CraftaxMobKind.Troll
    | "Bat" => return CraftaxMobKind.Bat
    | "Snail" => return CraftaxMobKind.Snail
    | _ => throw s!"invalid craftax mob kind '{s}'"

instance : ToJson Cow where
  toJson c := Json.mkObj [("pos", coordToJsonTuple c.pos), ("health", toJson c.health)]

instance : FromJson Cow where
  fromJson? j := do
    let o <- j.getObj?
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let health <- getFieldOr o "health" (UInt8.ofNat 3)
    return { pos := pos, health := health }

instance : ToJson Zombie where
  toJson z := Json.mkObj [("pos", coordToJsonTuple z.pos), ("health", toJson z.health), ("cooldown", toJson z.cooldown)]

instance : FromJson Zombie where
  fromJson? j := do
    let o <- j.getObj?
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let health <- getFieldOr o "health" (UInt8.ofNat 5)
    let cooldown <- getFieldOr o "cooldown" (UInt8.ofNat 0)
    return { pos := pos, health := health, cooldown := cooldown }

instance : ToJson Skeleton where
  toJson s := Json.mkObj [("pos", coordToJsonTuple s.pos), ("health", toJson s.health), ("reload", toJson s.reload)]

instance : FromJson Skeleton where
  fromJson? j := do
    let o <- j.getObj?
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let health <- getFieldOr o "health" (UInt8.ofNat 3)
    let reload <- getFieldOr o "reload" (UInt8.ofNat 0)
    return { pos := pos, health := health, reload := reload }

instance : ToJson Arrow where
  toJson a :=
    Json.mkObj
      [ ("pos", coordToJsonTuple a.pos)
      , ("facing", pairToJsonTuple a.facing)
      , ("kind", toJson a.kind)
      , ("damage", toJson a.damage)
      , ("source", toJson a.source)
      ]

instance : FromJson Arrow where
  fromJson? j := do
    let o <- j.getObj?
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let facing <- getFieldOr o "facing" (0, 1)
    let kind <- getFieldOr o "kind" ProjectileKind.Arrow
    let damage <- getFieldOr o "damage" (UInt8.ofNat 2)
    let source <- getFieldOr o "source" DamageSource.Arrow
    return { pos := pos, facing := facing, kind := kind, damage := damage, source := source }

instance : ToJson CraftaxMob where
  toJson m :=
    Json.mkObj
      [ ("kind", toJson m.kind)
      , ("pos", coordToJsonTuple m.pos)
      , ("health", toJson m.health)
      , ("cooldown", toJson m.cooldown)
      ]

instance : FromJson CraftaxMob where
  fromJson? j := do
    let o <- j.getObj?
    let kind <- getFieldAs o "kind"
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let health <- getFieldOr o "health" (UInt8.ofNat 1)
    let cooldown <- getFieldOr o "cooldown" (UInt8.ofNat 0)
    return { kind := kind, pos := pos, health := health, cooldown := cooldown }

instance : ToJson Plant where
  toJson p :=
    Json.mkObj
      [ ("pos", coordToJsonTuple p.pos)
      , ("health", toJson p.health)
      , ("grown", toJson p.grown)
      ]

instance : FromJson Plant where
  fromJson? j := do
    let o <- j.getObj?
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let health <- getFieldOr o "health" (UInt8.ofNat 1)
    let grown <- getFieldOr o "grown" (UInt16.ofNat 0)
    return { pos := pos, health := health, grown := grown }

@[inline] def getSingleField (o : Std.TreeMap.Raw String Json compare) : Except String (String × Json) := do
  let (count, value) :=
    o.foldl (init := (0, (none : Option (String × Json)))) fun acc k v =>
      let (c, cur) := acc
      if c == 0 then
        (1, some (k, v))
      else
        (c + 1, cur)
  match value with
  | some kv =>
      if count == 1 then
        return kv
      else
        throw "expected single-field object"
  | none => throw "empty object"

instance : ToJson Inventory where
  toJson i :=
    Json.mkObj
      [ ("health", toJson i.health)
      , ("food", toJson i.food)
      , ("drink", toJson i.drink)
      , ("energy", toJson i.energy)
      , ("sapling", toJson i.sapling)
      , ("wood", toJson i.wood)
      , ("stone", toJson i.stone)
      , ("coal", toJson i.coal)
      , ("iron", toJson i.iron)
      , ("diamond", toJson i.diamond)
      , ("sapphire", toJson i.sapphire)
      , ("ruby", toJson i.ruby)
      , ("wood_pickaxe", toJson i.woodPickaxe)
      , ("stone_pickaxe", toJson i.stonePickaxe)
      , ("iron_pickaxe", toJson i.ironPickaxe)
      , ("diamond_pickaxe", toJson i.diamondPickaxe)
      , ("wood_sword", toJson i.woodSword)
      , ("stone_sword", toJson i.stoneSword)
      , ("iron_sword", toJson i.ironSword)
      , ("diamond_sword", toJson i.diamondSword)
      , ("bow", toJson i.bow)
      , ("arrows", toJson i.arrows)
      , ("armor_helmet", toJson i.armorHelmet)
      , ("armor_chestplate", toJson i.armorChestplate)
      , ("armor_leggings", toJson i.armorLeggings)
      , ("armor_boots", toJson i.armorBoots)
      , ("potion_red", toJson i.potionRed)
      , ("potion_green", toJson i.potionGreen)
      , ("potion_blue", toJson i.potionBlue)
      , ("potion_pink", toJson i.potionPink)
      , ("potion_cyan", toJson i.potionCyan)
      , ("potion_yellow", toJson i.potionYellow)
      , ("xp", toJson i.xp)
      , ("level", toJson i.level)
      , ("stat_points", toJson i.statPoints)
      ]

instance : FromJson Inventory where
  fromJson? j := do
    let o <- j.getObj?
    let base := Inventory.default
    let health <- getFieldOr o "health" base.health
    let food <- getFieldOr o "food" base.food
    let drink <- getFieldOr o "drink" base.drink
    let energy <- getFieldOr o "energy" base.energy
    let sapling <- getFieldOr o "sapling" base.sapling
    let wood <- getFieldOr o "wood" base.wood
    let stone <- getFieldOr o "stone" base.stone
    let coal <- getFieldOr o "coal" base.coal
    let iron <- getFieldOr o "iron" base.iron
    let diamond <- getFieldOr o "diamond" base.diamond
    let sapphire <- getFieldOr o "sapphire" base.sapphire
    let ruby <- getFieldOr o "ruby" base.ruby
    let woodPickaxe <- getFieldOr o "wood_pickaxe" base.woodPickaxe
    let stonePickaxe <- getFieldOr o "stone_pickaxe" base.stonePickaxe
    let ironPickaxe <- getFieldOr o "iron_pickaxe" base.ironPickaxe
    let diamondPickaxe <- getFieldOr o "diamond_pickaxe" base.diamondPickaxe
    let woodSword <- getFieldOr o "wood_sword" base.woodSword
    let stoneSword <- getFieldOr o "stone_sword" base.stoneSword
    let ironSword <- getFieldOr o "iron_sword" base.ironSword
    let diamondSword <- getFieldOr o "diamond_sword" base.diamondSword
    let bow <- getFieldOr o "bow" base.bow
    let arrows <- getFieldOr o "arrows" base.arrows
    let armorHelmet <- getFieldOr o "armor_helmet" base.armorHelmet
    let armorChestplate <- getFieldOr o "armor_chestplate" base.armorChestplate
    let armorLeggings <- getFieldOr o "armor_leggings" base.armorLeggings
    let armorBoots <- getFieldOr o "armor_boots" base.armorBoots
    let potionRed <- getFieldOr o "potion_red" base.potionRed
    let potionGreen <- getFieldOr o "potion_green" base.potionGreen
    let potionBlue <- getFieldOr o "potion_blue" base.potionBlue
    let potionPink <- getFieldOr o "potion_pink" base.potionPink
    let potionCyan <- getFieldOr o "potion_cyan" base.potionCyan
    let potionYellow <- getFieldOr o "potion_yellow" base.potionYellow
    let xp <- getFieldOr o "xp" base.xp
    let level <- getFieldOr o "level" base.level
    let statPoints <- getFieldOr o "stat_points" base.statPoints
    return { base with
      health := health
      food := food
      drink := drink
      energy := energy
      sapling := sapling
      wood := wood
      stone := stone
      coal := coal
      iron := iron
      diamond := diamond
      sapphire := sapphire
      ruby := ruby
      woodPickaxe := woodPickaxe
      stonePickaxe := stonePickaxe
      ironPickaxe := ironPickaxe
      diamondPickaxe := diamondPickaxe
      woodSword := woodSword
      stoneSword := stoneSword
      ironSword := ironSword
      diamondSword := diamondSword
      bow := bow
      arrows := arrows
      armorHelmet := armorHelmet
      armorChestplate := armorChestplate
      armorLeggings := armorLeggings
      armorBoots := armorBoots
      potionRed := potionRed
      potionGreen := potionGreen
      potionBlue := potionBlue
      potionPink := potionPink
      potionCyan := potionCyan
      potionYellow := potionYellow
      xp := xp
      level := level
      statPoints := statPoints
    }

instance : ToJson Achievements where
  toJson a :=
    Json.mkObj
      [ ("collect_coal", toJson a.collectCoal)
      , ("collect_diamond", toJson a.collectDiamond)
      , ("collect_drink", toJson a.collectDrink)
      , ("collect_iron", toJson a.collectIron)
      , ("collect_sapling", toJson a.collectSapling)
      , ("collect_stone", toJson a.collectStone)
      , ("collect_wood", toJson a.collectWood)
      , ("defeat_skeleton", toJson a.defeatSkeleton)
      , ("defeat_zombie", toJson a.defeatZombie)
      , ("eat_cow", toJson a.eatCow)
      , ("eat_plant", toJson a.eatPlant)
      , ("make_iron_pickaxe", toJson a.makeIronPickaxe)
      , ("make_iron_sword", toJson a.makeIronSword)
      , ("make_stone_pickaxe", toJson a.makeStonePickaxe)
      , ("make_stone_sword", toJson a.makeStoneSword)
      , ("make_wood_pickaxe", toJson a.makeWoodPickaxe)
      , ("make_wood_sword", toJson a.makeWoodSword)
      , ("place_furnace", toJson a.placeFurnace)
      , ("place_plant", toJson a.placePlant)
      , ("place_stone", toJson a.placeStone)
      , ("place_table", toJson a.placeTable)
      , ("wake_up", toJson a.wakeUp)
      , ("collect_sapphire", toJson a.collectSapphire)
      , ("collect_ruby", toJson a.collectRuby)
      , ("open_chest", toJson a.openChest)
      , ("make_diamond_pickaxe", toJson a.makeDiamondPickaxe)
      , ("make_diamond_sword", toJson a.makeDiamondSword)
      , ("make_bow", toJson a.makeBow)
      , ("make_arrow", toJson a.makeArrow)
      , ("make_iron_armor", toJson a.makeIronArmor)
      , ("make_diamond_armor", toJson a.makeDiamondArmor)
      , ("defeat_orc_soldier", toJson a.defeatOrcSoldier)
      , ("defeat_orc_mage", toJson a.defeatOrcMage)
      , ("defeat_knight", toJson a.defeatKnight)
      , ("defeat_knight_archer", toJson a.defeatKnightArcher)
      , ("defeat_troll", toJson a.defeatTroll)
      , ("drink_potion", toJson a.drinkPotion)
      , ("gain_xp", toJson a.gainXp)
      , ("reach_level", toJson a.reachLevel)
      ]

instance : FromJson Achievements where
  fromJson? j := do
    let o <- j.getObj?
    let base := Achievements.default
    let collectCoal <- getFieldOr o "collect_coal" base.collectCoal
    let collectDiamond <- getFieldOr o "collect_diamond" base.collectDiamond
    let collectDrink <- getFieldOr o "collect_drink" base.collectDrink
    let collectIron <- getFieldOr o "collect_iron" base.collectIron
    let collectSapling <- getFieldOr o "collect_sapling" base.collectSapling
    let collectStone <- getFieldOr o "collect_stone" base.collectStone
    let collectWood <- getFieldOr o "collect_wood" base.collectWood
    let defeatSkeleton <- getFieldOr o "defeat_skeleton" base.defeatSkeleton
    let defeatZombie <- getFieldOr o "defeat_zombie" base.defeatZombie
    let eatCow <- getFieldOr o "eat_cow" base.eatCow
    let eatPlant <- getFieldOr o "eat_plant" base.eatPlant
    let makeIronPickaxe <- getFieldOr o "make_iron_pickaxe" base.makeIronPickaxe
    let makeIronSword <- getFieldOr o "make_iron_sword" base.makeIronSword
    let makeStonePickaxe <- getFieldOr o "make_stone_pickaxe" base.makeStonePickaxe
    let makeStoneSword <- getFieldOr o "make_stone_sword" base.makeStoneSword
    let makeWoodPickaxe <- getFieldOr o "make_wood_pickaxe" base.makeWoodPickaxe
    let makeWoodSword <- getFieldOr o "make_wood_sword" base.makeWoodSword
    let placeFurnace <- getFieldOr o "place_furnace" base.placeFurnace
    let placePlant <- getFieldOr o "place_plant" base.placePlant
    let placeStone <- getFieldOr o "place_stone" base.placeStone
    let placeTable <- getFieldOr o "place_table" base.placeTable
    let wakeUp <- getFieldOr o "wake_up" base.wakeUp
    let collectSapphire <- getFieldOr o "collect_sapphire" base.collectSapphire
    let collectRuby <- getFieldOr o "collect_ruby" base.collectRuby
    let openChest <- getFieldOr o "open_chest" base.openChest
    let makeDiamondPickaxe <- getFieldOr o "make_diamond_pickaxe" base.makeDiamondPickaxe
    let makeDiamondSword <- getFieldOr o "make_diamond_sword" base.makeDiamondSword
    let makeBow <- getFieldOr o "make_bow" base.makeBow
    let makeArrow <- getFieldOr o "make_arrow" base.makeArrow
    let makeIronArmor <- getFieldOr o "make_iron_armor" base.makeIronArmor
    let makeDiamondArmor <- getFieldOr o "make_diamond_armor" base.makeDiamondArmor
    let defeatOrcSoldier <- getFieldOr o "defeat_orc_soldier" base.defeatOrcSoldier
    let defeatOrcMage <- getFieldOr o "defeat_orc_mage" base.defeatOrcMage
    let defeatKnight <- getFieldOr o "defeat_knight" base.defeatKnight
    let defeatKnightArcher <- getFieldOr o "defeat_knight_archer" base.defeatKnightArcher
    let defeatTroll <- getFieldOr o "defeat_troll" base.defeatTroll
    let drinkPotion <- getFieldOr o "drink_potion" base.drinkPotion
    let gainXp <- getFieldOr o "gain_xp" base.gainXp
    let reachLevel <- getFieldOr o "reach_level" base.reachLevel
    return { base with
      collectCoal := collectCoal
      collectDiamond := collectDiamond
      collectDrink := collectDrink
      collectIron := collectIron
      collectSapling := collectSapling
      collectStone := collectStone
      collectWood := collectWood
      defeatSkeleton := defeatSkeleton
      defeatZombie := defeatZombie
      eatCow := eatCow
      eatPlant := eatPlant
      makeIronPickaxe := makeIronPickaxe
      makeIronSword := makeIronSword
      makeStonePickaxe := makeStonePickaxe
      makeStoneSword := makeStoneSword
      makeWoodPickaxe := makeWoodPickaxe
      makeWoodSword := makeWoodSword
      placeFurnace := placeFurnace
      placePlant := placePlant
      placeStone := placeStone
      placeTable := placeTable
      wakeUp := wakeUp
      collectSapphire := collectSapphire
      collectRuby := collectRuby
      openChest := openChest
      makeDiamondPickaxe := makeDiamondPickaxe
      makeDiamondSword := makeDiamondSword
      makeBow := makeBow
      makeArrow := makeArrow
      makeIronArmor := makeIronArmor
      makeDiamondArmor := makeDiamondArmor
      defeatOrcSoldier := defeatOrcSoldier
      defeatOrcMage := defeatOrcMage
      defeatKnight := defeatKnight
      defeatKnightArcher := defeatKnightArcher
      defeatTroll := defeatTroll
      drinkPotion := drinkPotion
      gainXp := gainXp
      reachLevel := reachLevel
    }
instance : ToJson TimeMode where
  toJson
    | TimeMode.Logical => Json.str "Logical"
    | TimeMode.RealTime ticks pause =>
        Json.mkObj
          [ ("RealTime", Json.mkObj [("ticks_per_second", toJson ticks), ("pause_on_disconnect", toJson pause)]) ]
    | TimeMode.Hybrid ticks allow =>
        Json.mkObj
          [ ("Hybrid", Json.mkObj [("ticks_per_second", toJson ticks), ("allow_manual_step", toJson allow)]) ]

instance : FromJson TimeMode where
  fromJson? j := do
    match j with
    | Json.str "Logical" => return TimeMode.Logical
    | Json.obj o =>
        if let some inner := o.get? "RealTime" then
          let real <- inner.getObj?
          let ticks <- getFieldAs real "ticks_per_second"
          let pause <- getFieldAs real "pause_on_disconnect"
          return TimeMode.RealTime ticks pause
        else if let some inner := o.get? "Hybrid" then
          let hyb <- inner.getObj?
          let ticks <- getFieldAs hyb "ticks_per_second"
          let allow <- getFieldAs hyb "allow_manual_step"
          return TimeMode.Hybrid ticks allow
        else
          throw "invalid TimeMode object"
    | _ => throw "invalid TimeMode"

instance : ToJson CraftaxSpawnConfig where
  toJson s :=
    Json.mkObj
      [ ("sapphire_density", toJson s.sapphireDensity)
      , ("ruby_density", toJson s.rubyDensity)
      , ("chest_density", toJson s.chestDensity)
      , ("orc_soldier_density", toJson s.orcSoldierDensity)
      , ("orc_mage_density", toJson s.orcMageDensity)
      , ("knight_density", toJson s.knightDensity)
      , ("knight_archer_density", toJson s.knightArcherDensity)
      , ("troll_density", toJson s.trollDensity)
      , ("bat_density", toJson s.batDensity)
      , ("snail_density", toJson s.snailDensity)
      ]

instance : FromJson CraftaxSpawnConfig where
  fromJson? j := do
    let o <- j.getObj?
    let base := CraftaxSpawnConfig.default
    let sapphireDensity <- getFieldOr o "sapphire_density" base.sapphireDensity
    let rubyDensity <- getFieldOr o "ruby_density" base.rubyDensity
    let chestDensity <- getFieldOr o "chest_density" base.chestDensity
    let orcSoldierDensity <- getFieldOr o "orc_soldier_density" base.orcSoldierDensity
    let orcMageDensity <- getFieldOr o "orc_mage_density" base.orcMageDensity
    let knightDensity <- getFieldOr o "knight_density" base.knightDensity
    let knightArcherDensity <- getFieldOr o "knight_archer_density" base.knightArcherDensity
    let trollDensity <- getFieldOr o "troll_density" base.trollDensity
    let batDensity <- getFieldOr o "bat_density" base.batDensity
    let snailDensity <- getFieldOr o "snail_density" base.snailDensity
    return { base with
      sapphireDensity := sapphireDensity
      rubyDensity := rubyDensity
      chestDensity := chestDensity
      orcSoldierDensity := orcSoldierDensity
      orcMageDensity := orcMageDensity
      knightDensity := knightDensity
      knightArcherDensity := knightArcherDensity
      trollDensity := trollDensity
      batDensity := batDensity
      snailDensity := snailDensity
    }

instance : ToJson CraftaxLootConfig where
  toJson l :=
    Json.mkObj
      [ ("potion_drop_chance", toJson l.potionDropChance)
      , ("arrow_drop_chance", toJson l.arrowDropChance)
      , ("gem_drop_chance", toJson l.gemDropChance)
      ]

instance : FromJson CraftaxLootConfig where
  fromJson? j := do
    let o <- j.getObj?
    let base := CraftaxLootConfig.default
    let potionDropChance <- getFieldOr o "potion_drop_chance" base.potionDropChance
    let arrowDropChance <- getFieldOr o "arrow_drop_chance" base.arrowDropChance
    let gemDropChance <- getFieldOr o "gem_drop_chance" base.gemDropChance
    return { base with potionDropChance := potionDropChance, arrowDropChance := arrowDropChance, gemDropChance := gemDropChance }

instance : ToJson CraftaxConfig where
  toJson c :=
    Json.mkObj
      [ ("enabled", c.enabled)
      , ("mobs_enabled", c.mobsEnabled)
      , ("worldgen_enabled", c.worldgenEnabled)
      , ("items_enabled", c.itemsEnabled)
      , ("combat_enabled", c.combatEnabled)
      , ("chests_enabled", c.chestsEnabled)
      , ("potions_enabled", c.potionsEnabled)
      , ("xp_enabled", c.xpEnabled)
      , ("achievements_enabled", c.achievementsEnabled)
      , ("spawn", toJson c.spawn)
      , ("loot", toJson c.loot)
      ]

instance : FromJson CraftaxConfig where
  fromJson? j := do
    let o <- j.getObj?
    let base := CraftaxConfig.default
    let enabled <- getFieldOr o "enabled" base.enabled
    let mobsEnabled <- getFieldOr o "mobs_enabled" base.mobsEnabled
    let worldgenEnabled <- getFieldOr o "worldgen_enabled" base.worldgenEnabled
    let itemsEnabled <- getFieldOr o "items_enabled" base.itemsEnabled
    let combatEnabled <- getFieldOr o "combat_enabled" base.combatEnabled
    let chestsEnabled <- getFieldOr o "chests_enabled" base.chestsEnabled
    let potionsEnabled <- getFieldOr o "potions_enabled" base.potionsEnabled
    let xpEnabled <- getFieldOr o "xp_enabled" base.xpEnabled
    let achievementsEnabled <- getFieldOr o "achievements_enabled" base.achievementsEnabled
    let spawn <- getFieldOr o "spawn" base.spawn
    let loot <- getFieldOr o "loot" base.loot
    return { base with
      enabled := enabled
      mobsEnabled := mobsEnabled
      worldgenEnabled := worldgenEnabled
      itemsEnabled := itemsEnabled
      combatEnabled := combatEnabled
      chestsEnabled := chestsEnabled
      potionsEnabled := potionsEnabled
      xpEnabled := xpEnabled
      achievementsEnabled := achievementsEnabled
      spawn := spawn
      loot := loot
    }

instance : ToJson SessionConfig where
  toJson c :=
    Json.mkObj
      [ ("world_size", toJson c.worldSize)
      , ("seed", toJson c.seed)
      , ("chunk_size", toJson c.chunkSize)
      , ("tree_density", toJson c.treeDensity)
      , ("coal_density", toJson c.coalDensity)
      , ("iron_density", toJson c.ironDensity)
      , ("diamond_density", toJson c.diamondDensity)
      , ("cow_density", toJson c.cowDensity)
      , ("zombie_density", toJson c.zombieDensity)
      , ("skeleton_density", toJson c.skeletonDensity)
      , ("zombie_spawn_rate", toJson c.zombieSpawnRate)
      , ("zombie_despawn_rate", toJson c.zombieDespawnRate)
      , ("cow_spawn_rate", toJson c.cowSpawnRate)
      , ("cow_despawn_rate", toJson c.cowDespawnRate)
      , ("max_steps", toJson c.maxSteps)
      , ("day_night_cycle", toJson c.dayNightCycle)
      , ("day_cycle_period", toJson c.dayCyclePeriod)
      , ("hunger_enabled", toJson c.hungerEnabled)
      , ("hunger_rate", toJson c.hungerRate)
      , ("thirst_enabled", toJson c.thirstEnabled)
      , ("thirst_rate", toJson c.thirstRate)
      , ("fatigue_enabled", toJson c.fatigueEnabled)
      , ("health_enabled", toJson c.healthEnabled)
      , ("zombie_damage_mult", toJson c.zombieDamageMult)
      , ("arrow_damage_mult", toJson c.arrowDamageMult)
      , ("player_damage_mult", toJson c.playerDamageMult)
      , ("cow_health", toJson c.cowHealth)
      , ("zombie_health", toJson c.zombieHealth)
      , ("skeleton_health", toJson c.skeletonHealth)
      , ("view_radius", toJson c.viewRadius)
      , ("full_world_state", toJson c.fullWorldState)
      , ("time_mode", toJson c.timeMode)
      , ("default_ticks_per_second", toJson c.defaultTicksPerSecond)
      , ("craftax", toJson c.craftax)
      ]

instance : FromJson SessionConfig where
  fromJson? j := do
    let o <- j.getObj?
    let base := SessionConfig.default
    let worldSize <- getFieldOr o "world_size" base.worldSize
    let seed <- getFieldOr o "seed" base.seed
    let chunkSize <- getFieldOr o "chunk_size" base.chunkSize
    let treeDensity <- getFieldOr o "tree_density" base.treeDensity
    let coalDensity <- getFieldOr o "coal_density" base.coalDensity
    let ironDensity <- getFieldOr o "iron_density" base.ironDensity
    let diamondDensity <- getFieldOr o "diamond_density" base.diamondDensity
    let cowDensity <- getFieldOr o "cow_density" base.cowDensity
    let zombieDensity <- getFieldOr o "zombie_density" base.zombieDensity
    let skeletonDensity <- getFieldOr o "skeleton_density" base.skeletonDensity
    let zombieSpawnRate <- getFieldOr o "zombie_spawn_rate" base.zombieSpawnRate
    let zombieDespawnRate <- getFieldOr o "zombie_despawn_rate" base.zombieDespawnRate
    let cowSpawnRate <- getFieldOr o "cow_spawn_rate" base.cowSpawnRate
    let cowDespawnRate <- getFieldOr o "cow_despawn_rate" base.cowDespawnRate
    let maxSteps <- getFieldOr o "max_steps" base.maxSteps
    let dayNightCycle <- getFieldOr o "day_night_cycle" base.dayNightCycle
    let dayCyclePeriod <- getFieldOr o "day_cycle_period" base.dayCyclePeriod
    let hungerEnabled <- getFieldOr o "hunger_enabled" base.hungerEnabled
    let hungerRate <- getFieldOr o "hunger_rate" base.hungerRate
    let thirstEnabled <- getFieldOr o "thirst_enabled" base.thirstEnabled
    let thirstRate <- getFieldOr o "thirst_rate" base.thirstRate
    let fatigueEnabled <- getFieldOr o "fatigue_enabled" base.fatigueEnabled
    let healthEnabled <- getFieldOr o "health_enabled" base.healthEnabled
    let zombieDamageMult <- getFieldOr o "zombie_damage_mult" base.zombieDamageMult
    let arrowDamageMult <- getFieldOr o "arrow_damage_mult" base.arrowDamageMult
    let playerDamageMult <- getFieldOr o "player_damage_mult" base.playerDamageMult
    let cowHealth <- getFieldOr o "cow_health" base.cowHealth
    let zombieHealth <- getFieldOr o "zombie_health" base.zombieHealth
    let skeletonHealth <- getFieldOr o "skeleton_health" base.skeletonHealth
    let viewRadius <- getFieldOr o "view_radius" base.viewRadius
    let fullWorldState <- getFieldOr o "full_world_state" base.fullWorldState
    let timeMode <- getFieldOr o "time_mode" base.timeMode
    let defaultTicksPerSecond <- getFieldOr o "default_ticks_per_second" base.defaultTicksPerSecond
    let craftax <- getFieldOr o "craftax" base.craftax
    return { base with
      worldSize := worldSize
      seed := seed
      chunkSize := chunkSize
      treeDensity := treeDensity
      coalDensity := coalDensity
      ironDensity := ironDensity
      diamondDensity := diamondDensity
      cowDensity := cowDensity
      zombieDensity := zombieDensity
      skeletonDensity := skeletonDensity
      zombieSpawnRate := zombieSpawnRate
      zombieDespawnRate := zombieDespawnRate
      cowSpawnRate := cowSpawnRate
      cowDespawnRate := cowDespawnRate
      maxSteps := maxSteps
      dayNightCycle := dayNightCycle
      dayCyclePeriod := dayCyclePeriod
      hungerEnabled := hungerEnabled
      hungerRate := hungerRate
      thirstEnabled := thirstEnabled
      thirstRate := thirstRate
      fatigueEnabled := fatigueEnabled
      healthEnabled := healthEnabled
      zombieDamageMult := zombieDamageMult
      arrowDamageMult := arrowDamageMult
      playerDamageMult := playerDamageMult
      cowHealth := cowHealth
      zombieHealth := zombieHealth
      skeletonHealth := skeletonHealth
      viewRadius := viewRadius
      fullWorldState := fullWorldState
      timeMode := timeMode
      defaultTicksPerSecond := defaultTicksPerSecond
      craftax := craftax
    }

instance : ToJson Player where
  toJson p :=
    Json.mkObj
      [ ("pos", coordToJsonTuple p.pos)
      , ("facing", toJson p.facing)
      , ("inventory", toJson p.inventory)
      , ("achievements", toJson p.achievements)
      , ("sleeping", toJson p.sleeping)
      , ("hunger_counter", toJson p.hungerCounter)
      , ("thirst_counter", toJson p.thirstCounter)
      , ("fatigue_counter", toJson p.fatigueCounter)
      , ("recover_counter", toJson p.recoverCounter)
      , ("last_health", toJson p.lastHealth)
      , ("last_damage_source", toJson p.lastDamageSource)
      ]

instance : FromJson Player where
  fromJson? j := do
    let o <- j.getObj?
    let posVal <- getField o "pos"
    let pos <- coordFromJsonAny posVal
    let facing <- getFieldOr o "facing" (0, 1)
    let inventory <- getFieldOr o "inventory" Inventory.default
    let achievements <- getFieldOr o "achievements" Achievements.default
    let sleeping <- getFieldOr o "sleeping" false
    let hungerCounter <- getFieldOr o "hunger_counter" 0.0
    let thirstCounter <- getFieldOr o "thirst_counter" 0.0
    let fatigueCounter <- getFieldOr o "fatigue_counter" 0
    let recoverCounter <- getFieldOr o "recover_counter" 0.0
    let lastHealth <- getFieldOr o "last_health" (UInt8.ofNat 9)
    let lastDamageSource <- getFieldOr o "last_damage_source" (none : Option DamageSource)
    return { pos := pos, facing := facing, inventory := inventory, achievements := achievements, sleeping := sleeping, hungerCounter := hungerCounter, thirstCounter := thirstCounter, fatigueCounter := fatigueCounter, recoverCounter := recoverCounter, lastHealth := lastHealth, lastDamageSource := lastDamageSource }

instance : ToJson GameObject where
  toJson
    | GameObject.Player p => Json.mkObj [("Player", toJson p)]
    | GameObject.Cow payload => Json.mkObj [("Cow", toJson payload)]
    | GameObject.Zombie payload => Json.mkObj [("Zombie", toJson payload)]
    | GameObject.Skeleton payload => Json.mkObj [("Skeleton", toJson payload)]
    | GameObject.Arrow payload => Json.mkObj [("Arrow", toJson payload)]
    | GameObject.Plant payload => Json.mkObj [("Plant", toJson payload)]
    | GameObject.CraftaxMob payload => Json.mkObj [("CraftaxMob", toJson payload)]
    | GameObject.Unknown tag payload => Json.mkObj [(tag, payload)]

instance : FromJson GameObject where
  fromJson? j := do
    let o <- j.getObj?
    let (tag, payload) <- getSingleField o
    match tag with
    | "Player" => return GameObject.Player (← fromJson? payload)
    | "Cow" => return GameObject.Cow (← fromJson? payload)
    | "Zombie" => return GameObject.Zombie (← fromJson? payload)
    | "Skeleton" => return GameObject.Skeleton (← fromJson? payload)
    | "Arrow" => return GameObject.Arrow (← fromJson? payload)
    | "Plant" => return GameObject.Plant (← fromJson? payload)
    | "CraftaxMob" => return GameObject.CraftaxMob (← fromJson? payload)
    | _ => return GameObject.Unknown tag payload

instance [ToJson α] : ToJson (Grid α) where
  toJson g :=
    Json.mkObj
      [ ("width", toJson g.width)
      , ("height", toJson g.height)
      , ("data", toJson g.data)
      ]

instance [FromJson α] : FromJson (Grid α) where
  fromJson? j := do
    let o <- j.getObj?
    let width <- getFieldAs o "width"
    let height <- getFieldAs o "height"
    let data <- getFieldAs o "data"
    let expected := width * height
    if data.size == expected then
      return { width := width, height := height, data := data }
    else
      throw s!"grid data length {data.size} != {expected}"

instance : ToJson World where
  toJson w :=
    Json.mkObj
      [ ("materials", toJson w.materials)
      , ("daylight", toJson w.daylight)
      , ("rng_seed", toJson w.rngSeed)
      ]

instance : FromJson World where
  fromJson? j := do
    let o <- j.getObj?
    let materials <- getFieldAs o "materials"
    let daylight <- getFieldAs o "daylight"
    let rngSeed <- getFieldOr o "rng_seed" (0 : UInt64)
    return { materials := materials, daylight := daylight, rngSeed := rngSeed }

instance : ToJson Observation where
  toJson obs :=
    Json.mkObj
      [ ("step", toJson obs.step)
      , ("player_pos", toJson obs.playerPos)
      , ("daylight", toJson obs.daylight)
      ]

instance : FromJson Observation where
  fromJson? j := do
    let o <- j.getObj?
    let step <- getFieldAs o "step"
    let playerPos <- getFieldAs o "player_pos"
    let daylight <- getFieldAs o "daylight"
    return { step := step, playerPos := playerPos, daylight := daylight }

instance : ToJson StepOut where
  toJson out :=
    Json.mkObj
      [ ("obs", toJson out.obs)
      , ("reward", toJson out.reward)
      , ("done", toJson out.done)
      , ("info", toJson out.info)
      , ("achievements", toJson out.achievements)
      ]

instance : FromJson StepOut where
  fromJson? j := do
    let o <- j.getObj?
    let obs <- getFieldAs o "obs"
    let reward <- getFieldAs o "reward"
    let done <- getFieldAs o "done"
    let info <- getFieldAs o "info"
    let achievements <- getFieldOr o "achievements" ([] : List String)
    return { obs := obs, reward := reward, done := done, info := info, achievements := achievements }

end Crafter
