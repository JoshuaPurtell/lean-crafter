namespace Crafter

structure Achievements where
  collectCoal : UInt32
  collectDiamond : UInt32
  collectDrink : UInt32
  collectIron : UInt32
  collectSapling : UInt32
  collectStone : UInt32
  collectWood : UInt32
  defeatSkeleton : UInt32
  defeatZombie : UInt32
  eatCow : UInt32
  eatPlant : UInt32
  makeIronPickaxe : UInt32
  makeIronSword : UInt32
  makeStonePickaxe : UInt32
  makeStoneSword : UInt32
  makeWoodPickaxe : UInt32
  makeWoodSword : UInt32
  placeFurnace : UInt32
  placePlant : UInt32
  placeStone : UInt32
  placeTable : UInt32
  wakeUp : UInt32
  collectSapphire : UInt32
  collectRuby : UInt32
  openChest : UInt32
  makeDiamondPickaxe : UInt32
  makeDiamondSword : UInt32
  makeBow : UInt32
  makeArrow : UInt32
  makeIronArmor : UInt32
  makeDiamondArmor : UInt32
  defeatOrcSoldier : UInt32
  defeatOrcMage : UInt32
  defeatKnight : UInt32
  defeatKnightArcher : UInt32
  defeatTroll : UInt32
  drinkPotion : UInt32
  gainXp : UInt32
  reachLevel : UInt32
  deriving Repr

namespace Achievements

@[inline] def default : Achievements :=
  { collectCoal := 0
    collectDiamond := 0
    collectDrink := 0
    collectIron := 0
    collectSapling := 0
    collectStone := 0
    collectWood := 0
    defeatSkeleton := 0
    defeatZombie := 0
    eatCow := 0
    eatPlant := 0
    makeIronPickaxe := 0
    makeIronSword := 0
    makeStonePickaxe := 0
    makeStoneSword := 0
    makeWoodPickaxe := 0
    makeWoodSword := 0
    placeFurnace := 0
    placePlant := 0
    placeStone := 0
    placeTable := 0
    wakeUp := 0
    collectSapphire := 0
    collectRuby := 0
    openChest := 0
    makeDiamondPickaxe := 0
    makeDiamondSword := 0
    makeBow := 0
    makeArrow := 0
    makeIronArmor := 0
    makeDiamondArmor := 0
    defeatOrcSoldier := 0
    defeatOrcMage := 0
    defeatKnight := 0
    defeatKnightArcher := 0
    defeatTroll := 0
    drinkPotion := 0
    gainXp := 0
    reachLevel := 0 }

instance : Inhabited Achievements := ⟨default⟩

@[inline] def unlockedNames (a : Achievements) : List String :=
  let entries : List (String × UInt32) := [
    ("collect_coal", a.collectCoal),
    ("collect_diamond", a.collectDiamond),
    ("collect_drink", a.collectDrink),
    ("collect_iron", a.collectIron),
    ("collect_sapling", a.collectSapling),
    ("collect_stone", a.collectStone),
    ("collect_wood", a.collectWood),
    ("defeat_skeleton", a.defeatSkeleton),
    ("defeat_zombie", a.defeatZombie),
    ("eat_cow", a.eatCow),
    ("eat_plant", a.eatPlant),
    ("make_iron_pickaxe", a.makeIronPickaxe),
    ("make_iron_sword", a.makeIronSword),
    ("make_stone_pickaxe", a.makeStonePickaxe),
    ("make_stone_sword", a.makeStoneSword),
    ("make_wood_pickaxe", a.makeWoodPickaxe),
    ("make_wood_sword", a.makeWoodSword),
    ("place_furnace", a.placeFurnace),
    ("place_plant", a.placePlant),
    ("place_stone", a.placeStone),
    ("place_table", a.placeTable),
    ("wake_up", a.wakeUp),
    ("collect_sapphire", a.collectSapphire),
    ("collect_ruby", a.collectRuby),
    ("open_chest", a.openChest),
    ("make_diamond_pickaxe", a.makeDiamondPickaxe),
    ("make_diamond_sword", a.makeDiamondSword),
    ("make_bow", a.makeBow),
    ("make_arrow", a.makeArrow),
    ("make_iron_armor", a.makeIronArmor),
    ("make_diamond_armor", a.makeDiamondArmor),
    ("defeat_orc_soldier", a.defeatOrcSoldier),
    ("defeat_orc_mage", a.defeatOrcMage),
    ("defeat_knight", a.defeatKnight),
    ("defeat_knight_archer", a.defeatKnightArcher),
    ("defeat_troll", a.defeatTroll),
    ("drink_potion", a.drinkPotion),
    ("gain_xp", a.gainXp),
    ("reach_level", a.reachLevel)
  ]
  let unlocked := entries.foldl (init := []) fun acc entry =>
    let (name, count) := entry
    if count != 0 then name :: acc else acc
  unlocked.reverse

end Achievements

end Crafter
