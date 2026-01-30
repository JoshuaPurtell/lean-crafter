import Std
import CrafterLean.Oracle.State
import CrafterLean.Achievement

namespace Crafter

namespace Render

@[inline] def craftaxChar : CraftaxMobKind → Char
  | CraftaxMobKind.OrcSoldier => 'O'
  | CraftaxMobKind.OrcMage => 'M'
  | CraftaxMobKind.Knight => 'K'
  | CraftaxMobKind.KnightArcher => 'A'
  | CraftaxMobKind.Troll => 't'
  | CraftaxMobKind.Bat => 'B'
  | CraftaxMobKind.Snail => 'N'

@[inline] def objectChar : GameObject → Char
  | GameObject.Player _ => '@'
  | GameObject.Cow _ => 'C'
  | GameObject.Zombie _ => 'Z'
  | GameObject.Skeleton _ => 'S'
  | GameObject.Arrow _ => '*'
  | GameObject.Plant p => if p.isRipe then 'P' else 'p'
  | GameObject.CraftaxMob m => craftaxChar m.kind
  | GameObject.Unknown _ _ => '?'

@[inline] def offsets (r : Nat) : List Int :=
  let size := 2 * r + 1
  (List.range size).map (fun i => Int.ofNat i - Int.ofNat r)

@[inline] def renderView (s : OracleState) : String :=
  let p := OracleState.getPlayerOrDefault s
  let r := s.config.viewRadius
  let ys := offsets r
  let xs := offsets r
  let lines := ys.map fun dy =>
    xs.foldl (init := "") fun acc dx =>
      let pos := Coord.add p.pos dx dy
      let ch :=
        if pos == p.pos then
          '@'
        else
          match s.getObjectAt? pos with
          | some obj => objectChar obj
          | none =>
              if !s.world.inBounds pos then
                '?'
              else
                match s.world.getMaterial pos with
                | some mat => mat.displayChar
                | none => ' '
      acc.push ch
  String.intercalate "\n" lines

@[inline] def ascii (s : OracleState) : String :=
  let p := OracleState.getPlayerOrDefault s
  let header :=
    s!"Step: {s.step} | Episode: {s.episode} | Daylight: {(s.world.daylight * 100.0)}%\n" ++
    s!"Position: ({p.pos.x}, {p.pos.y}) | Facing: ({p.facing.1}, {p.facing.2})" ++
    (if p.sleeping then " [SLEEPING]\n" else "\n")
  let view := renderView s
  let vitals :=
    s!"\n\n=== VITALS ===\n" ++
    s!"Health: {p.inventory.health} | Food: {p.inventory.food} | Drink: {p.inventory.drink} | Energy: {p.inventory.energy}\n"
  let resources :=
    s!"\n=== RESOURCES ===\n" ++
    s!"Wood: {p.inventory.wood} | Stone: {p.inventory.stone} | Coal: {p.inventory.coal} | Iron: {p.inventory.iron} | Diamond: {p.inventory.diamond} | Sapling: {p.inventory.sapling}\n" ++
    s!"Sapphire: {p.inventory.sapphire} | Ruby: {p.inventory.ruby}\n"
  let tools :=
    s!"\n=== TOOLS ===\n" ++
    s!"Pickaxes: W{p.inventory.woodPickaxe} S{p.inventory.stonePickaxe} I{p.inventory.ironPickaxe} D{p.inventory.diamondPickaxe}\n" ++
    s!"Swords: W{p.inventory.woodSword} S{p.inventory.stoneSword} I{p.inventory.ironSword} D{p.inventory.diamondSword}\n" ++
    s!"Bow: {p.inventory.bow} | Arrows: {p.inventory.arrows}\n" ++
    s!"Armor: H{p.inventory.armorHelmet} C{p.inventory.armorChestplate} L{p.inventory.armorLeggings} B{p.inventory.armorBoots}\n" ++
    s!"Potions: R{p.inventory.potionRed} G{p.inventory.potionGreen} B{p.inventory.potionBlue} P{p.inventory.potionPink} C{p.inventory.potionCyan} Y{p.inventory.potionYellow}\n" ++
    s!"XP: {p.inventory.xp} | Level: {p.inventory.level} | Stat Points: {p.inventory.statPoints}\n"
  let ach :=
    let unlocked := Achievements.unlockedNames p.achievements
    let body := if unlocked.isEmpty then "(none)" else String.intercalate "\n" unlocked
    s!"\n=== ACHIEVEMENTS ===\n{body}\n"
  header ++ "\n=== VIEW ===\n" ++ view ++ vitals ++ resources ++ tools ++ ach

end Render

end Crafter
