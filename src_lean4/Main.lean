import CrafterLean

open Crafter

def parseAction (input : String) : Option Action :=
  let token := input.trimAscii.toString.toLower
  match token with
  | "noop" | "n" => some Action.Noop
  | "left" | "a" => some Action.MoveLeft
  | "right" | "d" => some Action.MoveRight
  | "up" | "w" => some Action.MoveUp
  | "down" | "s" => some Action.MoveDown
  | "do" | "e" => some Action.Do
  | "sleep" => some Action.Sleep
  | "place_stone" => some Action.PlaceStone
  | "place_table" => some Action.PlaceTable
  | "place_furnace" => some Action.PlaceFurnace
  | "place_plant" => some Action.PlacePlant
  | "make_wood_pickaxe" => some Action.MakeWoodPickaxe
  | "make_stone_pickaxe" => some Action.MakeStonePickaxe
  | "make_iron_pickaxe" => some Action.MakeIronPickaxe
  | "make_wood_sword" => some Action.MakeWoodSword
  | "make_stone_sword" => some Action.MakeStoneSword
  | "make_iron_sword" => some Action.MakeIronSword
  | "make_diamond_pickaxe" => some Action.MakeDiamondPickaxe
  | "make_diamond_sword" => some Action.MakeDiamondSword
  | "make_iron_armor" => some Action.MakeIronArmor
  | "make_diamond_armor" => some Action.MakeDiamondArmor
  | "make_bow" => some Action.MakeBow
  | "make_arrow" => some Action.MakeArrow
  | "shoot_arrow" => some Action.ShootArrow
  | "drink_red" => some Action.DrinkPotionRed
  | "drink_green" => some Action.DrinkPotionGreen
  | "drink_blue" => some Action.DrinkPotionBlue
  | "drink_pink" => some Action.DrinkPotionPink
  | "drink_cyan" => some Action.DrinkPotionCyan
  | "drink_yellow" => some Action.DrinkPotionYellow
  | _ =>
      match token.toNat? with
      | some n => Action.fromUInt32? (UInt32.ofNat n)
      | none => none

def replHelp : String :=
  String.intercalate "\n"
    [
      "Commands:",
      "  w/a/s/d     move",
      "  e           do/interact",
      "  noop|n      no-op",
      "  sleep",
      "  place_stone | place_table | place_furnace | place_plant",
      "  make_wood_pickaxe | make_stone_pickaxe | make_iron_pickaxe",
      "  make_wood_sword | make_stone_sword | make_iron_sword",
      "  make_diamond_pickaxe | make_diamond_sword",
      "  make_iron_armor | make_diamond_armor",
      "  make_bow | make_arrow | shoot_arrow",
      "  drink_red | drink_green | drink_blue | drink_pink | drink_cyan | drink_yellow",
      "  0..29       numeric action index",
      "  help",
      "  quit|q"
    ]

partial def replLoop (stdin : IO.FS.Stream) (s : OracleState) : IO Unit := do
  IO.print "> "
  let line <- stdin.getLine
  let input := line.trimAscii.toString
  if input == "" then
    replLoop stdin s
  else if input == "quit" || input == "q" then
    IO.println "bye"
  else if input == "help" then
    IO.println replHelp
    replLoop stdin s
  else
    match parseAction input with
    | none =>
        IO.println "unknown command (type 'help')"
        replLoop stdin s
    | some a =>
        let (s', out) := Oracle.step s a
        IO.println (Render.ascii s')
        if out.done then
          IO.println "done"
        else
          replLoop stdin s'

def main (args : List String) : IO Unit := do
  let cfg := SessionConfig.default
  let seed : UInt64 := 1
  if args.any (fun a => a == "repl") then
    IO.println "CrafterLean REPL (type 'help' for commands)"
    let stdin <- IO.getStdin
    replLoop stdin (OracleState.new cfg seed)
  else
    let state := OracleState.new cfg seed
    let (_, out) := Oracle.step state Action.Noop
    IO.println s!"step={out.obs.step} pos=({out.obs.playerPos.x}, {out.obs.playerPos.y})"
