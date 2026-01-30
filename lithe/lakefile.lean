import Lake
open Lake DSL

package crafter_web where

require lithe from "../../lithe"
require CrafterLean from "../src_lean4"

@[default_target]
lean_lib CrafterWeb
