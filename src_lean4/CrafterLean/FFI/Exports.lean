import Std
import CrafterLean.Action
import CrafterLean.Config
import CrafterLean.Serialize
import CrafterLean.Render.Ascii
import CrafterLean.Oracle.SaveLoad

open Std

namespace Crafter

namespace FFI

initialize sessionStore : IO.Ref (HashMap UInt64 OracleState) ← IO.mkRef {}
initialize nextHandle : IO.Ref UInt64 ← IO.mkRef 1

@[inline] def allocHandle (s : OracleState) : IO UInt64 := do
  let h <- nextHandle.get
  nextHandle.set (h + 1)
  sessionStore.modify fun m => m.insert h s
  return h

@[inline] def getSession (h : UInt64) : IO (Option OracleState) := do
  let store <- sessionStore.get
  return store.get? h

@[inline] def setSession (h : UInt64) (s : OracleState) : IO Unit :=
  sessionStore.modify fun m => m.insert h s

@[inline] def removeSession (h : UInt64) : IO Unit :=
  sessionStore.modify fun m => m.erase h

@[inline] def errorJson (msg : String) : ByteArray :=
  encodeJson (Lean.Json.mkObj [("error", msg)])

@[export crafter_lean_init]
def crafter_lean_init : IO UInt32 :=
  return 0

@[export crafter_lean_new]
def crafter_lean_new (cfgBytes : ByteArray) (seed : UInt64) : IO UInt64 := do
  let cfgResult :=
    if cfgBytes.isEmpty then
      Except.ok SessionConfig.default
    else
      decodeFromJson (α := SessionConfig) cfgBytes
  match cfgResult with
  | Except.error _ => return 0
  | Except.ok cfg =>
      let state := OracleState.new cfg seed
      allocHandle state

@[export crafter_lean_step]
def crafter_lean_step (h : UInt64) (action : UInt32) : IO ByteArray := do
  let stateOpt <- getSession h
  match stateOpt with
  | none => return errorJson "invalid_handle"
  | some s =>
      let a := (Action.fromUInt32? action).getD Action.Noop
      let (s', out) := Oracle.step s a
      setSession h s'
      return encodeToJson out

@[export crafter_lean_render]
def crafter_lean_render (h : UInt64) : IO ByteArray := do
  let stateOpt <- getSession h
  match stateOpt with
  | none => return errorJson "invalid_handle"
  | some s =>
      return (Render.ascii s).toUTF8

@[export crafter_lean_save]
def crafter_lean_save (h : UInt64) : IO ByteArray := do
  let stateOpt <- getSession h
  match stateOpt with
  | none => return errorJson "invalid_handle"
  | some s =>
      return SaveLoad.save s

@[export crafter_lean_load]
def crafter_lean_load (stateBytes : ByteArray) : IO UInt64 := do
  match SaveLoad.load stateBytes with
  | Except.ok s => allocHandle s
  | Except.error _ => return 0

@[export crafter_lean_free]
def crafter_lean_free (h : UInt64) : IO Unit := do
  removeSession h

end FFI

end Crafter
