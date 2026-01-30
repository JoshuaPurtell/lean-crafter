namespace Crafter

inductive TimeMode where
  | Logical
  | RealTime (ticksPerSecond : Float) (pauseOnDisconnect : Bool)
  | Hybrid (ticksPerSecond : Float) (allowManualStep : Bool)
  deriving Repr

instance : Inhabited TimeMode := ⟨TimeMode.Logical⟩

end Crafter
