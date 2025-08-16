-- Groupoid.lean
structure Env where
  b : Real
  K : Real
  H : Real

structure Policy where
  -- placeholder

def equiv (π₁ π₂ : Policy) : Prop := False -- fill with the setoid from the paper

def PolicySetoid : Setoid Policy := ⟨equiv, sorry, sorry, sorry⟩

def Morphospace := Quot PolicySetoid