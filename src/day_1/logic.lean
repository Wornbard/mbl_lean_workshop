-- We import all of Lean's standard tactics
import tactic

/-
 Since it's the easiest thing to start with, we first develop basic logic.
 
 # The logical symbols that Lean understands :

 * `→` ("implies" -- type with `\l`)
 * `¬` ("not" -- type with `\not` or `\n`)
 * `∧` ("and" -- type with `\and` or `\an`)
 * `↔` ("iff" -- type with `\iff` or `\lr`)
 * `∨` ("or" -- type with `\or` or `\v`)

 # Useful tactics : 

 * `intro`
 * `exact`
 * `apply`
 * `rw`
 * `cases`
 * `split`
 * `left`
 * `right`


-/

namespace mbl


variables (P Q R : Prop)
--We define three variables, each is a ' term of type `Prop` '

-- ### Introductory examples with implies (→)

--Exciting stuff
theorem id : P → P :=
begin
  -- let hP be a proof of P
  intro hP,
  -- then hP is a proof of P!
  -- Did I mention it's going to be an easy class?
  exact hP
end


--This one is immediate from definition.
-- in Lean, `P → Q → R` is _defined_ to mean `P → (Q → R)`
example : (P → Q → R) ↔ (P → (Q → R)) :=
begin
  -- look at the goal!
  refl -- true because ↔ is reflexive
end

theorem imp_intro : P → Q → P :=
begin
  -- remember that by definition the goal is P → (Q → P),
  -- so it's P implies something, so let's assume 
  -- that P is true and call this hypothesis hP.
  intro hP,
  -- Now we have to prove that Q implies P, so let's
  -- assume that Q is true, and let's call this hypothesis hQ
  intro hQ,
  -- We now have to prove that P is true.
  -- But this is exactly our hypothesis hP.
  exact hP,
end

lemma modus_ponens : P → (P → Q) → Q :=
begin
  -- remember this means "P implies that ((P implies Q) implies Q)"
  -- so let's assume P is true
  intro hP,
  -- and let's assume hypothesis hPQ, that P implies Q
  intro hPQ,
  -- now `hPQ` says `P → Q` and we're trying to prove `Q`!
  -- So by applying the hypothesis `hPQ`, we can reduce
  -- this puzzle to proving `P`.
  apply hPQ,
  -- Now we have to prove `P`. But this is just an assumption
  exact hP, -- or `assumption`
end

-- ### First independent steps

lemma imp_trans : (P → Q) → (Q → R) → (P → R) :=
begin
  -- The tactics you know should be enough
  sorry,
end

lemma forall_imp : (P → Q → R) → (P → Q) → (P → R) :=
begin
  -- `intros hPQR hPQ hP,` would be a fast way to start.
  sorry,
end

/-
### not

`not P`, with notation `¬ P`, is *defined* to mean `P → false` in Lean,
i.e., the proposition that P implies false. You can easily check with
a truth table that P → false and ¬ P are equivalent.


-/

theorem not_iff_imp_false : ¬ P ↔ (P → false) :=
begin
  -- true by definition
  refl
end

theorem not_not_intro : P → ¬ (¬ P) :=
begin
  --You can do it in a few ways. One of them is particularly short and slick
  sorry,
end

-- This is "modus tollens". Some mathematicians think of it as
-- "proof by contradiction".
theorem modus_tollens : (P → Q) → (¬ Q → ¬ P) :=
begin
  sorry,
end

-- This one cannot be proved using constructive mathematics!
-- You _have_ to use a tactic like `by_contra` (or, if you're happy
-- to cheat, the full "truth table" tactic `tauto!`.
-- Try it without using these, and you'll get stuck!
theorem double_negation_elimination : ¬ (¬ P) → P :=
begin
  sorry,
end

/-

### and

The hypothesis `hPaQ : P ∧ Q` in Lean, is equivalent to
hypotheses `hP : P` and `hQ : Q`. 

If you have `hPaQ` as a hypothesis, and you want to get to
`hP` and `hQ`, you can use the `cases` tactic.

If you have `⊢ P ∧ Q` as a goal, and want to turn the goal
into two goals `⊢ P` and `⊢ Q`, then use the `split` tactic.

Note that after `split` it's good etiquette to use braces
e.g.

example (hP : P) (hQ : Q) : P ∧ Q :=
begin
  split,
  { exact hP },
  { exact hQ }
end

-/

theorem and.elim_left : P ∧ Q → P :=
begin
  -- if `h : P ∧ Q` then `h.1 : P` and `h.2 : Q`
  sorry,
end

theorem and.elim_right : P ∧ Q → Q :=
begin
  sorry,
end

theorem and.intro : P → Q → P ∧ Q :=
begin
  -- remember the `split` tactic.
  sorry,
end

/-- the eliminator for `∧` -/ 
theorem and.elim : P ∧ Q → (P → Q → R) → R :=
begin
  sorry,
end

/-- The recursor for `∧` -/
theorem and.rec : (P → Q → R) → P ∧ Q → R :=
begin
  sorry,
end

/-- `∧` is symmetric -/
theorem and.symm : P ∧ Q → Q ∧ P :=
begin
  --Useful fact:
  -- `intro hPQ`
  -- `cases hPQ with hP hQ`
  --can be replaced with
  -- `rintro ⟨hP, hQ⟩`
  sorry,
end


/-- `∧` is transitive -/
theorem and.trans : (P ∧ Q) → (Q ∧ R) → (P ∧ R) :=
begin
  -- The `rintro` tactic will do `intro` and `cases` all in one go.
  -- If you like, try starting this proof with `rintro ⟨hP, hQ⟩` if you want
  -- to experiment with it. Get the pointy brackets with `\<` and `\>`,
  -- or both at once with `\<>`.

  sorry,

end

/-
Recall that the convention for the implies sign →
is that it is _right associative_, by which
I mean that `P → Q → R` means `P → (Q → R)` by definition.
Now note that if `P` implies `Q → R`
then this means that `P` and `Q` together, imply `R`,
so `P → Q → R` is logically equivalent to `(P ∧ Q) → R`.

We proved that `P → Q → R` implied `(P ∧ Q) → R`; this was `and.rec`.
Let's go the other way.
-/

lemma imp_imp_of_and_imp : ((P ∧ Q) → R) → (P → Q → R) :=
begin
  sorry,
end


/-!

### iff

The basic theory of `iff`.

In Lean, to prove `P ∧ Q` you have to prove `P` and `Q`.
Similarly, to prove `P ↔ Q` in Lean, you have to prove `P → Q`
and `Q → P`. Just like `∧`, you can uses `cases h` if you have
a hypothesis `h : P ↔ Q`, and `split` if you have a goal `⊢ P ↔ Q`.
-/

/-- `P ↔ P` is true for all propositions `P`, i.e. `↔` is reflexive. -/
theorem iff.refl : P ↔ P :=
begin
  split,
  apply id,
  apply id,
  /- or tauto, tauto-/
end

-- If you get stuck, there is always the "truth table" tactic `tauto!`
-- This literally solves everything above. It's a cool thing
-- but overrelying on it today would be pointless
example : P ↔ P :=
begin
  tauto!, -- the "truth table" tactic.
end

-- refl tactic also works
example : P ↔ P :=
begin
  refl -- `refl` knows that `=` and `↔` are reflexive.
end

/-- `↔` is symmetric -/
theorem iff.symm : (P ↔ Q) → (Q ↔ P) :=
begin
  sorry,
end

/-- `↔` is commutative -/
theorem iff.comm : (P ↔ Q) ↔ (Q ↔ P) :=
begin
  sorry,

end

-- without rw or cc this is painful!
/-- `↔` is transitive -/
theorem iff.trans :  (P ↔ Q) → (Q ↔ R) → (P ↔ R) :=
begin
  sorry,

end

-- This can be done constructively, but it's hard. You'll need to know
-- about the `have` tactic to do it. Alternatively the truth table
-- tactic `tauto!` will do it.
theorem iff.boss : ¬ (P ↔ ¬ P) :=
begin
  sorry,
end

-- Now we have iff we can go back to and.

/-!
### ↔ and ∧
-/

/-- `∧` is commutative -/
theorem and.comm : P ∧ Q ↔ Q ∧ P :=
begin
  sorry,
end


-- Note that ∧ is "right associative" in Lean, which means
-- that `P ∧ Q ∧ R` is _defined to mean_ `P ∧ (Q ∧ R)`.
-- Associativity can hence be written like this:
/-- `∧` is associative -/
theorem and_assoc : ((P ∧ Q) ∧ R) ↔ (P ∧ Q ∧ R) :=
begin
  sorry,
end



/-!

## Or

`P ∨ Q` is true when at least one of `P` and `Q` are true.
Here is how to work with `∨` in Lean.

If you have a hypothesis `hPoQ : P ∨ Q` then you 
can break into the two cases `hP : P` and `hQ : Q` using
`cases hPoQ with hP hQ`

If you have a _goal_ of the form `⊢ P ∨ Q` then you
need to decide whether you're going to prove `P` or `Q`.
If you want to prove `P` then use the `left` tactic,
and if you want to prove `Q` then use the `right` tactic.

-/

-- recall that P, Q, R are Propositions. We'll need S for this one.
variable (S : Prop)

-- You will need to use the `left` tactic for this one.
theorem or.intro_left : P → P ∨ Q :=
begin
  intro hP,
  left,
  exact hP,
end

theorem or.intro_right : Q → P ∨ Q :=
begin
  sorry,
end

/-- the eliminator for `∨`. -/
theorem or.elim : P ∨ Q → (P → R) → (Q → R) → R :=
begin
  sorry,
end

/-- `∨` is symmetric -/
theorem or.symm : P ∨ Q → Q ∨ P :=
begin
  sorry
end

/-- `∨` is commutative -/
theorem or.comm : P ∨ Q ↔ Q ∨ P :=
begin
  sorry,
end

/-- `∨` is associative -/
theorem or.assoc : (P ∨ Q) ∨ R ↔ P ∨ Q ∨ R :=
begin
  sorry,
end

/-!
### More about → and ∨
-/

theorem or.imp : (P → R) → (Q → S) → P ∨ Q → R ∨ S :=
begin
  sorry,
end

theorem or.imp_left : (P → Q) → P ∨ R → Q ∨ R :=
begin
  sorry,
end

theorem or.imp_right : (P → Q) → R ∨ P → R ∨ Q :=
begin
  sorry,
end

theorem or.left_comm : P ∨ Q ∨ R ↔ Q ∨ P ∨ R :=
begin
  -- Try rewriting `or.comm` and `or.assoc` to do this one quickly.
  sorry,
end

/-- the recursor for `∨` -/
theorem or.rec : (P → R) → (Q → R) → P ∨ Q → R :=
begin
  sorry,
end

theorem or_congr : (P ↔ R) → (Q ↔ S) → (P ∨ Q ↔ R ∨ S) :=
begin
  sorry,
end

/-!

### true and false

`true` is a true-false statement, which can be proved with the `trivial` tactic.

`false` is a true-false statment which can only be proved if you manage
to find a contradiction within your assumptions.

If you manage to end up with a hypothesis `h : false` then there's quite
a funny way to proceed, which we now explain.

If you have `h : P ∧ Q` then you can uses `cases h with hP hQ` to split
into two cases. 

If you have `h : false` then what do you think happens if we do `cases h`?
Hint: how many cases are there?
-/


/-- eliminator for `false` -/
theorem false.elim : false → P :=
begin
  intro h,
  cases h,


end

theorem and_true_iff : P ∧ true ↔ P :=
begin
  sorry,


end

theorem or_false_iff : P ∨ false ↔ P :=
begin
  sorry,
end

-- false.elim is handy for this one
theorem or.resolve_left : P ∨ Q → ¬P → Q :=
begin
  sorry,
end

-- this one you can't do constructively
theorem or_iff_not_imp_left : P ∨ Q ↔ ¬P → Q :=
begin
    
    sorry,
  
end

end mbl
