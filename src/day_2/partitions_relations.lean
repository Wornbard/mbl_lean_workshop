import tactic

/-!

# Equivalence relations are the same as partitions

In this file we prove that there's a bijection between
the equivalence relations on a type, and the partitions of a type. 

## Overview

Say `α` is a type, and `R : α → α → Prop` is a binary relation on `α`. 
The following things are already in Lean:

`reflexive R := ∀ (x : α), R x x`
`symmetric R := ∀ ⦃x y : α⦄, R x y → R y x`
`transitive R := ∀ ⦃x y z : α⦄, R x y → R y z → R x z`

`equivalence R := reflexive R ∧ symmetric R ∧ transitive R`

In the file below, we will define partitions of `α` and "build some
interface" (i.e. prove some propositions). We will define
equivalence classes and do the same thing.
Finally, we will prove that there's a bijection between
equivalence relations on `α` and partitions of `α`.


-/

/-
# Partitions

## Definition of a partition

Let `α` be a type. A *partition* on `α` is defined to be
the following data:

1) A set C of subsets of α, called "blocks".
2) A hypothesis (i.e. a proof!) that all the blocks are non-empty.
3) A hypothesis that every term of type α is in one of the blocks.
4) A hypothesis that two blocks with non-empty intersection are equal.
-/

@[ext] structure partition (α : Type) :=
(C : set (set α))
(Hnonempty : ∀ X ∈ C, (X : set α).nonempty)
(Hcover : ∀ a, ∃ X ∈ C, a ∈ X)
(Hdisjoint : ∀ X Y ∈ C, (X ∩ Y : set α).nonempty → X = Y)

/-

## Basic interface for partitions

Here's the way notation works. If `α` is a type (i.e. a set)
then a term `P` of type `partition α` is a partition of `α`,
that is, a set of disjoint nonempty subsets of `α` whose union is `α`.

The collection of sets underlying `P` is `P.C`, the proof that
they're all nonempty is `P.Hnonempty` and so on.

-/

namespace partition

variables {α : Type} {P : partition α } {X Y : set α}


theorem eq_of_mem (hX : X ∈ P.C) (hY : Y ∈ P.C) {a : α} (haX : a ∈ X) (haY : a ∈ Y) : X = Y := by
begin

  --apply P.Hdisjoint _ _ hX hY ⟨a, haX, haY⟩,
 
  /-
  This may be a good time to explain all the underscores, brackets and commas
  You can think of P.Hdisjoint as a function that takes some arguments and returns a proof
  that two distinct elements of a partition are distjoint.
  Here we need to provide two sets, a proof they belong to P.C, and a proof their intersection is nonempty
  `_` asks Lean to infer the appropriate value. Here it's very simple.
  It guesses that the two sets are X,Y
  Together with hX hY we prove the part - X Y ∈ C
  `⟨stuff⟩` is a bit more complicated (i.e. I don't know enough type theory to really understand it)
  In essence it uses the terms you provide to construct a term of another type. You don't need to specify the type
  Lean infers it on its own. Now it gets wild
  Lean can in fact construct a proof of `(X ∩ Y : set α).nonempty` using these terms
  'How the hell?' you may ask. This comes down to how `nonempty` is defined in Lean.
  It's not immediately visible in the statement of the goal
  So how to come up with this thing?
  Use `library_search`. It will come up with all sorts of slick one-liners that utilise the power of Lean's type system
  Or if you want to understand every step, do this : 
  -/
  apply P.Hdisjoint X Y hX hY,
  --now we may prove the non empty-interseciton 'manually'
  refine set.nonempty_def.mpr _,
  --bingo! We rewrote the goal using the definition of a non-empty set
  -- We can now complete with `use ⟨a, haX, haY⟩`,
  --Or if you REALLY want to understand everything under the hood, do this step by step
  use a,
  use ⟨haX, haY⟩,
  --The last step uses terms of the two types `a ∈ X, a ∈ Y` to construct a term of type `a ∈ X ∩ Y`
  --If it's still not clear how this works, try writing `rw set.inter_def` after `use a` instead
end

--Let's return to doing honest work instead of proving the same thing in 10 ways

/-- If a is in two blocks X and Y, and if b is in X,
  then b is in Y (as X=Y) -/
theorem mem_of_mem (hX : X ∈ P.C) (hY : Y ∈ P.C) {a b : α}
  (haX : a ∈ X) (haY : a ∈ Y) (hbX : b ∈ X) : b ∈ Y :=
begin
  -- you might want to start with `have hXY : X = Y`
  -- and prove it from the previous lemma
  sorry,
end

/-- Every term of type `α` is in one of the blocks for a partition `P`. -/
theorem mem_block (a : α) : ∃ X : set α, X ∈ P.C ∧ a ∈ X :=
begin
  -- an interesting way to start is
  -- `obtain ⟨X, hX, haX⟩ := P.Hcover a,`
 
  sorry,
end

end partition

/-
# Equivalence classes
-/

section equivalence_classes

/-
## Definition of equivalence classes 
-/

-- Notation and variables for the equivalence class section:

-- let α be a type, and let R be a binary relation on R.
variables {α : Type} (R : α → α → Prop)

-- Note for me to make sure everyone understands why R is a binary relation

/-- The equivalence class of `a` is the set of `b` related to `a`. -/
def cl (a : α) :=
{b : α | R b a}

/-

## Basic lemmas about equivalence classes

-/
/-- Useful for rewriting -- `b` is in the equivalence class of `a` iff
`b` is related to `a`. True by definition. -/

--Can you see why this is useful?
theorem mem_cl_iff {a b : α} : b ∈ cl R a ↔ R b a :=
begin   
  -- true by definition
  refl
end

-- Assume now that R is an equivalence relation.
variables {R} (hR : equivalence R)
include hR

/-- x is in cl(x) -/
lemma mem_cl_self (a : α) :
  a ∈ cl R a :=
begin
  -- Note that `hR : equivalence R` is a package of three things.
  -- You can extract the things with
  -- `rcases hR with ⟨hrefl, hsymm, htrans⟩,` or
  -- `obtain ⟨hrefl, hsymm, htrans⟩ := hR,`
  sorry,

end

--We're slowly entering the territory of theorems that take more than one sentence to prove in normal math.
--Apart from consulting `suggest` and `library_search` it's good to think
-- about how you would prove this with pen and paper and try to translate it
lemma cl_sub_cl_of_mem_cl {a b : α} :
  a ∈ cl R b →
  cl R a ⊆ cl R b :=
begin
  -- remember `set.subset_def` says `X ⊆ Y ↔ ∀ a, a ∈ X → a ∈ Y
  sorry,

end

lemma cl_eq_cl_of_mem_cl {a b : α} :
  a ∈ cl R b →
  cl R a = cl R b :=
begin
  -- remember `set.subset.antisymm` says `X ⊆ Y → Y ⊆ X → X = Y`
  sorry,
end

end equivalence_classes
/-
# The theorem

Let `α` be a type (i.e. a collection of stuff).

There is a bijection between equivalence relations on `α` and
partitions of `α`.

We prove this by writing down constructions in each direction
and proving that the constructions are two-sided inverses of one another.
-/


open partition

example (α : Type) : {R : α → α → Prop // equivalence R} ≃ partition α :=
-- We define constructions (functions!) in both directions and prove that
-- one is a two-sided inverse of the other
{ -- Here is the first construction, from equivalence
  -- relations to partitions.
  -- Let R be an equivalence relation.
  to_fun := λ R, {
    -- Let C be the set of equivalence classes for R.
    C := { B : set α | ∃ x : α, B = cl R.1 x},
    -- I claim that C is a partition. We need to check the three
    -- hypotheses for a partition (`Hnonempty`, `Hcover` and `Hdisjoint`),
    -- so we need to supply three proofs.
    Hnonempty := begin
      sorry,
    end,
    
    Hcover := begin
      cases R with R hR,
      -- The equivalence classes cover α
      show ∀ (a : α), ∃ (X : set α) (H : ∃ (b : α), X = cl R b), a ∈ X,
      sorry,
    end,
    Hdisjoint := begin
      cases R with R hR,
      -- If two equivalence classes overlap, they are equal.
      show ∀ (X Y : set α), (∃ (a : α), X = cl R a) →
        (∃ (b : α), Y = cl _ b) → (X ∩ Y).nonempty → X = Y,
      sorry,
    end },
  -- Conversely, say P is an partition. 
  inv_fun := λ P, 
    -- Let's define a binary relation `R` thus:
    --  `R a b` iff *every* block containing `a` also contains `b`.
    -- Because only one block contains a, this will work,
    -- and it turns out to be a nice way of thinking about it. 
    ⟨λ a b, ∀ X ∈ P.C, a ∈ X → b ∈ X, begin
      -- I claim this is an equivalence relation.
    split,
    { -- It's reflexive
      show ∀ (a : α)
        (X : set α), X ∈ P.C → a ∈ X → a ∈ X,
      sorry,
    },
    split,
    { -- it's symmetric
      show ∀ (a b : α),
        (∀ (X : set α), X ∈ P.C → a ∈ X → b ∈ X) →
         ∀ (X : set α), X ∈ P.C → b ∈ X → a ∈ X,
      sorry,
    },
    { -- it's transitive
      unfold transitive,
      show ∀ (a b c : α),
        (∀ (X : set α), X ∈ P.C → a ∈ X → b ∈ X) →
        (∀ (X : set α), X ∈ P.C → b ∈ X → c ∈ X) →
         ∀ (X : set α), X ∈ P.C → a ∈ X → c ∈ X,
      sorry,
    }
  end⟩,
  -- If you start with the equivalence relation, and then make the partition
  -- and a new equivalence relation, you get back to where you started.
  left_inv := begin
    rintro ⟨R, hR⟩,
    -- Tidying up the mess...
    suffices : (λ (a b : α), ∀ (c : α), a ∈ cl R c → b ∈ cl R c) = R,
      simpa,
    -- ... you have to prove two binary relations are equal.
    ext a b,
    -- so you have to prove an if and only if.
    show (∀ (c : α), a ∈ cl R c → b ∈ cl R c) ↔ R a b,
    sorry,
  end,
  -- Similarly, if you start with the partition, and then make the
  -- equivalence relation, and then construct the corresponding partition 
  -- into equivalence classes, you have the same partition you started with.  
  right_inv := begin
    -- Let P be a partition
    intro P,
    -- It suffices to prove that a subset X is in the original partition
    -- if and only if it's in the one made from the equivalence relation.
    ext X,
    show (∃ (a : α), X = cl _ a) ↔ X ∈ P.C,
    dsimp only,
    sorry,
  end }