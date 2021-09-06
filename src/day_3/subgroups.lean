import day_3.groups

/-!

## Subgroups

We define the structure `subgroup G`, whose terms are subgroups of `G`.
A subgroup of `G` is implemented as a subset of `G` closed under
`1`, `*` and `⁻¹`.

-/

namespace mbl

/-- A subgroup of a group G is a subset containing 1
and closed under multiplication and inverse. -/
structure subgroup (G : Type) [group G] :=
(carrier : set G)
(one_mem' : (1 : G) ∈ carrier)
(mul_mem' {x y} : x ∈ carrier → y ∈ carrier → x * y ∈ carrier)
(inv_mem' {x} : x ∈ carrier → x⁻¹ ∈ carrier)

/-

At this point, here's what we have.

A term `H` of type `subgroup G`, written `H : subgroup G`, is a
*quadruple*. To give a term `H : subgroup G` is to give the following four things:
1) `H.carrier` (a subset of `G`),
2) `H.one_mem'` (a proof that `1 ∈ H.carrier`),
3) `H.mul_mem'` (a proof that `H` is closed under multiplication)
4) `H.inv_mem'` (a proof that `H` is closed under inverses).

Note in particular that Lean, being super-pedantic, *distinguishes* between
the subgroup `H` and the subset `H.carrier`. One is a subgroup, one is
a subset. When we get going we will start by setting up some infrastructure
so that this difference will be hard to notice.

Note also that if `x` is in the subgroup `H` of `H` then the _type_ of `x` is still `G`,
and `x ∈ carrier` is a Proposition. Note also that `x : carrier` doesn't
make sense (`carrier` is a term, not a type, rather counterintuitively).

-/

namespace subgroup
open mbl.group

variables {G : Type} [group G] (H J K : subgroup G)

/-
We don't want to write `H.carrier` to refer to the underlying set
everywhere, because we want to be able to identify the subgroup `H` with
its underlying subset `H.carrier`. Note that these things are not _equal_,
firstly because `H` contains the proof that `H.carrier` is a subgroup, and
secondly because these terms have different types! `H` has type `subgroup G`
and `H.carrier` has type `set G`. Let's start by sorting this out.
-/

-- If `x : G` and `H : subgroup G` then let's define the notation
-- `x ∈ H` to mean `x ∈ H.carrier`
instance : has_mem G (subgroup G) := ⟨λ m H, m ∈ H.carrier⟩

-- Let's also define a "coercion", a.k.a. an "invisible map"
-- from subgroups of `G` to subsets of `G`, sending `H` to `H.carrier`. 
-- The map is not completely invisible -- it's a little ↑. So
-- if you see `↑H` in the future, it means the subset `H.carrier` by definition.
instance : has_coe (subgroup G) (set G) := ⟨λ H, H.carrier⟩

/-- `g` is in the underlying subset of `H` iff `g ∈ H`. -/
@[simp] lemma mem_carrier {g : G} : g ∈ H.carrier ↔ g ∈ H :=
begin
  -- true by definition
  refl
end

/-- `g` is in `H` considered as a subset of `G`, iff `g` is in `H` considered
as subgroup of `G`. -/
@[simp] lemma mem_coe {g : G} : g ∈ (↑H : set G) ↔ g ∈ H :=
begin
  -- true by definition
  refl
end

-- Now let's define theorems without the `'`s in, which use this
-- more natural notation

/-- A subgroup contains the group's 1. -/
theorem one_mem : (1 : G) ∈ H :=
begin
  sorry,
end

/-- A subgroup is closed under multiplication. -/
theorem mul_mem {x y : G} : x ∈ H → y ∈ H → x * y ∈ H :=
begin
  sorry,
end

/-- A subgroup is closed under inverse -/
theorem inv_mem {x : G} : x ∈ H → x⁻¹ ∈ H :=
begin
  sorry,
end

/-

So here are the three theorems which you need to remember about subgroups.
Say `H : subgroup G`. Then:

`H.one_mem : (1 : G) ∈ H`
`H.mul_mem {x y : G} : x ∈ H → y ∈ H → x * y ∈ H`
`H.inv_mem {x : G} : x ∈ H → x⁻¹ ∈ H`

These now look like the way a mathematician would write things.

Now let's start to prove basic theorems about subgroups (or, as a the computer
scientists would say, make a basic _interface_ or _API_ for subgroups),
using this sensible notation.

Here's an example; let's prove `x ∈ H ↔ x⁻¹ ∈ H`. Let's put the more
complicated expression on the left hand side of the `↔` though, because then
we can make it a `simp` lemma.
-/

-- Remember that `xena.group.inv_inv x` is the statement that `x⁻¹⁻¹ = x`

@[simp] theorem inv_mem_iff {x : G} : x⁻¹ ∈ H ↔ x ∈ H := 
begin
  sorry,
end

-- We could prove a bunch more theorems here. Let's just do one more.
-- Let's show that if x and xy are in H then so is y.

theorem mem_of_mem_mul_mem {x y : G} (hx : x ∈ H) (hxy : x * y ∈ H) : y ∈ H :=
begin
  sorry,
end



end subgroup
end mbl