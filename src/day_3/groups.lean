import tactic

namespace mbl

/-
# Groups

## Definition of a group

The `group` class will extend `has_mul`, `has_one` and `has_inv`. 

`has_mul G` means that `G` has a multiplication `* : G → G → G`
`has_one G` means that `G` has a `1 : G`
`has_inv G` means that `G` has an `⁻¹ : G → G`

All of `*`, `1` and `⁻¹` are notation for functions -- no axioms yet.

A `group` has all of this notation, and the group axioms too. 
Let's now define the group class.

A `group` structure on a type `G` is multiplication, identity and inverse,
plus the usual axioms 

-/

class group (G : Type) extends has_mul G, has_one G, has_inv G :=
(mul_assoc : ∀ (a b c : G), a * b * c = a * (b * c))
(one_mul : ∀ (a : G), 1 * a = a)
(mul_left_inv : ∀ (a : G), a⁻¹ * a = 1)

/-

Formally, a term of type `group G` is now the following data:
a multiplication, 1, and inverse function,
and proofs that the group axioms are satisfied.

The way to say "let G be a group" is now `(G : Type) [group G]`

The square bracket notation is the notation used for classes.
Formally, it means "put a term of type `group G` into the type class
inference system". In practice this just means "you can use group
notation and axioms in proofs, and Lean will figure out why they're true"

We have been extremely mean with our axioms. Some authors also add
the axioms `mul_one : ∀ (a : G), a * 1 = a`
and `mul_right_inv : ∀ (a : G), a * a⁻¹ = 1`.

But these follow from the three axioms we used. Our first job is
to prove them. As you might imagine, mathematically this is pretty
much the trickiest part, because we have to be careful not to
accidentally assume these axioms when we're proving them.

Here are the four lemmas we will prove next.

`mul_left_cancel : ∀ (a b c : G), a * b = a * c → b = c`
`mul_eq_of_eq_inv_mul {a x y : G} : x = a⁻¹ * y → a * x = y`
`mul_one (a : G) : a * 1 = a`
`mul_right_inv (a : G) : a * a⁻¹ = 1`
-/

namespace group

-- let `G` be a group.
variables {G : Type} [group G]

/-
This proof could be done using rewrites, but I will take this opportunity
to introduce the `calc` tactic.
The math is already done. All you need to do is apply correct axioms/assumptions
-/

lemma mul_left_cancel (a b c : G) (Habac : a * b = a * c) : b = c := 
begin
 calc b = 1 * b         : by sorry
    ... = (a⁻¹ * a) * b : by sorry
    ... = a⁻¹ * (a * b) : by sorry
    ... = a⁻¹ * (a * c) : by sorry
    ... = (a⁻¹ * a) * c : by sorry
    ... = 1 * c         : by sorry
    ... = c             : by sorry
end
/-
Next we prove that if `x = a⁻¹ * y` then `a * x = y`. Remember we are still
missing `mul_one` and `mul_right_inv`. A proof that avoids them is
the following: we want `a * x = y`. Now `apply`ing the previous lemma, it
suffices to prove that `a⁻¹ * (a * x) = a⁻¹ * y.`
Now use associativity and left cancellation on on the left, to reduce
to `h`.

Note that `mul_left_cancel` is a function, and its first input is 
called `a`, but you had better give it `a⁻¹` instead.
-/

lemma mul_eq_of_eq_inv_mul {a x y : G} (h : x = a⁻¹ * y) : a * x = y :=
begin
  sorry,
end

-- Let `a,b,c,x,y` be elements of `G`.
variables (a b c x y : G)
--From now on we don't need to redefine all the variables in our theorems

/-
We can use `mul_eq_of_eq_inv_mul` to prove the two "missing" axioms `mul_one`
and `mul_right_inv`, and then our lives will be much easier. Try `apply`ing it
in the theorems below.
-/

@[simp] theorem mul_one : a * 1 = a :=
begin
  sorry,
end

@[simp] theorem mul_right_inv : a * a⁻¹ = 1 :=
begin
  sorry,
end



end group
end mbl