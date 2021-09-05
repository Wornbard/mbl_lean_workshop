--I may or may not decide to show it at the beginning of the workshop.
--If I don't you won't really benefit much from reading it, so you're welcome to ignore it

import data.nat.prime

import tactic

open nat
 

theorem infinitude_of_primes : ∀ (N : ℕ ), ∃ p ≥ N, prime p :=

begin
  intro N,

  let M := factorial N +1,

  let p:= min_fac M,

  have pp : prime p :=
  begin
    refine min_fac_prime _,

    have : factorial N >0 :=
    begin
      apply factorial_pos N,
    end,
    linarith,
  end,

  use p,
  split,
  {
    by_contra,
    have h₁ : p ∣  factorial N + 1 := min_fac_dvd M,
    
    have h₂ : p ∣  factorial N := 
    begin
      refine pp.dvd_factorial.mpr _,

      exact le_of_not_ge h,
    end,
    have h : p∣ 1 := (nat.dvd_add_right h₂).mp h₁,

    exact prime.not_dvd_one pp h,

  },

  {
    assumption,
  },
end