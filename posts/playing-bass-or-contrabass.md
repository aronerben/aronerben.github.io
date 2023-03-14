---
title: Playing Bass or Contrabass
description: Introducing double negation elimination to a logical system is equivalent to introducing the law of the excluded middle. Both implications can be proved. Both also make the hair of constructivists stand on end.
published: 2021-12-16
mathjax: true
---

# Introduction
I've been playing around with [Lean](https://leanprover-community.github.io/) recently in the scope of my job. Much like Agda, it is a dependently-typed proof assistant (and general purpose programming language). So far, I enjoy Lean due to its extensive [standard library](https://leanprover-community.github.io/mathlib_docs/), good and accessible solvers and the tactic mode. Knowing Agda a bit helps me quite a lot in picking up the foundations of Lean, as there are many similarities between the two languages.

I've been working through the (very good) tutorial and completing all the exercises. In one chapter, the authors pose a theorem to the reader that I found harder to proof than the other exercises.

# Classical Logic
Agda and Lean are based on constructive logic. Generally, in these systems, a witness or "evidence" of a proposition has to be provided to prove that proposition. In the world of dependent types, this means finding a term that inhabits the type that represents the proposition (according to the [Curry-Howard isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)).

This is in contrast to classical logic, where a proposition is assigned a binary truth value (usually "true" and "false"), regardless if direct evidence for either case exists. This allows for proof techniques like proof by contradiction, where assuming that a proposition is false leads to a contradiction, thus showing that the proposition is true, even though no concrete witness of the proposition was provided. The concept of a binary truth value is called the law of the excluded middle, abbreviated as *LEM* or *EM*. As the name implies, a proposition or its negation must be true in this system, anything in between is excluded, so $p \lor \neg p$ must always hold. LEM is not present in constructive logic.

It is quite easy to see, that if LEM is added to the system, *double negation elimination*, abbreviated as *DNE*, is implied. DNE states that $\neg \neg p \rightarrow p$. In natural language: "If it's false that $p$ is false, then $p$ must be true". Once we declare that $p$ being false is false, we can apply LEM to conclude that $p$ is true, even though we don't have proof of $p$.

After some thought, you should be able to grok the implication $LEM \rightarrow DNE$. The implication $DNE \rightarrow LEM$, however, I found to be less obvious. It is a true implication, yet I can't use natural language to explain why it holds. Nonetheless, there is an understandable proof that we will explore in Lean.

# The Proof
There are two steps in this proof. The first step is to prove $\neg \neg (p \lor \neg p)$. Afterwards, you can apply DNE to $\neg \neg (p \lor \neg p)$ to prove $p \lor \neg p$.

## Proving $\neg \neg (p \lor \neg p)$
This proposition can, maybe surprisingly, be proved constructively[^1]. The reason for this is, is that we are making a contradictory hypothesis, which due to the [principle of explosion](https://en.wikipedia.org/wiki/Principle_of_explosion) can be used to prove anything. This principle is included in the constructive logic Lean is based on.

An important thing to note at this point is that in type theory, negation is represented as a function to the empty type. The empty type is not inhabited, so it corresponds to the type `false` in Lean. `¬p` is therefore the same[^4] as `p → false`. `¬¬(p ∨ ¬p)` then is the same as `((p ∨ (p → false)) → false) → false`.

The contradictory hypothesis we make is `¬(p ∨ ¬p)`, as, using this hypothesis, we can construct a proof of `¬p`. Using disjunction introduction, we get `p ∨ ¬p`. So from the hypothesis we arrive at the positive version of the hypothesis. From that we apply the principle of explosion. Here is the proof in detail with comments:

```agda
lemma em_constructive {p : Prop} : ¬¬(p ∨ ¬p) :=
assume h : ¬(p ∨ ¬p),
-- To prove: ¬p (= p → false)
have hnp : ¬p, from
  assume hp : p,
  -- Use left disjunction intro to construct proof of (p ∨ ¬p) from p
  -- Now we have ¬(p ∨ ¬p) and (p ∨ ¬p), that's absurd!
  -- Use principle of explosion to proof false, thus proving p → false
  absurd (or.inl hp) h,
-- Use right disjunction intro to construct proof of (p ∨ ¬p) from ¬p
-- Now we have ¬(p ∨ ¬p) and (p ∨ ¬p), another absurdity!
-- Use principle of explosion to proof false, thus proving ¬(p ∨ ¬p) → false
show false, from absurd (or.inr hnp) h
```

`absurd` gives us the principle of explosion, which we use twice to prove `false`. In this case, we could also apply `¬p` to `p` and `¬(p ∨ ¬p)` to `(p ∨ ¬p)` (remember the definition of `¬`). However, I like to use `absurd` to illustrate how having a proof of a proposition and a proof of its negation at the same time is absurd.

## Applying DNE and Playing Golf
Now we proof the actual implication $DNE \rightarrow LEM$. One way to do it, as mentioned before, is to import DNE from the standard library and apply it to `em_constructive`. Our way is to parameterize the theorem with DNE as a hypothesis. This means if DNE can be provided, LEM can be proved.
```agda
theorem dne_implies_em {p : Prop} : (∀ {p : Prop}, ¬¬p → p) → p ∨ ¬p :=
-- DNE as hypothesis
assume dne : (∀ {p : Prop}, ¬¬p → p),
dne em_constructive
```
DNE works for all propositions `p` (in classical logic), which is why we need to quantify it with `∀`. Lean does not view the `p` from `em_constructive` as the same `p` from `dne_implies_em`, so quantifying fixes this.

So that's it, the proof is done. We can golf it a bit at the cost of clarity:
```agda
theorem dne_implies_em_g {p : Prop} : (∀ {p : Prop}, ¬¬p → p) → p ∨ ¬p :=
λ dne, dne (λ k, k (or.inr (λ x, k (or.inl x))))
```
We use lambda abstraction and application to replace the proof-specific keywords, so the proof looks fairly Agda-like in this style. Additionally, the parentheses can be replaced with the function application operator `$`.
```agda
theorem dne_implies_em_g' {p : Prop} : (∀ {p : Prop}, ¬¬p → p) → p ∨ ¬p :=
λ dne, dne $ λ k, k $ or.inr $ λ x, k $ or.inl x
```

# Addendum
## Some Notes on Classical Logic
I get the feeling that in many introductory math courses, where simple proofs are tackled, LEM and DNE are assumed and never questioned. My guess is that classical logic is the more natural way to think, which is why people accept the usual proof by contradiction given for the irrationality of $\sqrt{2}$ without questioning it. However, there is a perfectly valid [constructive proof](https://en.wikipedia.org/wiki/Square_root_of_2#Constructive_proof) for this theorem.

Another interesting tidbit is that 3 of the 4 De Morgan implications are constructively true, but $\neg (p \land q) → \neg p \lor \neg q$ is not. The reason is that $p$ and $q$ not being both true does not give enough information to deduce which one is false[^2].

One more fact for you: It might be unintuitive, but the introduction rule to DNE $p \rightarrow \neg \neg p$ is constructively also true. The proof is fairly short and makes use of `absurd` again:

```agda
theorem dne_constructive {p : Prop} : p → ¬¬p :=
λ p, λ np, absurd p np
```

Having a proof of $p$ is regarded as "stronger" than having one for $\neg \neg p$, so the implication works in this way constructively. A loose explanation of this fact in natural language might go something like this: If you say "The food is good", you require explicit proof that the food is good. If you say "The food is not bad", you merely have to show that the proof of the food being good is not contradictory with your proposition[^3].

## Proof in Tactic Mode
Here is the proof in tactic mode:
```agda
theorem dne_implies_em_t {p : Prop} : (∀ {p : Prop}, ¬¬p → p) → p ∨ ¬p :=
begin
  intros dne,
  apply dne,
  intro h,
  have hnp : ¬p :=
    begin
      intro hp,
      apply h,
      apply or.inl,
      exact hp,
    end,
  apply h,
  apply or.inr,
  exact hnp,
end
```
And using the almighty `cc` solver:
```agda
theorem dne_implies_em_t' {p : Prop} : (∀ {p : Prop}, ¬¬p → p) → p ∨ ¬p :=
begin
  intros dne,
  apply dne,
  intro h,
  cc,
end
```

## Syntax Highlighting
The code snippets are not yet properly syntax highlighted, the tiny bit of highlighting comes from my Agda annotation to each code snippet. I plan on including proper highlighting and a proper formatter at one point.

[^1]: [Course on classical logic](https://www.cs.cmu.edu/~fp/courses/15317-f08/lectures/09-10-classical.pdf)

[^2]: [Lean tutorial on propositions](https://leanprover.github.io/theorem_proving_in_lean/propositions_and_proofs.html)

[^3]: These sentences are called [litotes](https://en.wikipedia.org/wiki/Litotes)

[^4]: "same" as in definitionally equivalent
