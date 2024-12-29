---
title: The Fundamental Theorem of Symmetric Polynomials
description: We introduce the fundamental theorem of symmetric polynomials and prove it.
published: 2024-12-19
mathjax: true
---

# Introduction
The fundemental theorem of symmetric polynomials is a classical result that finds applications in field theory and Galois theory among others. It states that every polynomial invariant under the permutation of its variables (called the symmetric polynomials) can be expressed as a polynomial in the elementary symmetric polynomials.

We want to introduce some concepts before we formally pose the theorem and prove it.

The proof is based on an Algebra course at the University of Zurich, given by Prof. A. Kresch.

## (Elementary) Symmetric Polynomials
As previously mentioned, a symmetric polynomial is a polynomial that remains unchanged under the permutation of its variables. For example, in the polynomial $X_1^2 + X_2^2 + X_3^2$, we can permute the variables $X_1, X_2, X_3$ in any way and the polynomial remains the same. Another example is the polynomial $X_1^3X_2^3X_3^3$.

The elementary symmetric polynomials are a special kind of symmetric polynomials. For $n$ variables over $\Z$, they are defined as
\begin{align*}
    e_1(X_1, X_2, \ldots, X_n) & := X_1 + X_2 + \ldots + X_n
    \\ e_2(X_1, X_2, \ldots, X_n) & := X_1X_2 + X_1X_3 + \ldots + X_{n-1}X_n
    \\ \vdots
    \\ e_n(X_1, X_2, \ldots, X_n) & := X_1X_2 \ldots X_n.
\end{align*}
The general form of the $k$-th elementary symmetric polynomial is thus
\begin{align*}
    e_k(X_1, X_2, \ldots, X_n) := \sum_{\substack{I \subset \{1, \ldots, n\} \\ |I| = k}} \prod_{i \in I} X_i.
\end{align*}
Clearly, all these polynomials are symmetric. This is so by construction. Essentially, for the $k$-th elementary symmetric polynomial, we choose $k$ variables out of $n$ and multiply them together. We then sum over all possible choices of $k$ variables. This implies that every product with $k$ variables appears, hence we have symmetry.

We will write $e_k$ for the $k$-th elementary symmetric polynomials for brevity (when the variables are clear). We define $e_0 := 1$ and $e_k := 0$ for $k > n$. Furthermore, the concept of (elementary) symmetric polynomials exists for an arbitrary commutative ring $R$, so we will work with this generalization.

# Example
We apply the theorem on the following element in $\Z[X_1, X_2, X_3]$ and get
\begin{align*}
    X_1^3 + X_2^3 + X_3^3 = e_1^3 - 3e_1e_2 + 3e_3.
\end{align*}
As the proof is constructive, we can extract an algorithm that gives us a way to find such expressions. At the end, we will cover this algorithm.

# Motivation
We want to briefly motivate the theorem with an application. Let $p \in R[T]$ be the monic univariate polynomial
\begin{align*}
    p := T^n + a_{n-1}T^{n-1} + \ldots + a_1T + a_0.
\end{align*}
Let $\alpha_1, \ldots, \alpha_n$ be the roots of $p$. Then by Vieta's formulas, we know that $a_{n-1} = -e_1(\alpha_1, \ldots, \alpha_n)$, $a_{n-2} = e_2(\alpha_1, \ldots, \alpha_n)$, and so on. Hence, we get
\begin{align*}
    p = T^n - e_1(\alpha_1, \ldots, \alpha_n)T^{n-1} + e_2(\alpha_1, \ldots, \alpha_n)T^{n-2} - \ldots + (-1)^ne_n(\alpha_1, \ldots, \alpha_n).
\end{align*}

Now, for some $f \in R[U]$, we are interested in finding a polynomial $q \in R[T]$ such that the roots of $q$ are $f(\alpha_1), \ldots, f(\alpha_n)$. For example, if we set $f := U^2$, we want to construct a polynomial $q$ that has the same roots of $p$ but squared. This means we then have
\begin{align*}
    q = T^n - e_1(\alpha_1^2, \ldots, \alpha_n^2)T^{n-1} + e_2(\alpha_1^2, \ldots, \alpha_n^2)T^{n-2} - \ldots + (-1)^ne_n(\alpha_1^2, \ldots, \alpha_n^2).
\end{align*}
Clearly, the coefficients are elementary symmetric polynomials in terms of $\alpha_i^2$. They are also symmetric in terms of $\alpha_i$, however, they aren't elementary anymore.


# Theorem

# Concepts

# Proof