---
title: The Fundamental Theorem of Symmetric Polynomials
description: We introduce the fundamental theorem of symmetric polynomials and prove it.
published: 2025-09-13
mathjax: true
---

# Introduction
The fundamental theorem of symmetric polynomials is a classical result that finds applications in field theory and Galois theory among others. It states that every polynomial invariant under the permutation of its variables (called the symmetric polynomials) can be uniquely expressed as a polynomial in the elementary symmetric polynomials. We will look at what this means in more detail in a moment.

After giving an introduction into the matter, posing the theorem more formally and covering some required concepts, we will prove the theorem.

The proof is based on an Algebra course at the University of Zurich, given by Prof. A. Kresch. The proof, as illustrated here, goes into quite a bit of detail. There are shorter proofs of this theorem out there.

As previously mentioned, a symmetric polynomial is a polynomial that remains unchanged under the permutation of its variables. For example, in the polynomial $X_1^2 + X_2^2 + X_3^2$, we can permute the variables $X_1, X_2, X_3$ in any way and the polynomial remains the same. Another example is the polynomial $X_1^3X_2^3X_3^3$.

The elementary symmetric polynomials are a special kind of symmetric polynomials. Over $\Z$, for $n$ variables, they are defined as
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
Clearly, all these polynomials are symmetric by construction. For the $k$-th elementary symmetric polynomial, we choose $k$ variables out of $n$ and multiply them together. We then sum over all possible choices of $k$ variables. This implies that every product with $k$ variables appears, hence we have symmetry.

We will write $e_k$ for the $k$-th elementary symmetric polynomials for brevity (when the variables are clear). We define $e_0 := 1$ and $e_k := 0$ for $k > n$. Furthermore, the concept of (elementary) symmetric polynomials exists over an arbitrary commutative ring $R$, not just $\Z$, so we will work with this generalization. The ring of symmetric polynomials over $R$ is denoted by $R[X_1, \ldots, X_n]^{S_n}$.

# Example
To illustrate, we apply the theorem on $X_1^3 + X_2^3 + X_3^3 \in \Z[X_1, X_2, X_3]$ and get
\begin{align*}
X_1^3 + X_2^3 + X_3^3 = e_1^3 - 3e_1e_2 + 3e_3.
\end{align*}
As the proof is constructive, we can extract an algorithm that gives us a way to find such expressions. At the end, we will cover this algorithm.

# Motivation
We want to briefly motivate the theorem with an application. Let $p \in R[X]$ be the monic univariate polynomial
\begin{align*}
p := X^n + a_{n-1}X^{n-1} + \ldots + a_1X + a_0.
\end{align*}
Let $\alpha_1, \ldots, \alpha_n$ be the roots of $p$. Then by Vieta's formulas, we know that $a_{n-1} = -e_1(\alpha_1, \ldots, \alpha_n)$, $a_{n-2} = e_2(\alpha_1, \ldots, \alpha_n)$, and so on. Hence, we get
\begin{align*}
p = X^n - e_1(\alpha_1, \ldots, \alpha_n)X^{n-1} + e_2(\alpha_1, \ldots, \alpha_n)X^{n-2} - \ldots + (-1)^ne_n(\alpha_1, \ldots, \alpha_n).
\end{align*}

Now, for some $f \in R[U]$, we are interested in finding a polynomial $q \in R[X]$ such that the roots of $q$ are $f(\alpha_1), \ldots, f(\alpha_n)$. For example, if we set $f := U^2$, we want to construct a polynomial $q$ that has the same roots as $p$ but squared. This means we then have
\begin{align*}
q = X^n - e_1(\alpha_1^2, \ldots, \alpha_n^2)X^{n-1} + e_2(\alpha_1^2, \ldots, \alpha_n^2)X^{n-2} - \ldots + (-1)^ne_n(\alpha_1^2, \ldots, \alpha_n^2).
\end{align*}
Clearly, the coefficients are elementary symmetric polynomials in terms of $\alpha_i^2$. In terms of $\alpha_i$, they are still symmetric, but not elementary anymore. This is where the theorem comes into play. It tells us that we can express these symmetric polynomials $e_k(\alpha_1^2, \ldots, \alpha_n^2)$ as polynomials in the elementary symmetric polynomials $e_k(\alpha_1, \ldots, \alpha_n)$. So, without knowing the roots, we can manipulate the coefficients to construct $q$.

For example, let $t := X^2 + aX + b \in \Z[X]$ with roots $\alpha_1, \alpha_2 \in \Z$. Again, by Vieta's formulas, we can write
\begin{align*}
t = X^2 - e_1(\alpha_1, \alpha_2)X + e_2(\alpha_1, \alpha_2) = X^2 - (\alpha_1 + \alpha_2)X + \alpha_1\alpha_2.
\end{align*}
We want to square the roots, so we write $e_i(\alpha_1^2, \alpha_2^2)$ as a polynomial in $e_i(\alpha_1, \alpha_2)$ for $i \in \{1, 2\}$. We get
\begin{align*}
e_1(\alpha_1^2, \alpha_2^2) & = \alpha_1^2 + \alpha_2^2 = \alpha_1^2 + 2\alpha_1\alpha_2 + \alpha_2^2 - 2\alpha_1\alpha_2 = e_1(\alpha_1, \alpha_2)^2 - 2e_2(\alpha_1, \alpha_2) = a^2 - 2b
\\ e_2(\alpha_1^2, \alpha_2^2) & = e_2(\alpha_1, \alpha_2)^2 = b^2
\end{align*}
Then, our polynomial $s \in \Z[X]$ with squared roots can be written as
\begin{align*}
s = X^2 + (a^2 - 2b)X + b^2.
\end{align*}
We were thus able to construct $s$ using just the coefficients of $t$ without computing the roots explicitly. The calculation for $e_1(\alpha_1^2, \alpha_2^2)$ already gives us a hint on how the algorithm might work.

In fact, this game of changing the roots of a given polynomial is used in the proof of the transcendence of $\pi$[^1]. In that case, given a polynomial $p$ with some roots, we find another polynomial where the roots are sums of some of the roots of $p$. This is a slightly different idea than applying a (polynomial) function to the roots, but the idea is analogous and the fundamental theorem of symmetric polynomials is used in the same way.

# Theorem
We now pose the theorem formally.

$\textbf{Fundamental Theorem of Symmetric Polynomials}$:
Let $R$ be a commutative ring. Then the $R$-algebra homomorphism 
\begin{align*}
\iota : R[E_1, \ldots, E_n] \to R[X_1, \ldots, X_n]^{S_n}
\end{align*}
defined by
\begin{align*}
\iota(E_k) := e_k
\end{align*}
is an isomorphism.

Intuitively, this is exactly what we described. Surjectivity ensures that every symmetric polynomial can be expressed as a polynomial in terms of $e_k$ and injectivity tells us that this can be done so uniquely.

We view the domain and codomain as $R$-algebras given by the inclusion maps
\begin{align*}
R \xhookrightarrow{} R[E_1, \ldots, E_n], \quad R \xhookrightarrow{} R[X_1, \ldots, X_n]
\end{align*}
which then tells us that $\iota$ is a ring homomorphism that also respects scalar multiplication by elements of $R$. Concretely, for all $r \in R$ and $a \in R[E_1, \ldots, E_n]$, we have
\begin{align*}
\iota(ra) = r\iota(a)
\end{align*}
and in particular, when applied to the identity of $R[E_1, \ldots, E_n]$, we get
\begin{align*}
\iota(r) = r.
\end{align*}
So, essentially, this ensures that all constant polynomials are mapped to themselves.

To prove the theorem we need to establish a strategy using (weighted) homogeneous (symmetric) polynomials.

# Additionally Required Concepts 
We will use the idea of homogeneous polynomials, denoted by $R[X_1, \ldots, X_n]_d$, where $d \in \N$ is the degree of all monomials in such a polynomial. For example $X_1^3 + X_2^2X_1 + X_1X_2X_3 \in R[X_1, X_2, X_3]_3$ as every monomial has degree $3$. The homogeneous polynomials form an $R$-submodule of $R[X_1, \ldots, X_n]$ and we have the decomposition
$$
\begin{aligned}
R[X_1, \ldots, X_n] = \bigoplus_{d \in \N} R[X_1, \ldots, X_n]_d
\end{aligned}\htmlId{decomp_hom}{}\tag{1}
$$
which holds as every arbitrary polynomials in $R[X_1, \ldots, X_n]$ is simply the sum of its monomials, and such a monomial is trivially homogeneous of some degree. Note here that these polynomials are not necessarily symmetric.

Furthermore, it is not hard to convince ourselves that an element of $R[X_1, \ldots, X_n]$ is symmetric if and only if every homogeneous component is symmetric, or in other words when all monomials of the element grouped by their degrees are symmetric among their group. We introduce the notation $R[X_1, \ldots, X_n]_d^{S_n}$ to mean the homogeneous symmetric polynomials of degree $d \in \N$. Given by $\href{#decomp_hom}{\text{(1)}}$, we have the decomposition
\begin{align*}
R[X_1, \ldots, X_n]^{S_n} = \bigoplus_{d \in \N} R[X_1, \ldots, X_n]_d^{S_n}.
\end{align*}

We get the weighted variant of homogeneous polynomials by assigning the weight $j \in \N$ to the variable $E_j$. We denote the weighted homogeneous polynomials of "weighted" degree $d$ by $R[E_1, \ldots, E_n]_d$. This is again an $R$-submodule of $R[E_1, \ldots, E_n]$ spanned by the monomials $E_1^{a_1} \cdots E_n^{a_n}$ that fulfill
\begin{aligned}
\sum_{j}ja_j = d.
\end{aligned}\htmlId{weighted_cond}{}\tag{2}
Intuitively, every weighted homogeneous polynomial is a sum of monomials where the "further" you go along the variables, the higher the weight, so they contribute more to the total weighted degree $d$ the monomial has to have. For example, in $R[E_1, E_2, E_3]$, the elements $E_1^3$, $E_1E_2$ and $E_3$ all have the same weighted degree $3$, so they would all be elements of $R[E_1, E_2, E_3]_3$. We have a very similar decomposition as before, given by
\begin{align*}
R[E_1, \ldots, E_n] = \bigoplus_{d \in \N} R[E_1, \ldots, E_n]_d
\end{align*}
which holds for the same reason as before. 

These weighted homogeneous polynomials might seem a bit arbitrary at first. However, it makes sense when we look at what $\iota$ does to such a polynomial. A variable $E_k$ is mapped to $e_k$ and, going back to our definition of the elementary symmetric polynomials, we can see that $e_k$ has degree $k$. This then means that $\iota$ ensures that a weighted homogeneous polynomial of weighted degree $d$ is mapped to a homogeneous polynomial of degree $d$. For example, in $\iota(E_1E_2) = e_1e_2 = (X_1 + X_2)(X_1X_2) \in R[X_1, X_2]$, we see that $E_1E_2$ has weighted degree $3$, according to our definition, and the output multiplies monomials of degrees $1$ and $2$, which sums to $3$ as well.

Before we tackle the proof, we introduce two orders. Let $\lambda, \mu$ be $n$-tuples. We will need the lexicographic order $\lambda \leq_{\text{Lex}} \mu$, defined in the [usual way](https://en.wikipedia.org/wiki/Lexicographic_order#Cartesian_products). Furthermore, we require the concept of a dominance order, defined as
\begin{align*}
    \lambda \trianglelefteq \mu : \Leftrightarrow \forall k, \lambda_1 + \ldots + \lambda_k \leq \mu_1 + \ldots + \mu_k.
\end{align*}
We note that $\trianglelefteq$ is a partial order ($(1,1) \not\trianglelefteq (3,0)$ and $(3,0) \not\trianglelefteq (1,1)$), while $\leq_{\text{Lex}}$ is a total order.

We are now ready to tackle the proof.

# Proof
We have seen that $R[X_1, \ldots, X_n]^{S_n}$ and $R[E_1, \ldots, E_n]$ can be decomposed into a direct sum of $R[X_1, \ldots, X_n]_d^{S_n}$ and $R[E_1, \ldots, E_n]_d$ over all degrees. As $\iota$ preserves the degree, as we established, it suffices to prove that $\iota$ is an isomorphism for every degree $d$.

The monomials in the elements of $R[X_1, \ldots, X_n]_d^{S_n}$ and $R[E_1, \ldots, E_n]_d$ can only be picked from a finite amount of elements (ignoring the coefficients). Then, clearly, both spaces are finitely generated as $R$-modules. Furthermore, they are both free, as a monomial term cannot be expressed as a linear combination of other monomial terms with differing degrees, in either space.

This reduction now allows us to employ linear algebra. So, our strategy for the proof is to find bases for $R[X_1, \ldots, X_n]_d^{S_n}$ and $R[E_1, \ldots, E_n]_d$ such that $\iota$ can be expressed as a matrix of the form
\begin{align*}
\begin{pmatrix}
    1 &        & * \\
        & \ddots &   \\
    0 &        & 1
\end{pmatrix}.
\end{align*}
The determinant is $1$, so we have an invertible matrix, which tells us $\iota$ is bijective. $\iota$ is also an $R$-linear module homomorphism, so it is then an isomorphism.

Let $d \in \N$. For $R[E_1, \ldots, E_n]$, we will use the aforementioned monomials $E_1^{a_1} \cdots E_n^{a_n}$ as our basis. We remember that such a monomial fulfills condition $\href{#weighted_cond}{\text{(2)}}$. We can transform this condition a bit by defining
\begin{align*}
(\lambda_1, \ldots, \lambda_n) := (a_1 + \ldots + a_n, a_2 + \ldots + a_n, \ldots, a_n)
\end{align*}
which then tells us that
\begin{aligned}
\sum_{j} \lambda_j = d.
\end{aligned}\htmlId{lam_cond}{}\tag{3}
We note that this definition imposes the ordering $\lambda_1 \ge \lambda_2 \ge \ldots \ge \lambda_n$. For a given $n$ and $d$, we call the set of all such $(\lambda_1, \ldots, \lambda_n)$ fulfilling $\href{#lam_cond}{\text{(3)}}$ $I_{n, d}$. We notice that $I_{n, d}$ is precisely the ways to partition the integer $d$ with at most $n$ parts, up to reordering. For example
\begin{align*}
I_{3, 6} = \{(6), (5, 1), (4, 2), (4, 1, 1), (3, 3), (3, 2, 1), (2, 2, 2)\}.
\end{align*}
Technically, all elements should be $3$-tuples, but we omit all $0$ terms in our partitions for brevity.

We now index our monomial basis of $R[E_1, \ldots, E_n]_d$ by $I_{n, d}$. For a given partition $(\lambda_1, \ldots, \lambda_n) \in I_{n, d}$, we can recover the corresponding monomial as we have
\begin{aligned}
a_i = \lambda_i - \lambda_{i+1} \quad \text{for } i = 1, \ldots, n-1, \quad \text{ and } a_n = \lambda_n.
\end{aligned}\htmlId{recover_monomial}{}\tag{4}
We note that the elements are indeed a basis as they span trivially and linear independence is clear.

For our pervious example $I_{3, 6}$, we have (in the order of the indices) the monomial basis
\begin{align*}
    E_1^6, \quad E_1^4E_2, \quad E_1^2E_2^2, \quad E_1^3E_3, \quad E_2^3, \quad E_1E_2E_3, \quad E_3^2.
\end{align*}

We now need a basis for $R[X_1, \ldots, X_n]_d^{S_n}$. We can start with a monomial, but we need to ensure symmetry. We do this by summing over all permutations of the exponents in the monomial (including $0$ exponents). So, for the partition $\lambda := (\lambda_1, \ldots, \lambda_n) \in I_{n, d}$, we define
\begin{align*}
m_\lambda := \sum_{\substack{q_1, \ldots, q_n \in \N \\ \text{s.t. } \exists \sigma \in S_n \text{ with }q_{\sigma(j)} = \lambda_j \forall j}} X_1^{q_1} \cdots X_n^{q_n},
\end{align*}
which constitutes our basis, also indexed by $I_{n, d}$. The basis elements here also span trivially and are clearly linearly independent.

For our example $I_{3, 6}$, we get (again in the order of the indices)
\begin{align*}
    X_1^6 + X_2^6 + X_3^6, 
    \quad X_1^5X_2 + X_1^5X_3 + X_2^5X_1 + X_2^5X_3 + X_3^5X_1 + X_3^5X_2, 
    \quad X_1^4X_2^2 + \ldots,
    \quad \ldots,
    \quad X_1^2X_2^2X_3^2.
\end{align*}

We order $I_{n, d}$ lexicographically, which gives us a bijection to $\{1, \ldots m\}$ where $m$, as we know, is the number of ways to partition $d$ with at most $n$ parts. For $I_{3,6}$, the order would be
\begin{align*}
I_{3, 6} = \{(2, 2, 2), (3, 2, 1), (3, 3), (4, 1, 1), (4, 2), (5, 1), (6)\}.
\end{align*}
Let $\lambda \in I_{n,d}$ arbitrary. We can now recover the monomial $E_1^{a_1} \cdots E_n^{a_n}$ by $\href{#recover_monomial}{\text{(4)}}$, which $\iota$ maps to $e_1^{a_1} \cdots e_n^{a_n}$. Clearly, $e_1^{a_1} \cdots e_n^{a_n}$ is a symmetric polynomial and can therefore be expressed in terms of our monomial basis $m_{I_{n,d}}$. We now claim that it suffices to show, for
\begin{aligned}
    e_1^{a_1} \cdots e_n^{a_n} = \sum_{\mu \in I_{n,d}} c_{\lambda\mu} m_{\mu}
\end{aligned}\htmlId{lin_comb}{}\tag{5}
that we have
\begin{align*}
    c_{\lambda\lambda} & = 1, \\
    \mu \not\trianglelefteq \lambda \Rightarrow c_{\lambda\mu} & = 0
\end{align*}
where $c_{\lambda\mu} \in R$ are the coefficients in the linear combination.

It might not be obvious why this suffices, so we elaborate. We assume the above is true. Then, as $R[E_1, \ldots E_n]_d$ and $R[X_1, \ldots X_n]^{S_n}_d$ are finitely generated and free (as previously established), the two spaces are isomorphic to $R^m$ for some $m \in \N$. Ordering our indexed bases via the lexicographic order of the index set $I_{n,d}$, we can express $\iota$ as a matrix $N \in M_n(R)$. We also note that the standard basis of $R^m$ can be indexed by the lexicographically ordered index set analogously. Let $v_{\lambda}$ be the $\lambda$-th basis element of $R[E_1, \ldots E_n]_d$. Then, $\iota(v_{\lambda})$ is as in the left-hand side of $\href{#lin_comb}{\text{(5)}}$.

The statement now says that if the indices match, the coefficient is $1$, so $\iota(v_{\lambda})$ gets exactly one contribution from $m_{\lambda}$ and no contributions from $m_{\mu}$ where $\mu \not\trianglelefteq \lambda$, so elements "after" $\lambda$ in the dominance order. This includes all elements "after" $\lambda$ in the lexicographic order, as $\mu \not\leq_{\text{Lex}} \lambda \Rightarrow \mu \not\trianglelefteq \lambda$. We now use the $\lambda$-th standard basis element (which has a $1$ at the $\lambda$-th position and $0$ elsewhere) to express this idea via matrices:
\begin{align*}
    N \begin{pmatrix} 0 \\ \vdots \\ 0 \\ 1 \\ 0 \\ \vdots \\ 0 \end{pmatrix} = \begin{pmatrix} * \\ \vdots \\ * \\ 1 \\ 0 \\ \vdots \\ 0 \end{pmatrix}.
\end{align*}
Essentially, we are projecting out the $\lambda$-th column of $N$[^2], which tells us that $N$ has the desired form
\begin{align*}
N = \begin{pmatrix}
    1 &        & * \\
        & \ddots &   \\
    0 &        & 1
\end{pmatrix}.
\end{align*}

There is an easier version of our statement that we can prove, so we can forget about basis $m_{I_{n,d}}$. We claim that it suffices to show, for
\begin{aligned}
    e_1^{a_1} \cdots e_n^{a_n} = \sum_{q = (q_1, \ldots, q_n) \in \N^n} b_{\lambda q} X_1^{q_1} \cdots X_n^{q_n},
\end{aligned}\htmlId{lin_comb_2}{}\tag{6}
that we have
\begin{aligned}
    b_{\lambda\lambda} & = 1, \\
    q \not\trianglelefteq \lambda \Rightarrow b_{\lambda q} & = 0.
\end{aligned}\htmlId{simpler_cond}{}\tag{7}
As each such monomial $X_1^{q_1} \cdots X_n^{q_n}$ must appear with all permutations of the exponents (we have a symmetric polynomial), the basis elements $m_{I_{n,d}}$ are "encoded" in the sum in this statement.

We prove by induction on $\lambda_1$. We remember that $\lambda_1$ is the sum of all $a_i$, the exponents.
The base case $\lambda_1 = 0$ implies $\lambda = (0, \ldots, 0)$ and $e_1^{a_1} \cdots e_n^{a_n} = 1$, which tells us that $q_i = 0$ for all $i$, so $q = \lambda$ and then $b_{\lambda\lambda} = 1$. The second condition is vacuously true.

For the induction step, let $\lambda_1 > 0$. Now, we subtract $1$ from the last non-zero $a_i$, which we call $a_k$. We call $k$ the $\textbf{length}$ of $\lambda$. By the definition of $\lambda$, the subtraction gives us a new partition $\lambda'$ where every (non-zero) part is one less than the corresponding part in $\lambda$. In particular $\lambda_1' = \lambda_1 - 1$, so our statement holds for $\lambda'$. We now do some algebraic manipulations to $\href{#lin_comb_2}{\text{(6)}}$.
\begin{align*}
    e^{a_1}_1 \cdots e^{a_n}_n
     & = e^{a_1}_1 \cdots e^{a_k}_k \\
     & = (e^{a_1}_1 \cdots e^{a_k-1}_k)e_k \\
     & = \left(\sum_{q' = (q'_1, \ldots, q'_n) \in \N^n} b_{\lambda' q'} X^{q'_1}_1 \cdots X^{q'_n}_n\right) \left(\sum_{\substack{I \subseteq \{1,\ldots,n\} \\ |I| = k}} \prod_{i \in I} X_i\right) \\
     & = \sum_{\left(q' \in \N^n, \substack{I \subseteq \{1,\ldots,n\} \\ |I| = k}\right)} b_{\lambda' q'} X^{q'_1}_1\cdots X^{q'_n}_n  \prod_{i \in I} X_i \\
     & = \sum_{\left(q' \in \N^n, \substack{I \subseteq \{1,\ldots,n\} \\ |I| = k}\right)} b_{\lambda' q'} X^{q_1}_1\cdots X^{q_n}_n
\end{align*}
where
\begin{aligned}
    q_i = \begin{cases}
                q'_i     & \text{if } i \not\in I \\
                q'_i + 1 & \text{if } i \in I
          \end{cases}.
\end{aligned}\htmlId{increment_cond}{}\tag{8}
We elaborate on these manipulations a bit. In the first step, we only keep all $e_i$ with non-zero exponents. The second step pulls out an $e_k$ such that the last exponent is decremented by one, so in the third step, we can again use $\href{#lin_comb_2}{\text{(6)}}$ to rewrite the left factor using $\lambda'$. For the right factor, we apply the definition of the $k$-th elementary symmetric polynomial. In the fourth step, we combine the product of the two sums into one sum. The final step pulls in our product of $X_i$ into the monomial $X^{q'_1}_1\cdots X^{q'_n}_n$, incrementing the exponents by $1$ if $i \in I$ and leaving it as they are otherwise, giving us the definition of $q_i$ above. We note that $k$ exponents are incremented.

We want to sum over arbitrary $q$ and exclude the tuples of $q_i$ that don't fulfill $\href{#increment_cond}{\text{(8)}}$. We rewrite a bit to get
\begin{align*}
    e^{a_1}_1 \cdots e^{a_n}_n
     = \sum_{\left(q' \in \N^n, \substack{I \subseteq \{1,\ldots,n\} \\ |I| = k}\right)} b_{\lambda' q'} X^{q_1}_1\cdots X^{q_n}_n 
     = \sum_{q \in \N^n} \left(\sum_{\substack{I \subseteq \{i \mid q_i > 0\} \\ |I| = k}} b_{\lambda' q'}\right) X^{q_1}_1\cdots X^{q_n}_n.
\end{align*}
where
\begin{align*}
    q_i' = \begin{cases}
                q_i     & \text{if } i \not\in I \\
                q_i - 1 & \text{if } i \in I
          \end{cases}.
\end{align*}
This is an equivalent formulation as the inner sum is empty if $q$ does not have at least $k$ parts greater than $0$, this ensures that the condition for our $q_i$ as in $\href{#increment_cond}{\text{(8)}}$ is maintained. Given the premise $\href{#lin_comb_2}{\text{(6)}}$ in our statement, we see that
\begin{aligned}
      b_{\lambda q} = \sum_{\substack{I \subseteq \{i \mid q_i > 0\} \\ |I| = k}} b_{\lambda' q'}.
\end{aligned}\htmlId{b_rel}{}\tag{9}

We now show the two cases in $\href{#simpler_cond}{\text{(7)}}$.

If $q = \lambda$, then $I = \{1, \ldots, k\}$ is the only set we sum over in $\href{#b_rel}{\text{(9)}}$, as $k$ is the length of $\lambda$ and hence all $q_1, \ldots q_k$ are greater than $0$. Thus, we get
\begin{align*}
    b_{\lambda \lambda} = b_{\lambda' \lambda'}.
\end{align*}
Now, as $b_{\lambda' \lambda'} = 1$ by the induction hypothesis, we have $b_{\lambda \lambda} = 1$.

If $q \not\trianglelefteq \lambda$, then there exists some $j$ with $1 \leq j \leq k$, such
\begin{aligned}
      q_1 + \ldots + q_j > \lambda_1 + \ldots + \lambda_j.
\end{aligned}\htmlId{key_ineq}{}\tag{10}
We want to now find a relation between our $q_i'$ and $q_i$ as well as $\lambda_i'$ and $\lambda_i$. In total, we have $k$ increments from $q_i'$ to $q_i$, and we want to find how many of these increments are in the first $j$ indices. We find the relevant set of indices by intersecting $I$ with $\{1, \ldots, j\}$. Thus, we have
\begin{aligned}
      q_1' + \ldots + q_j' = q_1 + \ldots + q_j - |I \cap \{1, \ldots, j\}|.
\end{aligned}\htmlId{q_sum}{}\tag{11}
For $\lambda_i'$, it's a bit easier. By the definition of $\lambda'$, we can simply subtract $1$ from each of the first $j$ parts to get
\begin{aligned}
        \lambda_1' + \ldots + \lambda_j' = \lambda_1 + \ldots + \lambda_j - j.
\end{aligned}\htmlId{lam_sum}{}\tag{12}

In $\href{#q_sum}{\text{(11)}}$, we subtract at most $j$, in $\href{#lam_sum}{\text{(12)}}$ we subtract exactly $j$, so combining these with $\href{#key_ineq}{\text{(10)}}$, we get
\begin{align*}
    q_1' + \ldots + q_j' > \lambda_1' + \ldots + \lambda_j'.
\end{align*}
This tells us that $q' \not\trianglelefteq \lambda'$, so by the induction hypothesis, we have $b_{\lambda' q'} = 0$ for all $I$ we sum over in $\href{#b_rel}{\text{(9)}}$, which gives us $b_{\lambda q} = 0$. This concludes the proof.

# Algorithm
According to [this Math Stack Exchange post](https://math.stackexchange.com/a/14061), that comes with a reference, the algorithm presented here is due to Gauss. The essence of the algorithm is already encoded in the proof above.

Let $f \in R[X_1, \ldots, X_n]^{S_n}$ be a symmetric polynomial. We now find the $\textbf{lead monomial}$ by viewing the exponents of every monomial $c X_1^{q_1} \cdots X_n^{q_n}$, with $c \in R$, as the $n$-tuple $(q_1, \ldots, q_n)$ and picking the monomial with the largest "exponent tuple" with respect to lexicographic ordering. We want to now subtract an expression in terms of $e_i$ such that the lead monomial is cancelled. Let $cX_1^{q_1} \cdots X_n^{q_n}$ be the lead monomial. We compute
\begin{align*}
    f_1 := f - ce_1^{q_1 - q_2} e_2^{q_2 - q_3} \cdots e_{n-1}^{q_{n-1} - q_n} e_n^{q_n}.
\end{align*}
We get to this expression by applying the idea behind $\href{#recover_monomial}{\text{(4)}}$ and seeing that, due to the proof, $f$ gets exactly $c$ contributions from the subtrahend, so the lead monomial is cancelled. The new lead monomial in $f_1$ will be strictly smaller in the lexicographic order. We can now recurse until $f_n$ is $0$. We can then back substitute to get an expression of $f$ in terms of $e_i$. As the proof demonstrates, this algorithm must terminate as we repeatedly decrease the lead monomial in the lexicographic order.

As an example, let $f := X_1^3 + X_2^3 + X_3^3 \in \Z[X_1, X_2, X_3]^{S_3}$. The lead monomial is $X_1^3$ with partition $(3, 0, 0)$. We compute
\begin{align*}
    f_1 := f - e_1^3e_2^0e_3^0 = f - e_1^3
     & = X_1^3 + X_2^3 + X_3^3 - (X_1 + X_2 + X_3)^3                                                                        \\
     & = X_1^3 + X_2^3 + X_3^3                                                                                              \\
     & \quad - (X_1^3 + X_2^3 + X_3^3 + 3X_1^2X_2 + 3X_1^2X_3 + 3X_2^2X_1 + 3X_2^2X_3 + 3X_3^2X_1 + 3X_3^2X_2 + 6X_1X_2X_3) \\
     & = -3X_1^2X_2 - 3X_1^2X_3 - 3X_2^2X_1 - 3X_2^2X_3 - 3X_3^2X_1 - 3X_3^2X_2 - 6X_1X_2X_3.
\end{align*}
The lead monomial of $f_1$ is $-3X_1^2X_2$ with the partition $(2, 1, 0)$. The corresponding product of elementary symmetric polynomials is then $e_1^{2-1}e_2^{1-0}e_3^{0} = e_1e_2$. We subtract this, multiplied by the coefficient $-3$ from $f_1$ to get
\begin{align*}
    f_2 := f_1 + 3e_1e_2
     & = -3X_1^2X_2 - 3X_1^2X_3 - 3X_2^2X_1 - 3X_2^2X_3 - 3X_3^2X_1 - 3X_3^2X_2 - 6X_1X_2X_3 \\
     & \quad + 3(X_1 + X_2 + X_3)(X_1X_2 + X_1X_3 + X_2X_3)                                  \\
     & = 3X_1X_2X_3.
\end{align*}

We only have one monomial and we see the corresponding product of elementary symmetric polynomials is $e_1^{1-1}e_2^{1-1}e_3^{1} = e_3$ with coefficient $3$. Thus, we have
\begin{align*}
    f_3 := f_2 - 3e_3 = 0.
\end{align*}
Then, we resubstitute and get
\begin{align*}
    f - e_1^3 + 3e_1e_2 - 3e_3 & = 0                       \\
    \Rightarrow f              & = e_1^3 - 3e_1e_2 + 3e_3.
\end{align*}
This completes the example.



[^1]: It's used in the proof of the [Lindemann-Weierstrass theorem](https://en.wikipedia.org/wiki/Lindemann%E2%80%93Weierstrass_theorem), from which the transcendence of $\pi$ follows as a corollary.

[^2]: By convention, we usually index matrices with positive integers, so "the $\lambda$-th column" is not entirely valid. However, as we have a bijection between $I_{n,d}$ and $\{1, \ldots, m\}$, this is justified.