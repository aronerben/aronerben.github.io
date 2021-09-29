---
title: Literate Agda with Hakyll
description: Literate Agda has its own HTML generator. In this blog post I show how to hook it up with Hakyll, including a working watch mode!
published: 2021-05-28
updated: 2021-02-20
---

If you only care about how I did it, you can [click here to skip](#chosen-approach) all the mumbo jumbo!

## Introduction
I've been playing around with [Agda](https://agda.readthedocs.io/en/v2.6.2/getting-started/what-is-agda.html) recently to get into dependent types and theorem proving. After making some progress, I wanted to capture my advances and document some concepts. I figured the best way to do this is by making some blog posts! A neat way of documenting code with prose is [literate programming](https://en.wikipedia.org/wiki/Literate_programming). Agda supports various forms of literate programming. I picked Markdown due to familiarity with the syntax.

This blog is statically generated with [Hakyll](https://jaspervdj.be/hakyll/). Under the hood, [Pandoc](https://hackage.haskell.org/package/pandoc) is used to transform the Markdown files (and many more file types) into HTML. Literate Markdown Agda (`.lagda.md` files) mixes Agda code with Markdown and Pandoc is not able to convert both to HTML out of the box. So, I looked for a library that does this and the integration into Hakyll.

## Existing work
### Library `hakyll-agda`
[`hakyll-agda`](https://hackage.haskell.org/package/hakyll-agda) looked promising. It is actively maintained and seemed to provide what I was looking for. However, the lack of documentation made it hard to explore and play around with. The reason I did not opt for this solution is because external Agda libaries, like the standard library, are not explorable in the generated code sections by clicking on them. Maybe I've made a mistake in my folder structure that causes this.

### Library `agda-snippets-hakyll`
[`agda-snippets-hakyll`](https://hackage.haskell.org/package/agda-snippets-hakyll) seemed to have the full functionality that I needed and also had a minimal example on how to use it. However, this library is deprecated in favor of the next approach listed. It also has the upper bound `hakyll (<4.10)`, which won't work with my Hakyll 4.14.

### Agda programmatically
[Agda is implemented in Haskell](https://hackage.haskell.org/package/Agda) and thus provides an interface to programmatically run actions. There is an API to generate HTML for a given Literate Agda file. Both of the aforementioned libraries use this API internally. [PLFA](https://plfa.github.io/), which has been ported to Hakyll, also seems to go this path. This seems to be the cleanest way to do it, but I was not ready to commit too much time to figure and iron out all the intricacies involved. That would mean less time to learn Agda! So, I also did not choose this approach, though, at some point, something [like this](https://github.com/plfa/plfa.github.io/blob/a9f85c9ab16c3a1dfe25c69e5d2cc883791c4bc9/hs/Hakyll/Web/Agda.hs#L59) is the end goal. In hindsight, the Agda Haskell API does not seem too complex (at least when it comes to this task).

## Chosen approach
Continuing my search for a good enough, satisfactory solution, I stumbled upon [this blog post](https://jesper.sikanda.be/posts/literate-agda.html) by Jesper Cockx. In the last section of the post, he outlines his method to achieving what I set out to do and a problem causing Hakyll's watch-mode to not work with his approach. He includes his source code, which I liberally copy-pasted. To understand his approach, I recommend reading his code. It pretty much calls the `agda` executable in Haskell as a process. Now there was only the problem of getting the watch-mode to work.

To fix it, additionally, to the initial `processAgdaPosts` call, I added this route match:
```haskell
match "agda-posts/*.lagda.md" $
  compile $ do
    ident <- getUnderlying
    unsafeCompiler $ processAgdaPost $ takeFileName $ toFilePath ident
    makeItem (mempty :: String)
```
This creates a `Compiler a` in an "unsafe" way, meaning it performs an `IO` action ignoring the paradigm of mapping one input file to one output file, as described by Jesper Cockx.

what this does

cant get rid of initial call, unless can make dependencies

And boooom, here is the result:
```agda
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

plus1 : (x : Nat) → x + 1 ≡ suc x
plus1 zero = refl
plus1 (suc x) rewrite plus1 x = refl
```

You can click on pretty much any non-keyword symbol and you will be taken to the definition. Try clicking on `Nat`! That will take you to the definition of the standard library.

## Difficulties
not relevant

preprocess loop

github issue
