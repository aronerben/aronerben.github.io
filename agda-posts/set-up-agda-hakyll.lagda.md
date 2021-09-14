---
title: Literate Agda with Hakyll
description: Literate Agda has its own HTML transformer. In this blog post I show how to hook it up with Hakyll, including a working watch mode!
published: 2021-05-28
updated: 2021-02-20
---
## Gauss
### Gauss

```haskell
config :: Configuration
config = defaultConfiguration {deployCommand = "./deploy.sh"}

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` constField "blogClass" "underline"
    `mappend` constField "aboutClass" ""
    `mappend` defaultContext
```

And boooom, here is the result:
```agda
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

plus1 : (x : Nat) → x + 1 ≡ suc x
plus1 zero = refl
plus1 (suc x) rewrite plus1 x = refl
```
