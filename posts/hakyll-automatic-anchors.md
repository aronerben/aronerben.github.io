---
title: Automatic Heading Anchors in Hakyll
description: Many static site generators provide functionality to automatically insert anchors for headings. Hakyll does not (yet), so I illustrate a simple way to do it yourself.
published: 2021-10-12
---

If you only care about how I did it, you can [click here to skip](#solution) all the rambling!

# Introduction
I like being able to deep link pages directly with their headings. Wikipedia, for example, provides this functionality by giving their headings the `id` attribute. This allows constructing links with a `#` like `en.wikipedia.org/wiki/Third_place#Types`, which takes you directly to the corresponding heading.

My gripe with Wikipedia's current solution is that they don't provide a button near a heading to automatically fill in the URL bar with the deep link to that heading. You have to type the heading text (replacing spaces with underscores) by hand into the URL, which is super tedious if the heading is long. I want a clickable element per heading.

So, let's do better than Wikipedia!

Of course, as the title suggests, my solution is intended to work for Hakyll (or anything that can be represented as a [Pandoc AST](https://hackage.haskell.org/package/pandoc-types) for that matter!).

# Existing work
Cursory Google search did not lead me to an acceptable solution. I saw a solution where the anchor tags are inserted with JavaScript upon page load. I already have JS randomizing the greeting on the overview page of my blog, I don't want any more JS. Besides, this is a Hakyll blog, so statically generated. Anchor tags are not dynamic elements, they are very much static, so this is a job for Hakyll, not for JS.

# Solution
Hakyll provides an API for an integrated Pandoc Compiler. With [`pandocCompilerWithTransform`](https://hackage.haskell.org/package/hakyll-4.15.0.1/docs/Hakyll-Web-Pandoc.html#v:pandocCompilerWithTransform) you can use this Pandoc Compiler with a custom transformation step `(Pandoc -> Pandoc)`. I use [`walk`](https://hackage.haskell.org/package/pandoc-types-1.22/docs/Text-Pandoc-Walk.html#v:walk) to traverse the `Pandoc` AST and prepend my anchor element to each heading.

```haskell
customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    $ walk prependAnchor
  where
    prependAnchor :: Block -> Block
    prependAnchor (Header lvl attr@(id', _, _) txts) =
      Header
        lvl
        attr
        ( toList
            ( linkWith
                (mempty, ["anchor fas fa-xs fa-link"], mempty)
                ("#" <> id')
                mempty
                mempty
            )
            <> txts
        )
    prependAnchor x = x
```
`walk` has type `(Block -> Block) -> Pandoc -> Pandoc` here, as `Walkable Block Pandoc` is an instance of `Walkable`.

`prependAnchor` matches on a `Header`, takes the `id` that is generated for that heading by Pandoc and prepends `<a href="#id" ... />` to the heading. `["anchor fas fa-xs fa-link"]` is simply a list of CSS classes so I can use FontAwesome for a nice anchor icon. Of course you can use anything as an anchor. Hint: use the pseudo-element `::before` in combination with the CSS rule `content` to add anything without having to change the HTML structure.

If you want your anchor to be *appended* to the heading text, swap the arguments of the outer `<>`.

The other arguments can be empty lists and empty strings, as they represent additional data like link text, link attributes, labels and so on. Just `mempty` them all.

You can then use this custom Pandoc Compiler as you would for any Pandoc supported file type.

## <=== Look at that frog anchor
Cool, it works! If I ever change the position of the frog and forget to adjust this heading, I will look like an idiot.
