---
title: "Hello, World! in SKI - Part 0: Introduction"
description: The first part of the deep-dive into the SKI combinator calculus. The project goal is the implementation of a "Hello, World!" program using just three functions (with some glue) in Haskell.
published: 2023-05-09
---

# SKI
The SKI combinator calculus is an instance of a [combinatory logic system](https://en.wikipedia.org/wiki/Combinatory_logic), introduced in the 20th century. It can be used to model computations, as an alternative to, for example, the lambda calculus. The SKI system consists entirely of higher-order functions and function application, there is no lambda abstraction, so one never deals with arguments/lambda bindings in this system.

## The Combinators
The entire calculus consists of three primitives (functions) that can be combined to build other functions, hence "combinators". They are as follows:

```haskell
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x _ = x

i :: a -> a
i x = x
```

If you are familiar with Haskell, you can spot some equivalences: `i` is `id`, `k` is `const` and `s` is `ap`/`<*>` for functions ([the `Applicative` instance for `((->) r)`](https://hackage.haskell.org/package/base-4.18.0.0/docs/src/GHC.Base.html#line-1094)). 

The `i` combinator can be expressed in terms of the other two:

```haskell
i :: a -> a
i = s k k
```

# Motivation
These three/two functions form a Turing-complete language. This fact sparked my interest in the project a couple years ago. In computer science one often hears of these minimal systems that are Turing-complete, like NAND gates or the [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). What I was always curious about is how far this can be taken. What would it take to write a full program? I decided to write the simplest program using these three combinators. After checking the web quickly, I saw [someone else had already done that](https://old.reddit.com/r/haskell/comments/ypcu7/hello_world_in_ski_combinator_calculus/), a couple years prior. I decided to reimplement it and make some improvements.

# The Result
Without further elaboration, here is what I came up with after my improvements.

<div id="non-wrap">
```haskell
printHelloWorld :: IO ()
printHelloWorld =
  s
    (s i (k (s (k (>>)) (s (k putChar) (s (s i (k succ)) (k (toEnum 0)))))))
    (k (return ()))
    -- "Hello, World!"
    (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (k i)))) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (s (k s) k) (s i (k (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (s (k s) k) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (k i))))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s i (k (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))) (s (s (k s) k) (s (s (k s) k) (k i))))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s i (k (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (s (k s) k) (s i (k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (k i))))) (k i)))))))))))))))
```
</div>

I turned on line-wrapping for this snippet to illustrate its length. This program has about 3.5k symbols and if you copy this (and the `s`, `k` and `i` definitions from above) into a Haskell file, it should print "Hello, World!" into `stdout`.

Of course, since this program does I/O, we **have** to use the Haskell `IO` API to deal with it, so `putChar`, `(>>)` and `return` remain. Those cannot be encoded into SKI combinators[^1].

`succ` and `toEnum` are needed to give our encoding a semantic value that humans can understand when decoding, more on that later.

# How to Get There 
In the code snippet, everything below the comment is the string `"Hello, World!"` encoded into the calculus. As mentioned before, our combinators combine into other functions, so this encoded result is a function itself. What we apply the function to will, hopefully, become clear by the end of this blog series.

A string is a list of characters and a character is just an integer in `[0 .. 127]` representing that character. So we lay out our plan:

1. Learn how to encode an integer (just positive ones) into SKI
1. Learn how to encode a list into SKI
1. Turn "Hello, World!" into a list of integers
1. Apply the first two steps to the resulting list of integers
1. Encode as much as possible of the "glue" code into SKI

Before we start with the first step, I want to show the general encoding technique and some useful constructs using the simplest, non-trivial primitive there is: Booleans.

[Click here to read the next part of this series!](/posts/ski-hello-world-p1.html)


[^1]: Other monads, like `Maybe` and `Either` [can be encoded](https://hackage.haskell.org/package/gulcii-0.3/src/doc/encoding.md). Languages like [Unlambda](https://en.wikipedia.org/wiki/Unlambda) add I/O primitives to S and K separately.

<style>
#non-wrap code {
  white-space: break-spaces;
}
</style>