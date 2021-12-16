---
title: Growing Box Hover in CSS
description: Expanding a card-like box on hover is a simple effect that can look nice if used appropriately. A while back I found a neat way to implement it in CSS.
published: 2021-12-16
---

# Result
Hover over the card to see the effect:

<div class="grow-container">
<span>This text is not being pushed away!</span>
<div class="grow-main">
<div class="grow-expansion">
<div class="grow-content">
<div class="grow-img">
</div>
<div class="grow-content-text">
They asked if I knew anything about Theoritcal Physics, I said I had a theoritcal degree in Physics. They said welcome aboard.</div>
</div>
</div>
</div>
<span>Neither is this text!</span>
</div>

<style>
:root {
  --growth: 15px;
  --time: 0.2s;
}

.grow-container {
    width: 60%;
    margin: 0 auto;
}

.grow-main {
    top: calc(-1 * var(--growth));
    left: calc(-1 * var(--growth));
    position: relative;
}

.grow-expansion {
    position: relative;
    top: var(--growth);
    left: var(--growth);
    margin-bottom: 0px;
    box-sizing: content-box;
    padding: 0px;
    width: 100%;
    overflow: hidden;
    box-shadow: 
        rgba(0, 0, 0, 0.13) 0px 2px 5px 1px, 
        rgba(0, 0, 0, 0.18) 0px 1px 2px 0px;
    transition: 
        padding var(--time), 
        top var(--time), 
        left var(--time), 
        margin-bottom var(--time);
}

.grow-expansion:hover {
    margin-bottom: -30px;
    padding: var(--growth);
    top: 0px;
    left: 0px;
    z-index: 1;
}

.grow-content {
    display: flex;
    margin: calc(-1 * var(--growth));
    background-color: white;
}

.grow-img {
    background-image: url("/images/css-growing-box-hover/fallout.jpeg");
    background-repeat: no-repeat;
    background-size: cover;
    background-position: center center;
    width: 35%;
    flex-shrink: 0;
}

.grow-content-text {
  padding: 30px;
  width: 100%;
  font-size: 20px;
}

@media only screen and (max-width: 768px) {
    .grow-content {
        display: block;
    }

    .grow-img {
        width: 100%;
        min-height: 180px;
    }

    .grow-content-text {
        padding: 25px;
        width: 100%;
        font-size: 16px;
    }
}
</style>

# Introduction
3 years ago I was tasked at work with figuring out a way to "grow" a `div` in all directions by a few pixels without shifting the content inside the element and without affecting the content outside. There are various solutions already to be found on the web, all seem to cause various graphical problems on certain browsers or make use of CSS rules that limit how the element can be used. We wanted free rein on where and how the element can be used.

![Hovering should expand the `div` in all directions](/images/css-growing-box-hover/transition.svg)

# How it's done
After playing around (for many many hours) with pretty much every single CSS rule under the sun related to spacing, I came up with a solution. My solution puts a smaller "frame" or "window" `div` over the content and uses `overflow: hidden` to hide the overflowing content. This frame is then expanded in all directions. To get this to work, some tricks have to be used. Check out the HTML and CSS with explanations directly included.

```html
<!-- grow-main counteracts grow-expansion -->
<div class="grow-main">
  <!-- grow-expansion represents the "frame" that hides the 
  content not visible in that frame and does the growing -->
  <div class="grow-expansion">
  <!-- grow-content holds all your content -->
    <div class="grow-content">
    ...
    </div>
  </div>
</div>
```

```css
/* Use CSS variables for growth amount and growth speed */
:root {
    --growth: 15px; 
    --time: 0.2s;
}

.grow-main {
    /* Counteract the offset in grow-expansion
    This way your elements still go where you expect them to go */
    top: calc(-1 * var(--growth));
    left: calc(-1 * var(--growth));
    position: relative;
}

.grow-expansion {
    position: relative;
    /* Offset so we can grow to the left and to the top too */
    top: var(--growth);
    left: var(--growth);
    /* Required, since we grow using padding */
    box-sizing: content-box;
    /* Hide the content not shown in this "frame" */
    overflow: hidden;
    /* Make sure the frame covers all of the content */
    width: 100%;
    /* Optional box-shadow to make your div "card-like" */
    box-shadow: 
        rgba(0, 0, 0, 0.13) 0px 2px 5px 1px, 
        rgba(0, 0, 0, 0.18) 0px 1px 2px 0px;
    /* Transition values */
    margin-bottom: 0px;
    padding: 0px;
    transition: 
        padding var(--time), 
        top var(--time), 
        left var(--time), 
        margin-bottom var(--time);
}

.grow-expansion:hover {
    /* Grow the frame into all directions with padding */
    padding: var(--growth);
    /* Simultaneously, remove the offset 
    to simulate growing to top and left */
    top: 0px;
    left: 0px;
    /* This ensures that the layout outside the element is not affected
    As the element expands up and down (both by --growth), 
    "suck in" space from the bottom (moving bottom content closer)
    at the same rate the element is expanding */
    margin-bottom: calc(-2 * var(--growth));
}

.grow-content {
    /* This is needed to fit the content into the expanded frame.
    Negative margins are still a bit magical to me, so I'm not 100% sure
    how I came up with this, but it works */
    margin: calc(-1 * var(--growth));
    /* If your element expands over something else, the content might
    overlap, so setting a base background color fixes that */
    background-color: white;
}
```

# Additional notes
## Modifications
You can adjust the shape and size of the element to your liking and play around with the amount and speed of the transition. Maybe this could even be modified to work as a circular shape.

## Benefits
You do not have to `position: absolute` this element. It can be part of the document flow without affecting it, making this a purely visual effect.

The element takes up as much space as the unexpanded version does. This means that there is no "phantom space" being wasted. However, you have to manually add spacing should you not want your box to overlap other content. 

Any content can go inside the box, you are not limited to text and images.

The element is entirely self-contained and modular. You can make a React component out of it.

## Problems
There might be some graphical issues on certain browsers. I have not tested it too extensively (besides the common browsers).

Also, keep in mind, the state of my knowledge on this is 3 years old. You can pretend like this blog post was written back then. Maybe by now, there is a cleaner and easier implementation.
