---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 1 - Drawing, Moving, Colliding"
date: "0000-01-01"
description: "A brief explanation of what I intend to post here."
summary: "An introduction to the planned blog posts and setting up a project."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
TocOpen: true
draft: true
social:
  bluesky: "ppkfs@bsky.social"
hidden: true
---

# Chapter 1: Drawing the `@` symbol and moving around

Let's get down to business

to defeat the Huns

by drawing an `@` to the screen in Haskell and using the WASD keys to move it around.

This part, and future parts, aim to be aligned with the remade [python+libtcod rogueliketutorial](https://rogueliketutorials.com/tutorials/tcod/v2/part-1/). This should make it easier for me to write the posts (as I've got less chance to go on a rambling tangent) as well as making it followable for the "r/roguelikedev does the roguelike tutorial" event.

The lengths might vary quite significantly depending on how much additional infrastructure we need, but the resulting "game" should be roughly equivalent.

## Opening a window

We'll start with walking through the code for opening a window from Part 0, with some additional comments.

```haskell

import Rogue.Prelude
import Rogue.Window
import Rogue.Config
import Rogue.Events

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  -- a wrapper around `bracket`
  withWindow
  defaultWindowOptions { size = Just screenSize }
  pass
  (const runLoop)
  pass

runLoop :: MonadIO m => m ()
runLoop = do
  terminalRefresh
  shouldContinue <- handleEvents Blocking $ \case
    WindowEvent Resize -> return True
    WindowEvent WindowClose -> return False
    Keypress TkEsc -> return False
    _ -> return False
  when (and shouldContinue) runLoop
```

We'll walk through it a few lines at a time.

```haskell
screenSize :: V2
screenSize = V2 100 50
```

We start by defining the size of our screen, in tiles. `V2` is the generic (integer-valued) 2D vector/point/position type. Everything is done in tiles; changing the size of the font will change the size of the screen!


### Side note: Working with `V2`

We can access the two components in a couple of handy vanilla Haskell ways:

```haskell
-- pattern matching
f :: V2 -> a
f (V2 x y) = ...

-- if we want to hand a `V2` to a function expecting two ints,
-- we have `withV2` in `Rogue.Geometry.V2`,
-- which is re-exported by `Rogue.Prelude`:
withV2 :: (Int -> Int -> a) -> V2 -> a

-- e.g.
magnitude :: Int -> Int -> Double
magnitude x y = sqrt $ (x^2) + (y^2)

v2Magnitude :: V2 -> Double
v2Magnitude v = withV2 magnitude v
```

There's also a nice `optics` interface available with the power of `OverloadedLabels`. We can access the components of `V2` by either:

- using the overloaded labels, `#x` and `#y`. The short explanation of overloaded labels *in the context of optics* is that you can have record field accessors (functions of the form `field :: s -> a`) and as long as you derive a `Generic` instance, you get a *lens* version of the field for free at `#field`!

- `V2` also has an instance of `Field1` and `Field2`, which are the `optics` typeclasses for "acts like a tuple". That is, we can get, set, and modify the first and second targets of a `V2` with the lenses `_1` and `_2` respectively.

In the context of optics over `V2`, these are interchangeable. If the `V2` represents an `(x, y)` pair then it makes the most semantic sense to be accessed with `#x/#y` - but if it's something different (for instance, a size - where they are width/height fields) then I lean towards `_1/_2`.

Some examples:

```haskell

-- (^.) is "view the target of a lens"; in this case, we get the
-- target of _2 - the y component.
getY :: V2 -> Int
getY v = v ^. _2

-- and we also have a non-operator form of (^.), view
altGetY v = view #y v

-- (.~) is "set the target of a lens"; in this case, we set the
-- target of _1 to `x`
setX :: V2 -> Int -> V2
setX v x = v & #x .~ x

-- similarly, set is the non-operator version of `.~`
altSetX v x = set _1 v x

-- (%~) is "modify the target of a lens by a function". Here,
-- we can chain the operations. In this specific example, it would
-- probably be easier just to do `(V2 x y) = V2 (x+5) (y+5).
add5ToBothParts :: V2 -> V2
add5ToBothParts v = v & _1 %~ (+5) & _2 %~ (+5)

-- and finally, over is the non-operator version of `%~`.
-- Chaining multiple lens updates doesn't quite work so well with the non-operator versions...

add5ToBothParts v = (over #x (+5) v) & (over _2 (+5))
```

## Window handling
```haskell
withWindow
  defaultWindowOptions { size = Just screenSize }
  pass
  (const runLoop)
  pass
```

`withWindow` is a thin wrapper around [`bracket`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Exception.html#v:bracket). `bracket` (from `Control.Exception` amongst others like `unlift-io`) is a safe way to wrap an `IO` call with initialisation and cleanup logic that guarantees the cleanup logic will be called. Using `withWindow` means that no matter how we leave the window (either force quitting or closing normally) we will call the `terminal_close` function to free resources.

```haskell
withWindow ::
  MonadIO m
  => WindowOptions -- the options for creating a new window (size, font, etc)
  -> m a -- initialisation logic
  -> (a -> m b) -- the computation in-between
  -> m c -- cleanup logic
  -> m b
```

`pass` is an alias for `return ()`; the monadic do nothing. We don't have any special initialisation or cleanup logic to do (window opening and closing is done for us), so we just use `pass`. Note that this will only **call the computation in-between once**, so any looping needs to be done there. We do have to use `const runLoop` because `a ~ ()` and so the in-between computation is actually `() -> m b`.

Note: We could just as easily have done this without `withWindow` by calling `terminalOpen` and `terminalClose`, and using bearlibterminal's config strings to pass to [`terminalSet`](http://foo.wyrd.name/en:bearlibterminal:reference#set), either with or without `bracket` -- but we'll go for simplicity and use the higher level functions.

```haskell
runLoop :: MonadIO m => m ()
runLoop = do
  -- drawing code will go here
  terminalRefresh
  -- handle events
  shouldContinue <- handleEvents Blocking $ \case
    WindowEvent Resize -> return True
    WindowEvent WindowClose -> return False
    Keypress TkEsc -> return False
    _ -> return False
  -- update logic will go here
  when (and shouldContinue) runLoop
```

The main game loop: draw things to the screen (and then actually render it), handle incoming input, and finally do any game update logic. Right now we don't have any drawing to do, or game update logic. We do need to refresh the terminal at least once - a quirk of `bearlibterminal` is that it won't display a window until a call to `terminal_refresh/terminalRefresh`.

For the event handling, we want to handle all of the events that have arrived since the last time we checked. `handleEvents` takes an event handling function `Event ->
## Drawing an `@`

Without further ado, we can draw things.

```haskell


import Rogue.Prelude
import Rogue.Window
import Rogue.Config
import Rogue.Geometry.V2
import Rogue.Colour
import Rogue.Events

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize }
  pass
  (const runLoop)
  pass

runLoop :: MonadIO m => m ()
runLoop = do
  -- we have no update logic currently
  -- rendering
  terminalClear
  terminalColour (fromRGB 255 128 255)
  void $ terminalPrintText 10 10 "@"
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    WindowEvent Resize -> return True
    WindowEvent WindowClose -> return False
    Keypress TkEsc -> return False
    _ -> return False
  when (and shouldContinue) runLoop
```

If you `cabal run hs-rogue` this, you'll get a black screen with a magenta `@` on it. Hooray.

As I've previously said, I am assuming a degree of Haskell proficiency for the tutorial so I'll not step through with "this is a function, this is a constraint, this is what the `$` operator does" and so forth.

### Window handling

You can draw a window in two ways. One is to use the functions exposed directly from `bearlibterminal` - `terminalOpen :: MonadIO m => m Bool` and `terminalClose`. The [documentation for the original library](http://foo.wyrd.name/en:bearlibterminal:reference#open) explains things in slightly more detail. As a rule of thumb, the Haskell bindings for `bearlibterminal` are basically identical to the original C/C++ except with wrappers around `int`s (as `Bool` or some enum ADT).

The other recommended way is `withWindow :: MonadUnliftIO m => WindowOptions -> m a -> (a -> m b) -> m c -> m b`. This is a wrapper around `bracket_` that takes some initialisation function, some main loop, and some cleanup function and makes sure that we avoid memory leaks by closing the window if an error occurs. In our case, we have no additional logic for either setup or cleanup (opening and closing the terminal is handled automatically) so we pass `pass` (`pass = return ()` from `relude`. It's super handy).

We follow the standard game loop pattern of doing any per-turn (or per-frame) logic, rendering, and then handling any events. We currently don't have any logic. For rendering, we clear the screen before drawing our pink `@` and then refresh the window.

### Event handling

For event handling, we once again can use the raw `bearlibterminal` event handling with `terminalRead` and `terminalHasInput` or the `handleEvents` helper. `handleEvents` will clear all pending events - keypresses, window events, etc - and map over each of them. It can also either ensure there is at least one event (`Blocking`) in the queue or not (`NonBlocking`). As we're doing a traditional turn-based game, we'll handle it with `Blocking`.

A quirk of the library is that the first event will always be a window resize event, which we want to ignore. If we receive a `WindowClose` event (either pressing the `X` or sending a `SIGTERM/SIGKILL`) then we stop and quit, otherwise we run the loop again. We also close the window on `Esc` for ease. Note that because `handleEvents` may handle more than 1 event at once, we get a list of results and we only continue the loop if *none* of them are `WindowClose`.

## Moving around

Now let's add some movement handling. For now, we aren't going to do some sort of `Action` hierarchy for different kinds of actions (this will be a lot later) or setting up game objects (we'll do that when we add a map in the next part).

We want to separate the actual keypresses from the directions we want to move from the moving logic:

```haskell
import qualified Data.Map as M

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  , (TkUp, UpDir)
  , (TkDown, DownDir)
  , (TkLeft, LeftDir)
  , (TkRight, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir =
  case dir of
    LeftDir -> _1 %~ subtract 1
    RightDir -> _1 %~ (+1)
    UpDir -> _2 %~ subtract 1
    DownDir -> _2 %~ (+1)

```

Here we use `optics` to give us lenses to access the first and second components of the vector with `_1` and `_2` respectively. Normally these are the lenses for elements of a tuple, but because we have `instance Field1/Field2` for `V2` we can use `_1` and `_2` for it as well. Neat.

Now we need to track the player's position in a `State` effect. Whilst we're at it, we'll also keep track of whether we should quit the game to tidy up the event handling. We'll have to rewrite our loop to be in the `Eff` monad but fortunately, this is pretty easy.

```haskell
initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  } deriving stock (Generic)

runLoop :: (IOE :> es, State WorldState :> es) => Eff es ()
runLoop = do
  terminalClear
  terminalColour (fromRGB 255 128 255)
  playerPos <- gets playerPosition
  withV2 playerPos terminalPrintText "@"
  terminalRefresh
  void $ handleEvents Blocking $ \case
    WindowEvent WindowClose -> modify (#pendingQuit .~ True)
    Keypress TkEsc -> modify (#pendingQuit .~ True)
    Keypress other -> case asMovement other of
      Just dir -> modify (#playerPosition %~ calculateNewLocation dir)
      Nothing -> pass
    _ -> pass
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop

```

We update `runLoop` to now be in `Eff`, with [at least] two effects available to us: IO, with the `IOE` effect; and mutable state with the `State` effect. There could be more in `es` but if there are, we can't utilise them. Perhaps the `bearlibterminal` calls should be in their own effect, so that way we don't need `IO` explicitly and can't fire the missiles.

We now fetch the player position from the state and use that position to render the `@`. `withV2 :: V2 -> (Int -> Int -> r) -> r` is handy for automatically splitting out the components of a vector.

As we now have state, we rewrite the event handling loop to instead update that we should close the program at the next opportunity. `OverloadedLabels` and `optics` makes it very nice and clean to do.

If we press a key that maps to `Just direction` (one of WASD or the arrows), then we modify the player position.

The last thing we need to do is to call our effect handlers; this is a composition of functions that will evaluate one layer of the `es :: [Effect]` list until we have handled them all and we finish with `runPureEff` to evaluate to a pure value or `runEff` to evaluate to `IO a`.

```haskell

import Effectful.State.Static.Local

main :: IO ()
main = runEff $
  evalState (WorldState initialPlayerPosition False) $
    withWindow
    defaultWindowOptions { size = Just screenSize }
    pass
    (const runLoop)
    pass
```

Right now we only have 1 (2, including IO) effect so we just need to interpret the state effect and then use `runEff` to go back to `IO ()`.

Now when you run it, you can move around!

# Wrapping up

That's it for part 1. We now have something drawn and we can move around.