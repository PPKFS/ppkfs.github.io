---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 1 - Drawing & Moving"
date: "2025-04-18"
description: "This is the first 'proper' tutorial post; there's a walkthrough of the window opening example before jumping in with moving an @ around the screen."
summary: "Walking through the window opening code and drawing and moving a character."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
TocOpen: true
draft: false
social:
  bluesky: "ppkfs@bsky.social"
---

# Introduction

Welcome back for the first proper part of the Haskell roguelike tutorial! In this part, we're going to:
- first briefly go over the window opening code from Part 0;
- setup our own custom prelude;
- draw some things to the screen;
- add input handling to move around.

# Going over the window opening code

In the first part, we opened a (blank) window just to verify that we had setup the project correctly and had the libraries working. Whilst the code is relatively straightforward, it's worth covering it to make sure we can hit the ground running.

```haskell
module Main where

import Prelude

import BearLibTerminal
import Control.Monad (when)
import Rogue.Config
import Rogue.Events
import Rogue.Geometry.V2
import Rogue.Window

screenSize :: V2
screenSize = V2 100 50
```

As we have `NoImplicitPrelude` enabled (very shortly for an actual reason) we need to explicitly import `Prelude` here. We also do need to import `Control.Monad` for `when`, as well as `BearLibTerminal` (for keycode events and terminal functions) and four modules from `roguefunctor` for basic window management - `Rogue.Config` gives us `defaultWindowOptions`, `Rogue.Events` gives us an event loop, `Rogue.Geometry.V2` gives us a nice 2D vector type, and `Rogue.Window` has functionality for handling initialising and closing a window.

For now, we just want to hard-code the screen size (and ignore resizing or having it in a configuration file). We also want to, at least initially, tie the map size to the screen in some form - so we define it at the top level so it can be passed around later to our map generation functions.
```haskell
main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize }
  (return ()) -- no init logic
  (const runLoop)
  (return ()) -- no shutdown logic
```

Our `main` function delegates everything to `withWindow`, a convenient wrapper around `bracket`. For those who haven't seen `bracket/bracket_` before, it allows us to wrap some `IO a` action with an initialisation or acquisition logic and a finalising or releasing logic. Even if the internal computation hits a runtime exception or exits unexpectedly, the resource will be safely released. `withWindow` isn't *exactly* the same as `bracket` - `bracket`'s type signature looks like `IO a -> (a -> IO b) -> (a -> IO c) -> IO c`, where the order of parameters goes acquire-release-action and the resource acquired is passed as a parameter to both the main action and releasing action. In contrast, `withWindow` is:

```haskell
withWindow ::
  MonadIO m
  => WindowOptions -- to configure the window
  -> m a -- the initialisation logic to run *after* the window has been initialised
  -> a -> m b -- everything happens here
  -> m c -- the final logic to run *before* the window has been closed
  -> m b
```

Note that the order is shifted: because we typically want to run the middle action as a loop (i.e. we need it to recurse) rather than as a one-off, we don't need the benefits of writing `bracket init close $ \resource -> do ...`. Writing it in this order feels more sensible.

As we don't need to do any resource acquisition or releasing - `withWindow` handles the window for us, obviously - we do nothing with `return ()` both times.

```haskell
runLoop :: IO ()
runLoop = do
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    TkClose -> return False
    TkEscape -> return False
    _ -> return True
  when (and shouldContinue) runLoop
```

The first thing we need to do in our game loop is to refresh the screen. This is required by `BearLibTerminal` to show the window after it's been initialised, and `withWindow` doesn't do this step for us (in case e.g. you want to render the scene before showing the window). For event handling we use `handleEvents` to iterate over all the input events that have been registered since the last time it was checked. Usually this will just be one event, but it can be multiple! We can either do this with `Blocking` enabled (where it will wait if no events are pending) or `NonBlocking` (where, if there are no events, it'll return `[]` and continue execution). For now, because we want to just wait for a keypress we use the blocking mode. If we get a window close event (the user pressed the X) or an `Escape` keypress, we return a `False` and if we have any other event we return `True`. As this is a *list* of booleans (if we get multiple events at the same time), we want to loop - via recursion - if **none** of the booleans are `False` (that we did not get a close or escape event)...and thank you DeMorgan for letting us flip the conditions to get that we should loop if **all** the booleans are `True`! If this is the case we go back into the loop, and if not then the function ends.

So hopefully this was all fine to follow! Once again none of the things in `roguefunctor` are necessary, but they sure save time. One more small thing before we start actually drawing to the screen to save us some time later..

# Making our own Prelude

There's an awful lot of problems with the standard prelude. Custom preludes exist and are quite good but for this project we want to keep it relatively simple. Most of the stuff in the Prelude is fine for us, but there's a bunch of things we are going to make a lot of use of that would be easier if we had them bundled. So that's what we're going to do - make a module that simply re-exports the entire Prelude module and another half dozen modules, and then import that in each file.

Let's make a new folder for our game logic modules and call it `HsRogue`, and then make a new `HsRogue.Prelude` module in there:

```haskell
module HsRogue.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Control.Monad
  , module Control.Monad.State.Strict
  , module Rogue.Geometry.V2
  , module GHC.Generics
  , Text
  ) where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict
import GHC.Generics
import Rogue.Geometry.V2
import Data.Text (Text)
```

Here, we just re-export the entire modules we will be using the most:
- `Data.Maybe` is very handy for inline pattern matching on `Maybe` (e.g. `fromMaybe`).
-`Control.Monad` gives us convenient control flow options like `when` and `forM` (arguably the more useful cousin to `mapM` for us).
- As games are inherently stateful, and we're going to be working almost exclusively in `StateT s IO`, having this always imported helps.
-`GHC.Generics` is handy for when we want to derive `Generic` for our datatypes. It won't be something we'll actively use in this tutorial much (or at least for a while) - but if you're planning to use optics and lenses it is a lifesaver.
- `Rogue.Geometry.V2` is just ubiquitous.
- Finally, we only want to export the `Text` type from `Data.Text`. If we want the rest of the functions we'll explicitly import it qualified to avoid clashing with `Text.map` and `Text.fold` and similar.

Another option would be to use the bundled `Rogue.Prelude` module, which is based off `relude`. But, that's somewhat out of scope for here.

Let's amend our current `Main` module to use the new prelude:

```haskell
import HsRogue.Prelude
```

Now with a more convenient prelude out of the way, let's **finally** start on drawing to the screen.

# Drawing the @

We'll start by upgrading our monad stack. A game is an inherently stateful program; whilst we could abstract away the underlying state and produce a domain-specific model (and probably *should* but we'll leave that for a later date), there would still need to be some sort of state attached. We've not got a map yet, so the only parts of our state are the player's current position and whether we should quit at the next moment.

```haskell

-- define the starting location for the '@' we draw
initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

-- later this will include things like all the enemies and items in the world and the map
-- but for now we only need to store the player's position and whether we should quit the game
-- on the next loop
data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  }
```

And because we're going to be doing everything within a state monad transformer (for the game state) over IO (for drawing and input handling), we make a convenient type synonym:

```haskell

-- our Game monad will be a state monad with the current state of the world as we play
-- on top of the IO monad to handle reading the player's inputs and drawing things to the screen
type Game a = StateT WorldState IO a

```

Why are we using a concrete monad stack rather than `mtl`-style constraints? Purely for simplicity's sake. You could - and I would even possibly go as far as *should* - write your monadic computations like this:

```haskell

type GameConstraints m = (MonadState m, MonadIO m)

doStuff :: GameConstraints m => m a
...
```

This is a decent idea for many reasons:

- it allows you to avoid adding unnecessary constraints, such as having `IO` available for functions that only get or set the game's state;
- it makes it easier if you want to move to another effect system (an effect system here meaning "some kind of big monad that can do various things");
- it means you don't need to worry about having your `Game` synonym somewhere where it doesn't cause circular dependencies;
- it makes it easier to "add" some local additions (like a `MaybeT` transformer) for blocks of code.

It may turn out that later in this tutorial we will change it up, but for now - simplicity is key.

We then want to move our window creation and game loop into this new `Game` monad. We can do this by using one of the `StateT`'s family of interpreters; in this case we don't really care about either the return value or the eventual state value, so it doesn't matter if we use `execStateT`, `evalStateT`, or `runStateT`.

```haskell
main :: IO ()
-- evalState is the "run this computation and return the *value* it computes" state evaluator.
main = flip evalStateT (WorldState initialPlayerPosition False) $
  withWindow
    defaultWindowOptions { size = Just screenSize }
    (return ())
    (const runLoop)
    (return ())

runLoop :: Game ()
runLoop = do
  terminalRefresh
  void $ handleEvents Blocking $ \case
    TkClose -> modify (\worldState -> worldState { pendingQuit = True})
    TkEscape -> modify (\worldState -> worldState { pendingQuit = True})
    _ -> return True
  -- this is the equivalent of doing
  -- worldState <- get
  -- let shouldContinue = not (pendingQuit worldState)
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop
```

We move from a local binding of the pending quit (local to the `runLoop` function) to a stateful one, and adjust our looping condition accordingly. Finally, at long last, we can adjust `terminalRefresh` to additionally clear the screen and obtain the current player position to draw it to the screen.

```haskell
runLoop = do
  terminalClear
  -- we query the game state for the current position of the player
  playerPos <- gets playerPosition
  void $ withV2 playerPos terminalPrint "@"
  terminalRefresh
```

If you rebuild it now, you should see a white `@`! It doesn't do much yet. Let's add some keyboard handling.

*Note*: here, we use `withV2 :: V2 -> (Int -> Int -> a) -> a` to join together functions that expect the `x` and `y` coordinates individually (such as `terminalPrint`) to be used with 2D vectors without having to explicitly pattern match.

# Moving around

For now, we aren't going to do some sort of `Action` hierarchy for different kinds of actions (this will be a lot later) or setting up game objects (we'll do that when we add a map in the next part).

We want to separate the actual keypresses from the directions we want to move, and both of those from the moving logic itself. We start with a type for directions (diagonal movement will come later, or it can be left as an exercise to the reader!):

```haskell
data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Eq, Ord, Show, Generic, Read, Enum, Bounded)
```

Next, we need a mapping from the keypresses to the directions. We can support both WASD and the arrow keys for this fairly easily.

```haskell
import qualified Data.Map as M

-- because we have separated movement inputs from movement directions, we have a map to connect the two
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
```

Not every keypress is necessarily a movement action, which is why we return `Maybe Direction` - `Just direction` in the good case, and a `Nothing` otherwise. Eventually, we will a) expand this from a mapping of keys-directions to a mapping of keys-*actions* and b) envelop it within the game state to allow us to remap keys.

Given a direction, we want to turn it into a function that modifies a position (by moving 1 space in that direction):

```haskell

calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir v =
  let updateIt :: V2 -> V2
      updateIt = case dir of
        LeftDir -> modifyX (subtract 1)
        RightDir -> modifyX (+1)
        UpDir -> modifyY (subtract 1)
        DownDir -> modifyY (+1)
  in
    updateIt v
```

Now we've got all the building blocks of our movement system:

- we can go from movement keypresses to directions (`movementKeys`) that we can extend to any keypress to potentially a direction (`asMovement`);
- from directions to a vector modifying function (`calculateNewLocation`);
- and by glueing these together we can go from any keypress to potentially an updated position.

This is what's great about functional programming: it strongly steers you into writing very compartmentalised and self-contained parts that you can combine. Note that all 3 of the components are completely pure! There's no mutable state hanging around where we need to be cautious about whatever entity the `calculateNewLocation` function is operating on.

The last step is to check if the key pressed in our event handling loop was one of these movement keys and to adjust the state accordingly:

```haskell
void $ handleEvents Blocking $ \case
    -- if we get a WindowClose event, then we want to stop running our game loop.
    -- but rather than doing a break or jump, it's nicer to just record that we are expecting to quit at the end of
    -- this loop.
    TkClose -> modify (\worldState -> worldState { pendingQuit = True})
    -- we'll also listen for the Esc key as another way to quit
    TkEscape -> modify (\worldState -> worldState { pendingQuit = True})
    -- if we get another event (likely a keypress) that isn't Esc, we want to check if it's bound to moving in a direction
    other -> case asMovement other of
      -- success, the key pressed was indeed a movement direction. we update the player's position
      Just dir -> modify (\worldState ->
        worldState
          { playerPosition = calculateNewLocation dir (playerPosition worldState)
          })
      -- it wasn't an Esc, or a movement direction pressed, so we don't do anything.
      Nothing -> return ()
```

Again, a large amount of options which could make this a little less noisy - we could use pattern guards to inline our case statement; we could use lenses or optics to better deal with state updates (producing something closer to as you'd expect with setting a mutable field in an imperative language), or we could move the close events into the "action" map (the current direction map). Whilst my personal preference is to use optics for doing state updates, they *do* introduce a whole new layer of complexity that I'd like to avoid. We definitely will be wrapping these annoyingly wordy `modify` lambdas up shortly though!

Now if we build and run the program, you should not only see the @ in the centre of the screen but you can move it! Of course it's possible to move it off the screen and then forget which side of the screen you exited and therefore have no idea where your character is *but* it works!

# Wrapping up

We have something resembling a game!

In the next part, we're going to make it minorly more interesting - adding a map and collision.

And what will likely become the commonplace wrapping up:

The code is available at the accompaying github repo [here](https://github.com/PPKFS/roguelike-tutorial-parts/tree/part-1).

Hopefully this was understandable - any feedback is greatly appreciated. You can find me on the various functional programming/roguelike Discords as ppkfs and on Bluesky as @ppkfs.bsky.social.

Hope to see you for Part 2 soon!
