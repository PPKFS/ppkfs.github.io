---
draft: true
---
# A brief introduction to optics (with `optics`)

Okay, I *love* using lenses in Haskell. I honestly cannot imagine doing something like this - a complex, stateful thing where you are relying heavily on composition - without them. Whilst I've tried to strip as much unnecessary quality-of-life complexity out of this tutorial (e.g. `effectful` rather than `mtl` and a sort of data-only subtyping + ID lookup framework that I really should write a blog post about) I feel the minor complexity boost from using `optics` is easily worth its weight in gold. It has a reputation (even more than Haskell) of being complex, but 90% of it comes down to the basic 3 operations of getting (`s -> a`), setting (`a -> s -> s`), and modifying (`(a -> a) -> s -> s`).

---

Optics and lenses are a solution to Haskell's record problem. As most Haskellers know, trying to modify a record field can be annoying:

```haskell

data R2 = R2 { nestedField :: Int } deriving Generic
data Record = R { someField :: R2 } deriving Generic
modifyIt :: R -> (Int -> Int) -> R
modifyIt record f =
  let f1 = someField record
  in record { someField = f1 { nestedField = f (nestedField f1) } }
```

and so can a nested lookup with some logic:

```haskell
lookupNestedMap :: Record -> Key -> (Value -> b) -> Maybe b
lookupNestedMap record k f = fmap f . lookup k . field3WhichIsAMap . field2 . field1 $ record
```

Yuck. Fortunately, there exists a solution in the form of optics. An optic is a combination of "here is how to look up the target(s) associated with this optic, and here is how to modify the target(s) associated with this optic". I say "target(s)", because this can differ on the optic. 95% of the time, you'll just need `Lens` and `Prism`.

- A `Lens'` is a 1-1 relationship. It's basically a record field. You can get the field and set the field.
- A `Prism'` is a 0-1 relationship (with some additional reconstruction properties I won't get into). It's basically `Maybe` a record field. You can *maybe* get the field, you can set the field *if it exists*. Yes, set is `fmap`.

And when it comes to doing things with optics, there's three main things:

- you can `view` the target, `view theLens x` or `x ^. theLens`.
- you can `set` the target, `set theLens newValue x` or `x & theLens .~ newValue`.
- you can apply a function to (`over`) the target, `over theLens f x` or `x & theLens %~ f`.

The order of arguments means that you can sequence operations to perform multiple `set` and `over` on something by using reverse function composition `(&)`:

```haskell

x & lens1 .~ val1 & lens2 %~ f2 & lens3 .~ val3

-- is the same as

let x1 = x & lens1 .~ val1
    x2 = x1 & lens2 %~ f2
    in
      x2 & lens3 .~ val3
```

Optics can be composed. If you have a lens from `s` to `a`, and a lens from `a` to `z`, you can make a lens from `a` to `z`. For the `lens` library this is `.` (same as function composition) and for `optics` it's `%`.

The first example can be written with `optics` as

```haskell

modifyIt record f = record & (#someField % #nestedField %~ f)
```

and the second as

```haskell
-- ^? is roughly "view a maybe value"

lookupNestedMap record k f = f <$> (record ^? #field1 % #field2 % #field3WhichIsAMap % at k)
```

Much nicer!

Optics (the concept) are mostly implemented by two big libraries, `lens` and `optics` (the library) as well as some others (e.g. `microlens`). Both differ slightly in implementation, scope, and so on and a discussion is out of scope and also my wheelhouse here but I prefer to use `optics` because:

- it supports making lenses with `OverloadedLabels` from a `Generic` instance
- the type errors are nicer
- easier support for adding extra label optics

For a more thorough introduction, check out:

- [School of Haskell's introduction to lens](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
- [the tutorial for `lens`](https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html) (but swapping `%` and `.` and `lens` has a `Functor` based implementation)
- [documentation for `optics`](https://hackage.haskell.org/package/optics-0.4.2.1/docs/Optics.html)

But hopefully the uses of them in this tutorial series are well-enough explained at the place of their use.
