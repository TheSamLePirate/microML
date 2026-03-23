# Poule — Syntax Documentation

## Overview

A Poule file is a sequence of declarations:

* `type`
* `fundecl`
* `pred`
* `axiom`
* `theorem`
* `check`

---

## Type Declarations

### General form

```text
type 'a 'b T =
  C1
| C2 of t1 * t2 * ...
```

### Examples

```text
type Nat =
  Zero
| Succ of Nat

type 'a List =
  Nil
| Cons of 'a * List('a)
```

### Rules

* Type names must start with an uppercase letter
* Type parameters come before the type name
* Constructors start with uppercase letters
* First constructor is not prefixed with `|`

---

## Types

### Named types

```text
Nat
List('a)
```

### Type variables

```text
'a
```

### Function types

```text
Nat -> Nat
(Nat, Nat) -> Nat
```

### In declarations

Functions:

```text
fundecl plus : (Nat, Nat) -> Nat
```

Predicates:

```text
pred even : (Nat)
pred leq : (Nat, Nat)
```

---

## Terms

### Variables

```text
x
```

### Constructors

```text
Zero
Succ(x)
Cons(x, xs)
```

### Function symbols

```text
plus(x, y)
```

### Convention

* Uppercase: constructors
* Lowercase: variables or function symbols

---

## Formulas

### Constants

```text
Top
Bottom
```

### Predicates

```text
even(Zero)
leq(x, y)
```

### Equality

```text
x = y
plus(x, Zero) = x
```

### Connectives

```text
A /\\ B
A \\/ B
A -> B
```

Precedence (low → high):

* `->`
* `\\/`
* `/\\`

### Quantifiers

```text
forall (x : Nat). even(x) -> even(x)
exists (x : Nat). even(x)
```

Only one binder per quantifier.

---

## Proof Terms

### Variable

```text
h
```

### Trivial proof

```text
triv
```

### Absurd elimination

```text
abort p : F
```

### Pairs

```text
(p1, p2)
fst p
snd p
```

### Sum types

```text
left p : F
right F p
```

### Case analysis

```text
match p with
| left x => p1
| right y => p2
```

### Implication

```text
fun (h : A) => p
```

### Application

```text
p q
```

### Universal quantification

```text
gen (x : Nat) => p
```

### Instantiation

```text
p[t]
```

### Existentials

```text
pack [T, t, p]
unpack(t, x, h, p)
```

---

## Logical Declarations

### Axiom

```text
axiom even_zero : even(Zero)
```

### Theorem

```text
theorem id_even : forall (x : Nat). even(x) -> even(x) =
  gen (x : Nat) =>
  fun (h : even(x)) =>
    h
```

### Check

```text
check
  fun (p : even(Zero) /\\ even(Zero)) =>
    fst p
:
  even(Zero) /\\ even(Zero) -> even(Zero)
```

---

## Complete Example

```text
type Nat =
  Zero
| Succ of Nat

type 'a List =
  Nil
| Cons of 'a * List('a)

pred even : (Nat)
fundecl plus : (Nat, Nat) -> Nat

axiom even_zero : even(Zero)

theorem id_and : forall (x : Nat). even(x) -> even(x) =
  gen (x : Nat) =>
  fun (h : even(x)) =>
    h

check
  fun (p : even(Zero) /\\ even(Zero)) =>
    fst p
:
  even(Zero) /\\ even(Zero) -> even(Zero)
```

---

## Notes

Common causes of parse errors:

* lowercase type names instead of uppercase
* missing parentheses in binders (`(x : T)`)
* wrong arrow (`=>` instead of `->` in types)
* missing parentheses in `fun` and `gen`
* incorrect conjunction symbol (must be `/\\`)
