# Row polymorphism (RC1) — design

**Goal:** make the README's "row polymorphism" claim true. Today `TAttrsOpen` is a
tailless `Map`, so a second selection can't accumulate fields and
`unifyAttrsOpenOpen` forgets the union of two open records (REVIEW-3 RC1, #2, #8).

**Reference:** `willtim/Expresso` `TypeCheck.hs` (a shipping row-typed config
language). Confirmed approach: rows terminate in either an empty tail or a row
**variable**; no-duplicate-labels is enforced by **lacks-constraints carried on
the row variable** (Gaster–Jones qualified types), propagated on bind and
preserved through instantiate/generalize. Record **concatenation (`//`) is
deliberately omitted / degraded** — it doesn't fit row unification (Wand/Rémy).

## Representation (adapted to this codebase)

Expresso threads rows as `TRowExtend`/`TRowEmpty` chains. We keep a hybrid that
preserves our existing **optional-field flag** and reuses the `Map TypeVar NixType`
substitution unchanged:

```haskell
data NixType = …                         -- scalars, TList, TFun, TUnion, TAny as today
  | TRec !(Map Text (NixType, Bool)) !RowTail   -- replaces TAttrs AND TAttrsOpen

data RowTail
  = RClosed            -- exactly these fields  (was TAttrs)
  | ROpen !TypeVar     -- at least these; tail var carries lacks-constraints (was TAttrsOpen)
```

- `TAttrs m`     ⇒ `TRec m RClosed`
- `TAttrsOpen m` ⇒ `TRec m (ROpen r)` for a fresh `r`
- `(NixType, Bool)` per field keeps the optional flag (`{ a ? d }`, `x.a or …`).

**Lacks-constraints** live in a side store in `InferState`:
`inferLacks :: Map TypeVar (Set Text)` — "row var `r` must not gain these labels".
A row var substitutes to a `TRec` via the ordinary `Subst`, so `applySubst` /
`occursCheck` only need new `TRec` cases; no second substitution.

## Unification (`unifyRec`)

- `TRec m1 RClosed ~ TRec m2 RClosed` — keys must match exactly; unify common fields
  (today's `unifyAttrs`).
- `TRec m1 (ROpen r) ~ TRec m2 RClosed` — `keys(m1) ⊆ keys(m2)`; unify common;
  bind `r := TRec (m2 ∖ m1) RClosed` (checking `r`'s lacks).
- `TRec m1 (ROpen r1) ~ TRec m2 (ROpen r2)` — unify common; **accumulate the union**:
  fresh `r3`; bind `r1 := TRec (m2 ∖ m1) (ROpen r3)`, `r2 := TRec (m1 ∖ m2) (ROpen r3)`.
  This is the fix for "open∪open forgets the union".
- Bind step does the **row-occurs check** and propagates lacks (union the sets,
  reject a label that would duplicate).

## Selection / has-attr (fixes #2)

`e.k` on `TRec m tail`:
- `k ∈ m` → its field type.
- `RClosed`, `k ∉ m` → error (already done for closed; keep).
- `ROpen r`, `k ∉ m` → unify the record with `TRec {k : β} (ROpen r')` (fresh β, r'),
  i.e. **emit the row constraint** that the var has field `k`. Today this is a silent
  `freshVar`. So `x: x.foo` infers `∀ρβ. {foo:β | ρ} → β`, not `α → β`.

## `//` (NUpdate) — degrade, documented

Closed ∪ closed → exact right-biased merge (covers `cfg // { enable = true; }`).
If either side is open, the result is open (fresh tail) and we keep the
right operand's known labels — **sound** (never assert a label that might be absent),
imprecise on left-only fields. Documented as a known boundary.

## Generalize / instantiate

Quantify row vars like ordinary type vars, **carrying their lacks-constraints**
(instantiate copies the lacks to the fresh var; generalize keeps it). Getting this
wrong is silently unsound — validated only by the oracle (below). This is the var
namespace smell from #19/#20; give row vars a clean fresh supply.

## Blast radius

`Nix/Types.hs` (ADT, `applySubst`, `occursCheck`, `freeTypeVars`, pretty-printer,
JSON), `Nix/Infer.hs` (`unifyRec`/select/`NUpdate`/lambda param sets/attr builtins,
the four `unifyAttrs*` collapse), `Nix/Flake.hs` (25 sites build attrs types),
`Nix/ModuleSystem.hs` (3). Pretty form: `{ a : Int, b? : Bool | r }`.

## Validation (gating)

1. **Oracle** (`narsil-oracle`) — every change re-checked against
   `nix-instantiate`'s `builtins.typeOf`. The silent generalize/instantiate bugs
   only show up here.
2. **NixAdversarial row tests** — `nix_row_*`, `nested_union`, etc. (wired in
   PR #4, several as `expectFailure` tripwires) flip green as rows land; they are
   the acceptance criteria.
3. New positive properties for selection-accumulation and open∪open union.

## Staging (each a compilable, oracle-checked commit)

1. ADT + `applySubst`/`occursCheck`/`freeTypeVars`/pretty + row helpers
   (`rowFields`, `mkClosed`, `mkOpen`) — migrate `Types.hs` + the 4 consumers so it
   compiles with behavior ≈ today (open tails unconstrained). Suite stays green.
2. `unifyRec` (closed/closed, open/closed, open∪open union) + lacks store. Flip the
   open∪open tripwires.
3. Selection/has-attr emit row constraints (#2). Flip `select_on_var`.
4. `//` degrade; attr builtins (`getAttr`/`attrValues`/`removeAttrs`) get real
   row signatures; `import` can return a record. 
5. Nested-union flatten (#25, one-liner) folded in.
