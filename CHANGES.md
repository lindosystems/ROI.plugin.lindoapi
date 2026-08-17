# Changes

Newest first. Versions match `DESCRIPTION`.

## 0.3-5 (2026-08-17)

**Reordering reports itself.**

Reordering changes what the solver is handed and remaps the per-row results on
the way back, so it no longer happens invisibly. When a reorder actually occurs
the plugin names the permutation applied; a model already in the required order
permutes to the identity and stays silent.

```
NOTE: L-constraints do not all precede the Q-constraints.
NOTE: constraints reordered for LINDO as (2, 3, 1).
NOTE: duals and slacks are mapped back to the ROI order on return.
NOTE: set control 'reorder_constraints' to FALSE to reject instead.
```

Emitted with `message()` rather than `warning()`: reordering is correct,
documented, default behaviour, and a solve loop would otherwise collapse into
"There were 50 or more warnings". `suppressMessages()` silences it.

- Added `test_qcqp_reorder_notice`, which checks the notice appears for a
  permuted model and does **not** appear for an already-ordered one.

## 0.3-4 (2026-08-17)

**Q-constraints are reordered internally; the ordering restriction is lifted.**

`rLSaddQCterms()` attaches quadratic terms to a constraint by row index, while
ROI keeps constraints in the order they were supplied. The two agree only when
the Q-constraints occupy the tail of the constraint list, which is why the
loader used to demand that ordering and reject anything else.

The loader already handed LINDO a partitioned model: `rLSloadLPData()` takes the
L-rows in their original relative order and each Q-row is then appended by
`rLSaddConstraints()`. The reordering was therefore already happening -- the only
thing assuming a pre-sorted input was the row index passed to `rLSaddQCterms()`,
which used the ROI constraint index. It now targets the row the constraint was
actually appended to, and any input ordering loads correctly.

- New control `reorder_constraints`, default `TRUE`. Set it to `FALSE` to
  restore the strict ordering check, whose diagnostic now names the control.
- Duals and slacks come back indexed by LINDO row; `unpermute_rows()` maps them
  back to the ROI constraint order before they are returned. Primal values and
  reduced costs are indexed by variable and are left alone.
- `lindoapi_load_qp()` carries the permutation to `solve_QP()` in a `"row_perm"`
  attribute on its return value, so its integer result still behaves as one for
  the existing callers.
- `lindoapi_write_op()` gains misordered-model support for free.
- Tests: `qcpex1_permuted()` builds the `test_qcqp_02` model under any
  constraint permutation. `test_qcqp_reorder` checks all five orderings reach
  the same optimum; `test_qcqp_reorder_duals` checks duals and slacks follow
  their own constraint through a permutation; `test_qcqp_reorder_mip` covers the
  MIP branch of `lindoapi_solve_model`, which reports slacks but no duals.

## 0.3-3 (2026-08-17)

**Fixed the constraint-ordering guard in `lindoapi_load_qp()`.**

The guard added in `c9188bc` (0.3-2, 2025-07-31) crashed on every QCP whose last
constraint is quadratic -- the layout the plugin required -- with:

```
Error in if (any(is_lconstr[seq(last_false_idx + 1, length(is_lconstr))])) :
  missing value where TRUE/FALSE needed
```

`seq(a, b)` counts *down* when `a > b`. With the Q-constraints at the end,
`last_false_idx` equals `length(is_lconstr)`, so `seq(last_false_idx + 1,
length(is_lconstr))` yielded `c(n+1, n)` rather than `integer(0)`; the
out-of-range index produced `NA`, and `if (any(...))` failed on it. Tests
`test_qcqp_01`, `test_qcqp_02` and `test_qcqp_03` all reproduced it.

Anchoring on `max(which(!is_lconstr))` was wrong on its own terms as well: a
linear constraint sandwiched between two quadratic ones was invisible to the
check and would have reached `rLSaddQCterms()`, attaching the quadratic terms to
the wrong row and yielding a silently wrong answer. The check now anchors on the
first Q-constraint; `min(which(!is_lconstr))` is always in bounds, so the
out-of-range case disappears with it.

- Replaced the bare `stop()`, whose empty message made the failure
  undiagnosable, with one naming the first Q-constraint and the L-constraints
  that follow it.
- Tests: rewrote `test_qcqp_02b`, which asserted that a misordered model solves;
  added `test_qcqp_02c` for the interleaved `L,Q,L,Q` case; wired both into the
  runner and the dispatcher, where `02b` had never been called.

## 0.3-2 (2025-04-04) and earlier

See `git log`. Release points: 0.3-2 (2025-04-04), 0.3-1 (2025-03-10),
0.2-1 (2024-11-28), 0.2-0 (2024-11-01), 0.1-0 (2024-10-28).
