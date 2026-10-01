# TODO

Open work only, roughly by priority. What was done is in `CHANGES.md`; how
the package behaves is in `README.md`.

## Bugs

- `make_csc_matrix.matrix()` rejects every input: a stray `33` in the type
  guard (`R/plugin.R:13`) leaves `stop()` unconditional. Unreachable today,
  because `constraints(x)$L` always arrives as a `simple_triplet_matrix` and
  dispatches to the other method. Fix the guard and add a test that reaches
  the dense-matrix method.

## Blocked on upstream

- LINDO API 16.0.7099: `LSsolveGOP()` crashes on an API-built QCQP with an
  `==` quadratic row or a maximization. `README.md` release note 6 carries
  the workaround (`LS_IPARAM_GOP_QUAD_METHOD = 0`). Drop the note once a
  fixed library ships.
- rLindo: free the callback block in `rcLSdeleteModel()`. The plugin works
  around it by detaching the callback before deleting the model; the same
  will be needed for the standard and MIP callbacks once they are wired.

## Features

- Install `fn_callback_std` and `fn_callback_mip`, which are registered as
  controls but unused. Their return value (nonzero interrupts the solve)
  needs a test first; `tests/test_cb.R` has a `cbFunc` example.
- Constraint directions beyond `<=`, `>=` and `==`: ranges and free rows.
- Cones (`cones = "X"` in the solver signature); LINDO API supports SOC and
  SDP, ROI can express them.
- A user interrupt (Ctrl-C) serviced inside `fn_callback_log` still unwinds
  through the solver. The environment is released, the solver state is not.

## Testing

- Re-enable the LP/MILP tests in the run-all block of
  `tests/test_lindoapi.R`; all five pass by name.
- Re-enable or delete `test_read_mps`; it self-skips without `LINDOAPI_HOME`.
- Read `LSLOCAL` from an environment variable, default `FALSE`, instead of
  editing the test file.
- No automated regression run; the suite is run by hand.

## Documentation

- `man/` has only the package page and one example; the controls and the
  `on_before_optimize` / `on_after_optimize` hooks are documented in
  `README.md` only.
