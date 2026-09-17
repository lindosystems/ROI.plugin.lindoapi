# TODO

Open work, roughly by priority. Completed items move to `CHANGES.md`.

## Bugs

- `make_csc_matrix.matrix()` rejects every input. A stray `33` in the type
  guard (`R/plugin.R:13`) leaves `stop()` unconditional. The method is
  unreachable in practice, because `constraints(x)$L` arrives as a
  `simple_triplet_matrix` and dispatches to the other method. Fix the guard
  and add a test that exercises the dense-matrix method.

## Robustness

- **No cleanup on error paths.** The five model-creating entry points
  (`solve_LP`, `solve_QP`, `lindoapi_solve_file`, `lindoapi_read_op`,
  `lindoapi_write_op`) create a LINDO environment and model and delete them
  only on the success path; none registers `on.exit()`. Any error in between
  leaks the environment for the rest of the session. Two existing `stop()`
  calls already do this: the ordering check in `lindoapi_load_qp()` and the
  unsupported-extension branch in `lindoapi_write_file()`. Since 0.3-6 an
  error raised inside a `fn_callback_log` function does too, because
  `rLindo` evaluates the callback with plain `eval()` and the error unwinds
  through the solver. One open/close helper with `on.exit()`, plus a guard
  around the user callback, would close all of these at once.
- **Per-model allocation in `rLindo`.** Installing a function callback
  allocates a small block inside `rLindo` that is freed only when another
  callback is installed on the same model, never when the model is deleted.
  A long solve loop with `fn_callback_log` set therefore grows slowly. To be
  fixed in `rLindo` (free the block in `rcLSdeleteModel`); the same applies
  to the standard and MIP callbacks once they are wired.

## Features

- `fn_callback_std` and `fn_callback_mip` are registered as controls but not
  yet installed; as of 0.3-6 only `fn_callback_log` is. Their return-value
  contract (a nonzero integer interrupts the solve) needs a test before they
  are wired. `tests/test_cb.R` carries a `cbFunc` example.
- Constraint directions beyond `<=`, `>=` and `==`: ranges and free rows are
  not supported.
- Cones are not supported (`cones = "X"` in the solver signature). LINDO API
  supports SOC/SDP and ROI can express them.

## Testing

- Re-enable the LP/MILP tests in the run-all block of
  `tests/test_lindoapi.R`; all five pass when run by name.
- Re-enable or delete `test_read_mps`; it self-skips when `LINDOAPI_HOME` is
  unset, so it is safe to enable.
- Make `LSLOCAL` settable from an environment variable instead of by editing
  the test file, keeping `FALSE` as the default.
- No automated regression run; the suite is run by hand.

## Documentation

- `man/` holds only the package page and one example. The controls
  (`use_gop`, `time_limit`, `method`, `verbose`, `reorder_constraints`,
  `fn_callback_log`, the `on_before_optimize` / `on_after_optimize` hooks)
  are documented in `README.md` only.
