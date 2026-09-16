# TODO

Open work, roughly by priority. Completed items move to `CHANGES.md`.

## Bugs

- `make_csc_matrix.matrix()` rejects every input. A stray `33` in the type
  guard (`R/plugin.R:13`) leaves `stop()` unconditional. The method is
  unreachable in practice, because `constraints(x)$L` arrives as a
  `simple_triplet_matrix` and dispatches to the other method. Fix the guard
  and add a test that exercises the dense-matrix method.

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
