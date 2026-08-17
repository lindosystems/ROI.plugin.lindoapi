# TODO

Open work, roughly by priority. Completed items move to `CHANGES.md`.

## Bugs

- **`make_csc_matrix.matrix()` rejects every input.** [`R/plugin.R:13`] A stray
  `33` swallowed the body of the type check, leaving `stop()` unconditional:

  ```r
  if(!is.matrix(x))33
      stop("Argument 'x' must be a matrix.")
  ```

  Verified: `make_csc_matrix.matrix(matrix(c(1,0,0,2), 2))` errors with
  "Argument 'x' must be a matrix." on a perfectly good matrix. The method is
  currently unreachable in practice, because `constraints(x)$L` arrives as a
  `simple_triplet_matrix` and dispatch goes to the other method -- which is why
  this has gone unnoticed. Fix the guard and add a test that dispatches here on
  a dense matrix, so the method is actually exercised.

## Testing

- **Re-enable the LP/MILP tests in the "run all" block.** [`tests/test_lindoapi.R:643`]
  `test_lp_01..03` and `test_milp_01..02` sit behind `if (0>1)`. All five pass
  when invoked by name, so there is no reason to keep them off.
- **Re-enable or delete `test_read_mps`.** [`tests/test_lindoapi.R:670`] Commented
  out in the runner. It self-skips when `LINDOAPI_HOME` is unset, so it is safe
  to enable.
- **Make `LSLOCAL` settable without editing the file.** [`tests/test_lindoapi.R:11`]
  It is hardcoded `FALSE`, so testing the source tree instead of the installed
  package means editing the test file. Reading an environment variable, keeping
  `FALSE` as the default, would make source-tree runs repeatable.
- **No automated regression run.** The suite is run by hand. Nothing catches a
  repeat of the 0.3-2 guard bug, which shipped and reached a customer.

## Features / design

- **Confirm the customer's 16.0 report is fully closed.** The 0.3-2 guard crash
  aborted before `rLSloadLPData()` was ever called, so it would have masked any
  genuine downstream issue. Ask for a retest on 0.3-5 before closing.
- **Constraint types beyond `leq`/`geq`/`eq`.** `map_sense()` handles the three
  ROI directions; ranges and free rows are not covered.
- **Cones are unsupported.** `make_lindoapi_signatures()` registers
  `cones = "X"` [`R/zzz.R:6`]. LINDO API supports SOC/SDP; ROI can express them.

## Documentation

- `man/` holds only `ROI.plugin.lindoapi-package.Rd` and `Example_01.Rd`. The
  controls (`use_gop`, `time_limit`, `method`, `verbose`,
  `reorder_constraints`, the callbacks) are documented only in `README.md`.
