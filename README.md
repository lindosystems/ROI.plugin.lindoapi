# ROI.plugin.lindoapi

## Installation Requirements
Before installing `ROI.plugin.lindoapi`, make sure you have the [LINDO API](https://lindo.com/index.php/ls-downloads/try-lindo-api) and its corresponding R package `rLindo` installed.

### Step 1: Install `rLindo`
To install `rLindo`, refer to the instructions on the [lindoapi-R GitHub page](https://github.com/lindosystems/lindoapi-R). This package provides the R interface for the LINDO API, which is necessary for `ROI.plugin.lindoapi` to work.

### Step 2: Install `ROI.plugin.lindoapi`
Once `rLindo` is installed, you can install `ROI.plugin.lindoapi` directly from GitHub. 

#### - Build within R
Run the following command from within R:

```r
	# Install the remotes package if you haven�t already
	> install.packages("remotes")

	# Install ROI.plugin.lindoapi from GitHub
	> remotes::install_github("lindosystems/ROI.plugin.lindoapi")

```

#### - Alternative: Manual Build & Install
If you want to manually build and install the package:

1. **Clone source and navigate to the directory** containing your package source:
   ```sh
   $ git clone https://github.com/lindosystems/ROI.plugin.lindoapi.git
   $ cd ROI.plugin.lindoapi
   ```

2. **Build the package** (creates a `.tar.gz` file):
   ```sh
   $ R CMD build .
   ```

3. **Install the built package**:
   ```sh
   $ R CMD INSTALL ROI.plugin.lindoapi_*.tar.gz
   ```

### Step 3: Verification
After installation, verify that the package is correctly installed and loaded:
```r
	> library(ROI.plugin.lindoapi)
```

Optionally, run the test script
```sh
	$ cd tests
	$ Rscript test_lindoapi.R
```	


This should load the package without errors.

## Release Notes

1. `ROI.plugin.lindoapi` supports (mixed-integer) LP and QP models. 

2. Global optimization of (mixed-integer) non-convex QPs is available via the 'control$use_gop' option. 

3. Control parameter macros `LS_IPARAM_XXX` and `LS_DPARAM_XXX` are native to LINDO API, and they are registered as ROI control keys in the solver database. 
They can be used to adjust optimization parameters before calling ROI_solve. 

```r
		> control$LS_DPARAM_SOLVER_FEASTOL <- 1e-6
		> control$LS_DPARAM_SOLVER_OPTTOL <- 1e-6
		> control$LS_DPARAM_SOLVER_TIMLMT <- 100
		...
```

The set of control parameters in LINDO API is comprehensive, please refer to the official LINDO API user manual for detailed information.

4. Quadratic constraints are loaded after all linear ones. LINDO API attaches quadratic terms to a constraint
by row index, whereas ROI keeps constraints in the order they were supplied, so the two only agree when the
Q-constraints occupy the tail of the constraint list. The plugin now reorders the constraints internally, so a
`Q_constraint` may list them in any order. Duals and slacks are mapped back to the original ROI constraint
order before they are returned, so `solution(opt, "msg")$pi` and `$slack` stay aligned with the constraints as
you supplied them.

Reordering is on by default. Set `control$reorder_constraints` to `FALSE` to turn it off, in which case a model
whose L-constraints do not all precede its Q-constraints is rejected rather than reordered.

```r
		> control$reorder_constraints <- FALSE   # default is TRUE
```

When a model actually has to be reordered the plugin reports it, naming the permutation applied. A model whose
constraints are already in the required order is reordered by the identity and stays silent. The report is a
`message()`, so `suppressMessages()` silences it when solving in a loop.

```r
		NOTE: L-constraints do not all precede the Q-constraints.
		NOTE: constraints reordered for LINDO as (2, 3, 1).
		NOTE: duals and slacks are mapped back to the ROI order on return.
		NOTE: set control 'reorder_constraints' to FALSE to reject instead.
```

5. Redirecting the LINDO log to a file. By default the LINDO log (model statistics at load time, then the
solver's progress and summary lines) is printed on the R console, because `rLindo` installs a console printer
on every model it creates. The `fn_callback_log` control replaces that printer with your own function. It is
called once per log line as `function(sModel, sLine, sData)`; `sLine` already ends with a newline, and `sData`
is the environment the function is evaluated in. To log to a file, open a connection before the solve and
write each line to it:

```r
		> con <- file("lindo.log", open = "wt")
		> control$fn_callback_log <- function(sModel, sLine, sData) cat(sLine, file = con)
		> opt <- ROI_solve(x, solver = "lindoapi", control = control)
		> close(con)
```

The callback is installed right after the model is created, so the file receives the load-time output as
well as the solve. Keep the function simple and let nothing inside it signal an error: it runs while the
solver holds the call stack. To silence the log instead, set the control to `FALSE`:

```r
		> control$fn_callback_log <- FALSE   # no LINDO log at all
```

The amount of log written is governed by LINDO's own print-level parameters, which are controls too:
`LS_IPARAM_LP_PRINTLEVEL` (default 0), `LS_IPARAM_MIP_PRINTLEVEL` (default 2) and
`LS_IPARAM_GOP_PRINTLEVEL` (default 1).

Two more routes exist that need no version-specific support. `sink()` around the solve captures the default
console printer, together with everything else R prints, but not the license banner, which bypasses R's
console. And on 0.3-5 and earlier, where `fn_callback_log` is accepted but never installed, the same
function can be installed from the `on_before_optimize` hook, which receives the model object; that hook
runs after the data is loaded, so the load-time statistics still go to the console:

```r
		> library(rLindo)
		> control$on_before_optimize <- function(rEnv, rModel, control) {
		+     rLSsetModelLogfunc(rModel, function(sModel, sLine, sData) cat(sLine, file = con), new.env())
		+     invisible(NULL)
		+ }
```