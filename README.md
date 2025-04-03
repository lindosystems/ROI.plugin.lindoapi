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