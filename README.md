# ROI.plugin.lindoapi

## Installation Requirements
Before installing `ROI.plugin.lindoapi`, make sure you have the [LINDO API](https://lindo.com/index.php/ls-downloads/try-lindo-api) and its corresponding R package `rLindo` installed.

### Step 1: Install `rLindo`
To install `rLindo`, refer to the instructions on the [lindoapi-R GitHub page](https://github.com/lindosystems/lindoapi-R). This package provides the R interface for the LINDO API, which is necessary for `ROI.plugin.lindoapi` to work.

### Step 2: Install `ROI.plugin.lindoapi`
Once `rLindo` is installed, you can install `ROI.plugin.lindoapi` directly from GitHub. Run the following command from within R:

```r
# Install the remotes package if you haven�t already
install.packages("remotes")

# Install ROI.plugin.lindoapi from GitHub
remotes::install_github("lindosystems/ROI.plugin.lindoapi")
```
## Release Notes

1. `ROI.plugin.lindoapi` supports (mixed-integer) LP and QP models. 

2. Global optimization of (mixed-integer) non-convex QPs is available via the 'use_gop' option. 

3. `LS_IPARAM_XXX` and `LS_DPARAM_XXX` macros, native to LINDO API, are registered as control keys in the solver database. 
They can be used to adjust optimization parameters before calling ROI_solve. 

```r
		control$LS_DPARAM_SOLVER_FEASTOL <- 1e-6
		control$LS_DPARAM_SOLVER_OPTTOL <- 1e-6
		control$LS_DPARAM_SOLVER_TIMLMT <- 100
		...
```	