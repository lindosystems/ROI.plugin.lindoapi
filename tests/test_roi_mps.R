### Test script for the ROI.plugin.lindoapi package

## CHANGES
## Moved on_before_optimize and on_after_optimize to test_cb.R
Sys.setenv("ROI_LOAD_PLUGINS" = FALSE)

library(ROI)
library(Matrix)
library(slam)
LSLOCAL <- FALSE
Sys.setenv("ROI_LOAD_PLUGINS" = FALSE)
library(ROI)
if (LSLOCAL==FALSE) {
    library(ROI.plugin.lindoapi)
} else {
    cat("DEBUG_MODE: Loading the package files manually\n")
    source("../R/io.R")
    source("../R/plugin.R")
    source("../R/status_codes.R")
    source("../R/zzz.R")
    .onLoad(libname = NULL, pkgname = "ROI.plugin.lindoapi", LSLOCAL)
}
library(rLindo)

source("test_cb.R")

# Now write the rest of your test code here
# For example:
cat("Running tests for ROI.plugin.lindoapi...\n")
mytol <- 1e-4

# Define the new wrapper function
myequal <- function(actual, expected, tol = 1e-08, verbose = TRUE) {
  
  # Call the original equal() function with the same arguments
  result <- equal(actual, expected, tol = tol)
  # If verbose is TRUE, print the arguments for debugging
  if (verbose) {
    cat("Comparing:\n")
    cat("  Actual: ", actual, "\n")
    cat("  Expected:", expected, "\n")
    cat("  Tolerance:", tol, "\n")
    cat("  Result:", result, "\n")
  }
  # Return the result
  return(result)
}


## Test reading MPS files
test_read_mps <- function(solver, control) {
    probdir <- Sys.getenv("LINDOAPI_HOME")
    if (nchar(probdir) == 0) {
        cat("LINDOAPI_HOME is not set. Skipping test_read_mps.\n")
        return(invisible(NULL))
    }
    probfile <- as.character(file.path(probdir, "samples", "data", "testqp.mps"))
    x <- ROI_read(probfile, "lindo_io")
    opt <- ROI_solve(x, solver = solver, control)

    probfile <- as.character(file.path(probdir, "samples", "data", "testmip.mps"))
    x <- ROI_read(probfile, "lindo_io")
    opt <- ROI_solve(x, solver = solver, control)    
}

## Test writing MPS files
test_write_mps <- function(solver, control) {
    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    QC <- list(NULL, NULL, diag(2, nrow = 3))
    LC <- matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            Q_constraint(Q = QC, L = LC, dir = leq(3), rhs = c(20, 30, 1)),
            maximum = TRUE)    
    probdir <- Sys.getenv("LINDOAPI_HOME")
    if (nchar(probdir) == 0) {
        cat("LINDOAPI_HOME is not set. Skipping test_write_mps.\n")
        return(invisible(NULL))
    }
    probfile <- as.character(file.path(probdir, "samples", "data", "roi_test_write.mps"))
    ROI_write(x, probfile, "lindo_io")
    ROI_solve(x, solver = solver, control)
}


solver <- "lindoapi"
if ( !any(solver %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat(sprintf("ROI.plugin.%s is not registered.\n", solver))
} else {
    print("Start Testing!")
    control <- list()
    # ROI-like control parameters
    control$time_limit <- 60
    control$use_gop <- FALSE
    control$method <- LS_METHOD_FREE
    control$verbose <- TRUE
    
    # Native Lindo parameters
    control$LS_DPARAM_SOLVER_FEASTOL <- 1e-6
    control$LS_DPARAM_SOLVER_OPTTOL <- 1e-6
    control$LS_DPARAM_SOLVER_TIMLMT <- 100   
    #control$LS_IPARAM_MIP_PRELEVEL <- 0 

    # Callback functions
    control$on_before_optimize <- on_before_optimize
    control$on_after_optimize <- on_after_optimize
    control$fn_callback_std <- cbFunc # from tests/test_cb.R
    control$fn_callback_log <- logFunc # from tests/test_cb.R    

    # Retrieve command-line arguments
    args <- commandArgs(trailingOnly = TRUE)

    # Print the arguments to debug
    cat("Command-line arguments:", args, "\n")

    # Check if the file path argument is provided
    if (length(args) == 0) {
      stop("\n\tUsage: Rscript test_roi_io.R <file>")
    }

    # Use the first argument as the file path
    file <- args[1]

    # Print the file path to verify the argument is passed correctly
    cat("File path provided:", file, "\n")

    x <- lindoapi_read_op(file, control)
	sol <- ROI_solve(x, solver = solver, control)    
    print(sol$message)
}
