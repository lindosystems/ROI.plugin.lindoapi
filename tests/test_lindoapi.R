### Test script for the ROI.plugin.lindoapi package

## LSLOCAL is a flag to indicate if the package is to be loaded locally from the source code.
## This is useful for tracing the ROI.plugin.lindoapi src. Default is FALSE.
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

mytol <- 1e-4

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    warning(msg)
    return(invisible(NULL))
}

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

## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0
test_lp_01 <- function(solver, control) {
    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            maximum = TRUE)

    sol <- c(0, 20/3, 50/3)
    
    opt <- ROI_solve(x, solver = solver, control)
    check("LP-01@01", myequal(opt$solution, sol, tol = mytol))
    check("LP-01@02", myequal(opt$objval, 230/3, tol = mytol))
}

## Test if ROI can handle empty constraint matrix.
test_lp_02 <- function(solver, control) {
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L=matrix(0, nrow=0, ncol=3), 
                                       dir=character(), rhs=double()),
            maximum = FALSE)

    sol <- c(0, 0, 0)

    opt <- ROI_solve(x, solver = solver, control)
    check("LP-02@01", myequal(opt$solution, sol, tol = mytol))
    check("LP-02@02", myequal(opt$objval, 0, tol = mytol))
}

## Test if ROI can handle when the constraint is equal to NULL.
test_lp_03 <- function(solver, control) {
    x <- OP(objective = c(2, 4, 3), constraints = NULL, maximum = FALSE)

    sol <- c(0, 0, 0)

    opt <- ROI_solve(x, solver = solver, control)
    check("LP-03@03", myequal(opt$solution, sol, tol = mytol))
    check("LP-03@03", myequal(opt$objval, 0, tol = mytol))
}

## MILP - Example - 1
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0, z >= 2, x <= 4, y <= 100
test_milp_01 <- function(solver, control) {
    obj <- c(3, 1, 3)
    A <- rbind(c(-1,  2,  1),
               c( 0,  4, -3),
               c( 1, -3,  2))
    b <- c(4, 2, 3)
    bounds <- V_bound(li = c(1L, 3L), ui = c(1L, 2L),
                      lb = c(-Inf, 2), ub = c(4, 100))

    x <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         bounds = bounds,
         maximum = TRUE)

    sol <- c(4, 2.5, 3)

    opt <- ROI_solve(x, solver = solver, control)
    
    check("MILP-01@01", all(A %*% opt$solution <= b))
    check("MILP-01@04", myequal(opt$solution , sol, tol = mytol))
}


## MILP - Example - 2
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
test_milp_02 <- function(solver, control) {
    obj <- c(3, 1, 3)
    A <- rbind(c(-1,  2,  1),
               c( 0,  4, -3),
               c( 1, -3,  2))
    b <- c(4, 2, 3)

    x <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         maximum = TRUE)

    sol <- c(5, 2.75, 3)

    opt <- ROI_solve(x, solver = solver, control)
    check("MILP-02@01", all(A %*% opt$solution <= b))
    check("MILP-02@04", myequal(opt$solution , sol, tol = mytol))
}


## QP - Example - 1
##
## from the quadprog package
## (c) S original by Berwin A. Turlach R port by Andreas Weingessel
## GPL-3
##
## min: -(0 5 0) %*% x + 1/2 x^T x
## under the constraints:      A^T x >= b
## with b = (-8,2,0)^T
## and      (-4  2  0)
##      A = (-3  1 -2)
##          ( 0  0  1)
## we can use solve.QP as follows:
##
## library(quadprog)
## D <- diag(1, 3)
## d <- c(0, 5, 0)
## A <- cbind(c(-4, -3, 0), 
##            c( 2,  1, 0), 
##            c( 0, -2, 1))
## b <- c(-8, 2, 0)
## 
## sol <- solve.QP(D, d, A, bvec=b)
## deparse(sol$solution)
## deparse(sol$value)
test_qp_01 <- function(solver, control) {

    A <- cbind(c(-4, -3, 0), 
               c( 2,  1, 0), 
               c( 0, -2, 1))
    x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
            L_constraint(L = t(A),
                         dir = rep(">=", 3),
                         rhs = c(-8, 2, 0)))

    opt <- ROI_solve(x, solver = solver, control)
    solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
    check("QP-01@01", myequal(solution(opt), solution, tol = mytol) )
    check("QP-01@02", myequal(solution(opt, "objval"), -2.38095238095238, tol = mytol) )

}


## This Test detects non-conform objective functions.
## minimize 0.5 x^2 - 2 x + y
## s.t. x <= 3
## Type 1:   0.5 x'Qx + c'Lx => c(2, 0)  objval=-2
## Type 2:       x'Qx + c'Lx => c(3, 0)  objval=-3.75
test_qp_02 <- function(solver, control) {

    zero <- .Machine$double.eps * 100
    qo <- Q_objective(Q=rbind(c(1, 0), c(0, zero)), L=c(-2, 1))
    lc1 <- L_constraint(L=matrix(c(1, 0), nrow = 1), dir = "<=", rhs = 3)
    lc2 <- L_constraint(L=matrix(c(1, 0), nrow = 1), dir = ">=", rhs = 0)
    x <- OP(qo, c(lc1, lc2))

    opt <- ROI_solve(x, solver = solver, control)
    solution <- c(2, 0)
    check("QP-02@01", myequal(solution(opt), solution, tol = mytol) )
    check("QP-02@02", myequal(solution(opt, "objval"), -2, tol = mytol) )

}

## as qp_01 but maximize
test_qp_03 <- function(solver, control) {
    A <- cbind(c(-4, -3, 0), 
               c( 2,  1, 0), 
               c( 0, -2, 1))
    x <- OP(Q_objective(-diag(3), L = -c(0, -5, 0)),
            L_constraint(L = t(A),
                         dir = rep(">=", 3),
                         rhs = c(-8, 2, 0)),
            maximum = TRUE)

    opt <- ROI_solve(x, solver = solver, control)
    solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
    check("QP-03@01", myequal(opt$solution, solution, tol = mytol) )
    check("QP-03@02", myequal(opt$objval, 2.38095238095238, tol = mytol) )
}

## Another QP (this is qpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3  x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +    x_3 <= 20
##               x_1 - 3 x_2 +    x_3 <= 30
##
test_qp_04 <- function(solver, control) {
    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    L1 <- matrix(c(-1, 1, 1, 1, -3, 1), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            L_constraint(L = L1, dir = leq(2), rhs = c(20, 30)),
            maximum = TRUE)
    opt <- ROI_solve(x, solver = solver, control)    
    solution <- c(0.13911493553437, 0.598465474706656, 0.898395723872851)
    check("QP-04@01", myequal(solution(opt), solution, tol = mytol) )
    check("QP-04@02", myequal(solution(opt, "objval"), 2.01561652328916, tol = mytol) )    
}

## This test detects if each solver is using the same definition
## for quadratic constraints.
## minimize:    0.5 * (x^2 + y^2)
## subject to:  0.5 * x^2 >= 0.5
##      x, y >= 0
## solution <- c(1, 0)
test_qcqp_01 <- function(solver, control) {
    qo <- Q_objective(Q = diag(2), L =  numeric(2))
    qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
    x <- OP(qo, qc)

    sol <- c(1, 0)

    opt <- ROI_solve(x, solver = solver, control)
   
    ## local_opts <- list( algorithm = "NLOPT_LD_LBFGS", xtol_rel  = 1e-4 )
    ## opt <- ROI_solve(x, solver="nloptr", start=c(2, 2), method="NLOPT_LD_MMA")

    check("QCQP-01@01", myequal(opt$solution, sol) )
    check("QCQP-01@02", myequal(opt$objval, 0.5) )
}

## QCP (this is qcpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3 x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +   x_3   <= 20
##               x_1 - 3 x_2 +   x_3   <= 30
##               1/2 (2 x_1^2 + 2 x_2^2 + 2 x_3^2) <= 1
test_qcqp_02 <- function(solver, control) {
`    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    QC <- list(NULL, NULL, diag(2, nrow = 3))
    LC <- matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            Q_constraint(Q = QC, L = LC, dir = leq(3), rhs = c(20, 30, 1)),
            maximum = TRUE)`
    
    opt <- ROI_solve(x, solver = solver, control)#, method = "lpopt")

    solution <- c(0.12912360513025, 0.549952824880058, 0.825153905632591)
    check("QCQP-02@01", myequal(solution(opt), solution, tol = mytol) )
    check("QCQP-02@02", myequal(solution(opt, "objval"), 2.00234664731505, tol = mytol) )  
}

test_qcqp_03 <- function(solver, control) {
    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    QC <- list(NULL, NULL, diag(2, nrow = 3))
    LC <- matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            Q_constraint(Q = QC, L = LC, dir = leq(3), rhs = c(20, 30, 1)),
            types = c("B", "C", "I"),
            maximum = TRUE)
    
    opt <- ROI_solve(x, solver = solver, control)#, method = "lpopt")
    solution(opt)

    solution <- c(0, 0.090909094928918, 0)
    check("QCQP-02@01", myequal(solution(opt), solution, tol = mytol) )
    check("QCQP-02@02", myequal(solution(opt, "objval"), 0.0909090909090907, tol = mytol) )  
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

## Test reading MPS files
solve_read_mps <- function(solver, probname, control) {    
    probfile <- as.character(probname)
    print(probfile)
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

### Callback function to act on rEnv and rModel in 'rLindo' style before the optimization starts
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param control A list of control parameters.
## @remark This function is called right before the optimization starts, this means
## no other changes are made to the model after this function is called.
on_before_optimize <- function(rEnv, rModel, control)
{
    if ( is.null(rEnv) || is.null(rModel) ) return(invisible(NULL))

    if (!is.null(control$verbose) && control$verbose) {
        cat(">>> on_before_optimize acting on rEnv and rModel\n")
    }    
    ###############################
    ## Insert your code here
    ###############################
    
    ## e.g. Display model stats
    numVars <- rLSgetIInfo(rModel,LS_IINFO_NUM_VARS)[2]$pnResult
    numCont <- rLSgetIInfo(rModel,LS_IINFO_NUM_CONT)[2]$pnResult
    modelType <- rLSgetIInfo(rModel,LS_IINFO_MODEL_TYPE)[2]$pnResult
    if (!is.null(control$verbose) && control$verbose) {
        cat(sprintf(">>> Model has %d variables, %d continuous variables and has a model-id '%d'\n", numVars, numCont, modelType))
    }

    ## e.g. write an MPS file
    if (0>1) {
        filename <- "on_before_test.mps"
        nErr = rLSwriteMPSFile(rModel, filename, LS_UNFORMATTED_MPS)$ErrorCode    
        if (!is.null(control$verbose) && control$verbose) {    
            if (nErr==0) {
                cat(">>> Model written to file: ", filename, "\n")
            } else {
                cat(">>> Error writing model to file: ", filename, "\n")
            }
        }
    }

    ## e.g. display the parameters which are set to non-default values right before the optimization starts
    if (2>1) {
        cat("\n>>> Displaying non-default parameters\n")
        cnt <- ROI_registered_solver_control(solver)
        for (i in 1:nrow(cnt)) {
            par_key <- as.character(cnt$control[i])
            r <- rLSgetParamMacroID(rEnv, par_key)
            if (r$ErrorCode != 0 && grepl("LS_", par_key)) {
                cat(sprintf(">>> Error getting parameter id for %s\n", par_key))
                next
            } else {
                par_id = r$pnParam
                if (grepl("LS_IPARAM", par_key)) {
                    par_e <- rLSgetEnvIntParameter(rEnv, par_id)
                    par_m <- rLSgetModelIntParameter(rModel, par_id)
                    if (!is.null(par_e$pnValue) && !is.na(par_e$pnValue) && 
                        !is.null(par_m$pnValue) && !is.na(par_m$pnValue) && 
                        par_m$pnValue != par_e$pnValue) {
                        cat(sprintf(">>> %s = %d (default = %d)\n", par_key, par_m$pnValue, par_e$pnValue))
                    }
                } else if (grepl("LS_DPARAM", par_key)) {
                    par_e <- rLSgetEnvDouParameter(rEnv, par_id)
                    par_m <- rLSgetModelDouParameter(rModel, par_id)
                    if (!is.null(par_e$pdValue) && !is.na(par_e$pdValue) && 
                        !is.null(par_m$pdValue) && !is.na(par_m$pdValue) && 
                        par_m$pdValue != par_e$pdValue) {
                        cat(sprintf(">>> %s = %g (default = %g)\n", par_key, par_m$pdValue, par_e$pdValue))
                    }
                }
            }
        }
    }

    return(invisible(NULL))
}

find_iis <- function(rModel, iis_level=1+2) {
    res <- rLSfindIIS(rModel,iis_level)
    if (res$ErrorCode == 0) {
        cat("IIS found\n")
        res <- rLSwriteIIS(rModel,"iis.ilp")
        if (res$ErrorCode == 0) {
            cat("IIS written to iis.ilp\n")
        } else {
            cat("Error writing IIS\n")
        }
        res <- rLSgetIIS(rModel) 
        if (res$ErrorCode == 0) {
            cat("IIS retrieved\n")
            print(res)
            cat(sprintf("\n\t ***  LSfindIIS Summary ***\n\n"))
            cat(sprintf("\t Number of Sufficient Rows = %d\n",res$pnSuf_r))
            cat(sprintf("\t Number of Sufficient Cols = %d\n",res$pnSuf_c))
            cat(sprintf("\t Number of Necessary  Rows = %d\n",res$pnIIS_r - res$pnSuf_r))
            cat(sprintf("\t Number of Necessary  Cols = %d\n",res$pnIIS_c - res$pnSuf_c))            
            # Print the IIS rows
            for (i in 1:res$pnIIS_r) {
                cat(sprintf("\t IIS Row %d: %s\n",i,res$paiCons[i]))
            }
            cat("\n")
            # Print the IIS columns
            for (i in 1:res$pnIIS_c) {
                cat(sprintf("\t IIS Col %d: %s\n",i,res$paiVars[i]))
            }            
        } else {
            cat("Error retrieving IIS\n")
        }
    }
    return(res)
}

### Callback function to act on rEnv and rModel in 'rLindo' style after the optimization ends
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param control A list of control parameters.
## @param result A list of results.
### @remark This function is called right after the optimization ends, this means
## no other changes are made to the model before this function is called.
on_after_optimize <- function(rEnv, rModel, control, result)
{
    if ( is.null(rEnv) || is.null(rModel) ) return(invisible(NULL))

    if (!is.null(control$verbose) && control$verbose) {
        cat(">>> on_after_optimize acting on rEnv and rModel\n")
    }    
    ###############################
    ## Insert your code here
    ###############################
    if (result$status == LS_STATUS_INFEASIBLE) {
        cat(">>> Model is infeasible\n")
        res <- find_iis(rModel)
    } else if (result$status == LS_STATUS_UNBOUNDED) {
        cat(">>> Model is unbounded\n")
    } else if (result$status == LS_STATUS_OPTIMAL) {
        cat(">>> Model is optimal\n")
        ## e.g. write a solution file
        solfile <- "on_after_test.sol"
        nErr = rLSwriteSolution(rModel, solfile)$ErrorCode    
        if (!is.null(control$verbose) && control$verbose) {
            if (nErr==0) {
                cat(">>> Solution written to file: ", solfile, "\n")
            } else {
                cat(">>> Error writing solution to file: ", solfile, "\n")
            }        
        }
    } else if (result$status == LS_STATUS_FEASIBLE) {
        cat(">>> Model is feasible\n")
    } else {
        cat(">>> Model is not solved\n")
    }

    return(invisible(NULL))
}

source("test_cb.R")

solver <- "lindoapi"
if ( !any(solver %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat(sprintf("ROI.plugin.%s is not registered.\n", solver))
} else {
    print("Start Testing!")
    control <- list()
    # ROI-like control parameters
    control$time_limit <- 60
    control$use_gop <- TRUE
    control$method <- LS_METHOD_FREE
    control$verbose <- TRUE
    
    # Native Lindo parameters
    control$LS_DPARAM_SOLVER_FEASTOL <- 1e-6
    control$LS_DPARAM_SOLVER_OPTTOL <- 1e-6
    control$LS_DPARAM_SOLVER_TIMLMT <- 100    

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
        message("No test specified, running all.")
        if (0>1) {
            local({test_lp_01(solver, control)})
            local({test_lp_02(solver, control)})
            local({test_lp_03(solver, control)})
            local({test_milp_01(solver, control)})
            local({test_milp_02(solver, control)})
        }

        if (2>1) {
            control$method <- LS_METHOD_BARRIER ## switch to barrier method
            control$use_gop <- TRUE ## use global optimization to solve non-convex QPs
            local({test_qp_01(solver, control)})
            local({test_qp_02(solver, control)})
            local({test_qp_03(solver, control)})
            local({test_qp_04(solver, control)})
            local({test_qcqp_01(solver, control)})
            #local({test_qcqp_02(solver, control)})
            local({test_qcqp_03(solver, control)})
        }

        if (2>1) {
            ##local({test_read_mps(solver, control)})
            local({test_write_mps(solver, control)})
        }
    } else {
        # Use the first argument as the file path
        test_name <- args[1]
        cat("Test name provided:", test_name, "\n")
        if (grepl("qp", test_name)) {
            control$method <- LS_METHOD_BARRIER ## switch to barrier method
            control$use_gop <- TRUE ## use global optimization to solve non-convex QPs
        }        
        if (test_name == "test_lp_01") {
            local({test_lp_01(solver, control)})
        } else if (test_name == "test_lp_02") {
            local({test_lp_02(solver, control)})
        } else if (test_name == "test_lp_03") {
            local({test_lp_03(solver, control)})
        } else if (test_name == "test_milp_01") {
            local({test_milp_01(solver, control)})
        } else if (test_name == "test_milp_02") {
            local({test_milp_02(solver, control)})
        } else if (test_name == "test_qp_01") {
            local({test_qp_01(solver, control)})
        } else if (test_name == "test_qp_02") {
            local({test_qp_02(solver, control)})
        } else if (test_name == "test_qp_03") {
            local({test_qp_03(solver, control)})
        } else if (test_name == "test_qp_04") {
            local({test_qp_04(solver, control)})
        } else if (test_name == "test_qcqp_01") {
            local({test_qcqp_01(solver, control)})
        } else if (test_name == "test_qcqp_02") {
            local({test_qcqp_02(solver, control)})
        } else if (test_name == "test_qcqp_03") {
            local({test_qcqp_03(solver, control)})
        } else if (test_name == "test_read_mps") {
            local({test_read_mps(solver, control)})
        } else if (test_name == "solve_read_mps") {
            probname = args[2]
            if (length(probname) == 0) {
                message("No problem name provided. Exiting..")
                return(invisible(NULL))
            }
            local({solve_read_mps(solver, probname, control)})            
        } else if (test_name == "test_write_mps") {
            local({test_write_mps(solver, control)})
        } else {
            message("Specified test not found..")
        }
    }

}

