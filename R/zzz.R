make_lindoapi_signatures <- function()
    ROI_plugin_make_signature( objective = c("L", "Q"),
                               constraints = c("X", "L", "Q"),
                               types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )

### Register the control parameters
### This function is called when the package is loaded
## @param solver The name of the solver
.add_controls <- function(solver) {

    # Register as non-LINDO (generic/aux) control parameters
    ROI_plugin_register_solver_control(solver, "use_gop", "X")
    ROI_plugin_register_solver_control(solver, "time_limit", "X")
    ROI_plugin_register_solver_control(solver, "method", "X")
    ROI_plugin_register_solver_control(solver, "verbose", "X")

    # Register callbacks as LINDO API control parameters
    ROI_plugin_register_solver_control(solver, "on_before_optimize", "X")  # Before optimization callback
    ROI_plugin_register_solver_control(solver, "on_after_optimize", "X")  # After optimization callback
    ROI_plugin_register_solver_control(solver, "fn_callback_std", "X") # Standard callback
    ROI_plugin_register_solver_control(solver, "fn_callback_log", "X") # Log callback    
    ROI_plugin_register_solver_control(solver, "fn_callback_mip", "X") # MIP callback (every time a new MIP solution is found)
    ROI_plugin_register_solver_control(solver, "fn_callback_fox", "X") # F(x), Function (objective and constraints)
    ROI_plugin_register_solver_control(solver, "fn_callback_jox", "X") # J(x), Jacobian


    # Collect rest of parameters from LINDO enviroment object
    rEnv <- rLScreateEnv()

    #Specify control parameters range
    ib <- 1000
    ie <- 10000
    # Initialize empty lists
    ls_parkey <- c()  # Using a vector to store parameter names
    ls_parnum <- list()  # Using a list to map names to integer identifiers

    # Loop through the parameter index range, empty slots are ignored (ErrorCode > 0)
    for (i in ib:ie) {
        # retrieve parameter names from LINDO API environment
        r <- rLSgetParamMacroName(rEnv, i)

        # Check for successful retrieval
        if (r$ErrorCode == 0) {
            # Append parameter name to ls_parkey only if a valid parameter name is returned
            ls_parkey <- c(ls_parkey, r$szParam)

            # Map parameter name to its integer identifier in ls_parnum
            ls_parnum[[r$szParam]] <- i
        }
    }
    #Delete the  environment
    rLSdeleteEnv(rEnv)

    # Register controls
    for (i in seq_along(ls_parkey)) {
        ROI_plugin_register_solver_control(solver, ls_parkey[i], "X")
    }
    cat(sprintf("Registered %d LINDO API parameters as controls for ROI.plugin.%s\n", length(ls_parkey), solver))
    debug = FALSE
    if (debug) {
        for (i in seq_along(ls_parkey)) {
            par_key <- ls_parkey[i]
            par_id <- ls_parnum[par_key]
            if (grepl("LS_IPARAM", par_key)) {
                par <- rLSgetEnvIntParameter(rEnv,par_id)
                cat((paste(ls_parkey[i], " ( ", par_id, "); default.value = ", par$pnValue, "\n")))
            } else if (grepl("LS_DPARAM", par_key)) {
                par <- rLSgetEnvDouParameter(rEnv,par_id)
                cat((paste(ls_parkey[i], " ( ", par_id, "); default.value = ", par$pdValue, "\n")))
            } 
        }
    }
    invisible( TRUE )
}

## Register i/o functions
## @param solver The name of the solver
.add_reader_writer <- function(solver) {
    ROI_plugin_register_reader("lindo_io", solver, lindoapi_read_op)

    ROI_plugin_register_writer("lindo_io", solver, make_lindoapi_signatures(), lindoapi_write_op)

    invisible(NULL)
}

## Only register the solver if it is not already registered
register_solver <- function(libname, pkgname, LSLOCAL = FALSE) {
    solver <- "lindoapi"
    ## Solver plugin name (based on package name)
    registered_solvers <- ROI_registered_solvers()

    ## Only register if the solver is not already registered
    if (!solver %in% registered_solvers) {
        message("Registering solver '", solver, "'")
        
        ## Register solver methods here.
        method <- if (LSLOCAL) solve_OP else getFunction("solve_OP", where = getNamespace(pkgname))

        ROI_plugin_register_solver_method(
            signatures = make_lindoapi_signatures(),
            solver = solver,
            method = method  # Use the locally defined solve_OP
        )

        ## Finally, for status code canonicalization add status codes to data base
        .add_reader_writer(solver)
        .add_status_codes(solver)
        .add_controls(solver)
    } else {
        message("Solver '", solver, "' is already registered. Skipping registration.")
    }
}


## Only register the solver if it is not already registered
.onLoad <- function(libname, pkgname, LSLOCAL = FALSE) {
    message("Loading package ", pkgname)
    register_solver(libname, pkgname, LSLOCAL)    
    invisible(NULL)
}
