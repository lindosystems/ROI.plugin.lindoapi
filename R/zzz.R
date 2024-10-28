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
    cat((paste("Registered ", length(ls_parkey), " controls for ", solver, "\n")))  
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

.onLoad <- function(libname, pkgname) {
    solver <- "lindoapi"
    ## Solver plugin name (based on package name)
    registered_solvers <- ROI_registered_solvers()

    ## Only register if the solver is not already registered
    if (!solver %in% registered_solvers) {
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        ROI_plugin_register_solver_method(
            signatures = make_lindoapi_signatures(),
            solver = solver,
            method = getFunction("solve_OP", where = getNamespace(pkgname))
        )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes(solver)
        .add_controls(solver)
    } else {
        message("Solver '", solver, "' is already registered. Skipping registration.")
    }
}
