.add_status_codes <- function(solver) {
    ROI_plugin_add_status_code_to_db(solver,
                              1L,
                              "OPTIMAL",
                              "Model was solved to optimality (subject to tolerances), and an optimal solution is available.",
                              0L # this is for roi_code. 0 means we have got the solution. 1L is the default value.
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              2L,
                              "BASIC_OPTIMAL",
                              "Model was solved to optimality (subject to tolerances), and a basic optimal solution is available.",
                              0L # this is for roi_code. 0 means we have got the solution. 1L is the default value.
                              )                              
    ROI_plugin_add_status_code_to_db(solver,
                              3L,
                              "INFEASIBLE",
                              "Model was proven to be infeasible."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              4L,
                              "UNBOUNDED",
                              "Model was proven to be unbounded."
                              )                              
     ROI_plugin_add_status_code_to_db(solver,
                              5L,
                              "FEASIBLE",
                              "Model was proven to be feasible (subject to tolerances), and a primal feasible solution is available."
                              )                              
    ROI_plugin_add_status_code_to_db(solver,
                              6L,
                              "INF_OR_UNBD",
                              "Model was proven to be either infeasible or unbounded."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              7L,
                              "SUBOPTIMAL",
                              "Unable to satisfy optimality tolerances; a sub-optimal solution is available."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              8L,
                              "LOCAL_OPTIMAL",
                              "Satisfied optimality tolerances at a local-optimal solution, better solutions could be available. Try global optimization methods or change starting solution."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              9L,
                              "LOCAL_INFEASIBLE",
                              "Model stopped at an infeasible solution due to inability to move within selected method's iterative framework. Try global optimization methods or change starting solution."
                              )                                                            
    ROI_plugin_add_status_code_to_db(solver,
                              10L,
                              "CUTOFF",
                              "Optimal objective for model was proven to be worse than the value specified in the Cutoff parameter. No solution information is available."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              11L,
                              "NUMERIC",
                              "Optimization was terminated due to unrecoverable numerical difficulties."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              12L,
                              "UNKNOWN",
                              "Model and solution status is unknown possibly due to user-interrupt, check the error code returned by the optimizer."
                              )                              
    ROI_plugin_add_status_code_to_db(solver,
                              13L,
                              "UNLOADED",
                              "Model and any solution of it is unloaded."
                            )                              
    ROI_plugin_add_status_code_to_db(solver,
                              14L,
                              "LOADED",
                              "Model is loaded, but no solution information is available."
                            )                              
    invisible(TRUE)
}









































