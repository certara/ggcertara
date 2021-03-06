#' Sample model output table.
#'
#' This is an example of an output table as would typically be generated by
#' pharmacometric modeling software such as NONMEM
#'
#' @format A \code{data.frame} with 20 columns and 960 rows:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{TIME}{Time}
#'   \item{DV}{Dependent variable}
#'   \item{PRED}{Population prediction}
#'   \item{IPRED}{Individual prediction}
#'   \item{RES}{Residual}
#'   \item{WRES}{Weighted residual}
#'   \item{CWRES}{Conditional weighted residual}
#'   \item{CIWRES}{Conditional individual weighted residual}
#'   \item{IRES}{Individual residual}
#'   \item{IWRES}{Individual weighted residual}
#'   \item{MDV}{Missing dependent value indicator}
#'   \item{EVID}{Event type identifier}
#'   \item{TAD}{Time after dose}
#'   \item{CL}{Individual estimated clearance}
#'   \item{V}{Individual estimated volume of distribution}
#'   \item{KA}{Individual estimated absorption rate constant}
#'   \item{ETA1}{Individual estimated random effect on CL}
#'   \item{ETA2}{Individual estimated random effect on V}
#'   \item{ETA3}{Individual estimated random effect on KA}
#' }
"nmtable"
