#' VarSetup file
#'
#' A training data set for debugging.
#'
#' @format A data frame with 19 rows and 3 or 4 columns
#' \describe{
#'    \item{Type}{Id number (character)}
#'    \item{Transition}{Variable names}
#'    \item{Duration}{Variable value (character)}
#'    \item{mode}{Optional; if present it gives the storage mode
#'    for each variable. Possible values are "numeric",
#'    "factor", "character", and "date". If not present, all variables
#'    in the episodes data frame are stored in character mode.}
#' }
"varset"
