#' Instruction file
#'
#' A training data instruction set
#'
#' @format A data frame with 19 rows and 4 or 5 columns
#' \describe{
#'    \item{Type}{Id number (character)}
#'    \item{Transition}{Variable names}
#'    \item{Duration}{Variable value (character)}
#'    \item{mode}{Ggives the storage mode
#'    for each variable. Possible values are "numeric",
#'    "factor", "character", and "date". If not present, all variables
#'    in the episodes data frame are stored in character mode.}
#'    \item{variable}{Variable name corresponding to event}
#' }
"instruction"
