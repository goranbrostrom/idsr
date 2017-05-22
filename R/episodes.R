#' Create an episodes file from a Chronicle and a Personal data frame.
#'
#' @export
#' @param chronicle The Chronicle (time-varying) data frame.
#' @param personal The  personal (time-fixed) data frame.
#' @param start_event The start event in a survival context.
#' @param end_event The terminal event in a survival context.
#' @return An episodes data frame.

episodes <- function(chronicle,
                     personal,
                     start_event = "Birth",
                     end_event = "Death"){
 
    ## ----get chronichle------------------------------------------------------

    ## ----spread_it-----------------------------------------------------------
    chron <- chronicle[, c("Id_I", "Variable", "Value", "date", "Type")]
    chron <- chron[order(chron$Id_I, chron$date), ]
    chron <- chron[!duplicated(chron), ]
    chron <- tidyr::spread_(chron, key_col = "Variable", 
                            value_col = "Value", convert = TRUE)
    chron <- dplyr::group_by_(chron, "Id_I")
    chron <- tidyr::fill_(chron, names(chron)[-(1:2)])
    chron <- dplyr::ungroup(chron)

    ## ----startdate-----------------------------------------------------------
    starting <- chron[chron$Type == start_event, c("Id_I", "date")]
    indx <- match(chron$Id_I, starting$Id_I)
    chron$start_date <- starting$date[indx]

    ## ------------------------------------------------------------------------
    enter <- as.numeric(chron$date) - as.numeric(chron$start_date)
    chron$enter <- round(enter / 365.2425, 3) # 3 decimals is enough

    ## ------------------------------------------------------------------------
    indx <- tapply(chron$Id_I, chron$Id_I)
    no <- unlist(tapply(chron$Id_I, chron$Id_I, function(x) 1:length(x)))
    norec <- tapply(chron$Id_I, chron$Id_I, length)[indx]

    ## ----addexit-------------------------------------------------------------
    chron$exit <- c(chron$enter[-1], NA)
    chron$exit[no == norec] <- NA

    ## ------------------------------------------------------------------------
    erows <- which(chron$Type == end_event) - 1
    chron$event <- FALSE
    chron$event[erows] <- TRUE
    
    ## ----cleanup-------------------------------------------------------------
    chron <- chron[!is.na(chron$present) & (chron$present == "yes"), ]
    chron <- chron[chron$alive == "yes", ]
    remove <- !chron$event & chron$enter == chron$exit
    chron <- chron[!remove, ]
    chron$present <- chron$date <- chron$Type <- chron$alive <- NULL
    ##chron <- chron[, c("Id_I", "start_date", "enter", "exit", "event", "civil_status", "occupation")]
    ##chron$civil_status <- factor(chron$civil_status)
    ##chron$occupation <- factor(chron$occupation)

    ## ----personal------------------------------------------------------------

    ## ----spreadpersonal------------------------------------------------------
    personal <- tidyr::spread_(personal, key_col = "Type",
                               value_col = "Value", convert = TRUE)


    ## ----joinall-------------------------------------------------------------
    indx <- match(chron$Id_I, personal$Id_I)
    vars <- names(personal)[-1]
    for (i in vars){
        chron[i] <- personal[[i]][indx]
    }

    chron
}

