#' Create an episodes file from a Chronicle and a VarSetup file.
#'
#' @importFrom magrittr "%>%"
#' @export
#' @param Chronicle The chronicle data frame.
#' @param VarSetup The VarSetup data frame.
#' @return An episodes data frame.
efc <- function(Chronicle, VarSetup){

    ## The 'main' function: Calls part1, part2, ... etc

    ## But first, note that the data frame 'VarSetup' is appended
    ## a column 'mode', which gives the storage mode of variables
    ## in the final result, i.e., numeric, logical, factor, etc.

part1 <- function(VarSetup, atrisk = "At_risk", using = "", keep = FALSE){
    ## --- INPUT: ---
    ##
    ## From variable list (not disk):
    ## VarSetup.dta
    ##
    ## --- OUTPUT: ---
    ##
    ## VarSetup1
    ## TypeTransition
    ## TypeDuration
    ## TypeReplaceMini1
    ## ValueLabel
    ##
    ## These objects are (with the exception of 'ValueLabel')
    ## (i) Returned
    ## (ii) Written to disk for debugging purposes, if 'keep'.
    ## -------------------------------------------------------

    ## 'atrisk' is a character variable containing the name of the
    ## 'AtRisk' (logical) variable.

    ## Reading and preparing the variable set up file

    ##library(haven) ## For reading stata files

    ##if (file.exists("dta/VarSetup.dta")){
      ##  VarSetup <- read_dta("dta/VarSetup.dta")
    ##}else{
      ##  stop("The file 'dta/VarSetup.dta' does not exist")
    ##}

    ## Checking that the variable setup file contains the correct columns:
    correctC <- c("Type", "Duration", "Transition")
    there <- correctC %in% names(VarSetup)
    if (sum(there) != 3){
        cat("Missing variable(s) in VarSetup:", correctC[!there], "\n")
        stop("Correct and try again")
    }else{
        cat("VarSetup is OK\n")
    }

    ## Find Type = 'AtRisk'

    VarSetup$Type <- with(VarSetup, replace(Type, Type == atrisk, "AtRisk"))

    VarSetup1 <- VarSetup
    if (keep){
        save(VarSetup1, file = "VarSetup1.rda")
    }

    ## Transition:
    ## TypeTransition <- dplyr::select_(VarSetup1, Type, Transition)
    TypeTransition <- VarSetup1[, c("Type", "Transition")]
    if (keep){
        save(TypeTransition, file = "TypeTransition.rda")
    }

    ## Duration:
    ##TypeDuration <- dplyr::filter(VarSetup1, Transition != "End" &
      ##                                Duration == "Continuous")
    TypeDuration <- VarSetup1[with(VarSetup1, Transition != "End" &
                                       Duration == "Continuous"), ]
    ##TypeDuration <- dplyr::select(TypeDuration, Type)
    TypeDuration <- TypeDuration[, c("Type")]
    if (keep){
        save(TypeDuration, file = "TypeDuration.rda")
    }

    ## Minus1
    TypeReplaceMin1 <- TypeDuration # ?? Check this!
    if (keep){
        save(TypeReplaceMin1, file = "TypeReplaceMin1.rda")
    }

    ## Value labels ('using')
    #  Skip this for the time being: The file 'ValueLabel' is not created.

    ## Return value:
    list(VarSetup1 = VarSetup1,
         TypeTransition = TypeTransition,
         TypeDuration = TypeDuration,
         TypeReplaceMin1 = TypeReplaceMin1)

}

part2 <- function(Chronicle, atrisk = "At_risk", tt, keep = TRUE){

    ## --- INPUT: ---
    ##
    ## From variable list (not disk):
    ## Chronicle.dta
    ##
    ## From argument list:
    ## atrisk: Name of 'at risk' variable.
    ## tt = TypeTransition (from part1).
    ## keep: Logical, if TRUE, write to disk, see below.
    ##
    ## --- OUTPUT: ---
    ##
    ## TypeDateFormat
    ## ExtractionFile
    ##
    ## These objects are
    ## (i) Returned
    ## (ii) Written to disk for debugging purposes, if 'keep'.
    ## -------------------------------------------------------

    ## READ AND PREPARE THE CHRONICLE FILE
    ##library(haven)
    ##library(dplyr)
    ##if (file.exists("dta/Chronicle.dta")){
      ##  Chronicle <- read_dta("dta/Chronicle.dta")
    ##}else{
      ##  stop("The file 'Chronicle.dta' does not exist!")
    ##}

    ## Check variable names:
    vn <- names(Chronicle)
    if ("ID_I" %in% vn){ # Change "ID_I" --> "Id_I" (if any).
        vn[vn == "ID_I"] <- "Id_I"
        names(Chronicle) <- vn
    }

    mandat <- c("Id_I", "Day", "Month", "Year", "DayFrac", "Type", "Value")
    notPresent <- !(mandat %in% vn)
    if (any(notPresent)){
        cat("Missing variables in 'Chronicle': ", mandat[notPresent], "\n")
        stop("Correct and retry!")
    }else{
        cat("'Chronicle.dta' is OK.\n") # Will be removed later.
    }

    ## Generating DateFormat for Types which have no Value but
    ## the Timestamp is their value. This Stata code:
    ## ----------------------------------------------------
    ## use Chronicle.dta, clear
    ## merge m:1 Type using TypeTransition.dta, nogen norep
    ## ----------------------------------------------------
    ## is equvalent to (?) (tt == TypeTransition):

    TypeDateFormat <- dplyr::left_join(Chronicle, tt, by = "Type")
    TypeDateFormat$emptyType <- TypeDateFormat$Value == ""
    TypeDateFormat <- dplyr::filter(TypeDateFormat, (Transition != "End") &
                                                    (Type != "AtRisk")) %>%
        dplyr::group_by(Type) %>%
        dplyr::summarise_at(dplyr::vars(emptyType), dplyr::funs(min, max)) %>%
        dplyr::filter(max == 1 & min == 1) %>% #
        ##Same as 'filter(minempty == 1)'?
        dplyr::select(Type) %>%
        dplyr::filter(!duplicated(Type)) %>%
        dplyr::mutate(DateFormat = "%Y-%m-%d")
    ##TypeDateFormat
    if (keep){
        save(TypeDateFormat, file = "TypeDateFormat.rda")
    }

    ## Create the 'ExtractionFile' (from Chronicle):
    ExtractionFile <- Chronicle
    sw <- (ExtractionFile$Type == atrisk)
    ExtractionFile$Type[sw] <- "AtRisk"

    if (!sum(ExtractionFile$Type == "AtRisk")){
        cat("No 'at risk' variable found in 'Chronicle'.")
        return(1) # A suitable return value?
    }

    ## Fix 'ExtractionFile' and continue with 'DayFracOneDate':
    if (keep){
        save(ExtractionFile, file = "ExtractionFile.rda")
    }
    DayFracOneDate <- dplyr::left_join(ExtractionFile, tt, by = "Type")
    DayFracOneDate$Transition[DayFracOneDate$Type == "AtRisk"] <- "Start"


    ## Now the
    who <- with(DayFracOneDate, Value == "" & Transition != "End")
    DayFracOneDate$ChangeDate <- with(DayFracOneDate,
                                      paste(Year, Month, Day, sep = "-")) %>%
        as.Date(format = TypeDateFormat$DateFormat)
    DayFracOneDate$Value[who] <- as.character(DayFracOneDate$ChangeDate[who])
    DayFracOneDate <- dplyr::select(DayFracOneDate, -Year, -Month, -Day)

    DayFracOneDate$DayFrac[is.na(DayFracOneDate$DayFrac)] <- 0

    ##DayFracOneDate$dtype <- is.na(DayFracOneDate$ChangeDate) # Not really necessary (?)
    ##DayFracOneDate <- arrange(DayFracOneDate, Id_I, ChangeDate, dtype)
    ##DayFracOneDate <- group_by(DayFracOneDate, Id_I, ChangeDate, dtype)
    ##DayFracOneDate <- mutate(DayFracOneDate, temp = seq_len(n()))

    DayFracOneDate <- DayFracOneDate %>%
        dplyr::mutate(dtype = is.na(ChangeDate)) %>% # Not really necessary (?)
        dplyr::arrange(Id_I, ChangeDate, dtype) %>%
        dplyr::group_by(Id_I, ChangeDate, dtype) %>%
        dplyr::mutate(temp = seq_len(n()), temp1 = (temp == 1 & !is.na(ChangeDate))) %>%
        dplyr::group_by(Id_I, temp1) %>%
        dplyr::mutate(temp2 = temp1 * seq_len(n())) %>%
        dplyr::group_by(Id_I) %>%
        dplyr::mutate(numDate = max(temp2)) %>%
        dplyr::select(-temp, -temp1, -temp2, -dtype)
    if (keep){
        save(DayFracOneDate, file = "DayFracOneDate.rda")
    }
    ## DayFracOneDate1 :

    DayFracOneDate1 <- dplyr::filter(DayFracOneDate, numDate == 1 & !is.na(ChangeDate))

    ## What if DayFracOneDate1 is empty? Ignored for now: Implications?

    DayFracOneDate1 <- DayFracOneDate1 %>%
        dplyr::group_by(Id_I, ChangeDate) %>%
        dplyr::summarize(DayFrac1 = max(DayFrac)) %>%
        dplyr::mutate(Transition = "End")
    if (keep){
        save(DayFracOneDate1, file = "DayFracOneDate1.rda")
    }

    ## ExtractionFile:

    ExtractionFile <- dplyr::select(DayFracOneDate, -numDate) %>%
        dplyr::left_join(DayFracOneDate1, by = c("Id_I", "ChangeDate", "Transition"))
    ## Here 'Stata' continues with
    ## drop if _merge == 2
    ## drop _merge
    ## which I believe can be ignored (here).
    repl <- with(ExtractionFile, !is.na(DayFrac) & !is.na(DayFrac1) &
                     DayFrac < 0.01 & DayFrac1 > 0.01)
    ExtractionFile$DayFrac[repl] <- ExtractionFile$DayFrac1[repl]
    ExtractionFile$DayFrac1 <- NULL # Remove
    ExtractionFile$DayFrac[is.na(ExtractionFile$DayFrac)] <- 0
    ExtractionFile$ChangeDate <- with(ExtractionFile, ChangeDate + DayFrac)
    ExtractionFile$DayFrac <- NULL
    if (keep){
        save(ExtractionFile, file = "ExtractionFile.rda")
    }

    ## Check duplicates in 'ExtractionFile':
    dups <- with(ExtractionFile, paste(Id_I, as.numeric(ChangeDate), Type))%>%
        duplicated()
    cat("There are", sum(dups), " duplicated rows in 'ExtractionFile'.\n")

    list(ExtractionFile = ExtractionFile,
        TypeDateFormat = TypeDateFormat,
         DayFracOneDate = DayFracOneDate,
         DayFracOneDate1 = DayFracOneDate1)
}

part3 <- function(vs = x$VarSetup1, ch = y$ExtractionFile){
    ## Checking that the chronicle and variable setup files
    ## contain the same types.
    ##
    ## But we take 'ExtractionFile' instead of 'Chronicle'
    ## and 'VarSetup1' instead of 'VarSetup'

    ## Should return 'TRUE'. If not, search for errors.

    type.vs <- sort(unique(vs$Type))
    type.ch <- sort(unique(ch$Type))
    all.equal(type.vs, type.ch)
}

part4 <- function(ef, save = FALSE){
    ## "ef = ExtractionFile, output from part2"

    ## This part creates a wide file containing one column for each type of
    ## covariate that changes value at the beginning of a spell
    ## (Transition = Start).

    ef1 <- dplyr::filter(ef, tolower(Transition) == "start")

    if (!NROW(ef1)){ # ef1 empty
        ef1 <- ef %>%
            dplyr::select(Id_I) %>%
            dplyr::filter(!duplicated(Id_I)) %>%
            dplyr::mutate(Type = "EmptyVar1", Value = "EmptyVal1",
                   ChangeDate = as.Date("1900-01-01")) # Note: Differs from Luciana!
    }

    ef1$Value <- as.numeric(ef1$Value) ## Added by me, risk of NA?

    ctv <- tidyr::spread(ef1, Type, Value)

    ctv <- ctv %>%
        dplyr::group_by(Id_I) %>%
        tidyr::fill(3:NCOL(ctv))
    Covariates_time_varying <- ctv
    if (save){
        save(Covariates_time_varying, file = "Covariates_time_varying.rda")
    }
    ctv
}

part5 <- function(ef, save = FALSE){
    ## "ef = ExtractionFile, output from part2"

    ## Rectangularisation of time-invariant variables

    ef1 <- dplyr::filter(ef, tolower(Transition) == "invariant")

    if (!NROW(ef1)){ # ef1 empty
        ef1 <- ef %>%
            dplyr::select(Id_I) %>%
            dplyr::filter(!duplicated(Id_I)) %>%
            dpyr::mutate(Type = "EmptyVar2", Value = "EmptyVal2")
    }

    cti <- tidyr::spread(ef1, Type, Value)

    cti <- cti %>%
        dplyr::group_by(Id_I) %>%
        tidyr::fill(2:NCOL(cti))
    Covariates_time_invariant <- cti
    if (save){
       save(Covariates_time_invariant, file = "Covariates_time_invariant.rda")
    }

    cti
}

part6 <- function(ef, save = FALSE){
    ## ef = ExtractionFile, output from part2

    ## Rectangularisation of events
    ef1 <- ef %>%
        dplyr::filter(tolower(Transition) == "end") %>%
        dplyr::select(-Transition)

    if (!NROW(ef1)){
        ef1 <- ef %>%
            dplyr::select(Id_I) %>%
            dplyr::filter(!duplicated(Id_I)) %>%
            dplyr::mutate(Type = "EmptyVar0", Value = "EmptyVal0") %>%
            dplyr::mutate(ChangeDate = as.Date("1900-01-01"), DayFrac = NA)
    }

    eed <- ef1 %>%
        tidyr::spread(Type, Value)

    eed <- eed %>%
        dplyr::group_by(Id_I) %>%
        tidyr::fill(3:NCOL(eed))
    Events_end_dates <- eed
    if (save){
       save(Events_end_dates, file = "Events_end_dates.rda")
    }

    eed
}

part7 <- function(ef, ctv, cti, eed, save = FALSE){
    ## Construction of spells:

    ## "This part of the program constructs spells and merges start date and
    ## time-fixed covariates and end-date events."

    ef <- ef %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(ChangeDate)) %>%
        dplyr::select(Id_I, ChangeDate, Transition) %>%
        dplyr::distinct(Id_I, ChangeDate, Transition) %>%
        dplyr::arrange(Id_I, ChangeDate, Transition) %>%
        dplyr::group_by(Id_I) %>%
        dplyr::mutate(numRows = length(Transition))

    ef$rowType <- "-1"
    ef$rowType[ef$numRows == 2] <- ef$Transition[ef$numRows == 2]

    ef <- ef %>%
        dplyr::ungroup() %>%
        dplyr::select(Id_I, ChangeDate, rowType) %>%
        dplyr::distinct(Id_I, ChangeDate, rowType) %>%
        dplyr::select(-rowType)

    ef <- ef %>%
        dplyr::arrange(Id_I, ChangeDate) %>% # Alreay sorted?
        dplyr::rename(date1 = ChangeDate) %>%
        dplyr::group_by(Id_I) %>%
        dplyr::mutate(date2 = lead(date1)) %>%
        dplyr::filter(!is.na(date2)) %>%
        dplyr::ungroup()

    ## Merge time-varying covariates:
    ctv <- dplyr::rename(ctv, date1 = ChangeDate) # Missed in the Stata version 13.1!?
    ef <- ef %>%
        dplyr::left_join(ctv, by = c("Id_I", "date1"))

    ## Merge time-invariant covariates:
    ef <- ef %>%
        dplyr::left_join(cti, by = "Id_I")

    ## Merge events on end dates:
    eed <- dplyr::rename(eed, date2 = ChangeDate)
    ef <- ef %>%
        dplyr::left_join(eed, by = c("Id_I", "date2"))

    ef <- dplyr::filter(ef, !is.na(AtRisk))

    ## Just in case they exist...:
    ef$EmptyVar0 <- ef$EmptyVar1 <- ef$EmptyVar2 <- NULL

    PreEpisodes_file <- ef
    if (save){
       save(PreEpisodes_file, file = "PreEpisodes_file.rda")
    }

    ef
}

part8 <- function(pef, save = FALSE){

    ## This part contains both 'Part 8' and 'Part 9' from the Stata version.

    ## FORMATTING OF THE EPISODES FILE
    ## "The purpose of this part of the program is to convert variable formats
    ## and fill down down missing information."

    ## My note: I think 'fill down' is already taken care of.

    ## The parts (in the Stata version) exporting various stuff to csv files
    ## is skipped here.

    ## "Replace 'NoValue' with empty cells for cases where there was
    ## no value of a context variable on the date of entry of
    ## an individual into the context"

    ## My note: Skip this until it hurts.

    ## Reformat variables:

    ## Skip.

    ## "Dropping spells when the individual is not at risk"

    Episodes_file <- dplyr::filter(pef, as.numeric(AtRisk) != 0) ##%>%
        ##dplyr::select(-AtRisk, -DayFrac, -Transition)
    Episodes_file$AtRisk <- NULL
    Episodes_file$DayFrac <- NULL
    Episodes_file$Transition <- NULL

    datestamp <- Sys.time()
    datestamp <- gsub(" ", "_", datestamp)
    if (save){
       save(Episodes_file, file = paste("Episodes_file", datestamp, ".rda", sep = ""))
    }

    Episodes_file
}

    cat("part1: \n")
    p1 <- part1(VarSetup)
    cat("\npart2: \n")
    p2 <- part2(Chronicle, tt = p1$TypeTransition)
    cat("\npart3: \n")
    p3 <- part3(p1$VarSetup1, p2$ExtractionFile) # p3 is a logical
    if (!p3) stop("Mismatch!!")
    cat("part4: \n")
    Covariates_time_varying <- part4(p2$ExtractionFile)
    cat("part5: \n")
    Covariates_time_invariant <- part5(p2$ExtractionFile)
    cat("part6: \n")
    eed <- part6(p2$ExtractionFile)
    cat("part7: \n")
    PreEpisodes_file <- part7(p2$ExtractionFile, Covariates_time_varying, Covariates_time_invariant, eed)
    cat("part8+9: \n")
    Episodes_file <- part8(PreEpisodes_file)
    Episodes_file
}

