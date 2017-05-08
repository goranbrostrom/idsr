  #' Create an episodes file from a Chronicle and a VarSetup file.
#'
#' @export
#' @param Chronicle The chronicle data frame.
#' @param VarSetup The VarSetup data frame.
#' @return An episodes data frame.
efc <- function(Chronicle, VarSetup){

    ## The 'main' function: Calls part1, part2, ... etc

    ## But first, note that the data frame 'VarSetup' is appended
    ## a column 'mode', which gives the storage mode of variables
    ## in the final result, i.e., numeric, logical, factor, etc.

    #########################################
    ### Beginning of "main program"!        #
    #########################################

    ## Note: part3 is now first!
    cat("\npart3: \n")
    VarSetup <- part3(VarSetup, Chronicle, keep = FALSE) # p3 is a logical

    cat("part1: \n")
    p1 <- part1(VarSetup)
    cat("\npart2: \n")
    p2 <- part2(Chronicle, tt = p1$TypeTransition)
    cat("part4: \n")
    Covariates_time_varying <- part4(p2$ExtractionFile)
    cat("part5: \n")
    Covariates_time_invariant <- part5(p2$ExtractionFile)
    cat("part6: \n")
    eed <- part6(p2$ExtractionFile)
    cat("part7: \n")
    PreEpisodes_file <- part7(p2$ExtractionFile, Covariates_time_varying, Covariates_time_invariant, eed)
    cat("part8+9: \n")
    Episodes_file <- part8(PreEpisodes_file, VarSetup)
    Episodes_file
}


part3 <- function(vs = VarSetup, ch = Chronicle, keep = FALSE){

    ##*********************************************************
    ##    **** 						PART 3 : CHECK TYPES
    ##*********************************************************

    ## Checking that the chronicle and variable setup files
    ## contain the same types.
    ##
    ## If Types in VarSetup are not present in Chronicle,
    ## the corresponding rows in Varsetup are removed.
    ##
    ## If Types in Chronicle are not present in Varsetup,
    ## an error is thrown.
    ##
    ## In the future, VarSetup can be constructed fron Chronicle(?)
    ##

    ## Returns (a modified) VarSetup.

    type.vs <- sort(unique(vs$Type))
    type.ch <- sort(unique(ch$Type))
    ##all.equal(type.vs, type.ch) # Is this enough? (next row)
    ret <- all(type.ch %in% type.vs)
    ## New abbrewinkel: Cut down on 'Varsetup1':
    if (!ret) stop("Missing Types in 'Chronicle")
    rem.vs <- type.vs[!(type.vs %in% type.ch)]
    ##cat("rem.vs = ", rem.vs, "\n")
    VarSetup <- vs[!(vs$Type %in% rem.vs), ]
    if (keep) save(VarSetup, file = "VarSetup.rda")
    VarSetup
}

part1 <- function(VarSetup, atrisk = "At_risk", using = "", keep = FALSE){

    ##********************************************************************
    ##    **** 		PART 1: READ AND PREPARE VARIABLE SET UP FILE
    ##********************************************************************
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

    who <- VarSetup$Type == atrisk
    VarSetup$Type[who] <- "AtRisk"
    ##VarSetup$Type <- with(VarSetup, replace(Type, Type == atrisk, "AtRisk"))

    VarSetup1 <- VarSetup
    if (keep){
        save(VarSetup1, file = "VarSetup1.rda")
    }

    ## Transition:
    ## TypeTransition <- dplyr::select_(VarSetup1, Type, Transition)
    TypeTransition <- VarSetup1[, c("Type", "Transition")]
    ## Is this really necessary!!? Why not use VarSetup1?
    if (keep){
        save(TypeTransition, file = "TypeTransition.rda")
    }

    ## Duration:
    ##TypeDuration <- dplyr::filter_(VarSetup1, Transition != "End" &
      ##                                Duration == "Continuous")
    TypeDuration <- VarSetup1[VarSetup1$Transition != "End" &
                                       VarSetup1$Duration == "Continuous", ]

    ## For the time being, I do not think that 'TypeDuration' and
    ## 'TypeReplace1' are necessary (really?), so outcommented

    ##TypeDuration <- dplyr::select_(TypeDuration, Type)
    # TypeDuration <- TypeDuration[, c("Type")]
    # if (keep){
    #     save(TypeDuration, file = "TypeDuration.rda")
    # }
    #
    # ## Minus1
    # TypeReplaceMin1 <- TypeDuration # ?? Check this!
    # if (keep){
    #     save(TypeReplaceMin1, file = "TypeReplaceMin1.rda")
    # }

    ## Value labels ('using')
    ##  Skip this for the time being: The file 'ValueLabel' is not created.

    ## Return value:
    list(VarSetup1 = VarSetup1,
         TypeTransition = TypeTransition) #,
         ##TypeDuration = TypeDuration,
         ##TypeReplaceMin1 = TypeReplaceMin1)

}

part2 <- function(Chronicle, atrisk = "At_risk", tt, keep = FALSE){

    ##*****************************************************************
    ##    **** 		PART 2: READ AND PREPARE THE CHRONICLE FILE
    ##*****************************************************************
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
        cat("'Chronicle' is OK.\n") # Will be removed later.
    }

    ## Generating DateFormat for Types which have no Value but
    ## the Timestamp is their value. This Stata code:
    ## ----------------------------------------------------
    ## use Chronicle.dta, clear
    ## merge m:1 Type using TypeTransition.dta, nogen norep
    ## ----------------------------------------------------
    ## is equvalent to (?) (tt == TypeTransition):

    ##TypeDateFormat <- dplyr::left_join(Chronicle, tt, by = "Type")
    indx <- match(Chronicle$Type, tt$Type)
    TypeDateFormat <- Chronicle
    TypeDateFormat$Transition <- tt$Transition[indx]

    ##++++++++++++++++++++++++++++++++++++++++++++++++++++ Start ++++
    if (FALSE){
    TypeDateFormat$emptyType <- TypeDateFormat$Value == ""
    ##TypeDateFormat <- dplyr::filter_(TypeDateFormat, (Transition != "End") &
      ##                                              (Type != atrisk))
    TypeDateFormat <- TypeDateFormat[!is.na(TypeDateFormat$Transition) &
                                         !is.na(TypeDateFormat$Type), ]
    TypeDateFormat <- TypeDateFormat[TypeDateFormat$Transition != "End" &
                                         TypeDateFormat$Type != atrisk, ]
    TypeDateFormat <- dplyr::group_by_(TypeDateFormat, ~Type)
    TypeDateFormat <- dplyr::summarise_at(TypeDateFormat, dplyr::vars(emptyType),
                                          dplyr::funs(min, max))
    TypeDateFormat <- dplyr::filter_(TypeDateFormat, ~(max == 1 & min == 1))
        ##Same as 'filter_(minempty == 1)'?
    TypeDateFormat <- dplyr::select_(TypeDateFormat, ~Type)
    TypeDateFormat <- dplyr::filter_(TypeDateFormat, ~!duplicated(Type))
    }
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++ End ++

    ## The code from '++ Start ++' to '++ End ++' above is an attempt yo sort out the
    ## distinct Type's that has an empty value with Transition not equal to "End"
    ## or 'atrisk'.
    ##
    ## Can be done much simpler (I hope):

    ## ++++++++++++++++ New start ++++++++++++++++++++++++++++++++++
    TypeDateFormat <- TypeDateFormat[TypeDateFormat$Value == "", ]
    TypeDateFormat <- TypeDateFormat[!(TypeDateFormat$Transition %in%
                                           c(atrisk, "End")), ]
    TypeDateFormat <- TypeDateFormat["Type"]
    TypeDateFormat <- TypeDateFormat[!duplicated(TypeDateFormat$Type),]
    TypeDateFormat$DateFormat <- "%Y-%m-%d"
    ## ++++++++++++++++ New End ++++++++++++++++++++++++++++++++++++

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
    who <- DayFracOneDate$Value == "" & DayFracOneDate$Transition != "End"
    DayFracOneDate$ChangeDate <- paste(DayFracOneDate$Year,
                                       DayFracOneDate$Month,
                                       DayFracOneDate$Day, sep = "-")
    DayFracOneDate$ChangeDate <- as.Date(DayFracOneDate$ChangeDate,
                                         format = TypeDateFormat$DateFormat)
    DayFracOneDate$Value[who] <- as.character(DayFracOneDate$ChangeDate[who])
    ##DayFracOneDate <- dplyr::select_(DayFracOneDate, -Year, -Month, -Day)
    DayFracOneDate <- DayFracOneDate[!(names(DayFracOneDate) %in% c("Year", "Month", "Day"))]

    DayFracOneDate$DayFrac[is.na(DayFracOneDate$DayFrac)] <- 0

    ##DayFracOneDate$dtype <- is.na(DayFracOneDate$ChangeDate) # Not really necessary (?)
    ##DayFracOneDate <- arrange(DayFracOneDate, Id_I, ChangeDate, dtype)
    ##DayFracOneDate <- group_by_(DayFracOneDate, Id_I, ChangeDate, dtype)
    ##DayFracOneDate <- mutate_(DayFracOneDate, temp = seq_len(n()))

    DayFracOneDate$dtype <- is.na(DayFracOneDate$ChangeDate)
    DayFracOneDate <- DayFracOneDate[with(DayFracOneDate, order(Id_I, ChangeDate, dtype)), ]
    ##DayFracOneDate <- DayFracOneDate %>%
        ##dplyr::mutate_(dtype = is.na(ChangeDate)) %>% # Not really necessary (?)
        ##dplyr::arrange(Id_I, ChangeDate, dtype) %>%
    DayFracOneDate <- dplyr::group_by_(DayFracOneDate, ~Id_I, ~ChangeDate, ~dtype)
    DayFracOneDate <- dplyr::mutate(DayFracOneDate, temp = seq_len(n()),
                                    temp1 = (temp == 1 & !is.na(ChangeDate)))
    DayFracOneDate <- dplyr::group_by_(DayFracOneDate, ~Id_I, ~temp1)
    DayFracOneDate <- dplyr::mutate_(DayFracOneDate, temp2 = temp1 * seq_len(n()))
    DayFracOneDate <- dplyr::group_by_(DayFracOneDate, Id_I)
    DayFracOneDate <- dplyr::mutate_(DayFracOneDate, numDate = max(temp2))
    DayFracOneDate <- dplyr::select_(DayFracOneDate, -temp, -temp1, -temp2, -dtype)
    if (keep){
        save(DayFracOneDate, file = "DayFracOneDate.rda")
    }
    ## DayFracOneDate1 :

    DayFracOneDate1 <- dplyr::filter_(DayFracOneDate, numDate == 1 & !is.na(ChangeDate))

    ## What if DayFracOneDate1 is empty? Ignored for now: Implications?

    ##DayFracOneDate1 <- DayFracOneDate1 %>%
    DayFracOneDate1 <- dplyr::group_by_(DayFracOneDate1, Id_I, ChangeDate)
    DayFracOneDate1 <- dplyr::summarize(DayFracOneDate1, DayFrac1 = max(DayFrac))
    DayFracOneDate1 <- dplyr::mutate_(DayFracOneDate1, Transition = "End")
    if (keep){
        save(DayFracOneDate1, file = "DayFracOneDate1.rda")
    }

    ## ExtractionFile:

    ExtractionFile <- dplyr::select_(DayFracOneDate, -numDate)
    ExtractionFile <- dplyr::left_join(ExtractionFile, DayFracOneDate1,
                                       by = c("Id_I", "ChangeDate", "Transition"))
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
    dups <- with(ExtractionFile, paste(Id_I, as.numeric(ChangeDate), Type))
    dups <- duplicated(dups)

    if (sum(dups)){
        cat("There are", sum(dups), " duplicated rows in 'ExtractionFile'.\n")
    }

    list(ExtractionFile = ExtractionFile,
        TypeDateFormat = TypeDateFormat,
         DayFracOneDate = DayFracOneDate,
         DayFracOneDate1 = DayFracOneDate1)
}


part4 <- function(ef, keep = FALSE){

    ## "ef = ExtractionFile, output from part2"
    ##
    ## *******************************************************
    ## ****                PART 4: TIME_VARYING COVARIATES
    ## *******************************************************
    ## This part creates a wide file containing one column for
    ## each type of covariate that changes value at the
    ## beginning of a spell (Transition = Start).
    ## *******************************************************

    ef1 <- dplyr::filter_(ef, tolower(Transition) == "start")

    if (!NROW(ef1)){ # ef1 empty
        ##ef1 <- ef %>%
        ef1 <- dplyr::select_(ef, Id_I)
        ef1 <- dplyr::filter_(ef1, !duplicated(Id_I))
        ef1 <- dplyr::mutate_(ef1, Type = "EmptyVar1", Value = "EmptyVal1",
                   ChangeDate = as.Date("1900-01-01")) # Note: Differs from Luciana!
    }

    ef1$Value <- as.numeric(ef1$Value) ## Added by me, risk of NA?

    ctv <- tidyr::spread(ef1, Type, Value)

    ##ctv <- ctv %>%
    ctv <- dplyr::group_by_(ctv, Id_I)
    ctv <- tidyr::fill(ctv, 3:NCOL(ctv))
    Covariates_time_varying <- ctv
    if (keep){
        save(Covariates_time_varying, file = "Covariates_time_varying.rda")
    }
    ctv
}

part5 <- function(ef, keep = FALSE){

    ##*************************************************************************
    ##  ****					PART 5: TIME-INVARIANT COVARIATES
    ## This part of the program creates a wide file containing one column for
    ## each type of time-fixed covariate (Transition=Invariant)
    ##*************************************************************************

    ## "ef = ExtractionFile, output from part2"

    ## Rectangularisation of time-invariant variables

    ef1 <- dplyr::filter_(ef, tolower(Transition) == "invariant")
    ef1$ChangeDate <- NULL
    ef1$Transition <- NULL

    if (!NROW(ef1)){ # ef1 empty
    ##    ef1 <- ef %>%
        ef1 <- dplyr::select_(ef, Id_I)
        ef1 <- dplyr::filter_(ef1, !duplicated(Id_I))
        ef1 <- dplyr::mutate_(ef1, Type = "EmptyVar2", Value = "EmptyVal2")
    }

    cti <- tidyr::spread(ef1, Type, Value)

    ##cti <- cti %>%
    cti <- dplyr::group_by_(cti, Id_I)
    cti <- tidyr::fill(cti, 2:NCOL(cti))
    Covariates_time_invariant <- cti
    if (keep){
       save(Covariates_time_invariant, file = "Covariates_time_invariant.rda")
    }

    cti
}

part6 <- function(ef, keep = FALSE){

    ##*************************************************************************
    ##    **** 						PART 6: EVENTS
    ## This part of the program creates a wide file containing one column for
    ## each type of event occurring at the end of a spell (Transition=End).
    ##*************************************************************************

    ## ef = ExtractionFile, output from part2

    ## Rectangularisation of events
    ##ef1 <- ef %>%
    ef1 <- dplyr::filter_(ef, tolower(Transition) == "end")
    ef1 <- dplyr::select_(ef1, -Transition)

    if (!NROW(ef1)){
        ##ef1 <- ef %>%
        ef1 <- dplyr::select_(ef, Id_I)
        ef1 <- dplyr::filter_(ef1, !duplicated(Id_I))
        ef1 <- dplyr::mutate_(ef1, Type = "EmptyVar0", Value = "EmptyVal0")
        ef1 <- dplyr::mutate_(ef1, ChangeDate = as.Date("1900-01-01"), DayFrac = NA)
    }

    ef1$Value[is.na(ef1$Value) | ef1$Value == ""] <- 1
    ##eed <- ef1 %>%
    eed <- tidyr::spread(ef1, Type, Value)

    ##eed <- eed %>%
    eed <- dplyr::group_by_(eed, Id_I)
    eed <- tidyr::fill(eed, 3:NCOL(eed))
    Events_end_dates <- eed
    if (keep){
       save(Events_end_dates, file = "Events_end_dates.rda")
    }

    eed
}

part7 <- function(ef, ctv, cti, eed, keep = FALSE){
    ## Construction of spells:
    ##************************************************************
    ##  ****			PART 7: SPELLS CONSTRUCTION
    ## ***********************************************************
    ## "This part of the program constructs spells and merges
    ## start date and time-fixed covariates and end-date events."
    ## ***********************************************************
    ef <- dplyr::ungroup(ef)
    ef <- dplyr::filter_(ef, !is.na(ChangeDate))
    ef <- dplyr::select_(ef, Id_I, ChangeDate, Transition)
    ef <- dplyr::distinct(ef, Id_I, ChangeDate, Transition)
    ef <- dplyr::arrange(ef, Id_I, ChangeDate, Transition)
    ef <- dplyr::group_by_(ef, Id_I)
    ef <- dplyr::mutate_(ef, numRows = length(Transition))

    ef$rowType <- "-1"
    ef$rowType[ef$numRows == 2] <- ef$Transition[ef$numRows == 2]

    ef <- dplyr::ungroup(ef)
    ef <- dplyr::select_(ef, Id_I, ChangeDate, rowType)
    ef <- dplyr::distinct(ef, Id_I, ChangeDate, rowType)
    ef <- dplyr::select_(ef, -rowType)

    ##ef <- ef %>%
    ef <- dplyr::arrange(ef, Id_I, ChangeDate) # Alreay sorted?
    ef <- dplyr::rename(ef, date1 = ChangeDate)
    ef <- dplyr::group_by_(ef, Id_I)
    ef <- dplyr::mutate_(ef, date2 = lead(date1))
    ef <- dplyr::filter_(ef, !is.na(date2))
    ef <- dplyr::ungroup(ef)

    ## Merge time-varying covariates:
    ctv <- dplyr::rename(ctv, date1 = ChangeDate) # Missed in the Stata version 13.1!?
    ##ef <- ef %>%
    ef <- dplyr::left_join(ef, ctv, by = c("Id_I", "date1"))

    ## Merge time-invariant covariates:
    ##ef <- ef %>%
    ef <- dplyr::left_join(ef, cti, by = "Id_I")

    ## Merge events on end dates:
    eed <- dplyr::rename(eed, date2 = ChangeDate)
    ##ef <- ef %>%
    ef <- dplyr::left_join(ef, eed, by = c("Id_I", "date2"))

    ef <- dplyr::filter_(ef, !is.na(AtRisk))

    n <- length(ef)
    ef[is.na(ef[[n]]), n] <- 0 # Does this work?

    ## Just in case they exist...:
    ef$EmptyVar0 <- ef$EmptyVar1 <- ef$EmptyVar2 <- NULL

    PreEpisodes_file <- ef
    if (keep){
       save(PreEpisodes_file, file = "PreEpisodes_file.rda")
    }

    ef
}

part8 <- function(pef, vs, keep = FALSE){

    ## This part contains both 'Part 8' and 'Part 9' from the Stata version.

    ## ************************************************************
    ## *** PART 8: FORMATTING OF THE EPISODES FILE
    ## "The purpose of this part of the program is to convert
    ## variable formats and fill down down missing information."
    ## ************************************************************

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

    Episodes_file <- dplyr::filter_(pef, as.numeric(AtRisk) != 0) ##%>%
        ##dplyr::select_(-AtRisk, -DayFrac, -Transition)
    Episodes_file$AtRisk <- NULL
    Episodes_file$DayFrac <- NULL
    Episodes_file$Transition <- NULL

    ## Finally, give variables the correct storage mode, if 'mode'exists in
    ## 'VarSetup'

    vs <- vs[, c("Type", "mode")]
    for (ef_name in names(Episodes_file)){
        if (ef_name %in% vs$Type){
            i <- which(ef_name == names(Episodes_file))
            j <- which(ef_name == vs$Type)
            mod <- vs$mode[j]
            if (mod == "date"){
                Episodes_file[[ef_name]] <- as.Date(Episodes_file[[ef_name]])
            }else{
                if (mod == "factor"){
                    Episodes_file[[ef_name]] <- as.factor(Episodes_file[[ef_name]])
                }else{
                    ##cat("mod = ", mod, "\n")
                    storage.mode(Episodes_file[[ef_name]]) <- mod
                }
            }
        }
    }
    datestamp <- Sys.time()
    datestamp <- gsub(" ", "_", datestamp)
    if (keep){
       save(Episodes_file, file = paste("Episodes_file", datestamp, ".rda", sep = ""))
    }

    Episodes_file
}


