data_file <- "../data/Status_Non_Dod_2009_03.txt"
header_file <- "../data/headers.csv"
agency_file <- "../data/SCTFILE.TXT"

dat_header <- read.csv(header_file, header = TRUE)

agency_trans <- readLines(agency_file)
agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)

clean_file <- function(data_file_name) {
    dat_raw <- readLines(data_file_name)

    # apply headers
    df <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, dat_header[,2], dat_header[,3]))))
    dimnames(df) <- NULL
    df <- as.data.frame(df)
    colnames(df) <- dat_header[,1]

    original_len <- length(df$PseudoID)

    # make numeric fields numeric
    df$Pay <- as.numeric(as.character(df$Pay))

    # replace unknowns with NA
    df$Station <- replace(df$Station, df$Station == "#########", NA)
    df$Age <- replace(df$Age, df$Age == "UNSP", NA)
    df$Education <- replace(df$Education, df$Education == "" | df$Education == "*", NA)
    df$PayPlan <- replace(df$PayPlan, df$PayPlan == "" | df$PayPlan == "*", NA)
    df$Category <- replace(df$Category, df$Category == "" | df$Category == "*", NA)
    df$SupervisoryStatus <- replace(df$SupervisoryStatus, df$SupervisoryStatus == "" | df$SupervisoryStatus == "*", NA)
    df$Schedule <- replace(df$Schedule, df$Schedule == "" | df$Schedule == "*", NA)

    # make ordinal fields ordered factors
    df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
    df$Education <- factor(df$Education, ordered = TRUE, levels = levels(df$Education))

    # if Age is unspecified, use median age for agency
    df$Age <- with(df, ave(df$Age, df$Agency, FUN = function(x) replace(x, is.na(x), levels(df$Age)[median(as.integer(x), na.rm = TRUE)])))

    # fill NA pays with median pay for the Age of the employee at that agency
    df$Pay <- with(df, ave(df$Pay, df$Age, df$Agency, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))

    # drop any rows with NA pay after imputation
    na_pay <- is.na(df$Pay)
    df <- df[!na_pay,]

    # handle duplicate IDs
    # not touching employees who worked at multiple agencies in a quarter
    # remove the lower salary if an employee worked at the same agency twice in a quarter
    # 1. select rows with duplicate IDs
    df_dup_ids <- df[duplicated(df$PseudoID) | duplicated(df$PseudoID, fromLast = TRUE),]
    # 2. order selection by ID, then Agency, then descending Pay
    df_dup_ids <- df_dup_ids[order(df_dup_ids$PseudoID, df_dup_ids$Agency, -df_dup_ids$Pay),]
    # 3. select rows where the ID and Agency are duplicated (same employee at same agency)
    to_remove <- df_dup_ids[(duplicated(df_dup_ids[c("PseudoID", "Agency")]) | duplicated(df_dup_ids[c("PseudoID", "Agency")], fromLast = TRUE)),]
    # 4. get row numbers for rows with the lowest pay for each grouping in the above selection
    to_remove <- as.numeric(rownames(to_remove[duplicated(to_remove$PseudoID, to_remove$Agency),]))
    # 5. reselect from df where rows are not in to_remove
    df <- df[!(as.numeric(rownames(df)) %in% to_remove),]

    # create a quarterly pay column
    # for employees with one entry, QPay <- Pay * (3/12)
    # for employees with multiple entries (n), QPay <- Pay * ((3/n)/12)
    # assumes an employees time was equally split between all agencies they worked at in a quarter
    # n = 1: 3/12
    # n = 2: 1.5/12
    # n = 3: 1/12
    # n = 4: 0.75/12

    # add agency name
    m <- match(df$Agency, agency_trans_table$agency_ID)
    df$AgencyName <-  agency_trans_table$agency_name[m]

    final_len <- length(df$PseudoID)
    print(paste("[", data_file_name, "]", final_len, "of", original_len, "records maintained:", final_len/original_len*100, "%"))

    # cleaning
    # - subset to agencies I want to examine
    # - write to csv
    # ..- repeat for all data files
    return(df)
}

df <- clean_file(data_file)

sapply(df, class)

head(df)
summary(df)

# look at natual disasters and events