data_file <- "../data/Status_Non_Dod_2009_03.txt"
header_file <- "../data/headers.csv"
agency_file <- "../data/SCTFILE.TXT"

dat_header <- read.csv(header_file, header = TRUE)

agency_trans <- readLines(agency_file)
agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)

# helper function to remove entries where PseudoIDs are equal, Agencys are equal, and Pay is lowest
# takes rows with the same PseudoID
# returns list of entries to remove
# find_to_remove <- function() {
#     
# }


clean_file <- function(data_file_name) {
    dat_raw <- readLines(data_file_name)
    
    # apply headers
    df <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, dat_header[,2], dat_header[,3]))))
    dimnames(df) <- NULL
    df <- as.data.frame(df)
    colnames(df) <- dat_header[,1]
    
    # make numeric fields numeric
    df$Pay <- as.numeric(as.character(df$Pay))
    
    # make ordinal fields ordered factors
    df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
    df$Education <- factor(df$Education, ordered = TRUE, levels = levels(df$Education))
    
    # handle duplicate IDs
    # not touching employees who worked at multiple agencies in a quarter
    # remove the lower salary if an employee worked at the same agency twice in a quarter
    df_dup_ids <- df[duplicated(df$PseudoID) | duplicated(df$PseudoID, fromLast = TRUE),]
    df_dup_ids <- df_dup_ids[order(df_dup_ids$PseudoID, df_dup_ids$Agency),]
    
    # if Age is unspecified, use median age for agency
    # df$Age <- with(df, ave(df$Agency, df$Age, FUN = function(x) replace(x, x == "UNSP", median(x, na.rm = TRUE))))
    
    # fill NA pays with median pay for the Age of the employee at that agency
    
    # create a quarterly pay column
    # for employees with one entry, QPay <- Pay * (3/12)
    # for employees with multiple entries (n), QPay <- Pay * ((3/n)/12)
    # assumes an employees time was equally split between all agencies they worked at in a quarter
    # n = 1: 3/12
    # n = 2: 1.5/12
    # n = 3: 1/12
    # n = 4: 0.75/12
    
    # dup_ids <- data.frame(table(df$PseudoID))
    # df_dup_ids <- df[df$PseudoID %in% dup_ids$Var1[dup_ids$Freq > 1],]
    # df_dup_ids <-df_dup_ids[order(df_dup_ids$PseudoID, df_dup_ids$Agency),]
    
    
    # add agency name
    m <- match(df$Agency, agency_trans_table$agency_ID)
    df$AgencyName <-  agency_trans_table$agency_name[m]
    
    
    # cleaning
    # - subset to agencies I want to examine
    # - write to csv
    # ..- repeat for all data files
    return(df)
}

df <- clean_file(data_file)

sapply(df, class)

df$PseudoID <- as.numeric(as.character(df$PseudoID))
head(df)
summary(df)

# look at natual disasters and events