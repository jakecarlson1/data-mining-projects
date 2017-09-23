# clean a data file and return a data frame
clean_file <- function(data_file_name, agency_subset = c(), dat_header, agency_trans_table) {
    dat_raw <- readLines(data_file_name)

    # apply headers
    df <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, dat_header[,2], dat_header[,3]))))
    dimnames(df) <- NULL
    df <- as.data.frame(df)
    colnames(df) <- dat_header[,1]

    # save original length of df
    original_len <- length(df$PseudoID)

    # subset to employees who worked in agency_subset
    if (length(agency_subset) > 0) {
        df <- df[grep(paste(agency_subset, collapse = '|'), df$Agency),]
        # drop sheep industry
        df <- df[!(df$Agency == "AGSC"),]
    }

    # drop Name column
    df <- df[, !(colnames(df) %in% c("Name"))]

    # make numeric fields numeric
    df$Pay <- as.numeric(as.character(df$Pay))

    # replace unknowns with NA
    df$Station <- replace(df$Station, df$Station == "#########" | df$Station == "*********", NA)
    df$Age <- replace(df$Age, df$Age == "UNSP", NA)
    df$Education <- replace(df$Education, df$Education == "" | df$Education == "*" | df$Education == "**", NA)
    df$PayPlan <- replace(df$PayPlan, df$PayPlan == "" | df$PayPlan == "*" | df$PayPlan == "**", NA)
    df$Grade <- replace(df$Grade, df$Grade == "" | df$Grade == "**", NA )
    df$LOS <- replace(df$LOS, df$LOS == "UNSP", NA)
    df$Occupation <- replace(df$Occupation, df$Occupation == "" | df$Occupation == "****", NA)
    df$Category <- replace(df$Category, df$Category == "" | df$Category == "*" | df$Category == "**", NA)
    df$SupervisoryStatus <- replace(df$SupervisoryStatus, df$SupervisoryStatus == "" | df$SupervisoryStatus == "*" | df$SupervisoryStatus == "**", NA)
    df$Appointment <- replace(df$Appointment, df$Appointment == "" | df$Appointment == "**", NA)
    df$Schedule <- replace(df$Schedule, df$Schedule == "" | df$Schedule == "*" | df$Schedule == "**", NA)

    # make ordinal fields ordered factors
    df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
    df$Education <- factor(df$Education, ordered = TRUE, levels = levels(df$Education))

    # if Age is unspecified, use median age for agency
    df$Age <- with(df, ave(df$Age, df$Agency, FUN = function(x) replace(x, is.na(x), levels(df$Age)[median(as.integer(x), na.rm = TRUE)])))

    # fill NA Education with median Education for employees of the same Age at the Agency
    df$Education <- with(df, ave(df$Education, df$Age, df$Agency, FUN = function(x) replace(x, is.na(x), levels(df$Education)[median(as.integer(x), na.rm = TRUE)])))

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

    # add agency name
    m <- match(df$Agency, agency_trans_table$agency_ID)
    df$AgencyName <-  agency_trans_table$agency_name[m]

    # calculate percent of data saved
    final_len <- length(df$PseudoID)
    print(paste("[", data_file_name, "]", final_len, "of", original_len,"records maintained:",
                format(round(final_len/original_len*100, 2)), "%"))

    return(df)
}

# find and clean all data files
# save cleaned data frames to csvs by year
wash_and_dry <- function(data_d, out_data_d, years, agency_subset = c()) {
    data_files <- list.files(path = data_d)
    non_dod_files <- paste(data_d, grep("Status_Non_DoD_20[01][0-9]_[01][3692].txt", data_files, perl = TRUE, value = TRUE), sep = "")

    header_file <- paste(data_d, "headers.csv", sep = "")
    agency_file <- paste(data_d, "SCTFILE.TXT", sep = "")
    dat_header <- read.csv(header_file, header = TRUE)
    agency_trans <- readLines(agency_file)
    agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
    agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
    agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)

    for(y in years) {
        year_files <- grep(paste("Status_Non_DoD_", toString(y), "_[01][3692].txt", sep = ""), non_dod_files, perl = TRUE, value = TRUE)
        df <- do.call(rbind, lapply(year_files, agency_subset, dat_header, agency_trans_table, FUN = clean_file))
        write.csv(df, file = paste(out_data_d, toString(y), "-clean.csv", sep = ""))
    }
}

path_to_data <- "../data/"
output_data_path <- "../clean-data/"

data_years <- c(2001:2014)

# subset to agencies I want to examine
# agency | code
# EPA | AGEP, AHEP
# DO Health and Human Services | AGHE, AHHE
# DO Homeland Security | AGHS, AHHS
# DO Housing and Urban Development | AGHU, AHHU
# DO Energy | AGDN, AHDN
# DO Education | AGED, AHED
# DOJ | AGDJ, AHDJ
# DOD | AGDD, AHDD
# FEMA | AGEM, AHEM
# Gen Services | AGGS, AHGS
# DO Interior | AGIN, AHIN
# DO Transportation | AGTD, AHTD
# NASA | AGNN, AHNN
# NSA | AGSP, AHSP
# IRS | AGTR07, AGTR93
# VA | AGVA, AHVA
agencies_to_save <- c("AGEP", "AHEP",
                      "AGHE", "AHHE",
                      "AGHS", "AHHS",
                      "AGHU", "AHHU",
                      "AGDN", "AHDN",
                      "AGED", "AHED",
                      "AGDJ", "AHDJ",
                      "AGDD", "AHDD",
                      "AGEM", "AHEM",
                      "AGGS", "AHGS",
                      "AGIN", "AHIN",
                      "AGTD", "AHTD",
                      "AGNN", "AHNN",
                      "AGSP", "AHSP",
                      "AGTR07", "AGTR93",
                      "AGVA", "AHVA")
agencies_to_save <- sapply(agencies_to_save, FUN = function(x) substring(x, 3,6))

# clean all data and save to csv
wash_and_dry(path_to_data, output_data_path, data_years, agencies_to_save)


# data visualization
setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-1")
library('ggplot2')
data_dir <- "../clean-data/"
df_2001 <- read.csv(file = paste(data_dir, '2001-clean.csv', sep=""), header = TRUE, sep = ",", nrows = 1000)


# returns class of each column
sapply(df, class)

head(df)
summary(df)

# look at natual disasters and events