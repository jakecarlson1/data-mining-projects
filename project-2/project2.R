setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-2")
data_dir <- "../clean-data/"

# # read four years
# df_2001 <- read.csv(file = paste(data_dir, '2001-clean.csv', sep=""),
#                     header = TRUE, sep = ",")
# df_2005 <- read.csv(file = paste(data_dir, '2005-clean.csv', sep=""),
#                     header = TRUE, sep = ",")
# df_2009 <- read.csv(file = paste(data_dir, '2009-clean.csv', sep=""),
#                     header = TRUE, sep = ",")
# df_2013 <- read.csv(file = paste(data_dir, '2013-clean.csv', sep=""),
#                     header = TRUE, sep = ",")

# prepare data for classification, subsets data to agency_subset if provided
make_cleaner <- function(df, agency_subset = c()) {
    # apply subset if agency_subset provided
    if (length(agency_subset) > 0) {
        df <- df[sapply(
            df$Agency, FUN = function(x)
                substring(x, 1,2) %in% agency_subset),]
    }

    # make ordered factors
    lvls <- c("< 1","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30-34",
              "35+")
    df$LOS <- factor(df$LOS, ordered = TRUE, levels = lvls)
    df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
    df$Education <- factor(df$Education, ordered = TRUE,
                              levels = levels(ordered(unique(df$Education))))

    # remove columns
    to_save <- c("Agency","Station","Age","Education","LOS","Pay",
                 "SupervisoryStatus","AgencyName")
    df <-df[,to_save]

    # impute Station, Education, LOS, SupervisoryStatus
    df$Station <- sapply(df$Station, FUN = function(x)
        as.integer(substring(x, 1,2)))
    # fill NA Station with most common station for employees at that agency
    df$Station <- with(
        df, ave(df$Station, df$Agency, FUN = function(x)
            replace(x, is.na(x), median(x,na.rm = TRUE)))
    )
    # fill NA Education with median Education for employees at same Agency with
    # same Age
    df$Education <- with(
        df, ave(df$Education, df$Agency, df$Age, FUN = function(x)
            replace(x, is.na(x),
                    levels(df$Education)[median(as.integer(x),na.rm = TRUE)]))
    )
    # fill remaining NA Education with median education for Agency
    df$Education <- with(
        df, ave(df$Education, df$Agency, FUN = function(x)
            replace(x, is.na(x),
                    levels(df$Education)[median(as.integer(x),na.rm = TRUE)]))
    )
    # fill NA LOS with median LOS for employees of the same Age
    df$LOS <- with(
        df, ave(df$LOS, df$Age, FUN = function(x)
            replace(x, is.na(x),
                    levels(df$LOS)[median(as.integer(x),na.rm = TRUE)]))
    )
    # fill NA SupervisoryStatus with median status for employees with the
    # same pay
    df$SupervisoryStatus <- with(
        df, ave(df$SupervisoryStatus, df$Pay, FUN = function(x)
            replace(x, is.na(x), median(x, na.rm = TRUE)))
    )

    # convert station to region (state)
    state_trans <- read.csv('../project-1/state-trans.txt', header = TRUE)
    # prepend zero to states with number < 10
    state_trans$Num <- sapply(state_trans$Num, FUN = function(x)
        formatC(x, width = 2, format = "d", flag = "0"))
    # create state name column
    df$region <- tolower(state_trans$State[match(df$Station, state_trans$Num)])

    # make pay ordinal (cut)
    df$Pay <- cut(df$Pay,
                  breaks = c(0, 50000, 75000, 100000, Inf),
                  labels = c("<50k", "50-75k", "75k-100k", ">100k"))
    df$Pay <- factor(df$Pay, ordered = TRUE, levels = levels(df$Pay))

    # drop NA Pay
    df <- df[!is.na(df$Pay),]

    # make age an integer (take middle of range)
    df$Age <- sapply(df$Age, FUN = function(x)
        if(x == "75+") 75
        else (as.integer(substring(x, 1,2)) + as.integer(substring(x, 4,5)))/2)

    # make Education an integer to improve training speed
    df$Education <- as.numeric(as.character(df$Education))

    # make LOS a number (middle of range) to improve training speed
    df$LOS <- sapply(df$LOS, FUN = function(x)
        if(x == "< 1") 1
        else if(x == "35+") 35
        else floor((as.integer(strsplit(as.character(x), '-')[[1]][1]) +
                    as.integer(strsplit(as.character(x), '-')[[1]][2]))/2))

    return(df)
} # end make_cleaner

# that function is nice, but lets restart
replace_na <- function(file_name, dat_header, agency_trans_table) {
    dat_raw <- readLines(file_name)

    # apply headers
    df <- t(sapply(dat_raw, FUN = function(x)
        trimws(substring(x, dat_header[,2], dat_header[,3]))))
    dimnames(df) <- NULL
    df <- as.data.frame(df)
    colnames(df) <- dat_header[,1]

    # remove columns
    to_save <- c("Agency","Station","Age","Education","LOS",
                 "Category","Pay", "SupervisoryStatus")
    df <-df[,to_save]

    df$Station <- replace(df$Station, df$Station == "#########" |
                          df$Station == "*********", NA)
    df$Age <- replace(df$Age, df$Age == "UNSP", NA)
    df$Education <- replace(df$Education, df$Education == "" |
                            df$Education == "*" | df$Education == "**", NA)
    df$LOS <- replace(df$LOS, df$LOS == "UNSP", NA)
    df$Occupation <- replace(df$Occupation, df$Occupation == "" |
                             df$Occupation == "****", NA)
    df$Category <- replace(df$Category, df$Category == "" |
                           df$Category == "*" | df$Category == "**", NA)
    df$SupervisoryStatus <- replace(df$SupervisoryStatus,
                                    df$SupervisoryStatus == "" |
                                    df$SupervisoryStatus == "*" |
                                    df$SupervisoryStatus == "**", NA)

    # make numeric fields numeric
    df$Pay <- as.numeric(as.character(df$Pay))

    # use state encoding in station
    df$Station <- sapply(df$Station, FUN = function(x)
        as.integer(substring(x, 1,2)))

    # add agency name
    m <- match(df$Agency, agency_trans_table$agency_ID)
    df$AgencyName <-  agency_trans_table$agency_name[m]

    return(df)
}

# find and clean all data files
# save cleaned data frames to csvs by year
wash_and_dry <- function(data_d, out_data_d, years) {
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
        df <- do.call(rbind, lapply(year_files, dat_header, agency_trans_table, FUN = replace_na))
        write.csv(df, file = paste(out_data_d, toString(y), "-clean.csv", sep = ""))
    }
}

years <- c(2001,2005,2009,2013)
raw_data_d <- "../data/"
clean_data_d <- "../clean-data/"

wash_and_dry(raw_data_d, clean_data_d, years)

prep_attributes <- function(df, agency_subset = c()) {
    # apply subset if agency_subset provided
    if (length(agency_subset) > 0) {
        df <- df[sapply(
            df$Agency, FUN = function(x)
                substring(x, 1,2) %in% agency_subset),]
    }

    # make ordered factors
    lvls <- c("< 1","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30-34",
              "35+")
    df$LOS <- factor(df$LOS, ordered = TRUE, levels = lvls)
    df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
    df$Education <- factor(df$Education, ordered = TRUE,
                           levels = levels(ordered(unique(df$Education))))

    # convert station to region (state)
    state_trans <- read.csv('../project-1/state-trans.txt', header = TRUE)
    # prepend zero to states with number < 10
    state_trans$Num <- sapply(state_trans$Num, FUN = function(x)
        formatC(x, width = 2, format = "d", flag = "0"))
    # create state name column
    df$region <- tolower(state_trans$State[match(df$Station, state_trans$Num)])

    # make pay ordinal (cut)
    df$Pay <- cut(df$Pay,
                  breaks = c(0, 50000, 75000, 100000, Inf),
                  labels = c("<50k", "50-75k", "75k-100k", ">100k")) # mess with this
    df$Pay <- factor(df$Pay, ordered = TRUE, levels = levels(df$Pay))

    # drop NA Pay
    df <- df[!is.na(df$Pay),]

    # make age an integer (take middle of range)
    df$Age <- sapply(df$Age, FUN = function(x)
        if (is.na(x)) NA
        else if(x == "75+") 75
        else (as.integer(substring(x, 1,2)) + as.integer(substring(x, 4,5)))/2)

    # make Education an integer to improve training speed
    df$Education <- as.numeric(as.character(df$Education))

    # make LOS a number (middle of range) to improve training speed
    df$LOS <- sapply(df$LOS, FUN = function(x)
        if (is.na(x)) NA
        else if(x == "< 1") 1
        else if(x == "35+") 35
        else floor((as.integer(strsplit(as.character(x), '-')[[1]][1]) +
                        as.integer(strsplit(as.character(x), '-')[[1]][2]))/2))

    # one-hot encode agency and state

    return(df)
}


# load clean data and run prep_attributes
data_dir <- "../clean-data/"
agency_subset <- c()

# # read four years
df_2001 <- read.csv(file = paste(data_dir, '2001-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2005 <- read.csv(file = paste(data_dir, '2005-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2009 <- read.csv(file = paste(data_dir, '2009-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2013 <- read.csv(file = paste(data_dir, '2013-clean.csv', sep=""),
                    header = TRUE, sep = ",")

df_2001 <- prep_attributes(df_2001, agency_subset)
df_2005 <- prep_attributes(df_2005, agency_subset)
df_2009 <- prep_attributes(df_2009, agency_subset)
df_2013 <- prep_attributes(df_2013, agency_subset)

# count complete cases for each frame
count_complete <- function(df) {
    cat(sum(complete.cases(df)), sep="\n")
    cat(sum(complete.cases(df))/length(df$Pay))
}
count_complete(df_2001)
count_complete(df_2005)
count_complete(df_2009)
count_complete(df_2013)


# save year data frames
save(df_2001, df_2005, df_2009, df_2013,
     file = "../clean-data/persisted-full-frames.RData")

# load from workspace file
load("../clean-data/persisted-full-frames.RData")

# do agency subsetting
subset_agencies <- function(df, agency_subset = c()) {
    # apply subset if agency_subset provided
    if (length(agency_subset) > 0) {
        df <- df[sapply(
            df$Agency, FUN = function(x)
                substring(x, 1,2) %in% agency_subset),]
    }

    return(df)
}

agency_subset <- c("HS")

df_hs_2001 <- subset_agencies(df_2001, agency_subset)

# remove incomplete cases, ignoring columns that wont be used for that model








library(rpart)
model <- rpart(Pay ~ Age + Education + LOS, data = df_hs_2005)
model

library('rpart.plot')
rpart.plot(model, extra = 2, under = TRUE, varlen=0, faclen=0)
