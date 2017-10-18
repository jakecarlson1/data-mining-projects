setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-2")
data_dir <- "../clean-data/"

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
    non_dod_files <- paste(data_d,
                           grep("Status_Non_DoD_20[01][0-9]_[01][3692].txt",
                                data_files, perl = TRUE, value = TRUE),
                           sep = "")

    header_file <- paste(data_d, "headers.csv", sep = "")
    agency_file <- paste(data_d, "SCTFILE.TXT", sep = "")
    dat_header <- read.csv(header_file, header = TRUE)
    agency_trans <- readLines(agency_file)
    agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
    agency_name <- trimws(sapply(agency_trans,
                                 FUN = function(x) substring(x, 36,75)))
    agency_trans_table <- data.frame(agency_ID = agency_ID,
                                     agency_name = agency_name)

    for(y in years) {
        year_files <- grep(paste("Status_Non_DoD_", toString(y),
                                 "_[01][3692].txt", sep = ""),
                           non_dod_files, perl = TRUE, value = TRUE)
        df <- do.call(rbind, lapply(year_files, dat_header, agency_trans_table,
                                    FUN = replace_na))
        write.csv(df, file = paste(out_data_d, toString(y), "-clean.csv",
                                   sep = ""))
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
                  breaks = c(0, 30000, 50000, 70000, 90000, 110000, Inf),
                  labels = c("<30k", "30-50k", "50-70k", "70-90k", "90k-110k",
                             ">110k"))
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

# # read four years
df_2001 <- read.csv(file = paste(data_dir, '2001-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2005 <- read.csv(file = paste(data_dir, '2005-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2009 <- read.csv(file = paste(data_dir, '2009-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2013 <- read.csv(file = paste(data_dir, '2013-clean.csv', sep=""),
                    header = TRUE, sep = ",")

df_2001 <- prep_attributes(df_2001)
df_2005 <- prep_attributes(df_2005)
df_2009 <- prep_attributes(df_2009)
df_2013 <- prep_attributes(df_2013)

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
setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-2")
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

library(caret)
library(FSelector)
library('rpart.plot')
# pipeline to compare models
compare_models <- function(dat, label, vars, m1, c1, m2, c2) {
    f <- as.simple.formula(vars, label)
    fit1 <- train(f, data = dat, method = m1, trControl = c1,
                  tuneLength = 5)
    print(fit1$finalModel)
    fit2 <- train(f, data = dat, method = m2, trControl = c2,
                  tuneLength = 2)
    print(fit2$finalModel)
    resamps <- resamples(list(m1 = fit1, m2 = fit2))
    print(summary(resamps))
    return(list("m1fit" = fit1, "m2fit" = fit2))
}


agency_subset <- c("DJ")

df_dj_2005 <- subset_agencies(df_2005, agency_subset)
df_dj_2013 <- subset_agencies(df_2013, agency_subset)

# remove incomplete cases, ignoring columns that wont be used for that model
df_dj_2005 <- df_dj_2005[complete.cases(df_dj_2005),]
df_dj_2013 <- df_dj_2013[complete.cases(df_dj_2013),]


# remove unwanted columns
variables <- c("Age","Education","LOS",
               "Category","SupervisoryStatus")

df_dj_t_2005 <- df_dj_2005[,c(variables, "Pay")]
df_dj_t_2013 <- df_dj_2013[,c(variables, "Pay")]


## Pay prediction ##

# feature selection
consistency(Pay ~ ., data = df_dj_t_2005)
cfs(Pay ~ ., data = df_dj_t_2005)

# decision tree control
rpart_c <- trainControl(method = "cv", number = 10)
# random forest control
rf_c <- trainControl(method = "cv",
                       indexOut = createFolds(df_dj_2005$Pay, k = 10))

# 2005
fitted_models <- compare_models(df_dj_t_2005, "Pay", c("Age", "Education", "Category"),
                                "rpart", rpart_c, "rf", rf_c)
varImp(fitted_models$m1fit$finalModel)
varImp(fitted_models$m2fit$finalModel)
rpart.plot(fitted_models$m1fit$finalModel, extra = 2, under = TRUE, varlen = 0, faclen = 0)

# 2013
fitted_models <- compare_models(df_dj_t_2013, "Pay", c("Age", "Education", "Category"),
                                "rpart", rpart_c, "rf", rf_c)
varImp(fitted_models$m1fit$finalModel)
varImp(fitted_models$m2fit$finalModel)
rpart.plot(fitted_models$m1fit$finalModel, extra = 2, under = TRUE, varlen = 0, faclen = 0)


## Education prediction ##

# apply education classes
edu_breaks <- function(df) {
    df$Education <- cut(df$Education,
              breaks = c(0, 2, 6, 13, 20, Inf),
              labels = c("Elm","HS", "Col", "Grad", "Doc"))
    return(df)
}

df_dj_t_2005 <- edu_breaks(df_dj_t_2005)
df_dj_t_2013 <- edu_breaks(df_dj_t_2013)

consistency(Education ~ ., data = df_dj_t_2005)
cfs(Education ~ ., data = df_dj_t_2005)

# https://stackoverflow.com/questions/24142576/one-hot-encoding-in-r-categorical-to-dummy-variables
# encode pay dummy variables as strings
dj_edu_2005 <- with(df_dj_t_2005, data.frame(
    model.matrix(~Pay-1, df_dj_t_2005), Age, Education, Category))
dj_edu_2013 <- with(df_dj_t_2013, data.frame(
    model.matrix(~Pay-1, df_dj_t_2013), Age, Education, Category))

# 2005
fitted_models_2005 <- compare_models(dj_edu_2005, "Education", c("Age",
                                    "Category", "Pay.30k", "Pay30.50k",
                                    "Pay50.70k", "Pay70.90k", "Pay90k.110k",
                                    "Pay.110k"), "rpart", rpart_c, "rf", rf_c)
varImp(fitted_models_2005$m1fit$finalModel)
varImp(fitted_models_2005$m2fit$finalModel)
rpart.plot(fitted_models_2005$m1fit$finalModel, extra = 2, under = TRUE, varlen = 0, faclen = 0)

# 2013
fitted_models_2013 <- compare_models(dj_edu_2013, "Education", c("Age",
                                     "Category", "Pay.30k", "Pay30.50k",
                                     "Pay50.70k", "Pay70.90k", "Pay90k.110k",
                                     "Pay.110k"), "rpart", rpart_c, "rf", rf_c)
varImp(fitted_models_2013$m1fit$finalModel)
varImp(fitted_models_2013$m2fit$finalModel)
rpart.plot(fitted_models_2013$m1fit$finalModel, extra = 2, under = TRUE, varlen = 0, faclen = 0)



library(rpart)
model <- rpart(Pay ~ Age + Education + LOS + Station + SupervisoryStatus, data = df_dj_2005)
model



library('rpart.plot')
rpart.plot(model, extra = 2, under = TRUE, varlen=0, faclen=0)
