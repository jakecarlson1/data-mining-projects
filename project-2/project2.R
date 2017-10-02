setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-2")
data_dir <- "../clean-data/"

# read four years
df_2001 <- read.csv(file = paste(data_dir, '2001-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2005 <- read.csv(file = paste(data_dir, '2005-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2009 <- read.csv(file = paste(data_dir, '2009-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2013 <- read.csv(file = paste(data_dir, '2013-clean.csv', sep=""),
                    header = TRUE, sep = ",")

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
                  breaks = c(0, 50000, 75000, 100000 ,Inf),
                  labels = c("<50k", "50-75k", "75k-100k", ">100k"))
    df$Pay <- factor(df$Pay, ordered = TRUE, levels = levels(df$Pay))

    # drop NA Pay
    df <- df[!is.na(df$Pay),]

    # make age an integer (take middle of range)
    df$Age <- sapply(df$Age, FUN = function(x)
        if(x == "75+") 75
        else (as.integer(substring(x, 1,2)) + as.integer(substring(x, 4,5)))/2)

    return(df)
} # end make_cleaner

# two character identifier for the agency
agency_sub = c("HS")

df_hs_2005 <- make_cleaner(df_2005, agency_sub)


