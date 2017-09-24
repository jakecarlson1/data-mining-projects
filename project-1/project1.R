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


### data visualization ###
setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-1")
data_dir <- "../clean-data/"

## visualize basic changes in employment in each of my choosen departments ##
# read four years
df_2001 <- read.csv(file = paste(data_dir, '2001-clean.csv', sep=""), header = TRUE, sep = ",")
df_2005 <- read.csv(file = paste(data_dir, '2005-clean.csv', sep=""), header = TRUE, sep = ",")
df_2009 <- read.csv(file = paste(data_dir, '2009-clean.csv', sep=""), header = TRUE, sep = ",")
df_2013 <- read.csv(file = paste(data_dir, '2013-clean.csv', sep=""), header = TRUE, sep = ",")

# remove Department of Labor
df_2001 <- df_2001[!sapply(df_2001$Agency, FUN = function(x) substring(x, 1,2)) == "DL",]
# remove Department of Agriculture
df_2005 <- df_2005[!sapply(df_2005$Agency, FUN = function(x) substring(x, 1,2)) == "AG",]

# get employee count at each department
agency_freq_2001 <- table(sapply(df_2001$Agency, FUN = function(x) substring(x, 1,2)))
agency_freq_2005 <- table(sapply(df_2005$Agency, FUN = function(x) substring(x, 1,2)))
agency_freq_2009 <- table(sapply(df_2009$Agency, FUN = function(x) substring(x, 1,2)))
agency_freq_2013 <- table(sapply(df_2013$Agency, FUN = function(x) substring(x, 1,2)))

# plot four plots
par(mfrow = c(2,2), oma = c(2,2,1,1), mar = c(4,4,2,1))
plot(agency_freq_2001, main = "Number of Employees 2001", xlab = "", ylab = "",
     yaxp = c(0, 1400000, 5), las = 2)
plot(agency_freq_2005, main = "Number of Employees 2005", xlab = "", ylab = "",
     yaxp = c(0, 1400000, 5), las = 2)
plot(agency_freq_2009, main = "Number of Employees 2009", xlab = "", ylab = "",
     yaxp = c(0, 1400000, 5), las = 2)
plot(agency_freq_2013, main = "Number of Employees 2013", xlab = "", ylab = "",
     yaxp = c(0, 1400000, 5), las = 2)
mtext(text = "Department", side = 1, line = 0, outer = TRUE)
mtext(text = "Number of Employees", side = 2, line = 0, outer = TRUE)
# legend(x = "center", ncol = 3,
#        legend = c("DJ - DOJ", "DN - DOE", "ED - DO Education",
#                   "EM - FEMA", "EP - EPA", "GS - Gen. Services",
#                   "HE - DOHHS", "HS - Homeland Sec.", "HU - HUD",
#                   "IN - DO Interior", "NN - NASA", "TD - DOT",
#                   "TR - IRS", "VA - Veterans Affairs"))


## visualize the distribution of employees in continental US ##
# adapted from:
# https://stackoverflow.com/questions/24441775/how-do-you-create-a-us-states-heatmap-based-on-some-values
# https://www.r-bloggers.com/us-state-maps-using-map_data/
library('ggplot2')
library('maps')
library('gridExtra')
state_trans <- read.csv('./state-trans.txt', header = TRUE)
states <- map_data('state')

# prepend zero to states with number < 10
state_trans$Num <- sapply(state_trans$Num, FUN = function(x) formatC(x, width = 2, format = "d", flag = "0"))

# prep 2001
# get employment count per state
df_lower_states_2001 <- data.frame(table(tolower(state_trans$State[
    match(sapply(df_2001$Station, FUN = function(x) substring(x, 1,2)),
          state_trans$Num)])))

# create region column for matching
df_lower_states_2001$region <- df_lower_states_2001$Var1
df_lower_states_2001$Var1 <- NULL
# merge with states geospatial data
df_states_2001 <- merge(states, df_lower_states_2001, by = 'region', all.x = TRUE)

# prep 2013
# get employment count per state
df_lower_states_2013 <- data.frame(table(tolower(state_trans$State[
    match(sapply(df_2013$Station, FUN = function(x) substring(x, 1,2)),
          state_trans$Num)])))
# create region column for matching
df_lower_states_2013$region <- df_lower_states_2013$Var1
df_lower_states_2013$Var1 <- NULL
# merge with states geospatial data
df_states_2013 <- merge(states, df_lower_states_2013, by = 'region', all.x = TRUE)

# plot 2001
map_2001 <- ggplot() +
    geom_polygon(data = df_states_2001, aes(x = df_states_2001$long, y = df_states_2001$lat,
                                            group = df_states_2001$group,
                                            fill = df_states_2001$Freq),
                 colour="white") +
    scale_fill_gradientn(colours = c("thistle2", "darkred"), limits = range(0,250000)) +
    theme_bw() + labs(fill = "Employees per State",
                      title = "Federal Employment by State, 2001", x="", y="") +
    scale_y_continuous(breaks = c()) + scale_x_continuous(breaks = c()) +
    theme(panel.border = element_blank(), plot.title = element_text(hjust = 0.5))

# plot 2013
map_2013 <- ggplot() +
    geom_polygon(data = df_states_2013, aes(x = df_states_2013$long, y = df_states_2013$lat,
                                       group = df_states_2013$group,
                                       fill = df_states_2013$Freq),
                 colour="white") +
    scale_fill_gradientn(colours = c("thistle2", "darkred"), limits = range(0,250000)) +
    theme_bw() + labs(fill = "Employees per State",
                      title = "Federal Employment by State, 2013", x="", y="") +
    scale_y_continuous(breaks = c()) + scale_x_continuous(breaks = c()) +
    theme(panel.border = element_blank(), plot.title = element_text(hjust = 0.5))

grid.arrange(map_2001, map_2013, ncol = 2)

## simple stats of state employment counts ##
df_lower_states_2001 <- data.frame(table(tolower(state_trans$State[
    match(sapply(df_2001$Station, FUN = function(x) substring(x, 1,2)),
          state_trans$Num)])))

df_lower_states_2005 <- data.frame(table(tolower(state_trans$State[
    match(sapply(df_2005$Station, FUN = function(x) substring(x, 1,2)),
          state_trans$Num)])))

df_lower_states_2009 <- data.frame(table(tolower(state_trans$State[
    match(sapply(df_2009$Station, FUN = function(x) substring(x, 1,2)),
          state_trans$Num)])))

df_lower_states_2013 <- data.frame(table(tolower(state_trans$State[
    match(sapply(df_2013$Station, FUN = function(x) substring(x, 1,2)),
          state_trans$Num)])))

# order most to least employees
df_lower_states_2001 <- df_lower_states_2001[order(df_lower_states_2001$Freq, decreasing = TRUE),]
df_lower_states_2005 <- df_lower_states_2005[order(df_lower_states_2005$Freq, decreasing = TRUE),]
df_lower_states_2009 <- df_lower_states_2009[order(df_lower_states_2009$Freq, decreasing = TRUE),]
df_lower_states_2013 <- df_lower_states_2013[order(df_lower_states_2013$Freq, decreasing = TRUE),]

# print states with most employees
head(df_lower_states_2001)
head(df_lower_states_2005)
head(df_lower_states_2009)
head(df_lower_states_2013)

# get mean
mean(df_lower_states_2001$Freq)
mean(df_lower_states_2005$Freq)
mean(df_lower_states_2009$Freq)
mean(df_lower_states_2013$Freq)


## get simple stats for age ##
# plot histograms for the four years
par(mfrow = c(2,2), oma = c(2,2,1,1), mar = c(4,4,2,1))
barplot(table(df_2001$Age), main = "Ages of Employees 2001")
barplot(table(df_2005$Age), main = "Ages of Employees 2005")
barplot(table(df_2009$Age), main = "Ages of Employees 2009")
barplot(table(df_2013$Age), main = "Ages of Employees 2013")
mtext(text = "Age Intervals (years)", side = 1, line = 0, outer = TRUE)
mtext(text = "Number of Employees", side = 2, line = 0, outer = TRUE)


# returns class of each column
sapply(df, class)

head(df)
summary(df)

# look at natual disasters and events