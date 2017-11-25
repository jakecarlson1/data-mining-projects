setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-4")
load("../clean-data/persisted-full-frames.RData")

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

    # make supervisory status binary
    df$SupervisoryStatus <- sapply(df$SupervisoryStatus, FUN = function(x)
        if (is.na(x)) NA
        else if (x == 8) 0
        else 1)

    return(df)
}

# load clean data and run prep_attributes
data_dir <- "../clean-data/"

# read two years
df_2005 <- read.csv(file = paste(data_dir, '2005-clean.csv', sep=""),
                    header = TRUE, sep = ",")
df_2013 <- read.csv(file = paste(data_dir, '2013-clean.csv', sep=""),
                    header = TRUE, sep = ",")

df_2005 <- prep_attributes(df_2005)
df_2013 <- prep_attributes(df_2013)

save(df_2005, df_2013,
     file = "../clean-data/persisted-full-frames.RData")

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

prep_df <- function(df) {
    df$X <- NULL
    df$Occupation <- NULL

    return(df)
}


# start here
df_2005 <- prep_df(df_2005)
df_2013 <- prep_df(df_2013)

# subset to complete cases for selected columns
cols <- c("Station","Age","Education","LOS","Pay","SupervisoryStatus")
dfc_2005 <- df_2005[,cols]
dfc_2013 <- df_2013[,cols]

dfc_2005 <- scale(dfc_2005[complete.cases(dfc_2005),])
dfc_2013 <- scale(dfc_2013[complete.cases(dfc_2013),])


# k means
set.seed(1000)

k <- 3
km_2005 <- kmeans(dfc_2005, centers = k)

def.par <- par(no.readonly = TRUE)
layout(t(1:k))
for(i in 1:k) barplot(km_2005$centers[i,], ylim = c(-1,3),
                      main = paste("Cluster", i), las = 2)


# hierarchical clustering gen services
dfc_gs_2005 <- subset_agencies(df_2005, agency_subset = c("GS"))
dfc_gs_2005 <- dfc_gs_2005[,cols]
dfc_gs_2005 <- scale(dfc_gs_2005[complete.cases(dfc_gs_2005),])

d <- dist(dfc_gs_2005)
hc <- hclust(d, method = "complete")
plot(as.dendrogram(hc), leaflab = "none")
rect.hclust(hc, k = k)

