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

    # convert education encoding to the number of years needed to achieve a
    # certain degree
    df$Education <- as.numeric(as.character(df$Education))
    # edu_map <- list(1 = 4, 2 = 8, 3 = 10, 4 = 12, 5 = 13, 6 = 14, 7 = 12,
    #                 8 = 13, 9 = 14, 10 = 14, 11 = 15, 12 = 16, 13 = 16,
    #                 14 = 17, 15 = 20, 16 = 21, 17 = 18, 18 = 19, 19 = 18,
    #                 20 = 19, 21 = 20, 22 = 22)
    edu_map <- c(4,8,10,12,13,14,12,13,14,14,15,16,16,17,20,21,18,19,18,19,20,22)
    df$Education <- sapply(df$Education, FUN = function(x) edu_map[x])

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
cols <- c("Age","Education","LOS","Pay","SupervisoryStatus")
dfc_2005 <- df_2005[,cols]
dfc_2013 <- df_2013[,cols]

dfc_2005 <- scale(dfc_2005[complete.cases(dfc_2005),])
dfc_2013 <- scale(dfc_2013[complete.cases(dfc_2013),])

# k means
set.seed(1000)

k <- 3
km_2005 <- kmeans(dfc_2005, centers = k)
km_2013 <- kmeans(dfc_2013, centers = k)

def.par <- par(no.readonly = TRUE)

par(mar = c(8,3,2,2))
layout(t(1:k))
for(i in 1:k) barplot(km_2005$centers[i,], ylim = c(-1,3),
                      main = paste("Cluster", i), las = 2)
for(i in 1:k) barplot(km_2013$centers[i,], ylim = c(-1,2),
                      main = paste("Cluster", i), las = 2)

k <- 5
km_2013 <- kmeans(dfc_2013, centers = k)
layout(t(1:k))
for(i in 1:k) barplot(km_2013$centers[i,], ylim = c(-2,3),
                      main = paste("Cluster", i), las = 2)

ks <- 2:16
WSS <- sapply(ks, FUN=function(k) {
    kmeans(dfc_2013, centers=k, nstart=2)$tot.withinss
})
par(def.par)
plot(ks, WSS, type="l", main="Within Sum of Squares 2013")
abline(v=8, col="red", lty=2)

k <- 8
km_2013 <- kmeans(dfc_2013, centers = k)
layout(t(1:k))
for(i in 1:k) barplot(km_2013$centers[i,], ylim = c(-2,3),
                      main = paste("Cluster", i), las = 2)

# reset par
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar())

library(cluster)
clusplot(dfc_2005, km_2005$cluster)


# hierarchical clustering
# subset to complete cases for selected columns
cols <- c("Agency","Age","Education","Pay")
dfc_2005 <- df_2005[,cols]
dfc_2013 <- df_2013[,cols]

dfc_2005 <- dfc_2005[complete.cases(dfc_2005),]
dfc_2013 <- dfc_2013[complete.cases(dfc_2013),]

# read agency translation
agency_file <- paste("../data/", "SCTFILE.TXT", sep = "")
agency_trans <- readLines(agency_file)
agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,4))
agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)

# group by agencies, uses clustering of km (k = 3)
group_agencies <- function(df, km) {
    df$Agency <- sapply(df$Agency, FUN = function(x) substring(x, 1,2))

    sizes <- as.data.frame(table(df$Agency))
    colnames(sizes) <- c("Agency", "size")

    df_1 <- setNames(aggregate(cbind(Age, Education, Pay) ~ Agency,
                               data = df[km$cluster == 1,],
                               FUN = mean),
                     c("Agency", paste("k.1.", cols[2:4], sep = "")))
    df_2 <- setNames(aggregate(cbind(Age, Education, Pay) ~ Agency,
                               data = df[km$cluster == 2,],
                               FUN = mean),
                     c("Agency", paste("k.2.", cols[2:4], sep = "")))
    df_3 <- setNames(aggregate(cbind(Age, Education, Pay) ~ Agency,
                               data = df[km$cluster == 3,],
                               FUN = mean),
                     c("Agency", paste("k.3.", cols[2:4], sep = "")))

    df <- merge(merge(merge(df_1, df_2), df_3), sizes)

    rownames(df) <- df$Agency
    df <- df[,-1]

    # replace rowname with agency name
    # m <- match(rownames(df), agency_trans_table$agency_ID)
    # rownames(df) <-  agency_trans_table$agency_name[m]

    return(df)
}


df_agency_2005 <- group_agencies(dfc_2005, km_2005)
df_agency_2013 <- group_agencies(dfc_2013, km_2013)

d_2005 <- dist(scale(df_agency_2005))
cl_2005 <- hclust(d_2005)
plot(cl_2005)

d_2013 <- dist(scale(df_agency_2013))
cl_2013 <- hclust(d_2013)
plot(cl_2013)

# without size
d_2005 <- dist(scale(df_agency_2005[,colnames(df_agency_2005) != "size"]))
cl_2005 <- hclust(d_2005)
plot(cl_2005, main = "2005 Agency Cluster Dentrogram")
rect.hclust(cl_2005, k=6)

clusplot(df_agency_2005, cutree(cl_2005, k=6), labels=2, main="2005 Cluster Plot")

d_2013 <- dist(scale(df_agency_2013[,colnames(df_agency_2013) != "size"]))
cl_2013 <- hclust(d_2013)
plot(cl_2013, main = "2013 Agency Cluster Dentrogram")
rect.hclust(cl_2013, k=8)

clusplot(df_agency_2013, cutree(cl_2013, k=8), labels=3, main="2013 Cluster Plot")




pc_2005 <- prcomp(scale(df_agency_2005[,colnames(df_agency_2005) != "size"]))
biplot(pc_2005, col = c("grey", "red"), main = "2005 Hierarchical Clustering PCA")

library(seriation)
dissplot(d_2005, labels=kmeans(scale(df_agency_2005), centers=4)$cluster)

ks <- 2:16
ASW <- sapply(ks, FUN=function(k) {
    fpc::cluster.stats(d_2005, cutree(cl_2005, k))$avg.silwidth
})
plot(ks, ASW, type="l", main="2005 Average Silhouette Width")

# hierarchical clustering gen services
dfc_gs_2005 <- subset_agencies(df_2005, agency_subset = c("GS"))
dfc_gs_2005 <- dfc_gs_2005[,cols]
dfc_gs_2005 <- scale(dfc_gs_2005[complete.cases(dfc_gs_2005),])

d <- dist(dfc_gs_2005)
hc <- hclust(d, method = "complete")
plot(as.dendrogram(hc), leaflab = "none")
rect.hclust(hc, k = k)

