# load from workspace file
setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-3")
load("../clean-data/persisted-full-frames.RData")

library(arules)

df_2013$X <- NULL
df_2013$Agency <- NULL
which(sapply(df_2013, FUN = function(i) is.numeric(i)))

for(i in which(sapply(df_2013, FUN = function(i) is.numeric(i)))) {
    df_2013[[i]] <- discretize(df_2013[[i]], method = "frequency")
}

trans <- as(df_2013, "transactions")
