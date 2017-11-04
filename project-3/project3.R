# load from workspace file
setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-3")
load("../clean-data/persisted-full-frames.RData")

library(arules)
library(arulesViz)

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

# apply education classes
edu_breaks <- function(df) {
    df$Education <- cut(df$Education,
                        breaks = c(0, 2, 6, 13, 20, Inf),
                        labels = c("Elm","HS", "Col", "Grad", "Doc"))
    return(df)
}

prep_df <- function(df) {
    df$X <- NULL
    df$Agency <- NULL # same as AgencyName
    df$Station <- NULL # same as region
    df$Occupation <- NULL
    df$SupervisoryStatus <- factor(df$SupervisoryStatus)
    df$region <- factor(df$region)

    df <- edu_breaks(df)

    return(df)
}

as_trans <- function(df) {
    for(i in which(sapply(df, FUN = function(i) is.numeric(i)))) {
        df[[i]] <- discretize(df[[i]], method = "frequency")
    }
    df <- as(df, "transactions")
    return(df)
}

# gen services
df_gs_2005 <- subset_agencies(df_2005, agency_subset = c("GS"))
trans_gs_2005 <- as_trans(prep_df(df_gs_2005))
itemFrequencyPlot(trans_gs_2005, topN = 50)
rules_2005 <- apriori(trans_gs_2005, parameter = list(supp = .01, conf = .8))

df_gs_2013 <- subset_agencies(df_2013, agency_subset = c("GS"))
trans_gs_2013 <- as_trans(prep_df(df_gs_2013))
itemFrequencyPlot(trans_gs_2013, topN = 50)
rules_2013 <- apriori(trans_gs_2013, parameter = list(supp = .01, conf = .8))

inspect(head(sort(rules_2005, by = "lift")))
inspect(head(sort(rules_2013, by = "lift")))

# all
trans_2013 <- as_trans(prep_df(df_2013))
itemFrequencyPlot(trans_2013, topN = 50)
rules <- apriori(trans_2013, parameter = list(supp = .01, conf = .8))


summary(rules)
inspect(head(rules, by = "lift"))



plot(rules, engine = "html")

plot(rules, method = "grouped")
