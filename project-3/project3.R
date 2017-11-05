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

edu <- grep("Education=", itemLabels(trans_gs_2005), value = TRUE)


quality(rules_2005) <- cbind(quality(rules_2005),
    interestMeasure(rules_2005, measure=c("phi", "gini"),
        trans = trans_gs_2005))

# all
trans_2005 <- as_trans(prep_df(df_2005))

trans_2013 <- as_trans(prep_df(df_2013))
# itemFrequencyPlot(trans_2013, topN = 50)
# rules <- apriori(trans_2013, parameter = list(supp = .01, conf = .8))

freq_2005 <- apriori(trans_2005, parameter = list(target = "frequent",
                                                  support = .01))
freq_2005 <- sort(freq_2005, by = "support")
inspect(head(freq_2005, n = 10))

freq_2013 <- apriori(trans_2013, parameter = list(target = "frequent",
                                                  support = .01))
freq_2013 <- sort(freq_2013, by = "support")
inspect(head(freq_2013, n = 10))

# item size histogram
par(mfrow = c(1,2))
barplot(table(size(freq_2005)), xlab="Itemset Size", ylab="Count",
        main = "Itemset Size Frequency 2005")
barplot(table(size(freq_2013)), xlab="Itemset Size", ylab="Count",
        main = "Itemset Size Frequency 2013")

inspect(freq_2005[size(freq_2005)>5])
inspect(freq_2013[size(freq_2013)>5])

par(mfrow = c(2,1))
itemFrequencyPlot(items(freq_2005[size(freq_2005)==5]), support = 0.0103,
                  main = "Item Frequency for Itemsets of Size 5 (2005)")
itemFrequencyPlot(items(freq_2013[size(freq_2013)==5]), support = 0.0103,
                  main = "Item Frequency for Itemsets of Size 5 (2013)")

summary(rules)
inspect(head(rules, by = "lift"))



plot(rules, engine = "html")

plot(rules, method = "grouped")
