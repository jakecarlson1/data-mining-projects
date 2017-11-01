# load from workspace file
setwd("~/Desktop/School/CSE/CSE_5331/projects.nosync/project-3")
load("../clean-data/persisted-full-frames.RData")

library(arules)
library(arulesViz)

df_2013$X <- NULL
df_2013$Agency <- NULL # same as AgencyName
df_2013$Station <- NULL # same as region
df_2013$Occupation <- NULL
df_2013$region <- factor(df_2013$region)
## also need to ohe supervisory status

# apply education classes
edu_breaks <- function(df) {
    df$Education <- cut(df$Education,
                        breaks = c(0, 2, 6, 13, 20, Inf),
                        labels = c("Elm","HS", "Col", "Grad", "Doc"))
    return(df)
}

df_2013 <- edu_breaks(df_2013)

which(sapply(df_2013, FUN = function(i) is.numeric(i)))

for(i in which(sapply(df_2013, FUN = function(i) is.numeric(i)))) {
    df_2013[[i]] <- discretize(df_2013[[i]], method = "frequency")
}

trans <- as(df_2013, "transactions")

itemFrequencyPlot(trans, topN = 50)

rules <- apriori(trans, parameter = list(supp = .01, conf = .8))
summary(rules)
inspect(head(rules, by = "lift"))

plot(rules, engine = "html")
