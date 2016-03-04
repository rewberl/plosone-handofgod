#   Clear workspace
ls()
rm(list=ls())
ls()

#   Initialize workspace
library(alm)
library(twitteR)
library(dplyr)
library(tidyr)
library(plotly)

#   Get data
    # PLOS ONE
    doi = "10.1371/journal.pone.0146193"
    datepub = alm_datepub(doi)
    events = alm_events(doi)
    ids = alm_ids(doi, info="detail", sum_metrics='day')
        # Convert to data frames
    twitter.df = data.frame(events$twitter$events)
    counter.df = subset(ids$data, .id == "counter")[2:6]

    # Twitter #handofgod tag
    handofgod = searchTwitter("#handofgod", n=100000, since="2016-01-04")
        # Convert to data frame
        handofgod.df = twListToDF(handofgod)
        # Save result to CSV
        write.csv(handofgod.df,
                  paste("handofgod_",
                        as.character(format(Sys.time(),
                                            format="%Y-%m-%d_%H-%M-%S")),
                        ".csv", sep=""))

#   Wrangle data
twitter.df = cbind(rep("Twitter", nrow(twitter.df)), twitter.df[,c(3,4,2)])
# counter.df = ...
handofgod.df = cbind(rep("#handofgod", nrow(handofgod.df)), handofgod.df[,c(5,11,1)])

colnames(twitter.df) = c("source", "time", "user", "text")
colnames(handofgod.df) = c("source", "time", "user", "text")

    #   Process timepoints
    twitter.df[,2] = gsub("T", " ", twitter.df[,2])
    twitter.df[,2] = gsub("Z", "", twitter.df[,2])
    twitter.df[,2] = format(as.POSIXlt(twitter.df[,2]), format="%Y-%m-%d %H:%M:%S")

    #   Merge
    df = rbind(twitter.df, handofgod.df)
    
    #   Remove duplicates
    nrow(unique(df[c("time", "user")]))
    nrow(unique(df["user"]))
    # why are these different?

#   Create plot
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
    geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

(gg <- ggplotly(p))

# draw lines at times of PLOS comments, Retraction Watch article, Nature Communications article, retraction...