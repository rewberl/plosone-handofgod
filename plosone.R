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

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


#   Get data
    # PLOS ONE
        doi = "10.1371/journal.pone.0146193"
        datepub = alm_datepub(doi)
        events = alm_events(doi)
        ids = alm_ids(doi, info="detail", sum_metrics='day')
        # Convert to data frames
        twitshares.df = data.frame(events$twitter$events)
        counter.df = subset(ids$data, .id == "counter")[2:6]

    # Twitter #handofgod tag or phrase
        handofgod = searchTwitter("#handofgod", n=100000, since="2016-01-04")
        handofgodpaper = searchTwitter("hand of god paper", n=100000, since="2016-01-04")
        # Convert to data frame
        handofgod.df = twListToDF(handofgod)
        handofgodpaper.df = twListToDF(handofgodpaper)
        # Combine and remove duplicates
        handofgod.df = rbind(handofgod.df, handofgodpaper.df)
        handofgod.df = unique(handofgod.df)
        # Save result to CSV
        write.csv(handofgod.df,
                  paste("handofgod_",
                        as.character(format(Sys.time(),
                                            format="%Y-%m-%d_%H-%M-%S")),
                        ".csv", sep=""))

    # Twitter @PLOS/@PLOSONE or #PLOS/#PLOSONE tags
        twitplos = searchTwitter("@PLOS|@PLOSONE|#PLOS|#PLOSONE", n=100000, since="2016-01-04")
        # Convert to data frame
        twitplos.df = twListToDF(twitplos)
        # Save result to CSV
        write.csv(twitplos.df,
                  paste("twitplos_",
                        as.character(format(Sys.time(),
                                            format="%Y-%m-%d_%H-%M-%S")),
                        ".csv", sep=""))

#   Wrangle data
twitshares.df = cbind(rep("Shares", nrow(twitshares.df)), twitshares.df[,c(3,4,2)])
# counter.df = ...
handofgod.df = cbind(rep("#HandOfGod", nrow(handofgod.df)), handofgod.df[,c(5,11,1)])
twitplos.df = cbind(rep("#PLOSONE", nrow(twitplos.df)), twitplos.df[,c(5,11,1)])

colnames(twitshares.df) = c("source", "time", "user", "text")
colnames(handofgod.df) = c("source", "time", "user", "text")
colnames(twitplos.df) = c("source", "time", "user", "text")

    #   Process timepoints
    twitshares.df[,2] = gsub("T", " ", twitshares.df[,2])
    twitshares.df[,2] = gsub("Z", "", twitshares.df[,2])
    twitshares.df[,2] = format(as.POSIXlt(twitshares.df[,2]), format="%Y-%m-%d %H:%M:%S")
    handofgod.df[,2] = format(as.POSIXlt(handofgod.df[,2]), format="%Y-%m-%d %H:%M:%S")
    twitplos.df[,2] = format(as.POSIXlt(twitplos.df[,2]), format="%Y-%m-%d %H:%M:%S")

    #   Merge
    df = rbind(twitshares.df, handofgod.df, twitplos.df)
    
    #   Remove duplicates
#    dupes = df[duplicated(df$user),]
#    dupes = dupes[order(dupes$user, dupes$time),]
    
#    nrow(unique(df[c("time", "user")]))
#    nrow(unique(df["user"]))
    # why are these different?

#   Calculate counts per hour
#    counts = count(df, c("server","hr"))
    
#   Create plot
windows()

hist(as.POSIXct(twitshares.df$time), breaks="hours", freq=TRUE)
hist(as.POSIXct(handofgod.df$time), breaks="hours", freq=TRUE)
hist(as.POSIXct(twitplos.df$time), breaks="hours", freq=TRUE)

hist(as.POSIXct(df$time), breaks="hours", freq=TRUE)
    
    # Plotly
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
    geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

(gg <- ggplotly(p))

# draw lines at times of PLOS comments, Retraction Watch article, Nature Communications article, retraction...