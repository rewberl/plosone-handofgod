#   Clear workspace
ls()
rm(list=ls())
ls()

#   Initialize workspace
library(alm)
library(twitteR)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


#   Sentiment evaluation function
#   Source: Sergey Bryl', http://analyzecore.com/2014/04/28/twitter-sentiment-analysis/
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words){
        sentence = gsub('[[:punct:]]', "", sentence)
        sentence = gsub('[[:cntrl:]]', "", sentence)
        sentence = gsub('\\d+', "", sentence)
        sentence = tolower(sentence)
        word.list = str_split(sentence, '\\s+')
        words = unlist(word.list)
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, pos.words, neg.words, .progress=.progress)
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}


#   Get data
    #   PLOS ONE
    doi = "10.1371/journal.pone.0146193"
    datepub = alm_datepub(doi)
    events = alm_events(doi)
    ids = alm_ids(doi, info="detail", sum_metrics='day')
        #   Convert to data frames
        twitshares.df = data.frame(events$twitter$events)
        counter.df = subset(ids$data, .id == "counter")[2:6]
        #   Format
        twitshares.df = cbind(rep("Shares", nrow(twitshares.df)), twitshares.df[,c(3,4,2)])
        colnames(twitshares.df) = c("source", "time", "user", "text")
        twitshares.df[,2] = gsub("T", " ", twitshares.df[,2])
        twitshares.df[,2] = gsub("Z", "", twitshares.df[,2])
        twitshares.df[,2] = format(as.POSIXlt(twitshares.df[,2]), format="%Y-%m-%d %H:%M:%S")
        #   Merge with previous CSVs
        twitshares.data = read.csv("data/twitshares_data.csv")
        twitshares.data = rbind(twitshares.data, twitshares.df)
        twitshares.data = unique(twitshares.data)
        write.csv(twitshares.data, "data/twitshares_data.csv", row.names=FALSE)
        
        counter.data = read.csv("data/counter_data.csv")
        counter.data = rbind(counter.data, counter.df)
        counter.data = unique(counter.data)
        write.csv(counter.data, "data/counter_data.csv", row.names=FALSE)

    #   Twitter #handofgod and related tags/phrases
    handofgod = searchTwitter("#handofgod", n=100000, since="2016-01-04")
    handofgodpaper = searchTwitter("hand of god paper", n=100000, since="2016-01-04")
    creatorgate = searchTwitter("#creatorgate", n=100000, since="2016-01-04")
        #   Convert to data frames
        handofgod.df = twListToDF(handofgod)
        handofgodpaper.df = twListToDF(handofgodpaper)
        creatorgate.df = twListToDF(creatorgate)
        #   Combine and remove duplicates
        handofgod.df = rbind(handofgod.df, handofgodpaper.df, creatorgate.df)
        handofgod.df = unique(handofgod.df)
        #   Format
        handofgod.df = cbind(rep("#HandOfGod", nrow(handofgod.df)), handofgod.df[,c(5,11,1)])
        colnames(handofgod.df) = c("source", "time", "user", "text")
        handofgod.df[,2] = format(as.POSIXlt(handofgod.df[,2]), format="%Y-%m-%d %H:%M:%S")
        #   Merge with previous CSV
        handofgod.data = read.csv("data/handofgod_data.csv")
        handofgod.data = rbind(handofgod.data, handofgod.df)
        handofgod.data = unique(handofgod.data)
        write.csv(handofgod.data, "data/handofgod_data.csv", row.names=FALSE)

    #   Twitter @PLOS/@PLOSONE or #PLOS/#PLOSONE tags
    twitplos = searchTwitter("@PLOS|@PLOSONE|#PLOS|#PLOSONE", n=100000, since="2016-01-04")
        #   Convert to data frame
        twitplos.df = twListToDF(twitplos)
        #   Format
        twitplos.df = cbind(rep("#PLOSONE", nrow(twitplos.df)), twitplos.df[,c(5,11,1)])
        colnames(twitplos.df) = c("source", "time", "user", "text")
        twitplos.df[,2] = format(as.POSIXlt(twitplos.df[,2]), format="%Y-%m-%d %H:%M:%S")
        #   Merge with previous CSV
        twitplos.data = read.csv("data/twitplos_data.csv")
        twitplos.data = rbind(twitplos.data, twitplos.df)
        twitplos.data = unique(twitplos.data)
        write.csv(twitplos.data, "data/twitplos_data.csv", row.names=FALSE)


    #   Merge and remove duplicates
    df = rbind(twitshares.data, handofgod.data, twitplos.data)
    df = distinct(df, time, user)
    #   Merge with previous CSV
    df.data = read.csv("data/data.csv")
    df.data = rbind(df.data, df)
    df.data = unique(df.data)
    write.csv(df.data, "data/data.csv", row.names=FALSE)
    
    
#   Load data (to skip above steps)
df.data = read.csv("data/data.csv")

#   Format and sort data by time
df.data$time = format(as.POSIXlt(df.data$time))
df.data = df.data[order(df.data$time),]

#   Sentiment analysis
    #   Read word lists
    #   Source: http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
    #   Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
    #       Proceedings of the ACM SIGKDD International Conference on Knowledge 
    #       Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
    #       Washington, USA, 
    #   Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing 
    #       and Comparing Opinions on the Web." Proceedings of the 14th 
    #       International World Wide Web conference (WWW-2005), May 10-14, 
    #       2005, Chiba, Japan.
pos = scan("data/positive-words.txt", "character", comment.char=";")
neg = scan("data/negative-words.txt", "character", comment.char=";")
neg = c(neg, "wtf", "omg", "fail", "epicfail", "retraction", "resign")

df.data$score = score.sentiment(as.factor(df.data$text), pos, neg)[,1]
    #   Get mean score by source by hour
    df.data$hour = as.factor(cut(as.POSIXct(df.data$time), breaks="hour"))
    sent.means = group_by(df.data, source, hour)
    sent.means = summarize(sent.means, mean=mean(score))

    #   Create plot
    windows()    
    ggplot(sent.means, aes(x=as.POSIXct(hour), y=mean, color=source)) +
        geom_smooth(span=0.5, size=1.5) +
        geom_point(size=1) +
        geom_vline(xintercept=as.numeric(as.POSIXct("2016-03-02 04:25:00"))) +  # first comment on article
        geom_vline(xintercept=as.numeric(as.POSIXct("2016-03-02 11:56:00"))) +  # "Notification from PLOS Staff"
        geom_vline(xintercept=as.numeric(as.POSIXct("2016-03-03 12:45:00"))) +  # "Follow-up Notification from PLOS Staff"
        scale_x_datetime(limits=c(as.POSIXct("2016-02-23"), as.POSIXct("2016-03-11")))
    

    
#   Create plot
windows()

hist(as.POSIXct(df.data$time), breaks="hours", freq=TRUE)

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
# also scrape retraction article DOI
# sentiment analysis