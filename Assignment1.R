
## FIT3152 Assignment 1
## Written by: Zhiyue Li
## Last Modified date: 23th April 2021

# Reference used to finish the assignment
# https://stackoverflow.com/questions/53561299/rmin-and-max-of-a-date-column-in-a-dataframe/53561477
# https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
# https://www.kaggle.com/jmmvutu/eda-online-c2c-fashion-store-user-behaviour
# https://stackoverflow.com/questions/12976542/how-to-convert-in-both-directions-between-year-month-day-and-dates-in-r

## clear the environment
rm(list=ls())
set.seed(28280016)   # my student ID = 28280016
# Random seed to make subset reproducible

# Clear the console output
cat("\014")

## Install and load some necessary libraries if not dowloaded
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggcorrplot")
#install.packages("lubridate")
#install.packages(c("igraph","igraphdata))
#install.packages("sand")
#install.packages("date")
#install.packages("mapplots")
#install.packages("ggraph")

library(igraph)
library(igraphdata)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)
library(stringr)
library(ggcorrplot)
library(lubridate)
library(sand)
library(date)
library(mapplots)
library(network)
library(tidyverse)
library(ggraph)
#library("TTR")



# Step 1: Import the data
#setwd("~/desktop/FIT3152/Assignment 1")
webforum <- read.csv("webforum.csv",stringsAsFactors =FALSE)
webforum <- webforum[sample(nrow(webforum),20000), ] # 20000 rows
attach(webforum)
head(webforum)

## Preliminary Analysis
str(webforum)    # Overview of webforum

summary(webforum)
min(webforum$WC)
which(is.na(webforum$Date))   # No missing date value in Date column


# Notice there are few -1 for AuthorID,find out how many of them among dataset
AuthorID_1 <- with(webforum,length(AuthorID[AuthorID == -1]))
AuthorID_1

# Notice if there are some zero WC length
WC_zero <- with(webforum,length((WC[WC == 0])))


# Notice if there are duplicate rows
duplicateRows <- webforum[duplicated(webforum),]   # yes, there are duplicate rows

# Notice the domain of Date
range(webforum$Date, na.rm = TRUE)

# Extract linguistic variables
linguistic_var <- webforum[ ,6:19]

# Plot correlation between linguistics in the map
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#use-heatmap
# Use corrplot() function: Draw a correlogram
#install.packages("corrplot")
library("corrplot")
res <- cor(linguistic_var)
#install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(linguistic_var))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Plot correlation between linguistics in the bar
linguistic_cor <- as.data.frame(as.table(cor(linguistic_var)))
linguistic_cor <- linguistic_cor[!(linguistic_cor$Var1 == linguistic_cor$Var2),]
linguistic_cor$Freq <- abs(linguistic_cor$Freq)   
cdf <- as.data.frame(as.table(by(linguistic_cor, linguistic_cor$Var2, function(df) sum(df$Freq))))
cdf <- cdf[order(-cdf$Freq),]
plot(cdf[1:4,])
xaxes <- cdf$linguistic_cor.Var2
yaxes <- cdf$Freq
graph <- ggplot(data = cdf,aes(x = xaxes,y = yaxes)) 
graph <- graph + geom_col() +  labs(title = "Correlation of Linguisitcs", x = "Linguistics",y="Sum of correlation")
plot(graph)

# Based on this plot, top 5 linguistic plot,Affect, Positive emotions, Negative emotions, Clout and Tone are chosen for analysis
## Step 2: Tidying (Cleaning) 
# Assign new data set
web <- webforum
attach(web)

# Remove the row with -1 AuthorID
web <- web[web$AuthorID != -1,]

# Remove the WC with 0 count
web <- web[web$WC != 0,]

# Filter out data based on date
web <- web %>%
  filter(Date >= as.Date("2002-01-01") & Date <= as.Date("2011-12-31"))

## Step 3: Transform, Visualize and Model ( Data Analysis )

# Part a: Analyze activity and language on the forum over time.
# Participants active or not.
# Thread divided into two different parts: active or inactive
thread_activity <- ddply(web,.(ThreadID), function(df) length(unique(df$Date)))
#thread_activity
colnames(thread_activity)[2] <- "Thread_Frequency"
summary(thread_activity)
plot(thread_activity)
threshold_active <- ceiling(mean(thread_activity$`Thread_Frequency`))

## Find out the active participants flow diagram
melbourne <- as.data.frame(table(web$Date))
colnames(melbourne)[1] <- "Date"
colnames(melbourne)[2] <- "Frequency"
active_threshold <- median(melbourne$Frequency)
summary(melbourne)
active_melbourne <- melbourne[melbourne$Frequency>2,]
p <- ggplot(active_melbourne, aes(x=Date, y=Frequency,group = 1)) +
            geom_line(aes(y = Frequency)) 

active_participant <- ts(active_melbourne$Frequency,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_participant, main = "Active Participant",ylab = "Frequency",xlab = "Month")
summary(active_participant)

adf.test(active_participant) # p-value < 0.05 indicates the TS is stationary

# Calculate  the mean of 5 linguistic variables of active thread group ordered by month
ts_month_active_threads <- web[web$ThreadID > threshold_active,] %>%
                              dplyr::group_by(Date) %>%
                              dplyr::summarise(
                                affect = median(affect),
                                Clout = median(Clout),
                                Tone = median(Tone),
                                posemo = median(posemo),
                                negemo = median(negemo),
                                Analytic = median(Analytic)
                              )


dta.sum <- aggregate(x = ts_month_active_threads[c("Date")],
                     FUN = sum,
                     by = list(Group.date = ts_month_active_threads$Date))

fit <- lm(Clout~Tone + affect,data=ts_month_active_threads)
summary(fit)

fit1 <- lm(affect~posemo+negemo,data=ts_month_active_threads)
summary(fit1)

active_linguistic_posemo <- ts(ts_month_active_threads$posemo,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_linguistic, main = "Time Series(Linguistics) for Active Thread Group",ylab = "Positive emotions",xlab = "Month")

active_linguistic_negemo <- ts(ts_month_active_threads$negemo,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_linguistic_negemo, main = "Time Series(Linguistics) for Active Thread Group",ylab = "Negative emotions",xlab = "Month")

active_linguistic_affect <- ts(ts_month_active_threads$affect,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_linguistic_affect, main = "Time Series(Linguistics) for Active Thread Group",ylab = "Affect",xlab = "Month")

active_linguistic_analytic <- ts(ts_month_active_threads$Analytic,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_linguistic_analytic, main = "Time Series(Linguistics) for Active Thread Group",ylab = "Analytic",xlab = "Month")

active_linguistic_clout <- ts(ts_month_active_threads$Clout,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_linguistic_clout, main = "Time Series(Linguistics) for Active Thread Group",ylab = "Clout",xlab = "Month")

active_linguistic_tone <- ts(ts_month_active_threads$Tone,frequency = 12, start = c(2002,1), end = c(2011,12))
ts.plot(active_linguistic_tone, main = "Time Series(Linguistics) for Active Thread Group",ylab = "Tone",xlab = "Month")

acf(active_linguistic_tone, lag.max = 36)


ts_month_active_threads$Date <- substring(ts_month_active_threads$Date,1,4)

ts_posemo_mean <- aggregate(ts_month_active_threads$posemo,by =list(Category = ts_month_active_threads$Date),FUN = median)
ts_clout_mean <- aggregate(ts_month_active_threads$Clout,by =list(Category = ts_month_active_threads$Date),FUN = median)
ts_analytic_mean <- aggregate(ts_month_active_threads$Analytic,by =list(Category = ts_month_active_threads$Date),FUN = median)
ts_tone_mean <- aggregate(ts_month_active_threads$Tone,by =list(Category = ts_month_active_threads$Date),FUN = median)
ts_affect_mean <- aggregate(ts_month_active_threads$affect,by =list(Category = ts_month_active_threads$Date),FUN = median)


#install.packages("tseries")
library(tseries)
adf.test(active_linguistic_posemo) # p-value < 0.05 indicates the TS is stationary
adf.test(active_linguistic_negemo) # p-value < 0.05 indicates the TS is stationary
adf.test(active_linguistic_clout) # p-value < 0.05 indicates the TS is stationary
adf.test(active_linguistic_analytic) # p-value < 0.05 indicates the TS is stationary
adf.test(active_linguistic_affect) # p-value < 0.05 indicates the TS is stationary
adf.test(active_linguistic_tone) # p-value < 0.05 indicates the TS is stationary


Box.test(active_linguistic_posemo, lag=15, type="Ljung-Box") # test stationary signal


# Part b: Analyze the language used by groups
# New data used for analysis
# Group data by thread
web_task2<- data.frame()
web_task2 <- by(web,web$ThreadID,function(df) df[order(df$Date),])
head(web_task2)

newdata <- data.frame()
# Find ThreadID index for threadID
for (threadID in unique(web$ThreadID))
{
  temp <- web[web$ThreadID == threadID,]
  frequency <- nrow(temp)
  temp_clout <- median(temp$Clout)
  temp_affect <- median(temp$affect)
  temp_posemo <- median(temp$posemo)
  temp_negemo <- median(temp$negemo)
  temp_analytic <- median(temp$Analytic)
  temp_tone <- median(temp$Tone)

  if (frequency >9)
  {
    newdata <- rbind(newdata, c(threadID,temp_clout,temp_affect,temp_posemo,temp_negemo,frequency,temp_analytic,temp_tone))  
  }
}
colnames(newdata)[1] <-"ThreadID"
colnames(newdata)[2] <- "Clout"
colnames(newdata)[3] <- "affect"
colnames(newdata)[4] <- "posemo"
colnames(newdata)[5] <- "negemo"
colnames(newdata)[6] <- "Frequency"
colnames(newdata)[7] <- "Analytic"
colnames(newdata)[8] <- "Tone"




# Group by the ThreadID and analyse some linguistic variables( or all )
newdata1 <- aggregate(newdata$posemo,by =list(Category = newdata$ThreadID),FUN = sum)
colnames(newdata1)[1] <- "ThreadID"
colnames(newdata1)[2] <- "Positiveemo"
threshold_postive_emo <- mean(newdata1$Positiveemo)
ggplot(data=newdata1,aes(x=ThreadID,y=Positiveemo)) + geom_point() +geom_hline(yintercept = threshold_postive_emo,linetype="dashed",color="red")

newdata2 <- aggregate(newdata$negemo,by =list(Category = newdata$ThreadID),FUN = sum)
colnames(newdata2)[1] <- "ThreadID"
colnames(newdata2)[2] <- "negemo"
threshold_negemo<- mean(newdata2$negemo)
ggplot(data=newdata2,aes(x=ThreadID,y=negemo)) + geom_point() +geom_hline(yintercept = threshold_negemo,linetype="dashed",color="red")

newdata3 <- aggregate(newdata$affect,by =list(Category = newdata$ThreadID),FUN = sum)
colnames(newdata3)[1] <- "ThreadID"
colnames(newdata3)[2] <- "Affect"
threshold_affect <- mean(newdata3$Affect)
ggplot(data=newdata3,aes(x=ThreadID,y=Affect)) + geom_point() +geom_hline(yintercept = threshold_affect,linetype="dashed",color="red")


newdata4 <- aggregate(newdata$Clout,by =list(Category = newdata$ThreadID),FUN = sum)
colnames(newdata4)[1] <- "ThreadID"
colnames(newdata4)[2] <- "Clout"
threshold_clout <- mean(newdata4$Clout)
clout_plot <- ggplot(data=newdata4,aes(x=ThreadID,y=Clout))
plot(clout_plot)      

newdata5 <- aggregate(newdata$Analytic,by =list(Category = newdata$ThreadID),FUN = sum)
colnames(newdata5)[1] <- "ThreadID"
colnames(newdata5)[2] <- "Analytic"
threshold_analytic <- mean(newdata5$Analytic)
ggplot(data=newdata5,aes(x=ThreadID,y=Analytic)) + 
  geom_point() + 
  geom_hline(yintercept = threshold_analytic,linetype="dashed",color="red")

newdata6 <- aggregate(newdata$Tone,by =list(Category = newdata$ThreadID),FUN = sum)
colnames(newdata6)[1] <- "ThreadID"
colnames(newdata6)[2] <- "Tone"
threshold_tone <- mean(newdata6$Tone)
ggplot(data=newdata6,aes(x=ThreadID,y=Tone)) + 
  geom_point() + 
  geom_hline(yintercept = threshold_tone,linetype="dashed",color="red")


## Language used within threads (or between threads ) change over time
newdata_top_20 <- newdata[sample(nrow(newdata), 100), ] # randomly chose 10 threadID to see language change over time


sample_data <- web[web$ThreadID %in% newdata_top_20$ThreadID,]
sample_data$Date <- substring(sample_data$Date,1,4)

sample_group_date <- aggregate(sample_data$Tone,by = list(Category = sample_data$Date), FUN = median)
colnames(sample_group_date)[1] <- "Date"
colnames(sample_group_date)[2] <- "Tone"
head(sample_group_date)

ggplot(data = sample_group_date, aes(x =Date, y= Tone,group = 1)) + geom_line()


## Analysise the Language used within threads
sample_with_thread <- web[web$ThreadID == 252620, ]

sample_with_thread$Date <- substring(sample_with_thread$Date,1,7)

sample_group_date <- aggregate(sample_with_thread$Tone,by = list(Category = sample_with_thread$Date), FUN = median)


sample_group_date <- aggregate(sample_with_thread$Tone,by = list(Category = sample_with_thread$Date), FUN = median)
colnames(sample_group_date)[1] <- "Date"
colnames(sample_group_date)[2] <- "Tone"
ggplot(data = sample_group_date, aes(x =Date, y= Tone,group = 1)) + geom_line()

sample_group_date <- aggregate(sample_with_thread$posemo,by = list(Category = sample_with_thread$Date), FUN = median)
colnames(sample_group_date)[1] <- "Date"
colnames(sample_group_date)[2] <- "posemo"
ggplot(data = sample_group_date, aes(x =Date, y= posemo,group = 1)) + geom_line()

sample_group_date <- aggregate(sample_with_thread$negemo,by = list(Category = sample_with_thread$Date), FUN = median)
colnames(sample_group_date)[1] <- "Date"
colnames(sample_group_date)[2] <- "negemo"
ggplot(data = sample_group_date, aes(x =Date, y= negemo,group = 1)) + geom_line()

sample_group_date <- aggregate(sample_with_thread$Clout,by = list(Category = sample_with_thread$Date), FUN = median)
colnames(sample_group_date)[1] <- "Date"
colnames(sample_group_date)[2] <- "Clout"
ggplot(data = sample_group_date, aes(x =Date, y= Clout,group = 1)) + geom_line()

sample_group_date <- aggregate(sample_with_thread$Analytic,by = list(Category = sample_with_thread$Date), FUN = median)
colnames(sample_group_date)[1] <- "Date"
colnames(sample_group_date)[2] <- "Analytic"
ggplot(data = sample_group_date, aes(x =Date, y= Analytic,group = 1)) + geom_line()



## Group by AuthorID and Analyse linguistic variables (Analytic, Clout,anx,Tone, Authentic)
newdata_author <- data.frame()
for (authorID in unique(web$AuthorID))
{
  temp <- web[web$AuthorID == authorID,]
  frequency <- nrow(temp)
  temp_analytic <- median(temp$Analytic)
  temp_clout <- median(temp$Clout)
  temp_tone <- median(temp$Tone)
  temp_authentic <- max(temp$Authentic)
  temp_anx <- sum(temp$anx)
  
  if (frequency >5)
  {
    newdata_author <- rbind(newdata_author, c(authorID,temp_analytic,temp_clout,temp_tone,temp_authentic,temp_anx,frequency))  
  }
}
colnames(newdata_author)[1] <-"AuthorID"
colnames(newdata_author)[2] <- "Analytics"
colnames(newdata_author)[3] <- "Clout"
colnames(newdata_author)[4] <- "Tone"
colnames(newdata_author)[5] <- "Authentic"
colnames(newdata_author)[6] <- "anx"
colnames(newdata_author)[7] <- "Frequency"
summary(newdata_author)

newdata_author1 <- aggregate(newdata_author$anx,by =list(Category = newdata_author$AuthorID),FUN = sum)
colnames(newdata_author1)[1] <- "AuthorID"
colnames(newdata_author1)[2] <- "Anx"
threshold_anx <- mean(newdata_author1$Anx)
ggplot(data=newdata_author1,aes(x=AuthorID,y=Anx)) + geom_point() +geom_hline(yintercept = threshold_anx,linetype="dashed",color="red")


newdata_author2 <- aggregate(newdata_author$Tone,by =list(Category = newdata_author$AuthorID),FUN = sum)
colnames(newdata_author2)[1] <- "AuthorID"
colnames(newdata_author2)[2] <- "Tone"
threshold_tone <- mean(newdata_author2$Tone)
ggplot(data=newdata_author2,aes(x=AuthorID,y=Tone)) + geom_point() +geom_hline(yintercept = threshold_tone,linetype="dashed",color="red")



newdata_author3 <- aggregate(newdata_author$Analytics,by =list(Category = newdata_author$AuthorID),FUN = sum)
colnames(newdata_author3)[1] <- "AuthorID"
colnames(newdata_author3)[2] <- "Analytics"
threshold_analytic <- mean(newdata_author3$Analytics)
ggplot(data=newdata_author3,aes(x=AuthorID,y=Analytics)) + geom_point() +geom_hline(yintercept = threshold_analytic,linetype="dashed",color="red")



## Part c : Social network online (Network Analysis), trees are chosen
n_vertices <- length(unique(web$ThreadID))
n_dates <- length(unique(web$Date))

#https://www.jessesadler.com/post/network-analysis-with-r/

# Extract dates based on the range of date
temp1<-web %>%
  filter(Date >= as.Date("2002-01-01") & Date <= as.Date("2002-01-31"))

threadID_list <- unique(temp1$ThreadID)
authorID_list <- unique(temp1$AuthorID)
dots <- data.frame()
dots$ThreadID <- threadID_list
dots$AuthorID <- authorID_list

# Edge information
temp1<-aggregate(.~ThreadID+AuthorID, temp1, count)   # per route, count the number of occurrence
temp2 <- temp1 %>% select(c(1,2,6))

temp2$Post <- temp2$Analytic[ ,2]
temp2$Weight <- lengths(temp2$Post)

# Delete columns
temp2$Analytic <- NULL
temp2$Post <- NULL
colnames(temp2)[1] <- "source"
colnames(temp2[2]) <- "destination"

threadID_list <- unique(temp2$ThreadID)
authorID_list <- unique(temp2$AuthorID)
nodes <- data.frame(c(threadID_list,authorID_list))  %>% rowid_to_column("id")
colnames(nodes)[1] <- "id"
colnames(nodes)[2] <- "label"

edges <- temp2 %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)
 
temp2$ThreadID <- as.character(temp2$ThreadID)
temp2$AuthorID <- as.character(temp2$AuthorID)
class(temp2$AuthorID)   # char
class(temp2$ThreadID)    # char

# network objects
thread_network = network(temp2,vertex.attr = nodes, matrix.type = "edgelist")
thread_network
betweeness(thread_network)
plot(thread_network,vertex.cex = 3)

## Assumed: Participants posting to the same thread at similar times as a social network.
## When participatnts also post to other threads extend
## Check if one author post one in a thread and others also post in this thread, then these author have connection (like trees)
## Moreover, remove author who also post 1

web <- web %>%
  filter(Date >= as.Date("2002-01-01") & Date <= as.Date("2011-12-31"))


author_post_frequency = ddply(web, .(AuthorID), function(df) length(unique(df$ThreadID)))
colnames(author_post_frequency)[2] = c("PostNum")
threshold <- ceiling(median(author_post_frequency$PostNum))
summary(author_post_frequency$PostNum)
author_post_frequency = with(author_post_frequency, author_post_frequency[PostNum > 1,])
summary(author_post_frequency)
nrow(author_post_frequency)

author_post_frequency_authorID = author_post_frequency$AuthorID



## Extract information matching authorID post more than median
author_list_by_threads = web[web$AuthorID %in% author_post_frequency_authorID,] %>%
                      dplyr::group_by(ThreadID) %>%
                      dplyr::summarise(
                        AuthorID = list(unique(AuthorID))
                      )
#install.packages("gtools")
library("gtools")

## my_rel_comb function and creating table functions was cited from the kz_sher
## Here is the link:https://github.com/kz-sher/fit3152_a1/blob/master/FIT3152_A1.R
### Create a function that takes a list of values and generates all the combinations of connection among them
my_rel_comb = function(list){
  temp = length(list)
  if (temp >2)
  {
      return(combinations(n=temp, r=2, v=list))
  }
}

##### Apply combination function to each list of authors and combine them by rows
connection_table = do.call(rbind, lapply(author_list_by_threads$AuthorID, my_rel_comb))
connection_table = unique(connection_table) # Remove those duplicate connections between two authors

#### Convert table (data frame) into graph
g1 <- graph_from_data_frame(connection_table,directed=F)
plot(g1,vertex.color = "red",main = "social network analysis")

#degree <- as.table(degree(g1))
#betwness <- as.table(betweenness(g1))
#closeness <- as.table(closeness(g1))
#eig <- as.table(evcent(g1)$vector)

degree <- centralization.degree(g1)$res
betwness <- centralization.betweenness(g1)$res
closeness <- centralization.closeness(g1)$res
eig <- centralization.evcent(g1)$res


tabularised <- as.data.frame(rbind(degree,betwness,closeness,eig))
tabularised <- t(tabularised)

#install.packages("sna")
# Density
library("sna")
degree(connection_table)

averagePath <- average.path.length(g1)
diameter <- diameter(g1)
cat("\nAverage Path Length: ", averagePath, "\n\n")
cat("\nDiameter: ", diameter, "\n\n")

print(tabularised,digits = 3)

# Order by degrees
print(head(tabularised[order(-degree), ]), digits = 2)
cat("\nDegree: ", degree, "\n\n")

# Order by Betweenness
print(head(tabularised[order(-betwness), ]), digits = 2)
cat("\nBetweenness: ", betwness, "\n\n")


# Order by Eigenvector Centrality
print(head(tabularised[order(-eig), ]), digits = 2)


threadID_frequency = ddply(web, .(ThreadID), function(df) length(unique(df$AuthorID)))
colnames(threadID_frequency)[2] = c("PostNum")
threshold <- ceiling(median(threadID_frequency$PostNum))
summary(threadID_frequency$PostNum)
threadID_frequency = with(threadID_frequency, threadID_frequency[PostNum > 1,])
summary(threadID_frequency)
nrow(threadID_frequency)

threadID_post_frequency_authorID = threadID_frequency$ThreadID


## Extract information matching authorID post more than median
thread_list_by_author = web[web$ThreadID %in% threadID_post_frequency_authorID,] %>%
                            dplyr::group_by(AuthorID) %>%
                            dplyr::summarise(
                              AuthorID = list(unique(ThreadID))
                            )


##### Apply combination function to each list of authors and combine them by rows
connection_table_threadID = do.call(rbind, lapply(thread_list_by_author$AuthorID, my_rel_comb))
connection_table_threadID = unique(connection_table_threadID) # Remove those duplicate connections between two authors

#### Convert table (data frame) into graph
g2 <- graph_from_data_frame(connection_table_threadID,directed=F)
plot(g2,vertex.color = "red",main = "social network analysis")

degree <- centralization.degree(g2)$res
betwness <- centralization.betweenness(g2)$res
closeness <- centralization.closeness(g2)$res
eig <- centralization.evcent(g2)$res

tabularised_threadID <- as.data.frame(rbind(degree,betwness,closeness,eig))
tabularised_threadID <- t(tabularised_threadID)

#install.packages("sna")
# Density
library("sna")
degree(connection_table_threadID)

averagePath <- average.path.length(g2)
diameter <- diameter(g2)
cat("\nAverage Path Length: ", averagePath, "\n\n")
cat("\nDiameter: ", diameter, "\n\n")

print(tabularised,digits = 3)

# Order by degrees
print(head(tabularised[order(-degree), ]), digits = 2)
cat("\nDegree: ", degree, "\n\n")

# Order by Betweenness
print(head(tabularised[order(-betwness), ]), digits = 2)
cat("\nBetweenness: ", betwness, "\n\n")





