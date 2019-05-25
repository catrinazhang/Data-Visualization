---
title: "Homework 3 Kickstarter"
author: "Catrina Zhang"
date: 2019-04-07
output: 
  html_document:
    keep_md: true
  html_notebook: default
---



## Assignment Setup
In this section, I will be loading the package and data required for the assignment.

```r
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(rvest)
library(stringr)
library(plotly)
library(DT)
library(devtools)
library(readr)
library(leaflet)
library(tidytext)
library(quanteda)
library(tm)
library(rJava)
library(qdap)
library(wordcloud)
library(SnowballC)    

setwd("C:/Users/catri/Downloads")
ks=read_csv("kickstarter_projects.csv")
```
## 1) Identifying Successful Projects

There are several ways to identify success of a project:

State (state): Whether a campaign was successful or not.
Pledged Amount (pledged)
Achievement Ratio: Create a variable achievement_ratio by calculating the percentage of the original monetary goal reached by the actual amount pledged (that is pledged\goal *100).
Number of backers (backers_count)
How quickly the goal was reached (difference between launched_at and state_changed_at) for those campaigns that were successful.
Use one or more of these measures to visually summarize which categories were most successful in attracting funding on kickstarter. Briefly summarize your findings.

It seems that based on % of successes, "dance" is the most successful in attracting funding on kickstarter with about 80% of the projects being successful. However, base don pledge amounts "technology"" has way more than any other category, and "dance" is nowhere near the top. Perhaps dance had more % successes because the required amounts are low.

```r
success <- ks %>% 
  select(backers_count, state, top_category, sub_category, pledged)

success$state <- ifelse(success$state == "successful",1,0)

success2 <- success %>%
  group_by(top_category) %>% 
  summarize(state = sum(state)/length(state))

success3 <- success %>%
  group_by(top_category) %>% 
  summarize(pledged = sum(pledged)/1000000)

SuccessState <-
  ggplot(data = success2, aes(x=reorder(top_category, state), y=state, fill = state)) +
  geom_bar(stat = "identity") + 
  labs(title = "Success by % of Successful Campaigns",
       x = "Categories", y = "% of Campaigns Successful") +
  theme(legend.position = "none") +
  coord_flip()

SuccessState
```

![](figures/Q1-1.png)<!-- -->

```r
SuccessPledged <-
  ggplot(data = success3, aes(x=reorder(top_category, pledged), y=pledged, fill = pledged)) +
  geom_bar(stat = "identity") + 
  labs(title = "Success by Total Amount Pledged",
       subtitle = "In $millions",
       x = "Categories", y = "Top Amount Pledged") +
  theme(legend.position = "none") +
  coord_flip()

SuccessPledged
```

![](figures/Q1-2.png)<!-- -->

## 2)

a) To reduce the time for analysis, select the 1000 most successful projects and a sample of 1000 unsuccessful projects. Use the cleaning functions introduced in lecture (or write your own in addition) to remove unnecessary words (stop words), syntax, punctuation, numbers, white space etc. Note, that many projects use their own unique brand names in upper cases, so try to remove these fully capitalized words as well (since we are aiming to identify common words across descriptions). Stem the words left over and complete the stems. Create a document-term-matrix.

Provide a word cloud of the most frequent or important words (your choice which frequency measure you choose) among the most successful projects.

```r
library(qdapRegex)

story <- ks %>% 
  select(blurb, state, pledged) 

storytop <- story %>% 
  filter(state == "successful") %>% 
  arrange(desc(pledged)) %>% 
  head(1000)

storyfail <- story %>% 
  filter(state == 'failed') %>% 
  head(1000)

storie <- rbind(storytop, storyfail)

line = rep(1:1000,2)
doc_id = c(rep("success",1000),rep("fail",1000))

stories <- data.frame(doc_id, text = storie$blurb,
                           line, stringsAsFactors = FALSE)

stories2 <- data.frame(doc_id, pledged = storie$pledged, text = storie$blurb,
                           line, stringsAsFactors = FALSE)

source <- DataframeSource(stories)
corpus <- VCorpus(source)

metaid <- as_data_frame(doc_id)

meta(corpus) <- metaid

meta(corpus, type = 'local', tag = "document") <- corpus$document

removeNumPunct <- function(x){gsub("[^[:alpha:][:space:]]*", "", x)}

removeCaps <- function(x){gsub("[A-Z]+ ", "", x)}

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(removeNumPunct))
  corpus <- tm_map(corpus, content_transformer(removeCaps))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

corpus_clean <- clean_corpus(corpus)

corpus_stemmed <- tm_map(corpus_clean, stemDocument)

stemCompletion2 <- function(x, dictionary) {
   x <- unlist(strsplit(as.character(x), " "))
   x <- x[x != ""]
   x <- stemCompletion(x, dictionary=dictionary)
   x <- paste(x, sep="", collapse=" ")
   PlainTextDocument(stripWhitespace(x))
}

corpus_comp <- lapply( corpus_stemmed, stemCompletion2, 
                     dictionary=corpus_clean)

corpus_comp_all <- as.VCorpus(corpus_comp)


for (i in 1:dim(metaid)[1]){
  corpus_comp_all[[i]]$meta$document <- metaid[i,"value"]
  corpus_comp_all[[i]]$meta$id <- paste(metaid[i,"value"])
}

corpus_dtm <-  DocumentTermMatrix(corpus_comp_all)

corpus_dtm_s <- DocumentTermMatrix(corpus_comp_all[1:1000])
corpus_dtm_f <- DocumentTermMatrix(corpus_comp_all[1001:2000])

corpus_td <- tidy(corpus_dtm)
#meta <- as_data_frame(doc_id)
#corpus_td <- as_data_frame(cbind(corpus_td, meta))

corpus_td_s <- tidy(corpus_dtm_s)
corpus_td_f <- tidy(corpus_dtm_f)

corpus_td2 <- corpus_td_s %>%   
  group_by(term) %>%
                summarise(n = sum(count)) %>%
                top_n(n = 40, wt = n)  %>%
                ungroup() %>%
                mutate(term = reorder(term, n))

corpus_td2 <- corpus_td2[-34,]

wordcloud <- wordcloud(corpus_td2$term, corpus_td2$n, 
         max.words = 100, colors = "purple")
```

![](figures/Q2_a-1.png)<!-- -->

```r
wordcloud
```

```
## NULL
```

b) Success in words
Provide a pyramid plot to show how the words between successful and unsuccessful projects differ in frequency. A selection of 10 - 20 top words is sufficient here.

```r
library(plotrix)

corpus_s <- aggregate(count ~ term, data = corpus_td_s, sum)
corpus_f <- aggregate(count ~ term, data = corpus_td_f, sum)

common <- inner_join(corpus_s, corpus_f, by = "term")

difference <- abs(common[, 2] - common[, 3])

common_words <- cbind(common, difference)

common_words <- common_words[order(common_words[, 4], decreasing = TRUE), ]

top20 <- data.frame(x = common_words[1:20, 2], 
                       y = common_words[1:20, 3], 
                       labels = common_words[1:20, 1])


pyramidPlot <- pyramid.plot(top20$x, top20$y, labels = top20$labels, 
             gap = 15, top.labels = c("Success", " ", "Fail"), lxcol = "green",rxcol = "red",
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL, labelcex=0.8)
```

![](figures/Q2b-1.png)<!-- -->


c) Simplicity as a virtue
These blurbs are short in length (max. 150 characters) but let's see whether brevity and simplicity still matters. Calculate a readability measure (Flesh Reading Ease, Flesh Kincaid or any other comparable measure) for the texts. Visualize the relationship between the readability measure and one of the measures of success. Briefly comment on your finding.

It seems that there is no discernable relationship between readability measure and pledged amounts. There is a line of 0s along the y axis when x=0 because many of failed projects did not raise money at all. 

```r
require(quanteda)
require(dplyr)

# tmp <- data.frame(doc_id = ks$id, text = ks$blurb, pledged = ks$pledged, stringsAsFactors = FALSE) %>% 
#   DataframeSource() %>% 
#   Corpus() %>% 
#   corpus()

corpus_b <- corpus(stories2)
FK_corpus <- textstat_readability(corpus_b,
              measure=c('Flesch.Kincaid'))

FK_corpus2 <- cbind(FK_corpus, pledged = stories2$pledged)

colnames(FK_corpus2) <- c("document", "FK", "pledged")

FK2 <- data_frame(FK = FK_corpus2$FK,
                  pledged = FK_corpus2$pledged, 
                  words = ntoken(corpus_b))

ggplot(data=FK2, aes(x=pledged,y=FK)) + 
  geom_point(aes(color = words)) + geom_smooth() + guides(size=FALSE) + xlab("Amount Pledged") + ylab("Flesch-Kincaid Score") + theme(legend.position="none") + ggtitle("FK Score and Amount Pledged")
```

![](figures/Q2c-1.png)<!-- -->

##3. Sentiment Analysis
a) Stay positive
Calculate the tone of each text based on the positive and negative words that are being used. You can rely on the Hu & Liu dictionary provided in lecture or use the Bing dictionary contained in the tidytext package (tidytext::sentiments). Visualize the relationship between tone of the document and success. Briefly comment.

After I got the tone of each text based on positive and negative words, I made a scatter plot and plotted the sentiment of the document against pledged amounts. There seems to be no relationship between pledged amount and sentiment.

I also grouped pledged amounts by success/fail and then by amount pledged. It seems that for both successful and failed projects, tone is positively associated with pledged amounts, but there is no relationship between sentiment for successful and failed projects. 

```r
stories2$index <- paste(stories2$doc_id, stories2$line)

tidy_corpus <- stories2 %>%
  group_by(index) %>%
  ungroup() %>%
  unnest_tokens(word, text)

sentiments_s <- tidy_corpus %>%
  inner_join(get_sentiments("bing")) %>%
  count(index, pledged, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive-negative)/(positive+negative))

tidy_corpus$sgroup <- ifelse(tidy_corpus$pledged < 5000, "FAIL <5k", ifelse(tidy_corpus$pledged < 10000, "FAIL 5-10k", ifelse(tidy_corpus$pledged < 50000, "FAIL 10k+", ifelse(tidy_corpus$pledged < 250000, "SUCCESS < 250k", ifelse(tidy_corpus$pledged < 500000, "SUCCESS 250-500k", "SUCCESS >500k")))))

sentiments2 <- tidy_corpus %>%
  inner_join(get_sentiments("bing")) %>%
  count(sgroup, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive+negative))

ggplot(data=sentiments_s, aes(x=pledged,y=sentiment)) + 
  geom_point(aes(color = sentiment)) + geom_smooth() + guides(size=FALSE) + xlab("Amount Pledged") + ylab("Sentiment Score") + theme(legend.position="none") + ggtitle("Pledged Amounts and Sentiment Score")
```

![](figures/Q3_a-1.png)<!-- -->

```r
ggplot(data=sentiments2, aes(x=sgroup,y=sentiment, fill = sentiment)) +
  geom_bar(stat = "identity") + xlab("Amount Pledged") + ylab("Sentiment Score") + theme(legend.position="none") + ggtitle("Success and Sentiment Score")
```

![](figures/Q3_a-2.png)<!-- -->

b) Positive vs negative
Segregate all 2,000 blurbs into positive and negative texts based on their polarity score calculated in step (a). Now, collapse the positive and negative texts into two larger documents. Create a document-term-matrix based on this collapsed set of two documents. Generate a comparison cloud showing the most-frequent positive and negative words.

```r
stories_new <- left_join(stories2, sentiments_s, by.x = "index", by.y ="index")

stories_new[is.na(stories_new)] <- 0

stories_n2 <- stories_new %>% 
  select(index, pledged, text,sentiment, state = doc_id)

stories_n2$doc_id <- ifelse(stories_n2$sentiment > 0, "positive", ifelse(stories_n2$sentiment <0, "negative", "OTHER"))

stories_n2 <- stories_n2 %>% 
  filter(stories_n2$doc_id %in% c("positive", "negative"))

newdoc <- group_by(stories_n2, doc_id) %>%
    summarise(text = paste(text, collapse = ""))

source2 <- DataframeSource(newdoc)
corpus2 <- VCorpus(source2)

corpus_clean2 <- clean_corpus(corpus2)

corpus_stemmed2 <- tm_map(corpus_clean2, stemDocument)

corpus_comp2 <- lapply( corpus_stemmed2, stemCompletion2, 
                     dictionary=corpus_clean2)

corpus_comp_all2 <- as.VCorpus(corpus_comp2)

corpus_dtm2 <-  DocumentTermMatrix(corpus_comp_all2)

corpus_tdm2 <-  TermDocumentMatrix(corpus_comp_all2)

corpus_2 <- as.matrix(corpus_tdm2)

colnames(corpus_2) <- c("negative", "positive")

ccloud <- comparison.cloud(corpus_2, colors = c("red", "green"), 
                 scale=c(0.1,2), title.size= 1, 
                 max.words = 100)
```

![](figures/Q3_b-1.png)<!-- -->

```r
ccloud
```

```
## NULL
```

c) Get in their mind
Now, use the NRC Word-Emotion Association Lexicon in the tidytext package to identify a larger set of emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust). Again, visualize the relationship between the use of words from these categories and success. What is your finding?

For this graph, I used the total number of words for each category from successful and failed projects for the visualization. Therefore, we can only compare the relative proportions of each emotion between successful and failed projects. Interestingly, it seems that successful projects contains more anger and disgust, whereas failed projects contain more words with both joy and sadness. 

```r
nrc <- get_sentiments("nrc")

sentiments_nrc <- tidy_corpus %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sort = TRUE)

sentiments_nrc2 <- tidy_corpus %>%
  inner_join(get_sentiments("nrc")) %>%
  count(doc_id, sentiment)

ggplot(data = sentiments_nrc2, aes(x=reorder(sentiment, desc(n)), y=n, fill = doc_id)) +
geom_bar(stat = "identity", position = "dodge") +
scale_fill_manual(values = c("success" = "darkgreen", "fail" = "azure4"), name = "state") +
xlab("Sentiment") +
ylab("Number of Words") +
ggtitle("Success and Sentiment")
```

![](figures/Q3_c-1.png)<!-- -->
