library(rvest)
library(ggplot2)
library(dplyr)
library(lubridate)
library(qdap)
library(tm)
library(wordcloud)
library(syuzhet)
library(lsa)

# Download and combine text for the 85 Federalist Papers
federalist <- data.frame(Title=character(), Author=character(), Date=character(), Paper=character(), stringsAsFactors=FALSE)
for (i in 1:85){
    # Read HTML from URL
    url <- paste0("http://teachingamericanhistory.org/library/document/federalist-no-", i)
    print(paste("Retrieving data from", url))
    html <- read_html(url)
    
    # Simple extraction of title and date
    title <- html_nodes(html, '.post-title') %>% html_text()
    date <- html_nodes(html, '.single-docinfo-date') %>% html_text() %>% as.Date(format = '%B %d,  %Y')
    
    # Extract author, remove white space and Publius, and combine minor differences in formatting
    author <- html_nodes(html, '.single-docinfo-author') %>% html_text()
    author <- gsub(".*\\(|\\)|^\\ |\\ $", "", author)
    author <- gsub("^Hamilton$", "Alexander Hamilton", author)
    author <- gsub("^Madison With Hamilton$", "Madison with Hamilton", author)
    author <- gsub("^probably Madison", "James Madison", author)
    
    # Extract content, removing sources, annotations, and authorship info from the footnotes
    post <- html_nodes(html, '#doc-tabs-full') %>% html_nodes('p') %>% html_text()
    post <- gsub("^PUBLIUS|^Source:.*|\\*.*", "", post)
    post <- paste(post, collapse = ' ')
    
    federalist <- rbind(federalist, data.frame(title, author, date, post))   
}

federalist <- mutate(federalist, words = word_count(post))
write.csv(federalist, '~/dev/federalist/federalist.csv')

# Some Brief Analysis
federalist %>% group_by(paste0(year(date), '-', month(date)), author) %>% summarise(count = n())
federalist %>% group_by(author) %>% summarise(avg_words = mean(words), total_words = sum(words))

# Sentiment Analysis
get_nrc_sentiment(federalist$post)

# Generating a corpus for further (word cloud, semantic) analysis
wordCorpus <- Corpus(VectorSource(federalist$post))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english")) # Remove common words like 'the', 'of', etc
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(1776)
wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

# Stemming removes word endings so similar words are represented (govern, government, etc)
# This is desirable for similarity analysis, but many of the words (e.g. peopl) are not real
# Words and do not lend themselves to visual representation.
# For this reason, we perform this step after our word cloud and visualizations
wordCorpus <- tm_map(wordCorpus, stemDocument)

td.mat <- as.matrix(TermDocumentMatrix(wordCorpus))
dist.mat <- dist(t(td.mat)) # Calculate distance matrix

# Multi-Dimensional Scaling
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])

ggplot(points, aes(x = x, y = y)) + 
    geom_point(data = points, aes(x = x, y = y, color = federalist$author)) + 
    geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(federalist)))




# Multi-Dimensional Scaling with Latent Semantic Analysis
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix

fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])

ggplot(points, aes(x = x, y = y)) + 
    geom_point(data = points, aes(x = x, y = y, color = federalist$author)) + 
    geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(federalist)))
