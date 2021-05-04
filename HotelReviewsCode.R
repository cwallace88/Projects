install.packages('NLP')
install.packages('lubridate')
install.packages('data.table')
install.packages("tm")
install.packages("qdapTools")
install.packages('reader')
install.packages("gplots")
install.packages("corrplot")
install.packages("SentimentAnalysis")
install.packages("sentimentr")
install.packages("RColorBrewer")
install.packages('leaflet')
library(leaflet)
library(tm)
library(NLP)
library(lubridate)
library(data.table)
library(ggplot2)
library(rgdal)
library(sp)
library(dplyr)
library(wordcloud)
library(SnowballC)
library(qdapTools)
library(gplots)
library(corrplot)
library(sentimentr)
library(SentimentAnalysis)
#setwd("C:/Users/Pao/Downloads/DATA630_Spring2020/Group1 Project")
library(readr)
hotel <- read_csv("Desktop/Machine Learning/Week 12/HotelReviews.csv")

View(hotel) #Load Hotel review data into R
hotel2<-data.frame(hotel)
str(hotel)
head(hotel)


file.info('HotelReviews.csv')$size
hotel1<-readLines("Desktop/Machine Learning/Week 12/HotelReviews.csv",n=18000)


hotel1<-VCorpus(VectorSource(hotel$reviews.text))


# convert to lower case
hotel1 <- tm_map(hotel1, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
hotel1 <- tm_map(hotel1, content_transformer(removeURL)


# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
hotel1 <- tm_map(hotel1, content_transformer(removeNumPunct))

# remove stopwords
hotel1 <- tm_map(hotel1, removeWords, stopwords("english"))

# remove extra whitespace
hotel1 <- tm_map(hotel1, stripWhitespace)

# Remove numbers
hotel2 <- tm_map(hotel1, removeNumbers)

# Remove punctuations
hotel2 <- tm_map(hotel2, removePunctuation)

#clump words with similar origin 

hotel2 <- tm_map(hotel2, stemDocument)

#create a term matrix called word_frequencies
word_frequencies <- TermDocumentMatrix(hotel2)
print(word_frequencies) #Print document matrix
inspect(word_frequencies) #View Sample of words in the document
dtm<-DocumentTermMatrix(hotel2) #Reverse term and document in document matrix
inspect(dtm)
#terms which correlates with at least 0.2 correlation for the term room
findAssocs(dtm, "room", 0.2)
findFreqTerms(dtm,2000)# Fine terms which appear atleast 2000 times
inspect(removeSparseTerms(dtm, 0.8)) #remove sparse terms
dtm$nrow #display number or row in document matrix
dtm$ncol#display number of unique terms
#Displays the most word with number of times used
-findMostFreqTerms(dtm, n = 10, INDEX = rep(1, dtm$nrow))[[1]]
#Create Word Cloud
wordcloud(hotel2, max.words = 600,random.color = TRUE,random.order=FALSE)

# Frequency table
rst<-removeSparseTerms(dtm, 0.8) #Use the reduce document matrix to creat the frequency table
m <- t(as.matrix(rst))
freq_table <- data.frame(term = rownames(m), 
                         freq = rowSums(m), 
                         row.names = NULL)
freq_table <- freq_table[order(-freq_table$freq),][1:10,]
freq_table #Display frequency table

# Frequency plot
freq_plot <- ggplot(freq_table, aes(x = reorder(term, -freq), freq)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Terms", y = "Frequency", title = "Frequent terms") +
  geom_text(aes(label = freq), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
freq_plot

#find most frequent word
freq_words <- colnames(t(findMostFreqTerms(dtm, n = 10, INDEX = rep(1, dtm$nrow))[[1]]))
dtm_freq_words <- dtm[, Terms(dtm) %in% freq_words]
dtm_freq_words

# create a co-occurrence matrix of how often terms appear in thesam text

m_freq_words <- as.matrix(dtm_freq_words)
heatmap_data <- t(m_freq_words) %*% m_freq_words
heatmap_data

diag(heatmap_data) <- 0
heatmap_data[lower.tri(heatmap_data)] <- NA

# Crete heatmap
heatmap.2(heatmap_data, 
          dendrogram = "none", Colv = FALSE, Rowv = FALSE,
          scale = "none", col = brewer.pal(5, "Blues"),
          key = TRUE, density.info = "none", key.title = NA, key.xlab = "Frequency",
          trace = "none",
          main = "Term co-occurrence",
          xlab = "Term",
          ylab = "Term")

# Create correlation plot
cor_data <- cor(m_freq_words)
corrplot(cor_data, method = "square", type = "upper", tl.col = "black", order = "hclust", col = brewer.pal(n = 5, name = "RdYlBu"))


#IMPORTANT: Do not run lines 130 to 139 if you do not have 16 GB RAM
#increase the memory limit
memory.limit(size=100000)

#frequent term inspection
freq <- colSums(as.matrix(word_frequencies))
print(freq)

#find words that have a frequency of at least 100 
freqw<-findFreqTerms(word_frequencies, lowfreq=100) 
print(freqw)

#Plotting a leaflet plot for hotels haveing received 0 as a rating once.
#There are 337 such hotels


cities = subset(hotel, hotel$reviews.rating ==0,na.rm=T)
latitude = cities$latitude
longitude = cities$longitude
leaflet(cities) %>% addTiles() %>% setView(lng = -96.503906, lat = 38.68551, zoom = 4) %>%
  addCircleMarkers(lng = longitude, lat = latitude, weight = 0, radius=10, fillOpacity = 0.5, popup = cities$city) 



#Sentiment Analysis

positive_lexicon <- read.csv("Desktop/Machine Learning/Week 12/positive-words.txt")
negative_lexicon <- read.csv("Desktop/Machine Learning/Week 12/negative-words.txt")


total_pos_count <- 0
total_neg_count <- 0
pos_count_vector <- c()
neg_count_vector <- c()

size <- length(hotel2)

for(i in 1:size){
  corpus_words<- list(strsplit(hotel2[[i]]$content, split = " "))
  #print(intersect(unlist(corpus_words), unlist(positive_lexicon))) ## positive words in current review
  pos_count <- length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
  #print(intersect(unlist(corpus_words), unlist(negative_lexicon))) ## negative words in current review
  neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
  if(pos_count>neg_count){
    #print("It's a positive review")
  } else{
    #print("It's a negative review")
  }
  total_count_for_current_review <- pos_count + neg_count ## current positive and negative count
  pos_percentage <- (pos_count*100)/total_count_for_current_review
  neg_percentage <- (neg_count*100)/total_count_for_current_review
  #print(pos_percentage)                          ## current positive percentage
  #print(neg_percentage)                          ## current negtive percentage
  total_pos_count <- total_pos_count + pos_count ## overall positive count
  total_neg_count <- total_neg_count + neg_count ## overall negative count
  pos_count_vector <- append(pos_count_vector, pos_count)
  neg_count_vector <- append(neg_count_vector, neg_count)
}


# Sentiment score of each review and visualizing using boxplot
counts <- data.frame(pos_count_vector, neg_count_vector)
sentiment <- data.frame(c(1:size),(pos_count_vector - neg_count_vector) / (pos_count_vector + neg_count_vector))
boxplot(sentiment$X.pos_count_vector...neg_count_vector...pos_count_vector...neg_count_vector.[0:100]~sentiment$c.1.size.[0:100])


# Visualiztion of positive and negative count of first review
singe_review <- c(counts$pos_count_vector[1], counts$neg_count_vector[1])
barplot(t(as.data.frame(singe_review)), ylab = "Count", xlab = "Positve v/s Negative",  
        main = "Positive and Negative words in Review")




# Calculating overall percentage of positive and negative words of all the reviews
total_pos_count                                  ## overall positive count
total_neg_count                                  ## overall negative count
total_count <- total_pos_count + total_neg_count
overall_positive_percentage <- (total_pos_count*100)/total_count
overall_negative_percentage <- (total_neg_count*100)/total_count
overall_positive_percentage                      ## overall positive percentage
overall_negative_percentage                      ## overall negative percentage

# Visualization of positive and negative word count for all the reviews
review_count_frame <- data.frame(matrix(c(pos_count_vector, neg_count_vector), nrow = 100, ncol = 2))
colnames(review_count_frame) <- c("Positive Word Count", "Negative Word Count")
barplot(review_count_frame$`Positive Word Count`, ylab = "Positive Word Count", xlab = "Reviews from 1 to 100",  
        main = "Positive words in Reviews", col="lightblue")
barplot(review_count_frame$`Negative Word Count`, ylab = "Negative Word Count", xlab = "Reviews from 1 to 100",  
        main = "Negative words in Reviews", col="lightblue")

# Visualization of Overall positive and negative reviews
percent_vec <- c(overall_positive_percentage, overall_negative_percentage)
percent_frame <- as.data.frame(percent_vec)
rownames(percent_frame) <- c("Positive Reviews","Negative Reviews")
colnames(percent_frame) <- c("Percentage")
percentage <- t(percent_frame)
barplot(percentage, ylab = "Percentage", main = "Sentiment Analysis of Overall hotels in US", col="lightblue")

