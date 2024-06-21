library(tidyverse)
library(wordcloud)
library(tm)

crf <- read.csv("CovidReliefFunds/crf_full_data.csv")

pd <- crf$Project.description
docs <- Corpus(VectorSource(pd))

### formatting commas into numbers
crf$Award.amount <- formatC(crf$Award.amount, format = "d", big.mark = ",")
crf$Sub.award.amount<-formatC(crf$Sub.award.amount, format="d", big.mark =",")

docs <- docs %>%
  tm_map(tolower) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  ####tm_map(removeSparseTerms, 0.99)%>% not sure if I could remove before TDM
  tm_map(removeWords,stopwords("en"))
tdm <- TermDocumentMatrix(docs)
tdm <- removeSparseTerms(tdm, 0.99)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(v), freq = v)

set.seed(12)
x11(width = 20, height = 11)
wordcloud(
  words = df$word, freq = df$freq, min.freq = 1, 
  max.words=130, random.order = FALSE, rot.per = 0.35, 
  colors = brewer.pal(8, "Dark2"))

#regular expression
pdSanitize <- c('[Ss]anitiz', '[Cc]lean')
pdHealth <- c('[Hh]ealth', '[Hh]ospital')
