df <- read.csv('reddit_comments2.csv', sep="\t")


#Installing Packages
# install.packages("tm")
install.packages("wordcloud2")
# install.packages("RColorBrewer")

install.packages("stringr")
install.packages("qdapRegex")

#Loading Packages
library(tm)
library(wordcloud2)
library(stringr)       # Removing characters
library(qdapRegex)     # Removing URLs 


text <- str_c(df$document, collapse = "")

# continue cleaning the text
text <- 
  text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))  

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

# build wordcloud 
wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud

