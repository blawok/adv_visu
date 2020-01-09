#Installing Packages
# install.packages("tm")
# install.packages("wordcloud2")
# install.packages("RColorBrewer")
# install.packages("stringr")
# install.packages("qdapRegex")
devtools::install_github("lchiffon/wordcloud2")

#Loading Packages
library(tm)
library(wordcloud2)
library(stringr)       # Removing characters
library(qdapRegex)     # Removing URLs
library(data.table)
library(readr)



df <- read.csv('data_scrape/reddit_posts_all.csv', sep="\t")
text <- str_c(df$body, collapse = "")

# continue cleaning the text
# text <- 
#   text %>%
#   str_remove("\\n") %>%                   # remove linebreaks
#   rm_twitter_url() %>%                    # Remove URLS
#   rm_url() %>%
#   str_remove_all("#\\S+") %>%             # Remove any hashtags
#   str_remove_all("@\\S+") %>%             # Remove any @ mentions
#   removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
#   removeNumbers() %>%
#   stripWhitespace() %>%
#   removeWords(c("amp"))  

# writeLines(text, "cleaned_text.txt")
# Load pre-cleaned text
text <- read_file("cleaned_text.txt")
text_fixed <- str_replace_all(text,"[^[:graph:]]", " ") 

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text_fixed)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus),
                         freq=textCorpus,
                         row.names = NULL)

# build wordcloud 
wordcloud <- wordcloud2(data = textCorpus,
                        # minRotation = -pi/6, maxRotation = -pi/6,
                        # ellipticity = 0.6,
                        shape = 'circle',
                        backgroundColor="black")

iran_cloud <- wordcloud2(textCorpus, 
                       figPath = "./iran.png", 
                       size = 1, 
                       color = "skyblue", 
                       backgroundColor="black")

wordcloud

iran_cloud

# option with word shape graph (does not work)
# letterCloud(textCorpus,"WAR", color="white", backgroundColor="black")
