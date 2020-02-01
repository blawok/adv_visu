df <- read.csv('data/clean_reddit_df.csv', sep=",", encoding="UTF-8")

# old
df <- filter(df, df$thread_id == 'c35akk' | df$thread_id == 'c555x4' | df$thread_id == 'ccednx')  

# new
df <- filter(df, df$thread_id == 'ej95ak' | df$thread_id == 'ej7ykn' | df$thread_id == 'ekad1u' | df$thread_id == 'elku7e' | df$thread_id == 'elkxvb')  


text <- str_c(df$clean_body, collapse = "")

library(tm)

sw <- c('that', 'the', 'can', 'want', 'like', 'people', 'get', 'also', 'even', 'wars', 'what')

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  tm_map(., content_transformer(tolower)) %>% 
  tm_map(., removeWords, stopwords("english")) %>% 
  tm_map(., removeWords, sw) %>% 
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus),
                         freq=textCorpus,
                         row.names = NULL)

save(textCorpus, file = paste("objects/", 'new_clean', "_text_corpus.RData", sep=""))



textCorpus['indicator'] <- textCorpus['freq'] / 40735

pal <- colorRampPalette(colors = c("navy", "white"))(15)

barplot(textCorpus[1:15,]$indicator,
        las = 2,
        names.arg =textCorpus[1:15,]$word,
        col = pal,
        main ="Most frequent words",
        xlab = "Word frequency indicator",
        cex.names=0.8,
        axes = FALSE,
        horiz=TRUE)
# axis(side = 2)
axis(side = 1, at = seq(0, 0.8, 0.01))


