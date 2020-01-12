library(dplyr)
# read data
df <- read.csv('data/clean_reddit_df.csv',
               stringsAsFactors=FALSE)

df <- filter(df, df$thread_id == 'ej95ak')

library(tm)
textCorpus <- 
  Corpus(VectorSource(df$body)) %>%
  TermDocumentMatrix()
# %>%
#   as.matrix()

tdm <- removeSparseTerms(textCorpus,0.9999)

# textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
# textCorpus2 <- textCorpus[1:1000]
# textCorpus22 <- data.frame(word = names(textCorpus),
#                          freq=textCorpus,
#                          row.names = NULL)
# 
# 
# 
# myCorpus <- Corpus(VectorSource(df$body))
# tdm <- TermDocumentMatrix(myCorpus)

library(igraph)
termDocMatrix <- as.matrix(tdm)
termDocMatrix[termDocMatrix>=1] <- 1
termMatrix <- termDocMatrix %*% t(termDocMatrix)
# termMatrix <- termMatrix[5:50,5:50]

g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)


V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
# layout1 <- layout.fruchterman.reingold(g)
# plot(g, layout=layout1)


V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam

# tkplot(g, layout=layout.kamada.kawai)
plot(g, vertex.size = 4)
