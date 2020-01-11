# read data
df <- read.csv('data_scrape/reddit_posts_all.csv', sep="\t",fileEncoding = "UTF-8")

df_body <- data.frame(df$body)

library(tm)
myCorpus <- Corpus(VectorSource(df$body))
tdm <- TermDocumentMatrix(myCorpus)


library(igraph)
termDocMatrix <- as.matrix(tdm)
termDocMatrix[termDocMatrix>=1] <- 1
termMatrix <- termDocMatrix %*% t(termDocMatrix)
termMatrix <- termMatrix[5:50,5:50]

g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam

tkplot(g, layout=layout.kamada.kawai)
plot(g)
