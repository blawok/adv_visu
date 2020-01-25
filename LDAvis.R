library(readr)
library(tm)
library(SnowballC)
library(Matrix)
library(lda)
library(LDAvis)
library(servr)


###################################
# OLD
###################################


# read data
train <- read_csv("./data/clean_reddit_df.csv")

# old was made from c555x4 and c2spc2

train <- train[which(train$thread_id == "c555x4" | train$thread_id == "c2spc2"), ]  

txt <- paste(train$clean_body)


# pre-processing
txt <- gsub("'", "", txt)  # remove apostrophes
txt <- gsub("[[:punct:]]", " ", txt)  # replace punctuation with space
txt <- gsub("[[:cntrl:]]", " ", txt)  # replace control characters with space
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt <- tolower(txt)  # force to lowercase


# tokenize on space and output as a list:
doc.list <- strsplit(txt, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# stop words
stop_words <- c(stopwords("english"), c("s", "t", "r", "f", "m", "d", "u"))

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)



# Compute some statistics related to the data set:
D <- length(documents)
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)

# MCMC and model tuning parameters:
K <- 4    # 10
G <- 2000
alpha <- 0.01
eta <- 0.01

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  


### Visualizing the fitted model with LDAvis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './json_old/', open.browser = TRUE)
system("json/mv json/index.html json/results.html")

# save(results, file = "./objects/LDAVis_old.RData")

###################################
# NEW
###################################


# read data
train <- read_csv("./data/clean_reddit_df.csv")

# old was made from c555x4 and c2spc2

train <- train[which(train$thread_id == "ccednx"), ]  #  | train$thread_id == "elku7e"

txt <- paste(train$clean_body)


# pre-processing
txt <- gsub("'", "", txt)  # remove apostrophes
txt <- gsub("[[:punct:]]", " ", txt)  # replace punctuation with space
txt <- gsub("[[:cntrl:]]", " ", txt)  # replace control characters with space
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt <- tolower(txt)  # force to lowercase


# tokenize on space and output as a list:
doc.list <- strsplit(txt, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# stop words
stop_words <- c(stopwords("english"), c("s", "t", "r", "f", "m", "d", "u"))

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)



# Compute some statistics related to the data set:
D <- length(documents)
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)

# MCMC and model tuning parameters:
K <- 5    # 4
G <- 2000
alpha <- 0.01
eta <- 0.01

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  


### Visualizing the fitted model with LDAvis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './json_new/', open.browser = TRUE)
system("json_new/mv json_new/index.html json_new/results.html")

# save(results, file = "./objects/LDAVis_congress.RData")
