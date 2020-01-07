
## Natural Language Processing: POS tagging
library(pattern.nlp)
ceta_tagged <- mapply(article.id = ceta$article.id, content = ceta$txt, FUN=function(article.id, content){
  out <- pattern_pos(x = content, language = "english", core = TRUE)
  out$article.id <- rep(article.id, times = nrow(out))
  out
}, SIMPLIFY = FALSE)  
ceta_tagged <- rbindlist(ceta_tagged)

## Take only nouns
ceta_nouns <- subset(ceta_tagged, word.type %in% c("NN") & nchar(word.lemma) > 2)

## All data in 1 list
ceta <- list(ceta_txt = txt, ceta = ceta, ceta_tagged = ceta_tagged, ceta_nouns = ceta_nouns)

## Look at POS tags
library(lattice)
barchart(sort(table(ceta$ceta_tagged$word.type)), col = "lightblue", xlab = "Term frequency", 
         main = "Parts of Speech Tag term frequency\n in CETA treaty")