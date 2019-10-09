#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 2 : Load N1 Tokens - Subsample and build ngrams to 4th order
# 
require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)

### Globals

srctoken <- "data/tokens_n1_train.rds"
srcngram <- "data/ngram_"

### Support

# Flat dfm - manageable (for our purpose)

flatdfm <- function(tkn) {
    tkn <- tibble(token = unlist(tkn)) %>%
        count(token,sort=TRUE)
}


# Subsample full corpus (by size) and build ngram look-up table

subngrams <- function(size,ext = NA) {
    tokens_n1 <- tokens_sample(tokens_full,size)      # sample 
    tokens_n2 <- tokens_ngrams(tokens_n1, n=2)
    tokens_n3 <- tokens_ngrams(tokens_n1, n=3)
    tokens_n4 <- tokens_ngrams(tokens_n1, n=4)

    tokens_n1 <- flatdfm(tokens_n1)
    pad       <- which(tokens_n1$token=="")
    if (length(pad)>0) tokens_n1 <- tokens_n1[-pad,]  #remove padding
    tokens_n2 <- flatdfm(tokens_n2)
    tokens_n3 <- flatdfm(tokens_n3)
    tokens_n4 <- flatdfm(tokens_n4)

    ngramtbl  <- list(tokens_n1, tokens_n2, tokens_n3, tokens_n4)
    if (!is.na(ext)) write_rds(ngramtbl,paste0(srcngram,ext,".rds"))
    return(ngramtbl)
}

# Subsample full corpus (by chunks) and build ngram look-up table

subnchunk <- function(chunk,size,ext = NA) {
    a         <- 1+(chunk-1)*size
    b         <- chunk*size
    message(paste0("From ",a," to ",b))
    tokens_n1 <- tokens_full[a:b]
    tokens_n2 <- tokens_ngrams(tokens_n1, n=2)
    tokens_n3 <- tokens_ngrams(tokens_n1, n=3)
    tokens_n4 <- tokens_ngrams(tokens_n1, n=4)
    message("n-grams")
    tokens_n1 <- flatdfm(tokens_n1) # flat DFM
    pad       <- which(tokens_n1$token=="")
    if (length(pad)>0) tokens_n1 <- tokens_n1[-pad,]  #remove padding
    tokens_n2 <- flatdfm(tokens_n2)
    tokens_n3 <- flatdfm(tokens_n3)
    tokens_n4 <- flatdfm(tokens_n4)
    message("dfm")
    ngramtbl  <- list(tokens_n1, tokens_n2, tokens_n3, tokens_n4)
    if (!is.na(ext)) write_rds(ngramtbl,paste0(srcngram,ext,".rds"))
    return(ngramtbl)
}

### Do it

tokens_full <- read_rds(srctoken)

for(i in 1:51) subnchunk(i,100000,paste0(i*100,"K"))
