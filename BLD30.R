#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 30 : Load N1 Tokens - Subsample and build ngrams to 4th order
#           Choose sequential samples of given size (pre-randomized set) or
#           Random subsample the full token list
#           Unit is sentence.

### Include common

if (!exists("bldinclude")) source("./BLDInc.R")

# Subsample full corpus (by size) and build ngram look-up table

subngrams <- function(size,ext = NA) {
    message(paste0(size," random sentences"))
    tokens_n1 <- tokens_sample(tokens_full,size)      # sample 
    tokens_n2 <- tokens_ngrams(tokens_n1, n=2, concatenator = nsplit)
    tokens_n3 <- tokens_ngrams(tokens_n1, n=3, concatenator = nsplit)
    tokens_n4 <- tokens_ngrams(tokens_n1, n=4, concatenator = nsplit)
    message("n-grams")
    tokens_n1 <- flatdfm(tokens_n1)
    pad       <- which(tokens_n1$token=="")
    if (length(pad)>0) tokens_n1 <- tokens_n1[-pad,]  #remove padding
    tokens_n2 <- flatdfm(tokens_n2)
    tokens_n3 <- flatdfm(tokens_n3)
    tokens_n4 <- flatdfm(tokens_n4)
    message("dfm")
    ngramtbl  <- list(tokens_n1, tokens_n2, tokens_n3, tokens_n4)
    if (!is.na(ext)) write_rds(ngramtbl,paste0(dpath_ngram,ext,".rds"))
    return(ngramtbl)
}

# Subsample full corpus (by sequential chunks) and build ngram look-up table

subnchunk <- function(chunk,size,ext = NA) {
    a         <- 1+(chunk-1)*size
    b         <- chunk*size
    message(paste0("From ",a," to ",b))
    tokens_n1 <- tokens_full[a:b]
    tokens_n2 <- tokens_ngrams(tokens_n1, n=2, concatenator = nsplit)
    tokens_n3 <- tokens_ngrams(tokens_n1, n=3, concatenator = nsplit)
    tokens_n4 <- tokens_ngrams(tokens_n1, n=4, concatenator = nsplit)
    message("n-grams")
    tokens_n1 <- flatdfm(tokens_n1) # flat DFM
    pad       <- which(tokens_n1$token=="")
    if (length(pad)>0) tokens_n1 <- tokens_n1[-pad,]  #remove padding
    tokens_n2 <- flatdfm(tokens_n2)
    tokens_n3 <- flatdfm(tokens_n3)
    tokens_n4 <- flatdfm(tokens_n4)
    message("dfm")
    ngramtbl  <- list(tokens_n1, tokens_n2, tokens_n3, tokens_n4)
    if (!is.na(ext)) write_rds(ngramtbl,paste0(dpath_ngram,ext,".rds"))
    return(ngramtbl)
}

### Do it - choose to build samples sequentially (src is already random) or subsample

if (!exists("tokens_full")) {                # Load where we left if unchained execution
    tokens_full <- read_rds(dpath_tn_train)
    message("Tokens loaded")
}

for(i in 1:chnknum) {
    message("Loop ",i)
    subngrams(chnksize,paste0(i,ngext))
}

### Saved - Ctrl point 2