#
# DS Capstone - Common declarations and shared functions
#
require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)
require(lexicon)
require(data.table)

### Globals

bldinclude      <- TRUE                        # common loaded flag

dpath_base      <- "data/en_US/*.txt"          # provided corpus path
dpath_textf     <- "data/en_US.full.txt"       # combined, clean, text
dpath_tfull     <- "data/tokens_full.rds"      # full corpus tokenized
dpath_tn0       <- "data/tokens_n0.rds"        # profanity filtered (was also stopwords, stems...)
dpath_dfmn0     <- "data/dfm_n0.rds"           # doc feat matrix for tokens n0 - my simple style
dpath_tn1       <- "data/tokens_n1.rds"        # XX% coverage tokens, padded
dpath_dfmn1     <- "data/dfm_n1.rds"           # doc feat matrix for tokens n1 - my simple style
dpath_tn_train  <- "data/tokens_n1_train.rds"  # train set tokens, padded
dpath_tn_test   <- "data/tokens_n1_test.rds"   # test  set tokens, padded
dpath_textf     <- "data/en_US.full.txt"       # reshuffled corpus
dpath_tfull     <- "data/tokens_full.rds"      # full corpus tokenized
dpath_tn0       <- "data/tokens_n0.rds"        # profanity filtered (was also stopwords, stems...)
dpath_dfmn0     <- "data/dfm_n0.rds"           # doc feat matrix for tokens n0 - my simple style
dpath_tn1       <- "data/tokens_n1.rds"        # 90% coverage tokens, padded
dpath_dfmn1     <- "data/dfm_n1.rds"           # doc feat matrix for tokens n1 - my simple style
dpath_tn_train  <- "data/tokens_n1_train.rds"  # 90% coverage tokens, padded
dpath_tn_test   <- "data/tokens_n1_test.rds"   # 90% coverage tokens, padded
dpath_ngram     <- "data/ngram_"               # base name for ngrams from subtables
dpath_fcm       <- "data/fcm.rds"              # co-occurrence matrix - count
dpath_fcm_p     <- "data/fcm_p.rds"            # co-occurrence matrix - ML
dpath_ngrams    <- "data/ngrams.rds"           # Complete ngrams table
dpath_lookahead <- "data/lookahead.rds"        # Look ahead table
dpath_nbase     <- "data/"                     # sample ngrams folder
ngext           <- "S"                         # ngram subsample signature
dpath_pattern   <- paste0("*",ngext,".rds$")   # trail pattern of ngrams of subsamples

chnknum         <- 50                          # number of subsamples
chnksize        <- 20000                       # size of each sample

coverage        <- 0.8                         # corpus coverage threshold for vocabulary trimming
trsmpl          <- 1                           # percentage to total sample included in training set

nsplit          <- "_"                         # ngram separator symbol (sync prdinclude)


### Support

# Flat dfm - manageable (for our purpose)

flatdfm <- function(tkn) {
    tkn <- tibble(token = unlist(tkn)) %>%
        count(token,sort=TRUE)
}


# Combine two flat dfms  - not used as is - save just in case

merge_dfm <- function(uno,dos){
    uno <- rbind.data.frame(uno,dos)
    dos <- uno %>% group_by(token) %>%
        summarise(n=sum(n)) %>%
        arrange(desc(n))
    return(dos)
}


# Combine to ngram look-up tables (to merge data from two samples)  - not used as is - save just in case

merge_ngrams <- function(uno,dos) {
    for (i in 1:length(uno)) {
        dos[[i]] <-merge_dfm(uno[[i]],dos[[i]])
    }
    return(dos)
}

