#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 1 : Tokenize, drop sentences with profanity, remove low frequency tokens
#

require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)
require(lexicon)

### Globals

dpath_in       <- "data/en_US.full.txt"       # reshuffled corpus
dpath_tfull    <- "data/tokens_full.rds"      # full corpus tokenized
dpath_tn0      <- "data/tokens_n0.rds"        # profanity filtered (was also stopwords, stems...)
dpath_dfmn0    <- "data/dfm_n0.rds"           # doc feat matrix for tokens n0 - my simple style
dpath_tn1      <- "data/tokens_n1.rds"        # 90% coverage tokens, padded
dpath_dfmn1    <- "data/dfm_n1.rds"           # doc feat matrix for tokens n1 - my simple style
dpath_tn_train <- "data/tokens_n1_train.rds"  # 90% coverage tokens, padded
dpath_tn_test  <- "data/tokens_n1_test.rds"   # 90% coverage tokens, padded

### Support

# Flat dfm - manageable (for our purpose)

flatdfm <- function(tkn) {
    tkn <- tibble(token = unlist(tkn)) %>%
           count(token,sort=TRUE)
}

### Do it

srcbody <- read_lines(dpath_in)
message("Corpus loaded")

tokens_n1 <- tokens(srcbody, remove_punct   = TRUE,     # Single tokens, removing ...
                             remove_symbols = TRUE,
                             remove_numbers = TRUE,
                             remove_twitter = TRUE,
                             remove_url     = TRUE)

write_rds(tokens_n1,dpath_tfull)         # Save object for later use
rm(srcbody)                              # Free precious space
message("Tokens full list")
data("profanity_banned")                                                      # simple profanity list
profanity_banned <- c(profanity_banned,"fucking","fuckin'")                   # f-words missing...
profanity <- sapply(tokens_n1, function(x) (sum(x %in% profanity_banned)>0))  # find sentences with p*
tokens_n1 <- tokens_n1[!profanity]                                            # take them out
write_rds(tokens_n1,dpath_tn0)                                                # and save for later use
rm(profanity, profanity_banned)
message("Tokens N0 (profanity filtered)")

dfm_n1    <- flatdfm(tokens_n1)                   # DFM my style
write_rds(dfm_n1,dpath_dfmn0)                     # Save for later use
message("DFM N0")

tkcount   <- sum(dfm_n1$n)
dfm_n1$q  <- cumsum(dfm_n1$n)/tkcount                   # Cumul. coverage
thrshld   <- first(which(dfm_n1$q>0.90))                # cut at 90% coverage
tk2drop   <- dfm_n1$token[thrshld:length(dfm_n1$token)] # Tokens to drop
dfm_n1    <- dfm_n1[1:(thrshld-1),c(1,2)]               # Tokens left
write_rds(dfm_n1,dpath_dfmn1)                           # Save for later use
message("DFM N1 (trimmed)")

tokens_n1 <- tokens_remove(tokens_n1,tk2drop, padding = TRUE) # drop low occurrence tokens
write_rds(tokens_n1,dpath_tn1)                                # and save for later use
message("Tokens N1 (trimmed)")

insample <- length(tokens_n1)
insample <- sample(insample,0.8*insample)
write_rds(tokens_n1[ insample],dpath_tn_train)  # trainind set
write_rds(tokens_n1[-insample],dpath_tn_test)   # testing set

### Saved - Ctrl point 1