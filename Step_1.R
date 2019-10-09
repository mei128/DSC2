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

srcpath <- "data/en_US.full.txt"                          # reshuffled corpus

### Support

# Flat dfm - manageable (for our purpose)

flatdfm <- function(tkn) {
    tkn <- tibble(token = unlist(tkn)) %>%
           count(token,sort=TRUE)
}

### Do it

srcbody <- read_lines(srcpath)
message("Corpus loaded")

tokens_n1 <- tokens(srcbody, remove_punct   = TRUE,     # Single tokens, removing ...
                             remove_symbols = TRUE,
                             remove_numbers = TRUE,
                             remove_twitter = TRUE,
                             remove_url     = TRUE)

write_rds(tokens_n1,"data/tokens_full.rds")         # Save object for later use
rm(srcbody)                                         # Free precious space
message("Tokens full list")
data("profanity_banned")                                                      # simple profanity list
profanity_banned <- c(profanity_banned,"fucking","fuckin'")                   # a few words missing...
profanity <- sapply(tokens_n1, function(x) (sum(x %in% profanity_banned)>0))  # find sentences with p*
tokens_n1 <- tokens_n1[!profanity]                                            # take them out
write_rds(tokens_n1,"data/tokens_n0.rds")                                     # and save for later use
rm(profanity, profanity_banned)
message("Tokens N0 (profanity filtered)")

dfm_n1    <- flatdfm(tokens_n1)                         # DFM my style
write_rds(dfm_n1,"data/dfm_n0.rds")                     # Save for later use
message("DFM N0")

tkcount   <- sum(dfm_n1$n)
dfm_n1$q  <- cumsum(dfm_n1$n)/tkcount                   # Cum. coverage
thrshld   <- first(which(dfm_n1$q>0.90))                # cut at 90% coverage
tk2drop   <- dfm_n1$token[thrshld:length(dfm_n1$token)] # Tokens to drop
dfm_n1    <- dfm_n1[1:(thrshld-1),c(1,2)]               # Tokens left
write_rds(dfm_n1,"data/dfm_n1.rds")                   # Save for later use
message("DFM N1 (trimmed)")

tokens_n1 <- tokens_remove(tokens_n1,tk2drop, padding = TRUE) # drop low occurrence tokens
write_rds(tokens_n1,"data/tokens_n1.rds")                   # and save for later use
message("Tokens N1 (trimmed)")

insample <- length(tokens_n1)
insample <- sample(insample,0.8*insample)
write_rds(tokens_n1[ insample],"data/tokens_n1_train.rds")
write_rds(tokens_n1[-insample],"data/tokens_n1_test.rds")

### Saved - Ctrl point 1