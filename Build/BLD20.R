#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 20 : Tokenize, drop sentences with profanity, remove low frequency tokens
#

### Include common

if (!exists("bldinclude")) source("./BLDInc.R")

### Do it

if (!exists("srcbody")) {                  # Load where we left if unchained execution
    srcbody <- read_lines(dpath_textf)
    message("Corpus loaded")
}

tokens_full <- tokens(srcbody, remove_punct   = TRUE,     # Single tokens, removing ...
                               remove_symbols = TRUE,
                               remove_numbers = TRUE,
                               remove_twitter = TRUE,
                               remove_url     = TRUE)

write_rds(tokens_full,dpath_tfull)       # Save object for later use
rm(srcbody)                              # Free precious space
message("Tokens full list")
data("profanity_banned")                                                        # simple profanity list
profanity_banned <- c(profanity_banned,"fucking","fuckin'")                     # f-words missing...
profanity <- sapply(tokens_full, function(x) any(x %in% profanity_banned))      # find sentences with p*
tokens_full <- tokens_full[!profanity]                                          # take them out
write_rds(tokens_full,dpath_tn0)                                                # and save for later use
rm(profanity, profanity_banned)
message("Tokens N0 (profanity filtered)")

dfm_n1    <- flatdfm(tokens_full)                 # DFM my style
write_rds(dfm_n1,dpath_dfmn0)                     # Save for later use
message("DFM N0")

tkcount   <- sum(dfm_n1$n)                              # Total tokens in corpus
dfm_n1$q  <- cumsum(dfm_n1$n)/tkcount                   # Cumul. coverage

if (coverage < 1) {                                     # Coverage given as %
    thrshld   <- first(which(dfm_n1$q>coverage))        #    find cut point
} else {                                                # Coverage given as # of tokens
    thrshld   <- coverage                               #    set cut point
    message(paste0("Coverage ",dfm_n1[thrshld]$q))      #    show coverage
}

tk2drop   <- dfm_n1$token[(thrshld+1):length(dfm_n1$token)] # Tokens to drop
dfm_n1    <- dfm_n1[1:(thrshld),c(1,2)]                     # Tokens left
write_rds(dfm_n1,dpath_dfmn1)                               # Save for later use
message("DFM N1 (trimmed)")

tokens_full <- tokens_remove(tokens_full,tk2drop, padding = TRUE) # drop low occurrence tokens
tokens_null <- sapply(tokens_full, function(x) all(x==""))        # find empty sentences
tokens_full <- tokens_full[!tokens_null]                          # and drop them too
write_rds(tokens_full,dpath_tn1)                                  # and save for later use
message("Tokens N1 (trimmed)")

if (trsmpl < 1) {                                       # set trsmpl to a % to split
    insample <- length(tokens_full)
    insample <- sample(insample,trsmpl*insample)        #   train sample (smpl% of total size)
    write_rds(tokens_full[ insample],dpath_tn_train)    #   training set
    write_rds(tokens_full[-insample],dpath_tn_test)     #   testing set
    tokens_full <- tokens_full[insample]                #   get ready for chained exec
    rm(dfm_n1, tk2drop, tokens_null, insample)
} else {                                                # Otherwise
    write_rds(tokens_full,dpath_tn_train)               #   training set is really a full set
    rm(dfm_n1, tk2drop, tokens_null)
}   # NOT REALLY USED AFTER ABANDONING ML AND ACCURACY MEASURE
### Saved - Ctrl point 1
