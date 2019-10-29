#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 25 : Redo with different coverage, starting where we left
#

### Include common

if (!exists("bldinclude")) source("./BLDInc.R")

### Do it

tokens_full <- read_rds(dpath_tn0)      # Start after tokenization and profanity filter
dfm_n1      <- read_rds(dpath_dfmn0)
message("Tokens reloaded")

tkcount   <- sum(dfm_n1$n)                              # Total tokens in corpus
dfm_n1$q  <- cumsum(dfm_n1$n)/tkcount                   # Cumul. coverage

if (coverage < 1) {                                     # Coverage given as %
    thrshld   <- first(which(dfm_n1$q>coverage))        #    find cut point
} else {                                                # Coverage given as # of tokens
    thrshld   <- coverage                               #    set cut point
    message(paste0("Coverage ",dfm_n1$q[thrshld]))      #    show coverage
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
