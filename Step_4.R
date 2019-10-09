#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 4 : Build probablities for ngrams
#

require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)

# Globals

dpath_ngrams   <- "data/ngrams.rds"     # Complete ngrams table
dpath_ngramsgt <- "data/ngrams_gt.rds"  # 

## Support

# Good-Turing discounted count - apply when count<thr

gtcount <- function(x, thr=5) {
    gt  <- x                            # by default normal count
    nc1 <- sum(x == thr)                # count items with 'thr' occurences (Nc+1)
    for (c in (thr-1):1) {              # going backwards
        tnc <- (x == c)                 #    items with c occurences
        nc  <- sum(tnc)                 #    count them
        if (nc>0) {                     #    make sure we have some
            gt[tnc] <- (c+1)*nc1/nc     #    Good-Turing revised count
            nc1       <- nc             #    nc1 for next loop
        }   else {
            nc1       <- 1              #    or at least 1 if nc was 0
        }
    }
    gt
}

## Do it


ngrams <- read_rds(dpath_ngrams)        # Start where we left

tokens_v <- length(ngrams[[1]]$n)       # Unique token count: vocabulary size
tokens_n <- sum(ngrams[[1]]$n)          # Total  token count: corpus length (trimmed)
ngrams[[1]]$pml <- ngrams[[1]]$n/tokens_n               # ML probability
ngrams[[1]]$psm <-(ngrams[[1]]$n+1)/(tokens_v+tokens_n) # Smoothed unigram probability
ngrams[[1]]$gt  <- ngrams[[1]]$n                        # Good-Turing

for(n in 2:length(ngrams)) {
    tokens_n        <- sum(ngrams[[n]]$n)               # Total n_gram table length
    ngrams[[n]]$gt  <- gtcount(ngrams[[n]]$n)           # Revised Good-Turing count
    ngrams[[n]]$pml <- ngrams[[n]]$n/tokens_n           # ML probability
    ngrams[[n]]$psm <- ngrams[[n]]$gt/tokens_n          # GT discounted probability
}

### Saved - Ctrl point 4