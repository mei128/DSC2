#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 60 : Build probablities for ngrams, GT discounted count, and look ahead table
#

### Include common

if (!exists("common")) source("./Common.R")

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

# lookahead table: Split Wn from Wn-1...W1, convert to data table, and set key

lookahead <- function(x) {
    x$s <- sapply(x$token, function(t) tail(gregexpr(nsplit,t)[[1]],1))
    x   <- mutate(x,l=nchar(token),ahead=stri_sub(token,s+1,l),token=stri_sub(token,1,s-1))
    x   <- data.table(x[,c(1,2,3,4,5,8)])
    setkey(x,token,ahead)
    x
}

## Do it


if (!exists("ngrams")) {                # load where we left if unchained execution
    ngrams <- read_rds(dpath_ngrams)        # Start where we left
    message("n-grams loaded")
}

message("Level 1")
tokens_v <- length(ngrams[[1]]$n)       # Unique token count: vocabulary size
tokens_n <- sum(ngrams[[1]]$n)          # Total  token count: corpus length (trimmed)
ngrams[[1]]$pml <- ngrams[[1]]$n/tokens_n               # ML probability
ngrams[[1]]$psm <-(ngrams[[1]]$n+1)/(tokens_v+tokens_n) # Smoothed unigram probability
ngrams[[1]]$gt  <- ngrams[[1]]$n                        # Good-Turing
ngrams[[1]]     <- mutate(ngrams[[1]],ahead = token)    # N1 -> predict token
ngrams[[1]]     <- data.table(ngrams[[1]])              # Indexed data table
setkey(ngrams[[1]],token)

for(n in 2:length(ngrams)) {
    message(paste0("Level ",n))
    tokens_n        <- sum(ngrams[[n]]$n)               # Total n_gram table length
    ngrams[[n]]$gt  <- gtcount(ngrams[[n]]$n)           # Revised Good-Turing count
    ngrams[[n]]$pml <- ngrams[[n]]$n/tokens_n           # ML probability
    ngrams[[n]]$psm <- ngrams[[n]]$gt/tokens_n          # GT discounted probability
    ngrams[[n]]     <- lookahead(ngrams[[n]])           # Look ahead table for level n
}

write_rds(ngrams,dpath_lookahead)

### Saved - Ctrl point 4