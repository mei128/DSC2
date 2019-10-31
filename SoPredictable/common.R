#
# DSS Capstone - Manuel Esteban-Infantes
#
# Common procedures and data across all sessions
#

### Packages

require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)
require(data.table)

### Globals ###################################################################
#                                                                             #

dpath_fcm_p     <- "./fcm_p.rds"               # co-occurrence matrix (normalized) for context
dpath_lookahead <- "./lookahead.rds"           # Look ahead table

nsplit          <- "_"                         # Token separator in ngram tables
predsetsize     <- 40                          # max prediction set size: truncate prediction to this size
predsetshow     <-  8                          # number of terms to show from prediction set

### Support functions #########################################################
#                                                                             #

# intoken - Convert input to character vector by tokens #######################

tokenize <- function(input) {
    t <- tokens(stri_trans_tolower(input),
                remove_punct   = TRUE,
                remove_symbols = TRUE,
                remove_numbers = TRUE,
                remove_twitter = TRUE,
                remove_url     = TRUE)[[1]]
    return(t)
}

# bycontext - filter and reassign probabiliy by context #######################

bycontext <- function(prtoken,intoken) {

    prtoken     <- prtoken[!(prtoken$ahead %in% intoken),]    # Only tokens not already in the stream
    intoken     <- unique(intoken)
    intoken     <- intoken[intoken %in% alltokens]
    if ((length(prtoken$ahead)<2)|(length(intoken)<2)) return(prtoken)   # No context: nothing changes
    cntxtp      <- fcmp[prtoken$ahead,intoken]                # Cooccurrence probabilities
    cntxtp      <- apply(cntxtp,1,sum)                        # Add by prediction token
#    psmsum      <- sum(prtoken$psm)                           # total prob of pred tokens
    prtoken$psm <- prtoken$psm*(cntxtp)                       # stretch by context factor
#    psmsum      <- psmsum/sum(prtoken$psm)                    # scale after stretch
#    prtoken$psm <- prtoken$psm*psmsum                         #    renormalize to previous total prob.
    prtoken
}


# ahead - look ahead for level n (n>=2); returns NULL if nothing found ########

ahead <- function(tkns,n) {

    lakey <- paste(tail(tkns,n-1), collapse = nsplit)       # Look-ahead key
    pred  <- as.tbl(lookahead[[n]][lakey])                  #    ahead tibble
    if (is.na(pred$ahead[1])) return(NULL)                  # Nothing found: return null
    plen  <- length(pred$ahead)                             # Result count
    pred  <- arrange(pred,desc(psm))                        # Sort by descending smoothed prob
    pred[1:min(plen,predsetsize), c(3,2)]                   # Truncate to predsetsize - trim to "ahead" and "psm"
}

### guesswork - guess next word by max cooccurrence with input tokens #########

guesswork <- function(tkns) {

    tkns  <- tkns[tkns %in% alltokens]
    tknl  <- length(tkns)
    if (tknl==0) return(bestguess[1:(predsetsize+1),]) # (+1) dirty trick to pass back bestguess flag
    infcm <- fcmp[tkns,]
    if (tknl>1) infcm <- sort(apply(infcm, 2, sum),decreasing = TRUE)[1:predsetsize]
    else        infcm <- sort(infcm, decreasing = TRUE)[1:predsetsize]
    return(tibble(ahead=names(infcm),psm=infcm))
}


### addPredButtons: Create DIV with buttons for each predicted word ###########

addPredButtons <- function(prediction, part = FALSE) {
    insertUI("#predRow",where="afterBegin",div(id="predButtons"))
    btnclass <- ifelse(part,"btn btn-warning","btn btn-success")
    for(i in length(prediction):1)
        insertUI("#predButtons", where="afterBegin",
                 actionButton(paste0("BTN",i),
                              prediction[i], class=btnclass))
}


### isPartial: check if token tkn partially matches a token in predictor ######

isPartial <- function(tkn) {
    any(grep(paste0("^",tkn),alltokens)) & !(tkn %in% alltokens)
}


### Initialize predictor common data

lookahead <- read_rds(dpath_lookahead)                          # Load look ahead ngrams
for (i in 1:4)                                                  #    and for each level
        lookahead[[i]]<- select(lookahead[[i]],token,psm,ahead) #    keep only the variables we use
fcmp      <- read_rds(dpath_fcm_p)                              # Load co-occurence
lt4       <- length(lookahead[[4]]$ahead)                       # N-gram table sizes for look-ahead weighting
lt3       <- length(lookahead[[3]]$ahead)                       #
lt2       <- length(lookahead[[2]]$ahead)                       #
lt1       <- length(lookahead[[1]]$ahead)                       #
bestguess <- arrange(lookahead[[1]],desc(psm))[,c(3,2)]         # Best single token guess, for speed
alltokens <- bestguess$ahead                                    # Tokens in prediction set, for speed


