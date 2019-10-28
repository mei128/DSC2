#
# DDS Capstone - Manuel Esteban-Infantes
#
# Predict 10 : First prediction model
#
# The model will create tables of predicitions for n-1 (4 grams), n-2 (3-grams) n-3 (2-grams),
# and n-4 (single token freq), combining all tables weighting by discriminant value of each
# table. The predicted terms (and single token) will be boosted proportionally to their
# co-occurrence with all the other terms in the input stream beyong the current n-gram level.
# Test different weights and boosts to fine tune the model.
#
# Eventually extend the model to 5-grams, but tests done so far show that the increase in size 
# of the model does not increase prediction quality significantly.
#

### Packages

require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)
require(data.table)

### Globals

dpath_fcm_p     <- "./fcm_p.rds"               # co-occurrence matrix - ML
dpath_lookahead <- "./lookahead.rds"           # Look ahead table

nsplit          <- "_"                         # Token separator in ngram tables
predsetsize     <- 50                          # max prediction set size: truncate prediction to this size
predsetshow     <- 10                          # number of terms to show from prediction set

## Support

# intoken - Convert input to character vector by tokens

tokenize <- function(input) {
    t <- tokens(stri_trans_tolower(input),
                remove_punct   = TRUE,
                remove_symbols = TRUE,
                remove_numbers = TRUE,
                remove_twitter = TRUE,
                remove_url     = TRUE)[[1]]
    return(t)
}

# bycontext - filter and reassign probabiliy by context

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


# ahead - look ahead for level n (n>=2); returns NULL if nothing found

ahead <- function(tkns,n) {

    lakey <- paste(tail(tkns,n-1), collapse = nsplit)       # Look-ahead key
    pred  <- as.tbl(lookahead[[n]][lakey])                  #    ahead tibble
    if (is.na(pred$ahead[1])) return(NULL)                  # Nothing found: return null
    plen  <- length(pred$ahead)                             # Result count
    pred  <- arrange(pred,desc(psm))                        # Sort by descending smoothed prob
    pred[1:min(plen,predsetsize), c(3,2)]                   # Truncate to predsetsize - trim to "ahead" and "psm"
}

# guesswork - guess next word by max cooccurrence with input tokens

guesswork <- function(tkns) {

    tkns  <- tkns[tkns %in% alltokens]
    tknl  <- length(tkns)
    if (tknl==0) return(bestguess[1:predsetsize,])
    infcm <- fcmp[tkns,]
    if (tknl>1) infcm <- sort(apply(infcm, 2, sum),decreasing = TRUE)[1:predsetsize]
    else        infcm <- sort(infcm, decreasing = TRUE)[1:predsetsize]
    return(tibble(ahead=names(infcm),psm=infcm))
}

# whatnext - return prediction set for given input string - full tokens

nextfull <- function(tkns, tknl, cntxt = FALSE) {

    ng4 <- ng3 <- ng2 <- NULL
    ld4 <- ld3 <- ld2 <- 0
    
    if (tknl>0) {                   # 2-grams level
        ng2 <- ahead(tkns,2)        #   predictions
        ld2 <- length(ng2$ahead)    #   count
    }
    if ((tknl>1)&(ld2>0)) {         # 3-g rams level
        ng3 <- ahead(tkns,3)        #   predictions
        ld3 <- length(ng3$ahead)    #   count
    }
    if ((tknl>2)&(ld3>0)) {         # 4-g rams level
        ng4 <- ahead(tkns,3)        #   predictions
        ld4 <- length(ng3$ahead)    #   count
    }
    
    if (ld2>0) ld2 <- lt2/ld2       # discriminant value of N2 prediction
    if (ld3>0) ld3 <- lt3/ld3       #                    of N3 prediction
    if (ld4>0) ld4 <- lt4/ld4       #                    of N4 prediction
               lwg <- ld2+ld3+ld4   # total discriminant weight ML
    
    if (lwg > 0) {                  # if prediction success

        ld2 <- ld2/lwg
        ld3 <- ld3/lwg
        ld4 <- ld4/lwg
        
        ng2$psm <- ng2$psm*ld2              # "load" each level...
        ng3$psm <- ng3$psm*ld3
        ng4$psm <- ng4$psm*ld4 
        ngpred  <- rbind(ng4,ng3,ng2) %>%   # and merge all 
            group_by(ahead) %>%
            summarise(psm = sum(psm))
        
        if (cntxt&(tknl>((ld2>0)+(ld3>0)+(ld4>0))))
            ngpred <- bycontext(ngpred,tkns)
        
        ngpred  <- arrange(ngpred, desc(psm))
        ngpred  <- ngpred[1:min(length(ngpred$ahead),predsetsize),]
    
    } else {                        # else (if fail to precit) guess
        ngpred <- guesswork(tkns)
    }

    psmsum  <- sum(ngpred$psm)
    ngpred$psm <- ngpred$psm/psmsum
    return(ngpred)
}

# whatpart - return prediction set for given input string - partial last token

nextpart <- function(tkns, tknl, cntxt = FALSE) {
    
    pg4 <- pg3 <- pg2 <- pg1 <- NULL
    pd4 <- pd3 <- pd2 <- pd1 <- 0
    
    ltkn <- tkns[tknl]                              # 1-gram 
    lreg <- paste0("^",ltkn,".*")
    pg1  <- bestguess[grep(lreg,bestguess$ahead),]  #   check for partial last token
    pd1  <- length(pg1$ahead)

    if (pd1 ==0) return(NULL)                       # No partial match for last token: NULL

    tkns <- tkns[-tknl]                             # strip last token
    tknl <- tknl-1
    
    if (tknl>0) {                                   # 2-grams level
        pg2 <- ahead(tkns,2)                        #   predictions
        pg2 <- pg2[grep(lreg,pg2$ahead),]           #   partial match to tlast token
        pd2 <- length(pg2$ahead)                    #   count
    }
    if ((tknl>1)&(pd2>0)) {                         # 3-g rams level
        pg3 <- ahead(tkns,3)                        #   predictions
        pg3 <- pg3[grep(lreg,pg3$ahead),]           #   partial match to tlast token
        pd3 <- length(pg3$ahead)                    #   count
    }
    if ((tknl>2)&(pd3>0)) {                         # 4-g rams level
        pg4 <- ahead(tkns,3)                        #   predictions
        pg4 <- pg4[grep(lreg,pg4$ahead),]           #   partial match to tlast token
        pd4 <- length(pg4$ahead)                    #   count
    }
    
    if (pd1>0) pd1 <- lt1/pd1           # discriminant value of N1 prediction
    if (pd2>0) pd2 <- lt2/pd2           #                    of N2 prediction
    if (pd3>0) pd3 <- lt3/pd3           #                    of N3 prediction
    if (pd4>0) pd4 <- lt4/pd4           #                    of N4 prediction
               pwg <- pd1+pd2+pd3+pd4   # total discriminant weight ML
    
    pd1 <- pd1/pwg
    pd2 <- pd2/pwg
    pd3 <- pd3/pwg
    pd4 <- pd4/pwg
    
    pg1$psm <- pg1$psm*pd1              # "load" each level...
    pg2$psm <- pg2$psm*pd2
    pg3$psm <- pg3$psm*pd3
    pg4$psm <- pg4$psm*pd4
    pgpred  <- rbind(pg4,pg3,pg2,pg1) %>%   # and merge all 
        group_by(ahead) %>%
        summarise(psm = sum(psm))

    tkns   <- c(tkns,ltkn)              # restore in token list
    tknl   <- tknl + 1
    
    if (cntxt&(tknl>((pd1>0)+(pd2>0)+(pd3>0)+(pd4>0))))
        pgpred <- bycontext(pgpred,tkns)

    pgpred <- arrange(pgpred,desc(psm))
    pgpred <- pgpred[1:min(length(pgpred$ahead),predsetsize),]
    psmsum  <- sum(pgpred$psm)
    pgpred$psm <- pgpred$psm/psmsum
    return(pgpred)
}

### Initialize predictor data

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


