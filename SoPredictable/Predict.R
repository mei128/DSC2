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

### Include common

if (!exists("prdinclude")) source("./PRDInc.R")

## Support

# intoken - Convert input to character vector by tokens

inTokenize <- function(input) {
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
message("DEBUG in bycontext")
    prtoken     <- prtoken[!(prtoken$ahead %in% intoken),]    # Only tokens not already in the stream
    intoken     <- unique(intoken)
    intoken     <- intoken[intoken %in% alltokens]
    if ((length(prtoken$ahead)<2)|(length(intoken)<2)) return(prtoken)   # No context: nothing changes
    cntxtp      <- fcmp[prtoken$ahead,intoken]                # Cooccurrence probabilities
    cntxtp      <- apply(cntxtp,1,sum)                        # Add by prediction token
    cntxtsum    <- sum(cntxtp)
    cntxtp      <- cntxtp/cntxtsum
    psmsum      <- sum(prtoken$psm)                           # total prob of pred tokens
    prtoken$psm <- prtoken$psm*(cntxtp)                       #    scale by context factor
    psmcntxt    <- psmsum/sum(prtoken$psm)                    #    total scale
    prtoken$psm <- prtoken$psm*psmcntxt                       #    renormalize to previous total prob.
    prtoken
}


# ahead - look ahead for level n (n>=2); returns NULL if nothing found

ahead <- function(tkns,n, cntxt = FALSE) {
message("DEBUG in ahead")
    lakey <- paste(tail(tkns,n-1), collapse = nsplit)       # Look-ahead key
    pred  <- as.tbl(lookahead[[n]][lakey])                  #    ahead tibble
    if (is.na(pred$ahead[1])) return(NULL)                  # Nothing found: return null
    plen  <- length(pred$ahead)                             # Result count
    if ((plen>1)&cntxt)                                     # Context boosted
        pred <- bycontext(pred,tkns)                        #    rearrange by context influence
    pred  <- arrange(pred,desc(psm))                        # Sort by descending smoothed prob
    pred[1:min(plen,predsetsize), c(3,2)]                   # Truncate to predsetsize - trim to "ahead" and "psm"
}

# guesswork - guess next word by max cooccurrence with input tokens

guesswork <- function(tkns) {
message("DEBUG in guesswork")
    tkns  <- tkns[tkns %in% alltokens]
    tknl  <- length(tkns)
    if (tknl==0) return(bestguess)
    infcm <- fcmp[tkns,]
    if (tknl>1) infcm <- sort(apply(inin, 2, sum),decreasing = TRUE)[1:predsetsize]
    else        infcm <- sort(infcm, decreasing = TRUE)[1:predsetsize]
    return(tibble(ahead=names(infcm),psm=infcm))
}

# whatnext - return prediction set for given input string

whatnext <- function(tkns, tknl, cntxt = FALSE) {
    
    ng4 <- ng3 <- ng2 <- NULL
    if (tknl>2) ng4 <- ahead(tkns,4,cntxt)                  # 4-g rams level
    if (tknl>1) ng3 <- ahead(tkns,3,cntxt)                  # 3-g rams level
    if (tknl>0) ng2 <- ahead(tkns,2,cntxt)                  # 2-g rams level

    if ((ld4 <- length(ng4$ahead))>0) ld4 <- lt4/ld4        # discriminant value of N4 prediction
    if ((ld3 <- length(ng3$ahead))>0) ld3 <- lt3/ld3        #                    of N3 prediction
    if ((ld2 <- length(ng2$ahead))>0) ld2 <- lt2/ld2        #                    of N2 prediction
                                      lwg <- ld2+ld3+ld4    # total discriminant weight ML
    
    if (lwg == 0) {                                         # if not even 2 grams natched
        return(guesswork(tkns))
    }

    ld4 <- ld4/lwg
    ld3 <- ld3/lwg
    ld2 <- ld2/lwg
    
    ng4$psm <- ng4$psm*ld4              # "load" each level...
    ng3$psm <- ng3$psm*ld3
    ng2$psm <- ng2$psm*ld2
    ngpred  <- rbind(ng4,ng3,ng2) %>%   # and merge all 
                group_by(ahead) %>%
                summarise(psm = sum(psm)) %>%
                arrange(desc(psm))
    ngpred[1:min(length(ngpred$ahead),predsetsize),]
}

inText <- function(input) {
    tkns <- inTokenize(input)       # split input stream in tokens
    tknl <- length(tkns)            # input length in tokens
    whatnext(tkns,tknl,FALSE)
    
}
### Do it

lookahead <- read_rds(dpath_lookahead)                          # Load look ahead ngrams
for (i in 1:4)                                                  #    and for each level
        lookahead[[i]]<- select(lookahead[[i]],token,psm,ahead) #    keep only the variables we use
fcmp      <- read_rds(dpath_fcm_p)                              # Load co-occurence
lt4       <- length(lookahead[[4]]$ahead)                       # N-gram table sizes for look-ahead weighting
lt3       <- length(lookahead[[3]]$ahead)                       #
lt2       <- length(lookahead[[2]]$ahead)                       #
lt1       <- length(lookahead[[1]]$ahead)                       #
bestguess <- arrange(lookahead[[1]],desc(psm))[1:predsetsize,c(3,2)]  # Best single token guess, for speed
alltokens <- lookahead[[1]]$token                                     # Tokens in prediction set, for speed


