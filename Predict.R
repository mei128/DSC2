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

intoken <- function(input) {
    t <- tokens(stri_trans_tolower(input),
                remove_punct   = TRUE,
                remove_symbols = TRUE,
                remove_numbers = TRUE,
                remove_twitter = TRUE,
                remove_url     = TRUE)[[1]]
    return(t)
}

# context - filter and reassign probabiliy by context

bycontext <- function(prtoken,intoken) {
    prtoken     <- prtoken[prtoken$ahead %in% cntxguess,]     # Only tokens in cooccurrence matrix
    prtoken     <- prtoken[!(prtoken$ahead %in% intoken),]    # but not already in the stream
    intoken     <- unique(intoken)
    intoken     <- intoken[intoken %in% cntxguess]
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


# ahead - look ahead tokens t for level n (n>=2)

ahead <- function(intoken,n, cntxt = FALSE) {
    lakey <- paste(tail(intoken,n-1), collapse = nsplit)               # Look-ahead key
    t     <- as.tbl(lookahead[[n]][lakey])                             #    ahead tibble
    l     <- length(t$ahead)                                           #    result count (1 could be NA!)
    if ((l>1)&cntxt)                                                   # Context boosted
        t <- bycontext(t,intoken)                                      #
    t <- arrange(t,desc(psm))                                          #    sort by prob
    t[1:min(length(t$ahead),predsetsize),]                             #    truncate to predsize
}

# guesswork - guess next word by max cooccurrence with input tokens

guesswork <- function(t) {
    t<-t[t %in% cntxguess]
    if (length(t)==0) return(bestguess)
    inin <- fcmp[t,]
    if (length(t)>1) inin <- sort(apply(inin, 2, sum),decreasing = TRUE)[1:predsetshow]
    else             inin <- sort(inin,decreasing = TRUE)[1:predsetshow]
    return(tibble(ahead=names(inin),psm=inin))
}


# whatnext - return prediction set for given input string

nothing  <- function(intoken) tibble(token=paste0(intoken,nsplit),n=0,pml=0,psm=0,gt=0,ahead=NA)

whatnext <- function(input, cntxt = FALSE) {
    tkns <- intoken(input)            # split input stream in tokens
    tknl <- length(tkns)              # input length in tokens
    
    if (tknl>2) ng4 <- ahead(tkns,4,cntxt) # 4-g rams level
    else        ng4 <- nothing(tkns)
    if (tknl>1) ng3 <- ahead(tkns,3,cntxt) # 3-g rams level
    else        ng3 <- nothing(tkns)
    if (tknl>0) ng2 <- ahead(tkns,2,cntxt) # 2-g rams level
    else        ng2 <- nothing(tkns)

    la4 <- ifelse(is.na(ng4$ahead[1]), 0, lt4/length(ng4$ahead)) # discriminant value of N4 prediction
    la3 <- ifelse(is.na(ng3$ahead[1]), 0, lt3/length(ng3$ahead)) #                       N3 prediction
    la2 <- ifelse(is.na(ng2$ahead[1]), 0, lt2/length(ng2$ahead)) #                       N2 prediction
    lwg <- la2+la3+la4                                           # total discriminant weight ML
    
    if (lwg == 0) {                           # if not even 2 grams...
        return(guesswork(tkns))
    }

    la4 <- la4/lwg
    la3 <- la3/lwg
    la2 <- la2/lwg
    
    ng4$psm <- ng4$psm*la4              # "load" each level...
    ng3$psm <- ng3$psm*la3
    ng2$psm <- ng2$psm*la2
    ng4     <- rbind(ng4,ng3,ng2) %>%   # and merge all 
                group_by(ahead) %>%
                summarise(psm = sum(psm)) %>%
                arrange(desc(psm))
    ng4[1:min(length(ng4$ahead),predsetshow),]
}

### Do it

lookahead <- read_rds(dpath_lookahead)                          # Load look ahead ngrams
fcmp      <- read_rds(dpath_fcm_p)                              # Load co-occurence
lt4       <- length(lookahead[[4]]$ahead)                       # N-gram table sizes for look-ahead weighting
lt3       <- length(lookahead[[3]]$ahead)                       #
lt2       <- length(lookahead[[2]]$ahead)                       #
lt1       <- length(lookahead[[1]]$ahead)                       #
bestguess <- arrange(lookahead[[1]],desc(psm))[1:predsetshow,]  # Best single token guess, for speed
cntxguess <- colnames(fcmp)                                     # Tokens in cooccurrence matrix, for speed


