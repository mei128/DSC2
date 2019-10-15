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



# ahead - look ahead tokens t for level n (>=2)

ahead <- function(t,l) {
    lakey <- paste(tail(t,l-1), collapse = nsplit)                     # Look-ahead key
    if (length(t) < (l-1))                                             # If not enough input
        return(tibble(token=lakey,n=NA,gt=NA,pml=NA,psm=NA,ahead=NA))  #    return NA output
                                                                       # else 
    t <- as.tbl(lookahead[[l]][lakey])                                 #    look ahead tibble
    t <- arrange(t,desc(psm))                                          #    sort by prob
    t[1:min(length(t$ahead),predsetsize),]                              #    truncate to predsize
}

# guesswork - guess next word when no n-gram matches: max cooccurrence with input tokens

guesswork <- function(t) {
    t<-t[t %in% cntxguess]
    if (length(t)==0) return(bestguess)
    inin <- fcmp[t,]
    if (length(t)>1) inin <- sort(apply(inin, 2, sum),decreasing = TRUE)[1:predsetshow]
    else             inin <- sort(inin,decreasing = TRUE)[1:predsetshow]
    return(tibble(ahead=names(inin),psm=inin))
}


# whatnext - return prediction set for given input string

whatnext <- function(input) {
    tkns <- intoken(input)            # split input stream in tokens

    ng4     <- ahead(tkns,4)          # 4-grams level
    if (is.na(ng4$ahead[1]))          #   clonk!
        la4 <- ng4$psm[1] <- 0
    else
        la4 <- lt4/length(ng4$ahead)  # discriminant value of N4 prediction
    
    ng3     <- ahead(tkns,3)          # 3-grams level
    if (is.na(ng3$ahead[1]))          #   clonk!
        la3 <- ng3$psm[1] <- 0
    else
        la3 <- lt3/length(ng3$ahead)  # discriminant value of N3 prediction
    
    ng2     <- ahead(tkns,2)          # 2-grams level
    if (is.na(ng2$ahead[1]))          #   clonk!
        la2 <- ng2$psm[1] <- 0
    else
        la2 <- lt2/length(ng2$ahead)  # discriminant value of N2 prediction
    
    if (la2 == 0) {                           # if not even 2 grams...
        return(guesswork(tkns))
    }

    lall    <- la2+la3+la4            # total discriminant weight
    ng4$psm <- ng4$psm*la4/lall       # "load" each level...
    ng3$psm <- ng3$psm*la3/lall
    ng2$psm <- ng2$psm*la2/lall
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


