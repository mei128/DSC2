#
# DDS Capstone - Manuel Esteban-Infantes
#
# core prediction functions and shared data sourced within server.
#

# Shared data within server (per user session)

    ng4    <- tibble()  # ngram based prediction at each level
    ng3    <- tibble()
    ng2    <- tibble()
    ng1    <- tibble()
    ngpred <- tibble()
    ngmode <- 0L        # last prediction mode  0:Ngram 1:Context 2:Bestguess
    nglen  <- 0L        # prediction length
    
    ld4 <- numeric()    # matches on each level
    ld3 <- numeric()
    ld2 <- numeric()
    ld1 <- numeric()

    lw4 <- numeric()    # weigth assigned to each level
    lw3 <- numeric()
    lw2 <- numeric()
    lw1 <- numeric()
    
# predict on full words (don't check partial match)   
    
nextfull <- function(tkns, tknl, cntxt = FALSE) {

    ng4 <<- ng3 <<- ng2 <<- ng1 <<- ngpred <<- NULL
    ld4 <<- ld3 <<- ld2 <<- ld1 <<- 0
    lw4 <<- lw3 <<- lw2 <<- lw1 <<- 0
    
    if (tknl>0) {                                   # 2-grams level
        ng2     <<- ahead(tkns,2)                   #   predictions
        ld2     <<- length(ng2$ahead)               #   count
        ng2     <<- ng2[1:min(ld2,predsetsize),]    # trim tail
        mlk      <- sum(ng2$psm)
        ng2$psm <<- ng2$psm/mlk
    }
    if ((tknl>1)&(ld2>0)) {                         # 3-g rams level
        ng3     <<- ahead(tkns,3)                   #   predictions
        ld3     <<- length(ng3$ahead)               #   count
        ng3     <<- ng3[1:min(ld3,predsetsize),]    # trim tail
        mlk      <- sum(ng3$psm)
        ng3$psm <<- ng3$psm/mlk
    }
    if ((tknl>2)&(ld3>0)) {                         # 4-g rams level
        ng4 <<-     ahead(tkns,4)                   #   predictions
        ld4 <<-     length(ng4$ahead)               #   count
        ng4     <<- ng4[1:min(ld4,predsetsize),]    # trim tail
        mlk      <- sum(ng4$psm)
        ng4$psm <<- ng4$psm/mlk
    }
    
    if (ld2>0) lw2 <<- lt2/ld2      # discriminant value of N2 prediction
    if (ld3>0) lw3 <<- lt3/ld3      #                    of N3 prediction
    if (ld4>0) lw4 <<- lt4/ld4      #                    of N4 prediction
               lwg <-  lw2+lw3+lw4  # total discriminant weight ML
    
    if (lwg > 0) {                  # if prediction success

        lw2 <<- lw2/lwg
        lw3 <<- lw3/lwg
        lw4 <<- lw4/lwg
        
        ng2$psm <<- ng2$psm*lw2             # "load" each level...
        ng3$psm <<- ng3$psm*lw3
        ng4$psm <<- ng4$psm*lw4 
        ngpred  <<- rbind(ng4,ng3,ng2) %>%  # and merge all 
            group_by(ahead) %>%
            summarise(psm = sum(psm))
     
        if (cntxt&(tknl>((ld2>0)+(ld3>0)+(ld4>0)))) # if context toggle
            ngpred <<- bycontext(ngpred,tkns)       # rearrange by context

        nglen   <<- min(length(ngpred$ahead), predsetsize)
        ngpred  <<- arrange(ngpred, desc(psm))
        ngpred  <<- ngpred[1:nglen,]
        ngmode  <<- 0
    } else {                        # else (if fail to precit) guess
        ngpred <<- guesswork(tkns)
        ngmode <<- 1 + (nglen  <<- length(ngpred$ahead))-predsetsize # dirty trick to get bestguess flag
    }

    psmsum      <- sum(ngpred$psm)      # normalize (ML) probabilities
    ngpred$psm <<- ngpred$psm/psmsum
}

# predict on partial match for last token

nextpart <- function(tkns, tknl, cntxt = FALSE) {
    
    ng4 <<- ng3 <<- ng2 <<- ng1 <<- NULL
    ld4 <<- ld3 <<- ld2 <<- ld1 <<- 0
    lw4 <<- lw3 <<- lw2 <<- lw1 <<- 0
    
    ltkn  <- tkns[tknl]                             # 1-gram 
    lreg  <- paste0("^",ltkn,".*")
    ng1  <<- bestguess[grep(lreg,bestguess$ahead),] #   check for partial last token
    ld1  <<- length(ng1$ahead)
    ng1     <<- ng1[1:min(ld1,predsetsize),]    # trim tail
    mlk      <- sum(ng1$psm)
    ng1$psm <<- ng1$psm/mlk
    
    if (ld1 ==0) return(NULL)                       # No partial match for last token: NULL - shouldn't happen

    ng1     <<- ng1[1:min(ld1,predsetsize),]        # trim tail
    mlk      <- sum(ng1$psm)
    ng1$psm <<- ng1$psm/mlk
    
    tkns <- tkns[-tknl]                             # strip last token
    tknl <- tknl-1
    
    if (tknl>0) {                                   # 2-grams level
        ng2     <<- ahead(tkns,2)                   #   predictions
        ng2     <<- ng2[grep(lreg,ng2$ahead),]      #   partial match to tlast token
        ld2     <<- length(ng2$ahead)               #   count
        ng2     <<- ng2[1:min(ld2,predsetsize),]    # trim tail
        mlk      <- sum(ng2$psm)
        ng2$psm <<- ng2$psm/mlk
    }
    if ((tknl>1)&(ld2>0)) {                         # 3-g rams level
        ng3     <<- ahead(tkns,3)                   #   predictions
        ng3     <<- ng3[grep(lreg,ng3$ahead),]      #   partial match to tlast token
        ld3     <<- length(ng3$ahead)               #   count
        ng3     <<- ng3[1:min(ld3,predsetsize),]    # trim tail
        mlk      <- sum(ng3$psm)
        ng3$psm <<- ng3$psm/mlk
    }
    if ((tknl>2)&(ld3>0)) {                         # 4-g rams level
        ng4     <<- ahead(tkns,4)                   #   predictions
        ng4     <<- ng4[grep(lreg,ng4$ahead),]      #   partial match to tlast token
        ld4     <<- length(ng4$ahead)               #   count
        ng4     <<- ng4[1:min(ld4,predsetsize),]    # trim tail
        mlk      <- sum(ng4$psm)
        ng4$psm <<- ng4$psm/mlk
    }

    
    if (ld1>0) lw1 <<- lt1/ld1          # discriminant value of N1 prediction
    if (ld2>0) lw2 <<- lt2/ld2          #                    of N2 prediction
    if (ld3>0) lw3 <<- lt3/ld3          #                    of N3 prediction
    if (ld4>0) lw4 <<- lt4/ld4          #                    of N4 prediction
               lwg  <- lw1+lw2+lw3+lw4  # total discriminant weight ML
    
    lw1 <<- lw1/lwg
    lw2 <<- lw2/lwg
    lw3 <<- lw3/lwg
    lw4 <<- lw4/lwg
    
    ng1$psm <<- ng1$psm*lw1                 # "load" each level...
    ng2$psm <<- ng2$psm*lw2
    ng3$psm <<- ng3$psm*lw3
    ng4$psm <<- ng4$psm*lw4
    ngpred  <<- rbind(ng4,ng3,ng2,ng1) %>%  # and merge all 
        group_by(ahead) %>%
        summarise(psm = sum(psm))

    tkns   <- c(tkns,ltkn)              # restore in token list
    tknl   <- tknl + 1
    
    if (cntxt&(tknl>((ld1>0)+(ld2>0)+(ld3>0)+(ld4>0)))) # If context toggle
        ngpred <<- bycontext(ngpred,tkns)               # rearrange by context

    nglen   <<- min(length(ngpred$ahead), predsetsize)
    ngpred  <<- arrange(ngpred, desc(psm))
    ngpred  <<- ngpred[1:nglen,]
    ngmode  <<- 0
    psmsum      <- sum(ngpred$psm)      # normalize (ML) probabilities
    ngpred$psm <<- ngpred$psm/psmsum
}

