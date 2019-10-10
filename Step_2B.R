#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 2B : Create co-occurrence table for a large enough sample.
# 
require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)

### Globals

dpath_tn_train <- "data/tokens_n1_train.rds"
dpath_fcm      <- "data/fcm.rds"
dpath_fcm_p    <- "data/fcm_p.rds"


### Do it

tokens_full <- read_rds(dpath_tn_train)
message("Read tokens")
tokens_full <- tokens_sample(tokens_full,1000000)
fcmc <- fcm(tokens_full,tri = FALSE)          # Triangular co-ocurrence matrix
message("Co-occurence matrix done.")
pad  <- which(fcmc@Dimnames$features=="")     # Find padding (from dropped low freq tokens)
fcmc <- fcmc[-pad,-pad]                       # drop it
tkn  <- sum(fcmc[upper.tri(fcmc,diag=TRUE)])  # Total tokens count

write_rds(fcmc,dpath_fcm)
message("Saved matrix (count)")
fcmc <-apply(fcmc,c(1,2),function(x) x/tkn)

write_rds(fcmc,dpath_fcm_p)
message("Saved matrix (probability)")

### Saved - Ctrl point 2B