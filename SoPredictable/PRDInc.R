#
# DS Capstone - Common declarations and shared functions
#
require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)
require(data.table)

### Globals

prdinclude      <- TRUE                        # common loaded flag

dpath_ngram_2     <- "ngram_"               # base name for ngrams from subtables
dpath_fcm_2     <- "fcm.rds"              # co-occurrence matrix - count
dpath_fcm_p     <- "./fcm_p.rds"            # co-occurrence matrix - ML
dpath_lookahead <- "./lookahead.rds"        # Look ahead table

nsplit_2          <- "_"                         # ngram separator symbol (sync bldinclude)
predsetsize     <- 50                          # max prediction set size: truncate prediction to this size
predsetshow     <- 10                          # number of terms to show from prediction set

