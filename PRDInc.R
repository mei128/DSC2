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

dpath_ngram     <- "data/ngram_"               # base name for ngrams from subtables
dpath_fcm       <- "data/fcm.rds"              # co-occurrence matrix - count
dpath_fcm_p     <- "data/fcm_p.rds"            # co-occurrence matrix - ML
dpath_lookahead <- "data/lookahead.rds"        # Look ahead table

nsplit          <- "_"                         # ngram separator symbol (sync bldinclude)
predsetsize     <- 50                          # max prediction set size: truncate prediction to this size
predsetshow     <- 5                           # number of terms to show from prediction set

