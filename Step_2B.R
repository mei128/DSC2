#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 2B : Create co-occurrence table for a large enough sample.
# 

### Include common

if (!exists("common")) source("./Common.R")

### Do it

tokens_full <- read_rds(dpath_tn_train)
message("Read tokens")
tokens_full <- tokens_sample(tokens_full,chnknum*chnksize) # subsample of similar total size

fcmc <- fcm(tokens_full,tri = FALSE)          # Symmetric co-ocurrence matrix
fcmc <- as.matrix(fcmc)
message("Co-occurence matrix done.")
pad  <- which(colnames(fcmc)=="")             # Find padding (from dropped low freq tokens)
fcmc <- fcmc[-pad,-pad]                       # drop it
write_rds(fcmc,dpath_fcm)                     # Save fcm
message("Saved matrix (count)")

tknc <- apply(fcmc,1,sum)                     # Individual token count
d    <- dim(fcmc)[1]
for (i in 1:d)
    for (j in 1:d) fcmc[i,j]<-fcmc[i,j]/(tknc[i]+tknc[j])

write_rds(fcmc,dpath_fcm_p)
message("Saved matrix (probability)")

### Saved - Ctrl point 2B