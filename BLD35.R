#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 40 : Create co-occurrence table for similar total size sample.
# 

### Include common

if (!exists("bldinclude")) source("./BLDInc.R")

### Do it

if (!exists("tokens_full")) {                 # Load where we left if unchained execution
    tokens_full <- read_rds(dpath_tn_train)
    message("Read tokens")
}

tokens_full <- tokens_sample(tokens_full,chnknum*chnksize) # subsample of similar total size

tokens_full <- tokens_remove(tokens_full, c(stopwords(),"")) # remove stopwords and padding

fcmc <- fcm(tokens_full,tri = FALSE)          # Symmetric co-ocurrence matrix
fcmc <- as.matrix(fcmc)
message("Co-occurence matrix done.")
write_rds(fcmc,dpath_fcm)                     # Save fcm
message("Saved matrix (count)")

tknc <- apply(fcmc,1,sum)                     # Individual token count
d    <- dim(fcmc)[1]
for (i in 1:d)
    for (j in 1:d) fcmc[i,j]<-fcmc[i,j]/(tknc[i]+tknc[j])

write_rds(fcmc,dpath_fcm_p)
message("Saved matrix (probability)")
rm(tokens_full, fcmc)

### Saved - Ctrl point 2B