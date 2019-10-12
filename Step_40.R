#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 50 : Merge all ngram tables created
#

### Include common

if (!exists("common")) source("./Common.R")

## Do it #

nfiles <- list.files(dpath_nbase, pattern = dpath_pattern, full.names = TRUE) # Files ready to merge
nlist  <- list()                                                              # Empty container
ngrams <- list()

for (i in 1:length(nfiles)) {                             # Read all ngram tables
    message(paste0("Read ",nfiles[i]))
    nlist[[i]] <- read_rds(nfiles[i])
}

for (i in 1:length(nlist[[1]])) {                         # For each n-gram level
    mrgd <- tibble(token=character(), n=numeric())
    message(paste0(i,"-grams"))
    for (j in 1:length(nlist)) {
        mrgd <- rbind.data.frame(mrgd,nlist[[j]][[i]])    # Row bind all samples
    }
    mrgd <- mrgd %>% group_by(token) %>%                  # and reconsolidate counts
            summarise(n=sum(n)) %>%
            arrange(desc(n))
    ngrams[[i]] <- mrgd
}

write_rds(ngrams,dpath_ngrams)                            # Save for next step
rm(nlist)

### Saved - Ctrl point 3