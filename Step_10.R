#
# DDS Capstone - Manuel Esteban-Infantes
#
# Step 10 : Merge all availabe data, lower case, clean, reshuffle, and save.
#

### Include common

if (!exists("common")) source("./Common.R")

### Globals

rgx1        <- intToUtf8(c(8216,8217,8218,8219), multiple = TRUE)   # reg exprs to convert
rgx2        <- intToUtf8(c(8220,8221,8222,8223), multiple = TRUE)   # UTF8 quote symbols
rgx3        <- intToUtf8(c(45,58,64))


### Do it

if (!file.exists(dpath_textf)) {
    srctext <- readtext(dpath_base, encoding = "UTF-8")            # Read all files in
    message(paste0("Read ",length(srctext$doc_id)," documents."))
    srcbody <- corpus(srctext)                                     # Convert to corpus
    message("Corpus built")
    rm(srctext)                                                    # Free some precious space
    srcbody <- corpus_reshape(srcbody, to = "sentences")           # Reshape to sentences
    message("Reshaped to sentences.")
    srcbody <- srcbody$documents$text                              # Keep just the texts, drop file
    
    srcbody <- srcbody %>%                                         # Clean and tidy
        stri_trans_tolower()  %>%
        stri_replace_all_fixed(rgx1,"'", vectorize_all = FALSE) %>%
        stri_replace_all_fixed(rgx2,'"', vectorize_all = FALSE) %>%
        stri_replace_all_fixed(rgx3," ", vectorize_all = FALSE)
    
    srcbody <- srcbody[sample(length(srcbody))]                    # Reshuffle sentences
    message("Clean and reshuffled.")
    
    write_lines(srcbody,dpath_textf)                               # Save for next step
}

### Saved - Ctrl point 0
