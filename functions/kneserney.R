kneserney.old <- function(dat, ngram) {
    require(data.table)
    setDTthreads(threads = 0)
    require(beepr)
    
    freq <- dat[, .N, by = r][order(r)]
    beep(sound = 4)
    cat("File Loaded \n")
    
    Y <- freq[[1,2]]/(freq[[1,2]] + 2 * freq[[2,2]])
    D1 <- 1 - 2 * Y * (freq[[2,2]]/freq[[1,2]])
    D2 <- 2 - 3 * Y * (freq[[3,2]]/freq[[2,2]])
    D3 <- 3 - 4 * Y * (freq[[4,2]]/freq[[3,2]])
    rm(freq)
    
    termReg <- paste0("^([[:graph:]<>]*\\s*){",(ngram - 1),"}")
    dat[, root := gsub("\\s*[[:graph:]]*$", "", feature)]
    dat[, term := gsub(termReg, "", feature)]
    dat[, feature := NULL]
    setcolorder(dat, c("root", "term", "r"))
    cat("ngram sliced \n")
    setkey(dat, "root")
    beep(sound = 4)
    
    dat1 <- dat[r == 1]
    dat2 <- dat[r == 2]
    dat3 <- dat[r > 2]
    
    dat3 <- dat3[, .N, by = root]
    dat2 <- dat2[, .N, by = root]
    dat1 <- dat1[, .N, by = root]
    setnames(dat1, c("root", "N1"))
    setnames(dat2, c("root", "N2"))
    setnames(dat3, c("root", "N3"))
    setkey(dat1, root)
    setkey(dat2, root)
    setkey(dat3, root)
    dat[, root_sum := sum(r), by = root]
    dat <- merge(dat, dat1, all = TRUE)
    dat[is.na(N1), N1 := 0]
    dat <- merge(dat, dat2, all = TRUE)
    dat[is.na(N2), N2 := 0]
    dat <- merge(dat, dat3, all = TRUE)
    dat[is.na(N3), N3 := 0]
    cat("N1-3 generated \n")
    beep(sound = 4)
    rm(dat1, dat2, dat3)
    
    dat[, gamma := (N1 * D1 + N2 * D2 + N3 * D3)/root_sum]
    dat[, c("N1","N2","N3") := NULL]
    cat("Gamma calculated \n")
    beep(sound = 4)
    
    dat[, cont1 := paste(root, term)]
    dat[, cont1 := gsub("^([[:graph:]]*\\s*)", "", cont1)]
    setkey(dat, cont1)
    dat[, cont_num := .N, by = cont1]
    dat[, cont1 := NULL]
    
    dat[, cont2 := gsub("^([[:graph:]]*\\s*)", "", root)]
    setkey(dat, cont2)
    dat[, cont_denom := .N, by = cont2]
    dat[, cont2 := NULL]
    cat("Make continuation splits \n")
    beep(sound = 4)
    
    len <- length(dat$root)
    dat[, pcont := cont_num/cont_denom][order(-pcont)]
    cat("Calculate continuation probability \n")
    beep(sound = 4)
    
    dat[, c("cont_num", "cont_denom") := NULL]
    dat[r == 1, pKN := (1 - D1)/root_sum + gamma * pcont]
    dat[r == 2, pKN := (2 - D2)/root_sum + gamma * pcont]
    dat[r > 2, pKN := (r - D3)/root_sum + gamma * pcont]
    cat("pKN calculated \n")
    beep(sound = 4)
    
    dat <- list("ngram" = dat, "D1" = D1, "D2" = D2, "D3" = D3, "Y" = Y, "len" = len)
    saveRDS(dat, paste0("./KN/", ngram, "gram.rds"))
    
    cat(paste0("Output saved for ", ngram, "gram. \n"))
}

kneserney <- function(dt, ngram, DTthreads = 0, export_list = FALSE) {
    require(data.table)
    setDTthreads(threads = DTthreads)
    
    # Calculate the frequency distribution of frequency counts and take top 4
    freq <- dt[, .N, by = r][order(r)][1:4,]
    
    # Calculate the discounting values
    Y  <- freq[[1,2]]/(freq[[1,2]] + 2 * freq[[2,2]])
    D1 <- 1 - 2 * Y * (freq[[2,2]]/freq[[1,2]])
    D2 <- 2 - 3 * Y * (freq[[3,2]]/freq[[2,2]])
    D3 <- 3 - 4 * Y * (freq[[4,2]]/freq[[3,2]])
    rm(freq)
    
    # termReg sets the regex for the first (n-1) words
    termReg <- paste0("^([[:graph:]<>]*\\s*){",(ngram - 1),"}")

    # Split "feature" into root (w_{i-n+1}...w_{i-1}) and term (w_i)
    dt[, root := gsub("\\s*[[:graph:]]*$", "", feature)]
    dt[, term := gsub(termReg, "", feature)]
    dt[, feature := NULL]  # gets rid of feature
    setcolorder(dt, c("root", "term", "r"))
    setkey(dt, "root")

    # Separate words with r > 2 occurances    
    dt3 <- dt[r > 2]
    # Counts number of unique terminal words per root
    dt3 <- dt3[, .N, by = root]
    setnames(dt3, c("root", "N3"))
    setkey(dt3, root)
    
    # Separate words with 2 occurances
    dt2 <- dt[r == 2]
    # Counts number of unique terminal words per root
    dt2 <- dt2[, .N, by = root]
    setnames(dt2, c("root", "N2"))
    setkey(dt2, root)
    
    # Separate unique features
    dt1 <- dt[r == 1]
    # Counts number of unique terminal words per root
    dt1 <- dt1[, .N, by = root]
    setnames(dt1, c("root", "N1"))
    setkey(dt1, root)
    
    dt[, root_sum := sum(r), by = root]
    
    # Merge N1-N3 values with main table, and fill NAs with zeroes
    dt <- merge(dt, dt1, all = TRUE)
    dt[is.na(N1), N1 := 0]
    dt <- merge(dt, dt2, all = TRUE)
    dt[is.na(N2), N2 := 0]
    dt <- merge(dt, dt3, all = TRUE)
    dt[is.na(N3), N3 := 0]
    rm(dt1, dt2, dt3) # cleaning
    
    # Calculate the gamma values
    dt[, gamma := (N1 * D1 + N2 * D2 + N3 * D3)/root_sum]
    dt[, c("N1","N2","N3") := NULL]

    # Recreate the original n-gram to reslice for the continuation probability
    dt[, cont1 := paste(root, term)]
    
# NB: The Chen and Goodman (1999) treatment of this has the higher order
# generalization of p_{KN}(w_i|w_{i-n+2}...w_{i+1}) defined as the count of 
# the number of n-grams ending with (w_{i-n+2}...w_i) (a.k.a. the first word
# (w_{i-n+1}) removed), divided by the number of n-grams containing a middle of
# (w_{i-n+2}...w_{i+1}).
# 
# NB2: I wrote this out originally making new columns for cont1 and cont2
# representing the counts used for the numerator and the denominator, but
# realized in later code review that cont2 was just a further trimmed cont1 and
# I rewrote it like that. It might be even better to calculate something like:
# 
# dt[, pcont := .N, by = cont1]  # instead of cont_num
# dt[, cont1 := gsub("(\\s*[[:graph:]]*)$", "", cont1)]
# setkey(dt, cont1)
# dt[, pcont := pcont/.N, by = cont1]
#
# I didn't check to see if this was better in terms of efficiency, but it would
# seem to also be less readable.
    
    # Remove (w_{i-n+1}) from full n-gram
    dt[, cont1 := gsub("^([[:graph:]]*\\s*)", "", cont1)]
    setkey(dt, cont1)
    # Count unique n-grams that end in (w_{i-n+2}...w_i)
    dt[, cont_num := .N, by = cont1]

    # Remove (w_i) from cont1
    dt[, cont1 := gsub("(\\s*[[:graph:]]*)$", "", cont1)]
    setkey(dt, cont1)
    # Count unique n-grams with same middle portion
    dt[, cont_denom := .N, by = cont1]
    dt[, cont1 := NULL]  # clean up

    # Calculate continuation probability
    dt[, pcont := cont_num/cont_denom]
    
    dt[, c("cont_num", "cont_denom") := NULL]  # cleaning

    # Each probability is calculated
    dt[r == 1, pKN := (1 - D1)/root_sum + gamma * pcont]
    dt[r == 2, pKN := (2 - D2)/root_sum + gamma * pcont]
    dt[r > 2, pKN := (r - D3)/root_sum + gamma * pcont]

    # export_list tacks on the discounting values, if curious
    if (export_list == TRUE) {
        dt <- list("ngram" = dt, "D1" = D1, "D2" = D2, "D3" = D3, "Y" = Y)
    }
    
    return(dt)
}