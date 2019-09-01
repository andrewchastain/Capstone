kneserney <- function(dat, ngram) {
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