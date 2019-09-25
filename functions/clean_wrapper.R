clean_wrapper <- function(charVect) {
    len <- length(charVect)
    output <- vector("list", len)
    i = 1
    pb <- txtProgressBar(min = 1, max = len, initial = i, style = 3)
    while(i <= len) {
        output[[i]] <- clean2(charVect[[i]])
        setTxtProgressBar(pb, i)
        i <- i + 1
    }
    return(unlist(output))
}
