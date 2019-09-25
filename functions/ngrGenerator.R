ngrGenerator <- function(listOfwords, n) {
    output <- vector("list", length(listOfwords))
    pb <- txtProgressBar(min = 1, max = length(listOfwords), style = 3)
    i = 1
    while (i <= length(listOfwords)) {
        sentence <- listOfwords[[i]]
        len <- length(sentence)
        if (len >= n) {
            output[[i]] <- sapply(1:(len-n+1),
                                  function(j) do.call(paste,
                                                      as.list(sentence[j:(j+n-1)])
                                  )
            )
        } else {
            output[[i]] <- NULL
        }
        
        setTxtProgressBar(pb, i)
        i <- i + 1
    }
    return(unlist(output))
}