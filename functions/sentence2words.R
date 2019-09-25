sentence2words <- function(listOfsentences) {
    output <- vector("list", length(listOfsentences))
    pb <- txtProgressBar(min = 1, max = length(listOfsentences), style = 3)
    i = 1
    while(i <= length(listOfsentences)){
        output[[i]] <- unlist(strsplit(gsub("[.]",
                                            "",
                                            listOfsentences[[i]]),
                                       " "))
        setTxtProgressBar(pb, i)
        i <- i + 1
    }
    return(output)
}
