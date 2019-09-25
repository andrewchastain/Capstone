split_sentence <- function(list) {
# The function to split into sentences was found here:
# https://stackoverflow.com/questions/46884556/split-character-vector-into-sentences
    output <- vector("list", length(list))
    i <- 1
    pb <- txtProgressBar(min = 1, max = length(list), style = 3)
    while(i <= length(list)){
        output[[i]] <- unlist(strsplit(list[[i]], "(?<=\\.)\\s(?=[a-z<])", perl = T))
        setTxtProgressBar(pb, i)
        i <- i + 1
    }
    close(pb)
    return(unlist(output))
}