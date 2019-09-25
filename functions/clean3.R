clean3 <- function(input) {
    output <- tolower(input)
    
    # remove periods from titles
    output <- gsub("((^| )(st|ms|mr|mrs|dr|rev|jr|sr))[.]", "\\1", output)
    
    # remove non-internal dashes
    output <- gsub("(^[-]+)|( +[-])|([-] +)|([-]+$)", " ", output)
    output <- gsub('(^| )[-]( |$)', " ", output)
    
    # converts abbreviations like "u. n." into "un"
    output <- gsub("(^| )([[:alnum:]])[.]( )?([[:alnum:]])[.]", "\\1\\2\\4", output)

    output <- gsub("--", " ", output)
    output <- gsub("[.] -", ". ", output)
    output <- gsub(" ?[.] [.] ?", ". ", output)
    output <- gsub("[[:space:]]+", " ", output)    
    output <- gsub("(^ +)|( +$)", "", output)
    
    return(output)
}
