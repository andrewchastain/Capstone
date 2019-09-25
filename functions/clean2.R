clean2 <- function(line) {
    # fix apostrophes (replace lookalikes)
    str <- gsub("’|‘|\u2032|\u201A|\u0091|\u0092|\u2039|\u2039", "'", line)
    # remove funny quotes
    str <- gsub("[/\"“”«»]", " ", str)
    # convert smart dashes to normal dashes
    str <- gsub("(–|—|–|―)+", "-", str)
    # remove non-internal dashes
    str <- gsub("(^[-]+)|( +[-])|([-] +)|([-]+$)", " ", str)
    # remove non-internal asterisks
    str <- gsub("(^[']+)|( +['])|(['] +)|([']+$)", " ", str)
    # convert ellipses
    str <- gsub("…", "...", str)
    # remove twitter smiley faces, hearts, etc.
    str <- gsub("[:;=][-']?[)(pPD|/]", " ", str)
    str <- gsub("[)(D][-']?[:;]", " ", str)
    str <- gsub("[()_,]", " ", str)
    # replacing arroba symbols, depending on context
    if(grepl("[@]", str)) {
        str <- gsub(" [@] ", " at ", str)
        str <- gsub("[@]([[:alpha:]]) ", "a\\1 ", str)
        str <- gsub("[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]{2,}@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*", 
                    "<EMAIL>", str, perl = T)
    }
    # remove other weird punctuation
    str <- gsub("^ø", "", str)
    # replace floating ampersand with "and"
    if(grepl("[&]", str)) {
        str <- gsub(" & ", " and ", str)
        str <- gsub("(&middot)|(&hellip)|(&dash)|(&mdash)|(&amp)|(&nbsp)", "", str)
        str <- gsub("&", " ", str)
    }
    # remove trailing or leading apostrophes
    str <- gsub("(?<![[:alnum:]<>])'|'(?![[:alnum:]<>])", "", str, perl = T)
    # replace hashtags with <HT>
    if(grepl("[#]", str)) {
        str <- gsub("(^| )[#][']?s ", " numbers ", str)
        str <- gsub(" [#] of ", " number of ", str)
        str <- gsub("phone # ", "phone number ", str)
        str <- gsub("(^| )[#][[:alnum:]]+[[:space:]]?", " <HT> ", str)
        str <- gsub("[.][#][[:alnum:]]+", ". <HT>", str)
        str <- gsub(" [#][[:alnum:]]+$", " <HT>.", str)
    }
    # covert remove internal periods for abbreviations
    str <- gsub("U[.]S[.](A[.])?", "USA", str)
    str <- gsub("([A-Za-z]{1,2})[.]([A-Za-z]{1,2})[.](([A-Za-z]{1,2})[.])", "\\1\\2\\4", str)
    # replace all terminal punctuation with a period
    str <- gsub('[[:space:]]*[.?!:;]+[[:space:]]*', '. ', str)
    # remove square brackets
    str <- gsub("\\[(.*?)\\]", "\\1", str)
    
    # collapse whitespace
    str <- gsub('[[:space:]]+', ' ', str)
    
    # # remove whitespace and period combos
    str <- gsub('([.][[:space:]]+)+[.]', '.', str)
    str <- gsub("[[:space:]]+[.]", ".", str)
    # remove leading and trailing spaces
    str <- trimws(str)
    
    # str <- gsub("[<][/]?[3]+", " ", str);
    # str <- gsub("[.]{2,}$", ".", str);
    # str <- gsub("\"", ' ', str, perl = TRUE);
    # str <- gsub("[0oOT^-][_]+[0oOT^-]", " ", str);
    # str <- gsub("[oO^-][.][oO^-]", " ", str);

    # # remove "two singles quotes as double quotes"
    # str <- gsub("('')|(``)", "", str);
    # # remove period from abbreviated titles 
    # str <- gsub("([A-Z][a-zA-Z]{1,2})[.]","\\1", str);
    # # lower-case everything
    # str <- toupper(str);
    # # remove everything that isn't a letter or approved character
    # str <- gsub('[^A-Z\'.<>[:space:]]', ' ', str);
    # # make sure terminal . are tight
    # str <- gsub(' ?\\. ?', '.', str);
    # # put space back after period, for sentence detection
    # str <- gsub("[.]", ". ", str);
    # # remove floating apostrophes
    # str <- gsub(" ' ", " ", str);
    # # remove space after last sentence
    # str <- gsub(" $", "", str);

    return(str)
}
