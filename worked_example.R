require(readr)

scripts <- list("\\p{L}", "\\p{Ll}", "\\p{Lu}", "\\p{Lt}", "\\p{L&}",
                "\\p{Lm}", "\\p{Lo}", "\\p{M}", "\\p{Mn}", "\\p{Mc}",
                "\\p{Me}", "\\p{Z}", "\\p{Zs}", "\\p{Zl}", "\\p{Zp}",
                "\\p{S}", "\\p{Sm}", "\\p{Sc}", "\\p{Sk}", "\\p{So}",
                "\\p{N}", "\\p{Nd}", "\\p{Nl}", "\\p{No}", "\\p{P}",
                "\\p{Pd}", "\\p{Ps}", "\\p{Pe}", "\\p{Pi}", "\\p{Pf}",
                "\\p{Pc}", "\\p{Po}", "\\p{C}", "\\p{Cc}", "\\p{Cf}",
                "\\p{Co}", "\\p{Cs}", "\\p{Cn}",
                "\\p{Common}", "\\p{Arabic}", "\\p{Armenian}",
                "\\p{Bengali}", "\\p{Bopomofo}", "\\p{Braille}",
                "\\p{Buhid}", "\\p{Canadian_Aboriginal}", "\\p{Cherokee}",
                "\\p{Cyrillic}", "\\p{Devanagari}", "\\p{Ethiopic}", 
                "\\p{Georgian}", "\\p{Greek}", "\\p{Gujarati}", 
                "\\p{Gurmukhi}", "\\p{Han}", "\\p{Hangul}", "\\p{Hanunoo}", 
                "\\p{Hebrew}", "\\p{Hiragana}", "\\p{Inherited}", 
                "\\p{Kannada}", "\\p{Katakana}", "\\p{Khmer}", "\\p{Lao}", 
                "\\p{Latin}", "\\p{Limbu}", "\\p{Malayalam}", 
                "\\p{Mongolian}", "\\p{Myanmar}", "\\p{Ogham}", 
                "\\p{Oriya}", "\\p{Runic}", "\\p{Sinhala}", "\\p{Syriac}", 
                "\\p{Tagalog}", "\\p{Tagbanwa}", "\\p{Tamil}", 
                "\\p{Telugu}", "\\p{Thaana}", "\\p{Thai}", "\\p{Tibetan}", 
                "\\p{Yi}")

# load each source using readr::read_lines (because it handles encodings best)
blogs <- read_lines("./final/en_US/en_US.blogs.txt")
news <- read_lines("./final/en_US/en_US.news.txt")
twitter <- read_lines("./final/en_US/en_US.twitter.txt")
all_src <- c(news, blogs, twitter)
rm(blogs, news, twitter)

# count occurances of each unicode group for blogs
require(data.table)
setDTthreads(threads = 0)
dt <- data.table("script" = scripts, "count" = NA)
i = 1
pb <- txtProgressBar(min = 1, max = 82, style = 3)
while(i < 83) {
    dt[[i,2]] <- sum(grepl(dt[[i,1]], all_src, perl = T))
    setTxtProgressBar(pb, i <- i + 1)
}
close(pb)
beep(sound = 4)

View(dt)

# Remove modifier letters {Lm}, letters without upper or lower cases {Lo},  
# Character marks {M}, Symbols {S}, Numbers {N} and control characters {C}
all_src <- all_src[!grepl("\\p{Lm}|\\p{Lo}|\\p{M}|\\p{S}|\\p{N}|\\p{C}", all_src, perl = T)]

# Remove language scripts that weren't latin or common
all_src <- all_src[!grepl("\\p{Arabic}|\\p{Bengali}|\\p{Cherokee}|\\p{Cyrillic}|\\p{Devanagari}|\\p{Ethiopic}|\\p{Georgian}|\\p{Greek}|\\p{Han}|\\p{Hangul}|\\p{Hebrew}|\\p{Hiragana}|\\p{Inherited}|\\p{Kannada}|\\p{Katakana}|\\p{Myanmar}|\\p{Tamil}|\\p{Thai}", all_src, perl = T)]
# 3177852 remaining

rm(dt, pb, scripts, i)

if(!dir.exists("./data")) {
    dir.create("./data")
}

saveRDS(all_src, "./data/all_src.rds")

source("./functions/clean_wrapper.R")
source("./functions/clean2.R")

all_src_clean <- clean_wrapper(all_src)
beep(sound = 4)

all_src_samp <- all_src[1:100000]
system.time(clean_wrapper(all_src_samp))
# user  system elapsed 
# 65.05    0.12   65.38 

system.time(sapply(all_src_samp, clean2))
# user  system elapsed 
# 65.03    0.02   65.26 

## data_cleaning.R, final_report.rmd, Project Outline 2019-08-02.R, n_gram_generation.R, 2019-08-10 Progress.R, 2019-08-11 Progress.R, 2019-08-12 Progress.R

saveRDS(all_src_clean, "./data/all_src_clean.rds")
rm(all_src)

all_src2 <- gsub("[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz,.' <>-]",
                 "", all_src_clean)
all_src2 <- all_src2[all_src2 != ""]
all_src3 <- unlist(strsplit(paste0(all_src2, collapse = ""), ""))
all_src3 <- data.table("chars" = all_src3)
all_src3 <- all_src3[, .N, by = chars][order(-N)]

# I initially didn't include the below indicated lines in clean2. This code let
# just apply those cleaning processes to an already cleaned dataset.
#
# len <- length(all_src_clean)
# i = 1
# pb <- txtProgressBar(min = 1, max = len, initial = i, style = 3)
# while(i <= len) {
#     charVect <- all_src_clean[[i]]
#     # collapse whitespace
#     charVect <- gsub('[[:space:]]+', ' ', charVect)
#     
#     # # remove whitespace and period combos
#     charVect <- gsub('([.][[:space:]]+)+[.]', '.', charVect)
#     charVect <- gsub("[[:space:]]+[.]", ".", charVect)
#     # remove leading and trailing spaces
#     charVect <- trimws(charVect)
#     all_src_clean[[i]] <- charVect
#     setTxtProgressBar(pb, i)
#     i <- i + 1
# }
# close(pb)
# rm(charVect, i, pb, len)

View(all_src3)
# This represents all of the non-alphanumeric english characters that exist in
# the documents.

# Fix regex special character nature of * and \
all_src3[chars == "\\", 1] <- "\\\\"
all_src3[chars == "*", 1] <- "\\*"
all_src3[chars == "{", 1] <- "\\{"
all_src3[chars == "[", 1] <- "\\["

# Check how many lines each character is in (mostly to check that the next
# script doesn't accidentally swallow extra lines)

i <- 1
pb <- txtProgressBar(min = 1, max = length(all_src3$chars), initial = i, style = 3)
while(i <= length(all_src3$chars)) {
    all_src3[[i,2]] <- sum(grepl(all_src3[[i,1]], all_src_clean[1:10000]))
    setTxtProgressBar(pb, i)
    i <- i + 1
}
close(pb)
rm(i, pb)

# Everything looks good, so make single search string
bad_list <- paste0(all_src3$chars, collapse = "|")

# Count of bad lines should be >50,000
sum(grepl(bad_list, all_src_clean))
# Found 43259...looks good

# Remove all lines with hits in the "bad_list"
all_src_clean <- all_src_clean[!grepl(bad_list, all_src_clean)]

saveRDS(all_src_clean, "./data/all_src_clean.rds")

# clean up
rm(all_src3, all_src_samp, all_src2, bad_list)
gc()

# break the training data into training, test and validation sets
set.seed(321123)
test <- c(runif(length(all_src_clean)) > .05)

all_test <- all_src_clean[test]  # selects testing subset
all_training <- all_src_clean[!test]  # selects training subset

saveRDS(all_test, "./data/all_src_testing.rds")
rm(all_test, test)
set.seed(123321)
valid <- c(runif(length(all_training)) > .9)

all_valid <- all_training[valid]
all_src <- all_training[!valid]
rm(all_training, valid)

saveRDS(all_valid, "./data/all_src_validation.rds")
saveRDS(all_src, "./data/all_src_training.rds")
rm(all_valid, all_src_clean)
gc()
# 140,802 lines in training data

# all_src <- tolower(all_src)
# 
# # remove periods from titles
# all_src <- gsub("((^| )(st|ms|mr|mrs|dr|rev|jr|sr))[.]", "\\1", all_src)
# 
# # remove non-internal dashes
# sum(grepl("(^[-]+)|( +[-])|([-] +)|([-]+$)", all_src))
# all_src <- gsub("(^[-]+)|( +[-])|([-] +)|([-]+$)", " ", all_src)
# sum(grepl('(^| )[-]( |$)', all_src))
# all_src <- gsub('(^| )[-]( |$)', " ", all_src)
# 
# # remove period from abbreviated titles
# sum(grepl("(^| )[[:alnum:]][.]( )?[[:alnum:]][.]", all_src))
# all_src <- gsub("(^| )([[:alnum:]])[.]( )?([[:alnum:]])[.]", "\\1\\2\\4", all_src)
# 
# # removes double dashes
# all_src <- gsub("--", " ", all_src)
# all_src <- gsub(' ?[.] [.] ?', '. ', all_src)
# 
# all_src <- gsub('[[:space:]]+', ' ', all_src)
# all_src <- trimws(all_src)

all_src <- readRDS("./data/all_src_training.rds")

source("./functions/clean3.R")
i <- 1
pb <- txtProgressBar(min = 1, max = length(all_src), initial = i, style = 3)
while(i <= length(all_src)) {
    all_src[i] <- clean3(all_src[i])
    setTxtProgressBar(pb, i)
    i <- i + 1
}
close(pb)
rm(i, pb)

# These were fixed in clean3()
# all_src[grepl("(?<=\\.)\\s(?=[^a-z<])", all_src, perl = T)]
# all_src <- gsub("[.] -", ". ", all_src)
# all_src <- gsub(" ?[.] [.] ?", ". ", all_src)
source("./functions/split_sentence.R")

all_sent <- split_sentence(all_src)
rm(all_src)

saveRDS(all_sent, "./data/all_sent.rds")
beep(sound = 4)

if(!dir.exists("./data/frags")) {
    dir.create("./data/frags")
}

sent_count <- length(all_sent)
frag_01 <- all_sent[1:floor(sent_count * 0.1)]
saveRDS(frag_01, "./data/frags/frag_01.rds")
rm(frag_01)

frag_02 <- all_sent[(floor(sent_count * 0.1) + 1):floor(sent_count * 0.2)]
saveRDS(frag_02, "./data/frags/frag_02.rds")
rm(frag_02)

frag_03 <- all_sent[(floor(sent_count * 0.2) + 1):floor(sent_count * 0.3)]
saveRDS(frag_03, "./data/frags/frag_03.rds")
rm(frag_03)

frag_04 <- all_sent[(floor(sent_count * 0.3) + 1):floor(sent_count * 0.4)]
saveRDS(frag_04, "./data/frags/frag_04.rds")
rm(frag_04)

frag_05 <- all_sent[(floor(sent_count * 0.4) + 1):floor(sent_count * 0.5)]
saveRDS(frag_05, "./data/frags/frag_05.rds")
rm(frag_05)

frag_06 <- all_sent[(floor(sent_count * 0.5) + 1):floor(sent_count * 0.6)]
saveRDS(frag_06, "./data/frags/frag_06.rds")
rm(frag_06)

frag_07 <- all_sent[(floor(sent_count * 0.6) + 1):floor(sent_count * 0.7)]
saveRDS(frag_07, "./data/frags/frag_07.rds")
rm(frag_07)

frag_08 <- all_sent[(floor(sent_count * 0.7) + 1):floor(sent_count * 0.8)]
saveRDS(frag_08, "./data/frags/frag_08.rds")
rm(frag_08)

frag_09 <- all_sent[(floor(sent_count * 0.8) + 1):floor(sent_count * 0.9)]
saveRDS(frag_09, "./data/frags/frag_09.rds")
rm(frag_09)

frag_10 <- all_sent[(floor(sent_count * 0.9) + 1):sent_count]
saveRDS(frag_10, "./data/frags/frag_10.rds")
rm(frag_10, all_sent)
gc()

library(stringr)
source("./functions/sentence2words.R")
source("./functions/ngrGenerator.R")

# Generate NGram files for each fragment up to 6-grams
for (i in 1:10) {
    sentences <- readRDS(paste0("./data/frags/frag_", str_pad(i, 2, side = "left", pad = "0"), ".rds"))
    words <- sentence2words(sentences)
    for (j in 1:6) {
        if (j == 1) {
            saveRDS(unlist(words), paste0("./data/frags/frag_",
                                          str_pad(i, 2, side = "left", pad = "0"),
                                          "_",
                                          j,
                                          "gram.rds"))
            cat(paste0("\n", j, "-gram for fragment ", i, "\n"))
        } else {
            saveRDS(ngrGenerator(words, j), paste0("./data/frags/frag_",
                                                   str_pad(i, 2, side = "left", pad = "0"),
                                                   "_",
                                                   j,
                                                   "gram.rds"))
            cat(paste0("\n", j, "-gram for fragment ", i, "\n"))
        }
    }
}
beep(sound = 4)

for (j in 1:6) {
    frag_01 <- readRDS(paste0("./data/frags/frag_01_", j, "gram.rds"))
    frag_02 <- readRDS(paste0("./data/frags/frag_02_", j, "gram.rds"))
    frag_03 <- readRDS(paste0("./data/frags/frag_03_", j, "gram.rds"))
    frag_04 <- readRDS(paste0("./data/frags/frag_04_", j, "gram.rds"))
    frag_05 <- readRDS(paste0("./data/frags/frag_05_", j, "gram.rds"))
    frag_06 <- readRDS(paste0("./data/frags/frag_06_", j, "gram.rds"))
    frag_07 <- readRDS(paste0("./data/frags/frag_07_", j, "gram.rds"))
    frag_08 <- readRDS(paste0("./data/frags/frag_08_", j, "gram.rds"))
    frag_09 <- readRDS(paste0("./data/frags/frag_09_", j, "gram.rds"))
    frag_10 <- readRDS(paste0("./data/frags/frag_10_", j, "gram.rds"))
    
    ngram <- c(frag_01, frag_02, frag_03, frag_04, frag_05, frag_06, frag_07, frag_08, frag_09, frag_10)
    ngram <- data.table("ngram" = ngram, key = "ngram")[, .N, by = ngram][order(-N)]
    setnames(ngram, c("feature", "r"))
    saveRDS(ngram, paste0("./data/all_",j,"gram.rds"))
    cat(paste0("all_", j, "gram.rds has been saved. \n"))
}

rm(ngram, words, frag_01, frag_02, frag_03, frag_04, frag_05, frag_06, frag_07, frag_08, frag_09, frag_10, i, j, sent_count, sentences)

if(!dir.exists("./data/kn")) {
    dir.create("./data/kn")
}

saveRDS(kneserney(readRDS("./data/all_2gram.rds"), 2), "./data/kn/2gram.rds")
saveRDS(kneserney(readRDS("./data/all_3gram.rds"), 3), "./data/kn/3gram.rds")
saveRDS(kneserney(readRDS("./data/all_4gram.rds"), 4), "./data/kn/4gram.rds")
saveRDS(kneserney(readRDS("./data/all_5gram.rds"), 5), "./data/kn/5gram.rds")
saveRDS(kneserney(readRDS("./data/all_6gram.rds"), 6), "./data/kn/6gram.rds")