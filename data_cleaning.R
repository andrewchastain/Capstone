require(readr)
require(data.table)
setDTthreads(threads = 0)

# generate list of unicode categories and scripts
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

# load blogs using readr::read_lines (because it handles encodings best)
blogs <- read_lines("./final/en_US/en_US.blogs.txt")

# count occurances of each unicode group for blogs
dt <- data.table("script" = scripts, "count" = NA)
i = 1
pb <- txtProgressBar(min = 1, max = 82, style = 3)
while(i < 83) {
    dt[[i,2]] <- sum(grepl(dt[[i,1]], blogs, perl = T))
    setTxtProgressBar(pb, i <- i + 1)
}
close(pb)

# based upon the above list, the following categories/scripts were filtered out
blogs <- blogs[!grepl("\\p{Arabic}|\\p{Bengali}|\\p{Cyrillic}|\\p{Devanagari}|\\p{Ethiopic}|\\p{Greek}|\\p{Han}|\\p{Hangul}|\\p{Hebrew}|\\p{Hiragana}|\\p{Inherited}|\\p{Tamil}|\\p{Thai}|\\p{Katakana}", blogs, perl = T)]
blogs <- blogs[!grepl("\\p{M}|\\p{S}|\\p{No}|\\p{Lo}|\\p{Lm}|\\p{Lo}|\\p{C}", blogs, perl = T)]
blogs <- blogs[!grepl("\\p{N}", blogs, perl = TRUE)]

# save out the filtered blogs
saveRDS(blogs, "./seven/blogs.rds")

# load news
news <- read_lines("./final/en_US/en_US.news.txt")

# count occurances of each unicode group for news
dt <- data.table("script" = scripts, "count" = NA)
i = 1
pb <- txtProgressBar(min = 1, max = 82, style = 3)
while(i < 83) {
    dt[[i,2]] <- sum(grepl(dt[[i,1]], news, perl = T))
    setTxtProgressBar(pb, i <- i + 1)
}
close(pb)

# filter news based on above categories
news <- news[!grepl("\\p{Lo}|\\p{M}|\\p{S}|\\p{N}|\\p{C}|\\p{Arabic}|\\p{Greek}|\\p{Han}|\\p{Inherited}", news, perl = T)]

# save filted news
saveRDS(news, "./seven/news.rds")

# load twitter data
twitter <- read_lines("./final/en_US/en_US.twitter.txt")

# count occurances of each unicode group for news
dt <- data.table("script" = scripts, "count" = NA)
i = 1
pb <- txtProgressBar(min = 1, max = 82, style = 3)
while(i < 83) {
    dt[[i,2]] <- sum(grepl(dt[[i,1]], twitter, perl = T))
    setTxtProgressBar(pb, i <- i + 1)
}
close(pb)
rm(pb)

# filter twitter based on above categories
twitter <- twitter[!grepl("\\p{Lm}|\\p{Lo}|\\p{M}|\\p{S}|\\p{N}|\\p{C}|\\p{Arabic}|\\p{Cherokee}|\\p{Cyrillic}|\\p{Devanagari}|\\p{Georgian}|\\p{Greek}|\\p{Han}|\\p{Hangul}|\\p{Hebrew}|\\p{Hiragana}|\\p{Inherited}|\\p{Kannada}|\\p{Katakana}|\\p{Myanmar}|\\p{Thai}", twitter, perl = T)]

# save filtered twitter data 
saveRDS(twitter, "./seven/twitter.rds")

# combine all three sources into one object
all_src <- c(news, blogs, twitter)

# if the files are large individually, they can also be pasted together as such:
# all_src <- readRDS("./seven/news.rds")
# all_src <- c(all_src, readRDS("./seven/blogs.rds"))
# all_src <- c(all_src, readRDS("./seven/twitter.rds"))

# use the clean_wrapper() function to call the function clean2() to clean weird
# characters and punctuation that persisted after filtering

all_src_clean <- clean_wrapper(all_src)

# break the training data into training, test and validation sets
set.seed(321123)
test <- c(runif(length(all_src_clean)) > .5)

all_test <- all_src_clean[test]
all_training <- all_src_clean[!test]

saveRDS(all_test, "./test/all_test.rds")
rm(all_test)
set.seed(123321)
valid <- c(runif(length(all_training)) > .5)

all_valid <- all_training[valid]
all_src <- all_training[!valid]
rm(all_training)

saveRDS(all_valid, "./valid/all_src.rds")
saveRDS(all_src, "./train/all_src.rds")
rm(all_valid)
gc()

## NEEDS THE FINAL CLEANING REFACTORED