saveRDS(all_src, "./seven/all_src_presplit.rds")

all_sent <- split_sentence(readRDS("./seven/all_src_presplit.rds"))
rm(all_src)
rm(pb, pb2, pb3)

saveRDS(all_sent, "./all_sent.rds")

all_sent[1:20]

sent_count <- length(all_sent)
frag_01 <- all_sent[1:floor(sent_count * 0.1)]
saveRDS(frag_01, "./frags/frag_01.rds")
rm(frag_01)

frag_02 <- all_sent[(floor(sent_count * 0.1) + 1):floor(sent_count * 0.2)]
saveRDS(frag_02, "./frags/frag_02.rds")
rm(frag_02)

frag_03 <- all_sent[(floor(sent_count * 0.2) + 1):floor(sent_count * 0.3)]
saveRDS(frag_03, "./frags/frag_03.rds")
rm(frag_03)

frag_04 <- all_sent[(floor(sent_count * 0.3) + 1):floor(sent_count * 0.4)]
saveRDS(frag_04, "./frags/frag_04.rds")
rm(frag_04)

frag_05 <- all_sent[(floor(sent_count * 0.4) + 1):floor(sent_count * 0.5)]
saveRDS(frag_05, "./frags/frag_05.rds")
rm(frag_05)

frag_06 <- all_sent[(floor(sent_count * 0.5) + 1):floor(sent_count * 0.6)]
saveRDS(frag_06, "./frags/frag_06.rds")
rm(frag_06)

frag_07 <- all_sent[(floor(sent_count * 0.6) + 1):floor(sent_count * 0.7)]
saveRDS(frag_07, "./frags/frag_07.rds")
rm(frag_07)

frag_08 <- all_sent[(floor(sent_count * 0.7) + 1):floor(sent_count * 0.8)]
saveRDS(frag_08, "./frags/frag_08.rds")
rm(frag_08)

frag_09 <- all_sent[(floor(sent_count * 0.8) + 1):floor(sent_count * 0.9)]
saveRDS(frag_09, "./frags/frag_09.rds")
rm(frag_09)

frag_10 <- all_sent[(floor(sent_count * 0.9) + 1):sent_count]
saveRDS(frag_10, "./frags/frag_10.rds")
rm(frag_10, all_sent)
gc()

library(stringr)

all_sent <- readRDS("./seven/all_sent.rds")
all_sent <- trimws(gsub("[[:space:]]+", " ", all_sent))

library(data.table)
setDTthreads(threads = 0)

all_words <- sentence2words(all_sent)
saveRDS(all_words, "./seven/all_words.rds")

for (i in 1:10) {
    sentences <- readRDS(paste0("./frags/frag_", str_pad(i, 2, side = "left", pad = "0"), ".rds"))
    words <- sentence2words(sentences)
    for (j in 1:6) {
        if (j == 1) {
            saveRDS(unlist(words), paste0("./frags/frag_",
                                          str_pad(i, 2, side = "left", pad = "0"),
                                          "_",
                                          j,
                                          "gram.rds"))
            cat(paste("j is", j, "and i is", i, "\n"))
        } else {
            saveRDS(ngrGenerator(words, j), paste0("./frags/frag_",
                                                   str_pad(i, 2, side = "left", pad = "0"),
                                                   "_",
                                                   j,
                                                   "gram.rds"))
            cat(paste("j is", j, "and i is", i, "\n"))
        }
    }
}


for (j in 1:6) {
    frag_01 <- readRDS(paste0("./frags/frag_01_", j, "gram.rds"))
    frag_02 <- readRDS(paste0("./frags/frag_02_", j, "gram.rds"))
    frag_03 <- readRDS(paste0("./frags/frag_03_", j, "gram.rds"))
    frag_04 <- readRDS(paste0("./frags/frag_04_", j, "gram.rds"))
    frag_05 <- readRDS(paste0("./frags/frag_05_", j, "gram.rds"))
    frag_06 <- readRDS(paste0("./frags/frag_06_", j, "gram.rds"))
    frag_07 <- readRDS(paste0("./frags/frag_07_", j, "gram.rds"))
    frag_08 <- readRDS(paste0("./frags/frag_08_", j, "gram.rds"))
    frag_09 <- readRDS(paste0("./frags/frag_09_", j, "gram.rds"))
    frag_10 <- readRDS(paste0("./frags/frag_10_", j, "gram.rds"))
    
    ngram <- c(frag_01, frag_02, frag_03, frag_04, frag_05, frag_06, frag_07, frag_08, frag_09, frag_10)
    ngram <- data.table("ngram" = ngram, key = "ngram")[, .N, by = ngram][order(-N)]
    setnames(ngram, c("feature", "r"))
    saveRDS(ngram, paste0("./processed/all_",j,"gram.rds"))
    cat(paste0("all_", j, "gram.rds has been saved. \n"))
}

all_1g <- unlist(all_words)
all_1g_freq <- data.table("uni" = all_1g, key = "uni")[, .N, by = uni][order(-N)]
all_1g_freq[, pML := N/sum(N)]
saveRDS(all_1g_freq, "./processed/all_1g.rds")
rm(all_1g, all_1g_freq)
