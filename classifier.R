#### simple but working Classifier
#### DE / EN

setwd("/wd/r_wd")
system("mkdir langdetect")
setwd("/wd/r_wd/langdetect")
getwd()

# starte clean
rm(list = ls())

# Corpora laden
load("dewak100k_lsa.rda")
load("EN_100k_lsa.rda")
corpus.de <- dewak100k_lsa
corpus.en <- EN_100k_lsa

# Lade libraries
for (package in c("tm", "LSAfun")) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}


##### Bereinige Corpora von Redundanzen, bspw. Wörter die in De & En gleich sind (zB "hand") um Fehler zu vermeiden
corpus.de_clean <- setdiff(rownames(corpus.de), rownames(corpus.en))

corpus.en_clean <- setdiff(rownames(corpus.en), rownames(corpus.de))

#################################################
####################Lade je 3 Bücher auf EN & DE#
# english books##################################
#################################################
download.file("http://ebooks.hyperion.bz/api/get/5866d833164c57711f6d09e1/txt", 
  "rowling_en.txt", mode = "wb")
txt_1_en <- read.csv2("rowling_en.txt", stringsAsFactors = FALSE, 
  header = FALSE, strip.white = TRUE)

download.file("http://ebooks.hyperion.bz/api/get/5871f26a164c571591beeb57/txt", 
  "2_en.txt", mode = "wb")
txt_2_en <- read.csv2("2_en.txt", stringsAsFactors = FALSE, header = FALSE, 
  strip.white = TRUE)

download.file("http://ebooks.hyperion.bz/api/get/5866d990164c57711f6df739/txt", 
  "3_en.txt", mode = "wb")
txt_3_en <- read.csv2("3_en.txt", stringsAsFactors = FALSE, header = FALSE, 
  strip.white = TRUE)


###################################
# german books#####################
##################################
download.file("http://ebooks.hyperion.bz/api/get/5866d6ce164c57711f6c0fbd/txt", 
  "1_de.txt", mode = "wb")
txt_1_de <- read.csv2("1_de.txt", stringsAsFactors = FALSE, header = FALSE, 
  strip.white = TRUE)

download.file("http://ebooks.hyperion.bz/api/get/5866d7bc164c57711f6cb623/txt", 
  "2_de.txt", mode = "wb")
txt_2_de <- read.csv2("2_de.txt", stringsAsFactors = FALSE, header = FALSE, 
  strip.white = TRUE)

download.file("http://ebooks.hyperion.bz/api/get/5866d78a164c57711f6c925f/txt", 
  "3_de.txt", mode = "wb")
txt_3_de <- read.csv2("3_de.txt", stringsAsFactors = FALSE, header = FALSE, 
  strip.white = TRUE)


###################################
########Classifier#################
###################################
classify_lang <- function(txt) {
  # sample Satz > n Wörter
  wordcount <- 1
  while (wordcount < 26) {
    book.length <- 1:dim(txt)[1]
    sample.num <- sample(book.length, 1)  #ziehe zufällige Zeile
    sample.row <<- txt[sample.num, ]
    # zähle Wörter in Zeile
    count <- function(x) {
      length(unlist(strsplit(as.character(x), "\\W+")))
    }
    wordcount <- count(sample.row)
  }
  
  
  words.per.row <<- unlist(strsplit(as.character(sample.row), "\\W+"))
  words.per.row <<- tolower(words.per.row)
  
  words_corpus <- Corpus(VectorSource(words.per.row))
  
  
  dtm <- DocumentTermMatrix(words_corpus, control = list(removePunctuation = TRUE, 
    stopwords = (c("english", "german")), stemming = (c("english", 
      "german")), removeNumbers = TRUE))
  
  for_german <- intersect(dtm$dimnames$Terms, corpus.de_clean)
  for_english <- intersect(dtm$dimnames$Terms, corpus.en_clean)
  
  ratio <- function(de, en) {
    ratio.de <<- length(de)/length(words.per.row)
    ratio.en <<- length(en)/length(words.per.row)
    
    if (ratio.de == Inf) {
      ratio.de <- 0
    } else if (ratio.en == Inf) {
      ratio.en <- 0
    }
    
    if (ratio.en > ratio.de) {
      outcome <<- "english"
    } else {
      outcome <<- "deutsch"
    }
  }
  ratio(for_german, for_english)
  
  cat("Label = ", outcome, " ", "\n")
  label <<- outcome
}


####runtime
timestamp <- proc.time()
classify_lang(txt_1_de)
(runtime <- proc.time() - timestamp)


###################################
###erste Diagnose über n samples:##
###################################
#für je 3 Bücher pro Sprache 3mal (einzelnen Satz) gesampled

texts <- list(txt_1_de, txt_2_de, txt_3_de, txt_1_en, txt_2_en, txt_3_en)

count <- 0
row <- c()
diagnose <- c()
while (count <= 3) {
  count <- count + 1
  for (txt in seq_along(texts)) {
    classify_lang(texts[[txt]])
    if (label == "deutsch") {
      row <- c(label, "deutsch")
      diagnose <- rbind(diagnose, row)
    } else {
      row <- c(label, "english")
      diagnose <- rbind(diagnose, row)
    }
    rm(label)
  }
  
}

#View(diagnose)

right_classifications <- as.integer(sum(diagnose[, 1] == diagnose[, 2]))
performance <- (length(diagnose)/right_classifications/2)

cat("Performance der Klassifikation: ", (performance*100), "% richtig gelabelt")
