# script takes a directory with text files and
# 1) creates a simple term-document-matrix (including export to CSV)
# 2) creates a bigram term-document matrix (including export to CSV)

library(tm)

# set the directory where to find the files
txt <- "PATH_TO_TEXT_FILES" # replace with path, no slash at the end

# read directory into corpus
tm_articles <- VCorpus(DirSource(txt, encoding = ""),
    readerControl = list(language = "en"))

# preformat corpus
tm_articles <- tm_map(tm_articles, stripWhitespace) #white spaces
tm_articles <- tm_map(tm_articles, content_transformer(tolower))  #lower case
tm_articles <- tm_map(tm_articles, removeWords, stopwords("english")) #stop words
tm_articles <- tm_map(tm_articles, removePunctuation, preserve_intra_word_dashes = FALSE) #regular punctuation
tm_articles <- tm_map(tm_articles, content_transformer(function(row) iconv(row, "latin1", "ASCII", sub=""))) # non-ascii chars
tm_articles <- tm_map(tm_articles, removeNumbers) # numbers

# create term document matrix with stemmed terms

tm_articles_stem <- tm_map(tm_articles, stemDocument) # stemming
tdm_stem <- TermDocumentMatrix(tm_articles_stem)
tdm_df <- as.matrix(tdm_stem)
write.csv(tdm_df, "tdm_df.csv")

# create term document matrix with unstemmed bigrams

# function for tokenization on bigrams
BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

tdm_bigram <- TermDocumentMatrix(tm_articles, control = list(tokenize = BigramTokenizer))
tdm_bigram_df <- as.matrix(tdm_bigram)
write.csv(tdm_bigram_df, "tdm_bigram_df.csv")


