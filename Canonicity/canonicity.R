##### Loading Required Packages #####

require(tm)
require(SnowballC)
require(qdap)
require(qdapDictionaries)
require(dplyr)
require(RColorBrewer)
require(ggplot2)
require(scales)
require(Rgraphviz)

##### Load Canon Corpus Data #####

cname.1 <- file.path("corpus/canon")

canon <- Corpus(DirSource(cname.1))

##### Load Non-Canon Corpus Data #####

cname.2 <- file.path("corpus/noncanon/")

noncanon <- Corpus(DirSource(cname.2))

##### Preparing Our Canon Corpus #####

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

canon <- tm_map(canon, toSpace, "/|@|\\|")
canon <- tm_map(canon, content_transformer(tolower)) # set to lowercase
canon <- tm_map(canon, removeNumbers) # remove numbers
canon <- tm_map(canon, removePunctuation) # remove punctuation
canon <- tm_map(canon, removeWords, stopwords("English")) 
canon <- tm_map(canon, stripWhitespace) # remove whitespace
canon <- tm_map(canon, stemDocument) # remove common endings

##### Preparing Our Non-Canon Corpus #####

noncanon <- tm_map(noncanon, toSpace, "/|@|\\|")
noncanon <- tm_map(noncanon, content_transformer(tolower)) # set to lowercase
noncanon <- tm_map(noncanon, removeNumbers) # remove numbers
noncanon <- tm_map(noncanon, removePunctuation) # remove punctuation
noncanon <- tm_map(noncanon, removeWords, stopwords("English")) 
noncanon <- tm_map(noncanon, stripWhitespace) # remove whitespace
noncanon <- tm_map(noncanon, stemDocument) # remove common endings

##### Canon Document Term Matrix #####

canonDTM <- DocumentTermMatrix(canon) # create a document term matrix
freq.1 <- colSums(as.matrix(canonDTM)) # word frequencies
ord.1 <- order(freq) # ordered least to greatest
canonDTM.2 <- removeSparseTerms(canonDTM, 0.1) # remove sparse terms

##### Non-Canon Document Term Matrix #####

noncanonDTM <- DocumentTermMatrix(noncanon) # create a document term matrix
freq.2 <- colSums(as.matrix(canonDTM)) # word frequencies
ord.2 <- order(freq) # ordered least to greatest
noncanonDTM.2 <- removeSparseTerms(noncanonDTM, 0.1) # remove sparse terms

##### Quantitative Analysis Preparation #####

canonWords <- canonDTM.2 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) <20])

noncanonWords <- noncanonDTM.2 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) <20])

##### Word Length #####

data.frame(nletters.1 = nchar(canonWords)) %>%
  ggplot(aes(x = nletters.1)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = mean(nchar(canonWords)), colour = "green", size = 1,
             alpha = .5) +
  labs(x = "Number of Letters", y = "Number of Words")

data.frame(nletters.2 = nchar(noncanonWords)) %>%
  ggplot(aes(x = nletters.2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = mean(nchar(noncanonWords)), colour = "green", size = 1,
             alpha = .5) +
  labs(x = "Number of Letters", y = "Number of Words")

shapiro.test(nchar(canonWords))
shapiro.test(nchar(noncanonWords))

wilcox.test(x = nchar(canonWords), y = nchar(noncanonWords))

##### Word Length of Individual Texts #####

canon.data <- read.csv("canon_corpus_by_year.csv")

for (i in 1:length(canon.data$path)) {
  year <- Corpus(DirSource(as.character(canon.data$path[i])))
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  year <- tm_map(year, toSpace, "/|@|\\|")
  year <- tm_map(year, content_transformer(tolower)) # set to lowercase
  year <- tm_map(year, removeNumbers) # remove numbers
  year <- tm_map(year, removePunctuation) # remove punctuation
  year <- tm_map(year, removeWords, stopwords("English")) 
  year <- tm_map(year, stripWhitespace) # remove whitespace
  year <- tm_map(year, stemDocument) # remove common endings
  
  yearDTM <- DocumentTermMatrix(year) # create a document term matrix
  freq.1 <- colSums(as.matrix(yearDTM)) # word frequencies
  ord.1 <- order(freq) # ordered least to greatest
  yearDTM.2 <- removeSparseTerms(yearDTM, 0.1) # remove sparse terms
  
  year.words <- yearDTM.2 %>%
    as.matrix %>%
    colnames %>%
    (function(x) x[nchar(x) <20])
  
  canon.data$word.length.mean[i] <- mean(nchar(year.words))
}

