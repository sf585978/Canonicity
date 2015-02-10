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

cname <- file.path("corpus/canon")

canon <- Corpus(DirSource(cname))

##### Preparing Our Canon Corpus #####

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

canon <- tm_map(canon, toSpace, "/|@|\\|")
canon <- tm_map(canon, content_transformer(tolower)) # set to lowercase
canon <- tm_map(canon, removeNumbers) # remove numbers
canon <- tm_map(canon, removePunctuation) # remove punctuation
canon <- tm_map(canon, removeWords, stopwords("English")) 
canon <- tm_map(canon, stripWhitespace) # remove whitespace
canon <- tm_map(canon, stemDocument) # remove common endings

##### Canon Document Term Matrix #####

canonDTM <- DocumentTermMatrix(canon) # create a document term matrix
freq <- colSums(as.matrix(canonDTM)) # word frequencies
ord <- order(freq) # ordered least to greatest
canonDTM.2 <- removeSparseTerms(canonDTM, 0.1) # remove sparse terms


