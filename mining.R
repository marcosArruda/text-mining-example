

## -----------------------------------------------------------------------------
# Configuracoes necessarias (executar cada uma dessas linhas no console)

#     > Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust","cluster", "igraph", "fpc", "readr", "slam")
#			> Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

#     IMPORTANTE: Quando perguntado, instalar usando o binary ao executar as duas linhas abaixo!
#     > install.packages(Needed, dependencies=TRUE)
#     > install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")


## ----------------------------------------------------------------------------





importData <- function(fullPathOfFiles = "texts"){
	library(readr)
	platform <- R.Version()$platform
	if(grepl("apple",platform)){
		Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
	}else {
		Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
	}
	cname <- file.path(fullPathOfFiles)
	files <- list.files(cname, pattern = "txt$", full.names = TRUE )
	data <- enc2utf8(vector("character"))
	es <- locale("es", decimal_mark = ",", encoding = "UTF-8")
	for(i in 1:length(files)){
		data[i] <- read_file(files[i], es)
	}
	data
}

preProcess <- function(vectorOfTexts = NULL){
	if(is.null(vectorOfTexts)){
		vectorOfTexts <- importData();
	}
	library(tm)
	docs <- Corpus(VectorSource(vectorOfTexts))
	docs <- tm_map(docs, removePunctuation) # *Removing punctuation:*
	docs <- tm_map(docs, removeNumbers) # *Removing numbers:*
	docs <- tm_map(docs, tolower) # *Converting to lowercase:*
	docs <- tm_map(docs, removeWords, stopwords("portuguese")) # *Removing "stopwords"
	library(SnowballC)
	docs <- tm_map(docs, stemDocument, language = "portuguese")   # *Removing common word endings* (e.g., "ing", "es")
	docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace
	docs <- tm_map(docs, PlainTextDocument)
	## *This is the end of the preprocessing stage.*
	docs
}

stageData <- function(corpus = NULL){
  if(is.null(corpus)){
    corpus <- preProcess()
  }
  ### Stage the Data      
  dtm <- DocumentTermMatrix(corpus)
  tdm <- TermDocumentMatrix(corpus)
  list(dtm = dtm, tdm = tdm)
}

frequencyOfWordFull <- function(dtm = NULL){
  if(is.null(dtm)){
    dtm <- stageData(preProcess())$dtm
  }
  colSums(as.matrix(dtm))
}

getDTMS <- function(dtm = NULL){
  if(is.null(dtm)){
    dtm <- stageData(preProcess())$dtm
  }
  dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
  dtms
}

wordFrequencyFiltered <- function(dtm = NULL){
  if(is.null(dtm)){
    dtms <- getDTMS()
    freq <- frequencyOfWordFull()
  }else{
    dtms <- getDTMS(dtm)
    freq <- frequencyOfWordFull(dtm)
  }
  #head(table(freq), 20)
  tail(table(freq), 20)
}

wordFrequencyByWord <- function(dtms = NULL){
  if(!is.null(dtms)){
    freq <- colSums(as.matrix(dtms))
    freq
  }else{
    dtms <- getDTMS()
    freq <- colSums(as.matrix(dtms))
    freq
  }
}

getFreqTerms <- function(dtm = NULL, lowfreq=50){
  if(is.null(dtm)){
    dtm <- stageData()$dtm
  }
  findFreqTerms(dtm, lowfreq)
}

plotWordFrequencies <- function(freq = NULL){
  library(ggplot2)
  if(is.null(freq)){
    freq <- wordFrequencyByWord()
  }
  wf <- data.frame(word=names(freq), freq=freq)   
  p <- ggplot(subset(wf, freq>50), aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p
}

findAssociativeCorrelations <- function(dtm = NULL, words = c("recurso", "trabalho"), corlimit=0.79){
  if(is.null(dtm)){
    dtm <- stageData()$dtm
  }
  findAssocs(dtm, words, corlimit=0.79)
}

plotWordCloud <- function(dtm = NULL, percOfEmptySpaces = 0.15, max.words=100, rot.per=0.2){
  if(is.null(dtm)){
    dtm <- stageData()$dtm
  }
  library(wordcloud)
  dtms <- removeSparseTerms(dtm, percOfEmptySpaces) # Prepare the data (max 15% empty space)   
  freq <- colSums(as.matrix(dtm)) # Find word frequencies   
  dark2 <- brewer.pal(6, "Dark2")   
  wordcloud(names(freq), freq, max.words=max.words, rot.per=0.2, colors=dark2)    
}

plotClusterByTermSimilarity <- function(dtm = NULL){
  if(is.null(dtm)){
    dtm <- stageData()$dtm
  }
  ### Hierarchal Clustering   
  dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
  library(cluster)   
  d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
  fit <- hclust(d=d, method="ward")   
  plot.new()
  plot(fit, hang=-1)
  groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
  rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters
}

plotKmeansClustering <- function(dtm = NULL){
  if(is.null(dtm)){
    dtm <- stageData()$dtm
  }
  ### K-means clustering   
  library(fpc)   
  library(cluster)  
  dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
  d <- dist(t(dtms), method="euclidian")   
  kfit <- kmeans(d, 2)   
  clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
}