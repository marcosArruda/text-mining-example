

## -----------------------------------------------------------------------------
# Configuracoes necessarias (executar cada uma dessas linhas no console)

#     > Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust","cluster", "igraph", "fpc", "readr")
#			> Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

#     IMPORTANTE: Quando perguntado, instalar usando o binary ao executar as duas linhas abaixo!
#     > install.packages(Needed, dependencies=TRUE)
#     > install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")


## ----------------------------------------------------------------------------





importData <- function(fullPathOfFiles = "~/prog-apps/workspaces/workspace-R/courseraR/courseraR/texts"){
	library(readr)
	platform <- R.Version()$platform
	if(grepl("apple",platform)){
		Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
	}else {
		Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
	}
	cname = file.path(fullPathOfFiles)
	## cname = file.path("/Users/marcosarruda","prog-apps", "workspaces","workspace-R","courseraR","courseraR","texts")
	files <- list.files(cname, pattern = "txt$", full.names = TRUE )
	data <- vector("character")
	data <- enc2utf8(data)
	es <- locale("es", decimal_mark = ",", encoding = "UTF-8")
	for(i in 1:length(files)){
		data[i] <- read_file(files[i], es)
	}
	data
}

pre_process <- function(vectorOfTexts){
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
}
