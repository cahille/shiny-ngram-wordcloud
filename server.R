tools::dependsOnPkgs('tm')
library(shiny)
library(RCurl)
library(wordcloud)
library(tm)

contentEnv <- new.env(hash=T, parent=emptyenv())

getContent <- function(url) {
  if(is.null(contentEnv[[url]])) {
    thisContent <- getURLContent(url,.opts=curlOptions(followlocation=TRUE,cookiefile="nosuchfile"))
    thisContent <- gsub("\\s+", " ", thisContent, perl=TRUE)
    thisContent <- gsub("\n", " ", thisContent, perl=TRUE)
    thisContent <- gsub("<script.+?/script>", " ", thisContent, ignore.case=TRUE, perl=TRUE)
    thisContent <- gsub("<style.+?/style>", " ", thisContent, ignore.case=TRUE, perl=TRUE)
    thisContent <- gsub("<[^>]+>", " ", thisContent, perl=TRUE)
    thisContent <- gsub("&(?:nbsp|quot|lt|gt|amp);", " ", thisContent, perl=TRUE)
    thisContent <- gsub("[?.,!:;]", " ", thisContent, perl=TRUE)
    thisContent <- gsub("--", " ", thisContent, perl=TRUE)
    thisContent <- gsub("^\\s+|\\s+$", "", thisContent, perl=TRUE)

    thisContent <- tolower(thisContent)
    contentEnv[[url]] <<- thisContent
  }
  
  contentEnv[[url]]
}

getNgramDataFrame <- function(url, ngramLength, removeStopwords) {
  splitContent <- strsplit(getContent(url), " +")[[1]]
  
  if(removeStopwords == TRUE) {
    stopwordsEnv <- new.env(hash=T, parent=emptyenv())
    
    stopwordsList <- stopwords(kind = "en")
    
    for(stopword in stopwords(kind="en")) {
      stopwordsEnv[[stopword]] <- TRUE      
    }
      
    cleansedSplitContent <- c()
    for(word in splitContent) {
      if(is.null(stopwordsEnv[[word]])) {
        cleansedSplitContent <- c(cleansedSplitContent, word)
      }
    }
    
    splitContent <- cleansedSplitContent
  }
  
  counts <- new.env(hash=T, parent=emptyenv())
  
  for(i in 1:(length(splitContent)-ngramLength)) {
    thisNgram <- splitContent[i]
    if(ngramLength > 1) {
      for(j in ((i+1):(i+ngramLength-1))) {
        thisNgram <- paste0(thisNgram, " ", splitContent[j])
      }
    }
    
    if(nchar(thisNgram) > 12) {
      thisNgram <- gsub("^(.{10}).*", "\\1..", thisNgram, perl=TRUE)
    }
    
    if(is.null(counts[[thisNgram]])) {
      counts[[thisNgram]] <- 0
    }

    counts[[thisNgram]] <- counts[[thisNgram]] + 1
  }
  
  keys <- c()
  values <- c()
  
  for(key in ls(counts)) {
    value <- counts[[key]]
    
    keys <- c(keys, key)
    values <- c(values, value)
  }
  
  dataFrame <- data.frame(keys, values)
  names(dataFrame) <- c("ngram", "count")
  dataFrame <- dataFrame[order(-dataFrame$count), ]
  
  dataFrame
}

shinyServer(
  function(input, output) {
    ngramEvent <- eventReactive(input$myInput, {
      getNgramDataFrame(url=input$url, ngramLength=as.integer(input$ngramLength), removeStopwords=input$removeStopwords)
    })

    
    output$ngramcloud <- renderPlot(({
      ngramDataFrame <- ngramEvent()
      
      wordcloud(words=ngramDataFrame$ngram, freq=ngramDataFrame$count, min.freq=as.integer(input$minNgramFreq), max.words=as.integer(input$maxNgrams),  colors=brewer.pal(6,"Dark2"))
      #wordcloud(ngramDataFrame$ngram, ngramDataFrame$count, scale=c(4,.5), min.freq=3, input$maxNgrams, colors=brewer.pal(6,"Dark2"))
    }))
  }      
)