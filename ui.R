library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("A little Ngram fun :)"),
  sidebarPanel(
    textInput(inputId="url", label="url - address of the page to analyze", value="http://shakespeare.mit.edu/romeo_juliet/full.html"),
    selectInput(inputId="ngramLength", label="Ngram Length", c(1, 2, 3, 4, 5), selected=2),
    selectInput(inputId="maxNgrams", label="Maximum Ngrams", c(5, 10, 20, 30, 40, 50), selected=50),
    selectInput(inputId="minNgramFreq", label="Minimum Ngram Frequency", c(1, 2, 3, 4, 5, 10), selected=3),
    checkboxInput(inputId="removeStopwords", label="Remove Stopwords", value=TRUE),
    actionButton(label="submit url for frequency analysis", inputId = "myInput"),
    p(""),
    p(""),
    p("Submit a url and the app will make a pretty cool wordcloud. Ngrams are N contiguous words. You could split a simple phrase like 'I love you, I love you, I love you' into Ngrams for different values of N and get"),
    p("1 => 'i' x 3, 'love' x 3 and 'you' x 3"),
    p("2 => 'i love' x 2, 'you I' x 1 and 'love you' x 1"),
    p("3 => 'i love you' x 3"),
    p("'Ngram Length' above is to set the values of N"),
    p("'Maximum Ngrams' is the most Ngrams that will appear in the wordcloud"),
    p("'Minimum Ngram Frequency' is a threshold for how many times the Ngram must be repeated to be displayed"),
    p("Click 'Remove Stopwords' to clear out common words like 'of' or 'the'")
  ),
  mainPanel(
    plotOutput("ngramcloud")
  )
))