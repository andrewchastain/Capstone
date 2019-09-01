################################################################################
# Next word prediction application. It works by searching for the last six     #
# words in the input string in a lookup database, and if it fails to find it,  #
# then it searches for the string with the first word removed and repeats      #
# until a word is found or it runs out of words. If no words are found, and    #
# the search string has zero length, then it returns "the," the most common    #
# word in the training data.                                                   #
################################################################################

library(shiny)
library(data.table)
library(stringr)

db <- readRDS("./db_reduced.rds")  # load database with single prediction per 
                                   # root, as determined by calculating the
                                   # Kneser-Ney probability

NWpred <- function(string) {
  require(stringr)
  require(data.table)
  setDTthreads(threads = 0)
  ## Take input string and select the last 6 words
  str <- regmatches(string,
                    regexpr("[[:graph:]]*([[:space:]]*[[:graph:]]*){5}$",
                            string))
  output <- character(0)  # Init output as empty character vector
  while(identical(output, character(0))) {  # run until output isn't empty
    # use data.table to search for root == str and return the terminal word
    output <- db[root == str][["term"]] 
    # chop off first word from str
    str <- gsub("^([[:graph:]]*[[:space:]]?)", "", str)
    if(identical(output, character(0))) {  # check if a terminal word was found
      # if the terminal word wasn't found, check if the root was the last word
      if(str_length(str) == 0) {
        output <- "the"  # if there are no words left, output "the"
      }
    }
  }
  return(output)
}

ui <- fluidPage(
  
  titlePanel("Text Prediction (Modified Kneser-Ney)"),
  
  wellPanel(
    h3("Enter input string here"),
    p("(If nothing is found, the app outputs 'THE')"),
    textInput("search", "Text to predict:"),
    h2(textOutput("pred_text"))
  )
)

# Define server logic to run NWpred and find the next word
server <- function(input, output) {
   
   output$pred_text <- renderText({
     paste("The next word is:",NWpred(trimws(input$search)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

