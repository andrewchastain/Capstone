library(shiny)
library(data.table)
library(stringr)

db <- readRDS("./db_reduced.rds")

NWpred <- function(string) {
  require(stringr)
  require(data.table)
  setDTthreads(threads = 0)
  str <- regmatches(string,
                    regexpr("[[:graph:]]*([[:space:]]*[[:graph:]]*){5}$",
                            string))
  output <- character(0)
  while(identical(output, character(0))) {
    output <- db[root == str][["term"]]
    str <- gsub("^([[:graph:]]*[[:space:]]?)", "", str)
    if(identical(output, character(0))) {
      if(str_length(str) == 0) {
        output <- "the"
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
    #submitButton(text = "Predict next word"),
    h2(textOutput("pred_text"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$pred_text <- renderText({
     paste("The next word is:",NWpred(trimws(input$search)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

