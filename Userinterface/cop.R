ui <- fluidPage(
  
  
  
  
  tags$head(
    
    
    
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color:white;
                    }
                    
                    body {
                    background-color:#20B2AA;
                    }
                    column()
                    
                    
                    "))
    ),
  
  
  pageWithSidebar(
    headerPanel('National basket Association Analysis'),
    sidebarPanel(		
      getTool("tool")
    ),
    mainPanel(
      textOutput("char")
    )
  ),
  
  
  
  fluidRow(
    
    column(3,
           h3(""),
           actionButton("action", "PLAYERS' DETAILS"),
           br(),
           br(), 
           submitButton("Submit")),
    
    column(3,
           h3(""),
           actionButton("action", "SEASON STATISTICS"),
           br(),
           br(), 
           submitButton("Submit")),
    
    
    
    
    
    column(3,
           h3("Single checkbox"),
           checkboxInput("checkbox", "Choice A", value = TRUE)),
    
    column(3, 
           checkboxGroupInput("checkGroup", 
                              h3("Checkbox group"), 
                              choices = list("Choice 1" = 1, 
                                             "Choice 2" = 2, 
                                             "Choice 3" = 3),
                              selected = 1)),
    
    column(3, 
           dateInput("date", 
                     h3("Date input"), 
                     value = "2014-01-01"))   
  ),
  
  fluidRow(
    
    column(3,
           dateRangeInput("dates", h3("Date range"))),
    
    column(3,
           fileInput("file", h3("File input"))),
    
    column(3, 
           h3("Help text"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),
    
    column(3, 
           numericInput("num", 
                        h3("Numeric input"), 
                        value = 1))   
  ),
  
  fluidRow(
    
    column(3,
           radioButtons("radio", h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)),
    
    column(3, 
           sliderInput("slider1", h3("Sliders"),
                       min = 0, max = 100, value = 50),
           sliderInput("slider2", "",
                       min = 0, max = 100, value = c(25, 75))
    ),
    
    column(3, 
           textInput("text", h3("Text input"), 
                     value = "Enter text..."))   
  )
    )
