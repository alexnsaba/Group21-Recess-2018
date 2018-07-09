library(shiny)

# Define UI ----
ui <- fluidPage(
  tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                  
                  body {
                  font-family: 'Times new Roman';
                  line-height: 1.1;
                  color:white;
                  background-color:#037881;
                  }
                  
                  ")),
  h1("NATIONAL BASKETBALL ASSOCIATION ANALYSIS SYSTEM"),
  h2("choose the dataset to summarise, then click Finish"),
  selectInput("select", p("Choose dataset"), 
              choices = list("Player_data" = 1, "Players" = 2,
                             "Seasons_statistics" = 3), selected = 1),
  submitButton("Finish")
  )

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)