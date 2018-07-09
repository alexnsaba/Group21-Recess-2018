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
  h2("Enter your username and password, then click Login"),
  textInput("text", h3("Username"), 
            placeholder  = "Enter username..."),
  passwordInput("password", h3("Password"), 
            placeholder  = "Enter password..."),
  submitButton("Login"),
  tags$a(href="file:///C:/Users/alexnsaba/Desktop/R Interfaces/nba_signup.R", "Are you a new User? Click on this link to signup")

 
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)