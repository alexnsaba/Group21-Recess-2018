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
                  align:center;
                  }
                  
                  
                  ")),
  h1("NATIONAL BASKETBALL ASSOCIATION ANALYSIS SYSTEM"),
  h2("Fill the form bellow and click Signup"),
  textInput("text", p("Name:"),placeholder  = "Enter your name..."),
  dateInput("date", p("Date Of Birth")), 
 # emailInput("email",p("Email")),
  textInput("text", p("username:"),placeholder  = "Enter your username..."),
  passwordInput("password", p("Password"), placeholder  = "Enter password..."),
  passwordInput("password", p("comfirm Password"), placeholder  = "comfirm your password..."),
  submitButton("Signup")
  )

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)