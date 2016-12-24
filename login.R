shinyServer(function(input, output) {
USER <- reactiveValues(Logged = FALSE)

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("Username", "User Name:"),
      textInput("Password", "Pass word:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (input$Login > 0) {
      Username <- isolate(input$Username)
      Password <- isolate(input$Password)
      Id.username <- which(PASSWORD$Brukernavn == Username)
      Id.password <- which(PASSWORD$Passord    == Password)
      if (length(Id.username) > 0 & length(Id.password) > 0) {
        if (Id.username == Id.password) {
          USER$Logged <- TRUE
        } 
      } else  {
        "User name or password failed!"
      }
    } 
  }
})
})


shinyUI(uiOutput("uiLogin")
        )
