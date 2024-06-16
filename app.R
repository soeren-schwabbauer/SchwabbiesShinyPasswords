library(shiny)
library(googledrive)
library(googlesheets4)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "./.secrets"
)

source("master.R")

# Sample dataframe to use if master password is incorrect
sample_df <- data.frame(
  wp = c("example1", "example2", "example3"),
  seed = c(12345, 67890, 54321),
  length = c(12, 15, 10),
  stringsAsFactors = FALSE
)



# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Passwort Wallet"),
  
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js")
  ),
  
  wellPanel(
    fluidPage(
      column(3, passwordInput("master_pw", NULL, placeholder = "Master Passwort eingeben")),
      column(3),
      column(2, actionButton("login", "Login", icon = icon("sign-in-alt")))
    )
  ),
  
  wellPanel(
    fluidPage(  
      column(3, selectInput("select_wp", NULL, choices = NULL)),
      column(2, verbatimTextOutput("pw")),
      column(1, actionButton("copy_pw", "Copy", icon = icon("clipboard"), class = "btn-copy")),
      column(2, actionButton("delete_pw", "Passwort löschen", icon = icon("trash")))
    )
  ),
  
  wellPanel(
    fluidPage(
      column(3, textInput("wp_name", "Name der Webseite", placeholder = "Name")),
      column(2, numericInput("pw_length", "Länge", value = 12, min = 1, max = 20, step = 1)),
      column(1),
      column(2, actionButton("create_pw", "Passwort erstellen", icon = icon("cog")))
    )
  ),
  
  # JavaScript for enabling clipboard functionality
  tags$script(HTML("
    $(document).ready(function() {
      var clipboard = new ClipboardJS('.btn-copy', {
        text: function(trigger) {
          return document.getElementById('pw').innerText;
        }
      });
      
      clipboard.on('success', function(e) {
        alert('Password copied to clipboard!');
        e.clearSelection();
      });
    });
  "))
)

server <- function(input, output, session) {
  
  # Initialize with the sample dataframe
  pws_reactive <- reactiveVal(sample_df)
  
  observe({
    # Update the selectInput choices initially with sample data
    pws <- pws_reactive()
    updateSelectInput(session, "select_wp", choices = sort(unique(pws$wp)))
  })
  
  observeEvent(input$login, {
    if (input$master_pw == correct_master_pw) {
      # Read the existing dataframe from Google Sheets
      token <- drive_get("passwords")$id
      pws <- read_sheet(token)
      pws_reactive(pws)
      
      # Update the selectInput choices with actual data
      #updateSelectInput(session, "select_wp", choices = sort(unique(pws$wp)))

    } else {
      showNotification("Incorrect master password!", type = "error")
    }
  })
  
  seed <- eventReactive(input$create_pw, {
    req(input$master_pw == correct_master_pw)  # Ensure the master password is correct
    sample(0:9999, 1)
  })
  
  
  observeEvent(input$create_pw, {
    req(input$master_pw == correct_master_pw)  # Ensure the master password is correct
    req(input$select_wp)
    
    seed <- seed()
    
    message(seed)
    # Create a new row for the dataframe
    pw_neu <- data.frame(
      wp = input$wp_name,
      seed = seed(),
      length = input$pw_length,
      stringsAsFactors = FALSE
    )
    
    # Update the dataframe by appending the new row
    pws <- pws_reactive()
    
    # input sanity checks
    if (input$wp_name == "") {
      showNotification("Keine Webseite angegeben!", type = "error")
    } else if (!is.numeric(input$pw_length)) {
      showNotification("Passwortlänge muss numerisch sein", type = "error")
    } else if (input$wp_name %in% pws$wp) {
      showNotification("Passwort bereits erstellt", type = "error")
    } else {
      # if input sanity checks passed
      pws <- rbind(pws, pw_neu)
      # Write the updated dataframe back to Google Sheets
      token <- drive_get("passwords")$id
      write_sheet(pws, ss = token, sheet = 1)
    }
    
    pws_reactive(pws)
    
    # Update the input fields
    updateTextInput(session, "wp_name", "Name der Webseite", placeholder = "Name", value = "")
    updateNumericInput(session, "pw_length", "Länge", value = 12, min = 0, max = 20, step = 1)
    
    # Update the selectInput choices
    #updateSelectInput(session, "select_wp", choices = sort(unique(pws$wp)))
  })
  
  observeEvent(input$delete_pw, {
    req(input$select_wp)  # Ensure a selection is made
    req(input$master_pw == correct_master_pw)  # Ensure the master password is correct
    
    # Update the dataframe by removing the selected row
    pws <- pws_reactive()
    pws <- pws[pws$wp != input$select_wp, ]
    pws_reactive(pws)
    
    # Write the updated dataframe back to Google Sheets
    token <- drive_get("passwords")$id
    write_sheet(pws, ss = token, sheet = 1)
    
    # Update the selectInput choices
    #updateSelectInput(session, "select_wp", choices = sort(unique(pws$wp)), selected = NULL)
  })
  
  output$pw <- renderText({
    req(input$select_wp)  # Ensure a selection is made
    
    # Get seed and length from datatable
    pws <- pws_reactive()
    
    pw_seed   <- pws$seed[pws$wp == input$select_wp]
    pw_length <- pws$length[pws$wp == input$select_wp]
    pw        <- create_pw(nchars = pw_length, seed = pw_seed)
    
    return(pw)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
