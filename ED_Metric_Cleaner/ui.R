

#This app will take files from EPD and Birdeye and put them into a unified Google Sheet for presentation on Google Data Studio

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

# Define UI
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("TotalCare EPD Record Cleaner"),
  
  verticalLayout(
    wellPanel(
      div(id = "data_input_panel",
          helpText("Use the 'First Try Total' download in EPD and upload your data here. This will automatically join it to the old data."),
          fileInput("epd_upload", "New EPD Upload",
                    multiple = FALSE)
      ),
      shinyjs::hidden(
        div(id = "new_rows_number",
            textOutput("new_row_output")
        )
      ),
      br(),
      textOutput("initial_count"),
      br(),
      )
    )
  )
  
  
)
