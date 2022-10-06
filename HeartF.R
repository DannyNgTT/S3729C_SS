# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)
library(DT)

# Load data --------------------------------------------------------------------

HeartF <- read.csv(file = "https://raw.githubusercontent.com/DannyNgTT/S3729C_SS/main/Heartfailure.csv.csv", header = TRUE, sep = ",")


# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("simplex"),
                
                h1("Heart failure Virtualisation"),
                h4(tags$a(href = "https://shiny.rstudio.com/", "Powered by R Shiny")),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    HTML(paste0("Variable Used to compare Physiological factors & Health outcomes")),
                    
                    br(), br(),
                    
                     selectInput(
                      inputId = "y",
                      label = "Y-axis:",
                      choices = c(
                        "Blood Pressure" = "Blood.Pressure",
                        "White Blood Cell Count" = "White.Blood.Cell.Count",
                        "Hemoglobin Level" = "Hemoglobin",
                        "Creatine Kinase Level" = "Creatine.Kinase",
                        "Platelet Count" = "Platelet.Count"
                      ),
                      selected = "Blood.Pressure"
                    ),
                    
                    selectInput(
                      inputId = "x",
                      label = "X-axis:",
                      choices = c(
                        "BMI" = "BMI",
                        "Blood Glucose" = "Blood.Glucose.Responses",
                        "Blood Creatine" = "Blood.Creatine",
                        "Blood Urea" = "Blood.Urea",
                        "Blood Sodium" = "Blood.Sodium",
                        "Blood Potassium" = "Blood.Potassium",
                        "Max Heart Rate" = "Max.HR"
                      ),
                      selected = "BMI"
                    ),
                    
                    selectInput(
                      inputId = "z",
                      label = "Color by:",
                      choices = c(
                        "Age Group" = "Age.Group",
                        "Gender" = "Gender",
                        "Smoker" = "Smoker",
                        "Family History" = "Family.History",
                        "Diabetes" = "Diabetes",
                        "Hypertension" = "Hypertension"
                      ),
                      selected = "Gender"
                    ),
                    
                    sliderInput(
                      inputId = "alpha",
                      label = "Alpha:",
                      min = 0, max = 1,
                      value = 0.5
                    ),
                    
                    sliderInput(
                      inputId = "size",
                      label = "Size:",
                      min = 0, max = 5,
                      value = 2
                    ),
                    
                    textInput(
                      inputId = "plot_title",
                      label = "Plot title",
                      placeholder = "Enter text to be used as plot title"
                    ),
                    
                    actionButton(
                      inputId = "update_plot_title",
                      label = "Update plot title"
                    ),
                    
                    
                    
                    downloadButton('download',"Download data")
                    
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "scatterplot", hover = "plot_hover"),
                    dataTableOutput(outputId = "HeartFhover"),
                    br(),
                    dataTableOutput(outputId = "HeartFtable")
                  )
                )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  output$scatterplot <- renderPlot({
    ggplot(data = HeartF, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$HeartFhover <- renderDataTable({
    nearPoints(HeartF, input$plot_hover) %>%
      select(Age.Group, Gender, Smoker, Family.History, Diabetes, Hypertension, BMI, Max.HR, Blood.Pressure, Mortality)
    
  })
  
 
    

  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
