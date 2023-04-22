
# options(shiny.maxRequestSize = 30 * 1024 * 1024)

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Shiny apps always have firstpackages, then ui, then server and then the final command
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)

# Reading the data ----
europe <- readRDS("europe.rds")

europe <-europe %>%
  mutate(AvgTemperatureC=(AvgTemperatureF-32)*(5/9))


# App ----
## UI ----
# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("slate"), #darkly, superhero
  
  # Application title
  titlePanel(strong("Project - Digital Skills")), #strong() writes in bold
  
  h1("In this shiny app we look at data from europe."),

  ### sidebar ----  
  sidebarLayout(
    # Sidebar panel (What do i want in there?)
    # Ususally inputs (sliders/widgets/...)
    sidebarPanel(h2("Side Panel"), #give the panel a "title"
      
      br(), #create empty rows (to move down the slider/...)
      
      sliderInput(inputId = "slider_month", label = "Month", 
                  min = 1, 
                  max = 12,
                  value = 8,
                  step = 1
      ),
      
      selectInput(inputId = "dropdown_country", label = "Country",
                  choices = c(sort(unique(europe$Country))), #NULL,
                  multiple = TRUE,
                  selected = c("Switzerland")
      ),
      
      selectInput(inputId = "dropdown_city", label = "City",
                  choices = c(sort(unique(europe$City))), #NULL,
                  multiple = TRUE
      ),
      
      textInput(inputId = "text", label = "Text field", value = "Your Text"),
      
      radioButtons(inputId = "radio_Temperature", label = "Scale",
                   choices = list("Celsius" = "AvgTemperatureC", "Fahrenheit" = "AvgTemperatureF"), #"label" = value
                   selected = "AvgTemperatureC"),#selected gives starting value
      
      actionButton(inputId = "button", label = "Let's go!"),
      
      #fileInput(inputId = "file", label = "Upload File (only RDS)", multiple = FALSE, accept = c(".rds"))
      
      downloadButton(outputId = "download", label = "DOWNNN")
    ),
  
    ### Main panel ----
    # Main panel
    # What do i want in here? - Usually Output
    mainPanel(h2("Main Panel"),
              #here it accesses the output we created in the server
              #textOutput(outputId = "text_output"),
              #textOutput(outputId = "muh") to return a fixed output - same as before (just access id)
              
              br(),
              hr(),#horizontal line
              br(),
              hr(),
              
              tabsetPanel(type = "tabs",#create seperate panels
                          tabPanel(title = "info", textOutput(outputId = "text_output"),
                                   textOutput(outputId = "choices"),
                                   verbatimTextOutput(outputId = "summary")),
                          tabPanel(title = "Data", tableOutput(outputId = "table")),#now only display result
                          #of text_output on this panel
                          tabPanel(title = "Lineplot", 
                                   fluidRow(
                                     column(width = 12, plotOutput(outputId = "lineplot"))
                                     )
                                   #use this command to choose how many plots etc. per row (total width = 12)
                          ),
                          tabPanel(title = "Boxplot", plotOutput(outputId = "boxplot")))
            
    )
    #TextOutput is a UI output function (outputID - name we give our render text object)
    #
  )
)

## server ----

# Define server side logic
server <- function(input, output, session) {
  #render function -> typical R functions, use {} around the code
  # output$text_output <- renderText({ #render function needs to be assigned to an output function
  #   #there is also renderTable, renderPlot, ..., always need to be assigned to an output (list)
  #   #give a name ("text_output")
  #   paste("Inputs:", "slide:", input$slider_1, " /// ", "Drop down" , input$dropdown_1,
  #         " /// ", "text", input$text, " /// ", "Radiobutton", input$radio_1)
  # })
  #output$muh <- renderText("Muh") to add a fixed input (text/...), then we don't need {}'s
  
  # europe <- eventReactive(input$file, {
  #   readRDS(input$file$datapath) %>%
  #     mutate(AvgTemperatureC=(AvgTemperatureF-32)*(5/9))
  #   })
  
  output$download <- downloadHandler(
    filename = "europe.csv",
    content = function(file) {
      write.csv(table, file, row.names = FALSE)
      
    }
  )
  
  output$choices <- renderText({
    paste("Country:", input$dropdown_country, "City:", input$dropdown_city, "Scale:", input$radio_Temperature,
          "Month:", input$slider_month)
  })
  
  text <- reactive({
    paste("You typed:", input$text)
  })
  
  output$text_output <- renderText(
    text()
  )
  
  line_plot <- eventReactive(input$button, {europe %>%
      group_by(Country, Month) %>%
      summarise(tempF = mean(AvgTemperatureF),
                tempC = mean(AvgTemperatureC)) %>%
      filter(Country == input$dropdown_country) 
  })
  
  
  table <- eventReactive(input$button, {europe %>%
      group_by(Country, City, Month) %>%
      summarise(tempF = mean(AvgTemperatureF),
                tempC = mean(AvgTemperatureC)) %>%
      filter(Country %in% input$dropdown_country) %>%
      filter(Month == input$slider_month) %>%
      filter(City %in% input$dropdown_city)
  })
  
  boxplot <- eventReactive(input$button, { europe %>%
      filter(Country %in% input$dropdown_country) %>%
      filter(City %in% input$dropdown_city) %>%
      group_by(Month)
  })
  
  output$summary <- renderPrint({
    summary(europe)
  })
  
  output$lineplot <- renderPlot({
    if(input$radio_Temperature == "AvgTemperatureC"){
      ggplot(line_plot(), aes(x = Month, col = Country))+
        #geom_line(aes(y = tempF))+
        geom_line(aes(y = tempC))+
        scale_x_continuous(breaks = seq(1, 12, 1), limits = c(1,12), expand = c(0,0))+
        theme_bw() } else {
          
          ggplot(line_plot(), aes(x = Month, col = Country))+
            geom_line(aes(y = tempF))+
            #geom_line(aes(y = tempC))+
            scale_x_continuous(breaks = seq(1, 12, 1), limits = c(1,12), expand = c(0,0))+
            theme_bw()
        }
  })
  
  output$table <- renderTable({europe %>%
      group_by(Country, City, Month) %>%
      summarise(tempF = mean(AvgTemperatureF),
                tempC = mean(AvgTemperatureC)) %>%
      filter(Country %in% input$dropdown_country) %>%
      filter(Month == input$slider_month) %>%
      filter(City %in% input$dropdown_city)
  }) 
  
  output$boxplot <- renderPlot({
    
    if(input$radio_Temperature == "AvgTemperatureC"){
      ggplot(data = boxplot(), aes(x = as.factor(Month), col = City))+
        geom_boxplot(aes(y = AvgTemperatureC))+
        theme_bw()
    } else {
      
      ggplot(data = boxplot(), aes(x = as.factor(Month), col = City))+
        geom_boxplot(aes(y = AvgTemperatureF))+
        theme_bw()
    }
  })
  
  observe({ #observeEvent -> button, prints output/...
    
    # new_country_choices <- unique(europe$Country)
    # updateSelectInput(session, inputId = "dropdown_country")

    new_choices <- unique(europe$City[europe$Country %in% input$dropdown_country])
    updateSelectInput(session, inputId = "dropdown_city", choices = new_choices, 
                      selected = new_choices)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

