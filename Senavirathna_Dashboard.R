# Load required libraries
#install.packages('plotly')
#install.packages('shiny')
#install.packages('shinyWidgets')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('arules')
#install.packages('arulesViz')

library(plotly)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)

# Load the data
data <- tryCatch({
  read.csv("student.csv")
}, error = function(e) {
  stop("Error reading the dataset. Please ensure 'student.csv' exists and is formatted correctly.")
})

# Prepare the data for Association Rule Mining
data_new <- data[, 16:23]
rules <- apriori(data_new, parameter = list(supp = 0.1, conf = 0.8))

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar-static-top { 
        position: relative; 
        width: 100%; 
        z-index: 1000; 
      }
      body, .shiny-body {
        background-image:url('https://wallpapercave.com/wp/wp2599002.jpg');
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
        opacity: 0.99;
      }
      .main-title {
        text-align: center;
        font-weight: bold;
        color:#ff0000;
        font-family:sans-serif;
        font-size: 70px; 
        background-color:white;
      }
      .parag {
        background-color: #f0f0f0;
        padding: 20px;
        font-size: 20px;
        border-radius: 10px;
        width: 150%;
        font-weight: bold;
        text-align: justify;
      }
       .intro-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .intro-text {
        flex: 1;
      }
      .team {
        text-align:center;
        background-color: #f0f0f0;
        padding: 20px;
        border-radius: 10px;
        font-weight: bold;
        font-size:20px;
      }
      .gender-card, .plot-container {
        background-color: #ffffff;
        opacity: 0.9;
        border: 1px solid #ccc;
        border-radius: 15px;
        padding: 10px;
        margin-bottom: 10px;
        text-align: center;
      }
    "))
  ),
  
  titlePanel(h1("Student Dashboard", class = "main-title")),
  
  navbarPage("",
             tabPanel("Introduction",
                      fluidRow(
                        div(class = "intro-container",
                            div(class = "intro-text",
                                column(8, div(class = "parag",
                                              h1("Welcome to the Student Dashboard", align = "center"),
                                              p("This dashboard is a visual representation of key performance data points relevant to the details of some students according to the following data set.
                                                 It provides a centralized location for users to monitor, analyze, and interpret information in real-time, allowing for informed decision-making and strategic planning. 
                                                 Dashboards typically feature customizable bar chart, scatter plot, pie chart, histogram, box plot and 3D scatter plot formats for easy understanding. They serve as a valuable tool for tracking progress, identifying trends, and measuring success across various areas of the students."),
                                              tags$p("For the dataset used in this dashboard, please visit ", tags$a("this link.", href = "https://github.com/Emmanuel96/apriori_association_rule_mining/tree/master/Dataset")),
                                              p("We used R programming Language for create this dashboard using plotly package.")
                                ))),
                            column(4, div(class = "team",
                                          h3("Team Members"),
                                          p("D.L.S.Nadavi: D/ADC/23/0028"),
                                          p("Y.M.S.S.B.Senavirathna: D/ADC/23/0030"),
                                          p("A.G.A.U.S.Gunasekara: D/ADC/23/0034"),
                                          p("M.L.B.T.S.Perera: D/ADC/23/0047")
                            ))
                        )
                      )
             ),
             
             tabPanel("Dashboard",
                      fluidRow(
                        column(4,
                               pickerInput("ageFilter", "Filter by Age", choices = unique(data$age), selected = unique(data$age), options = list('actions-box' = TRUE), multiple = TRUE)
                        ),
                        column(4,
                               pickerInput("SchoolFilter", "Filter by School", choices = unique(data$school), selected = unique(data$school), options = list('actions-box' = TRUE), multiple = TRUE)
                        ),
                        column(4,
                               div(class = "gender-card",
                                   textOutput("genderSummary")
                               )
                        )
                      ),
                      fluidRow(
                        column(6, div(class = "plot-container", plotlyOutput("barChart"))),
                        column(6, div(class = "plot-container", plotlyOutput("scatterPlot")))
                      ),
                      fluidRow(
                        column(4, div(class = "plot-container", plotlyOutput("histogram"))),
                        column(4, div(class = "plot-container", plotlyOutput("boxPlot"))),
                        column(4, div(class = "plot-container", plotlyOutput("pieChart")))
                      ),
                      fluidRow(
                        column(6, offset = 3, div(class = "plot-container", plotlyOutput("scatter3DPlot")))
                      )
             ),
             
             tabPanel("Association Rules",
                      fluidRow(
                        column(6, plotOutput("qualityPlot")),
                        column(6, plotlyOutput("rulesPlot"))
                      ),
                      fluidRow(
                        column(6, plotOutput("groupedRulesPlot")),
                        column(6, plotlyOutput("rules_new_plot"))
                      )
             )
  )
)

# Server
server <- function(input, output) {
  
  # Filtered data
  filtered_data <- reactive({
    req(input$ageFilter, input$SchoolFilter)
    data %>% filter(age %in% input$ageFilter, school %in% input$SchoolFilter)
  })
  
  # Gender summary
  output$genderSummary <- renderText({
    req(filtered_data())
    total_males <- sum(filtered_data()$sex == "M")
    total_females <- sum(filtered_data()$sex == "F")
    paste("Males: ", total_males, " || Females: ", total_females, " || Total Students: ", total_males + total_females)
  })
  
  # Bar chart
  output$barChart <- renderPlotly({
    plot_ly(data = filtered_data(), type = "bar") %>%
      add_trace(x = ~Mjob, y = ~absences, name = "Mother's Job", marker = list(color = "blue")) %>%
      add_trace(x = ~Fjob, y = ~absences, name = "Father's Job", marker = list(color = "#37FD12")) %>%
      layout(barmode = "group", title = "Parental Employment vs Absenteeism", xaxis = list(title = "Job Type"), yaxis = list(title = "Absences"))
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~G1, y = ~G3, type = "scatter", mode = "markers",
            marker = list(color = "yellow", size = ~studytime * 2)) %>%
      layout(title = "First Grade vs Third Grade", xaxis = list(title = "First Grade (G1)"), yaxis = list(title = "Third Grade (G3)"))
  })
  
  # Histogram
  output$histogram <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~age, type = "histogram", marker = list(color = "blue")) %>%
      layout(title = "Age Distribution")
  })
  
  # Box plot
  output$boxPlot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Walc, y = ~G3, type = "box") %>%
      layout(title = "Grades vs Alcohol Consumption")
  })
  
  # Pie chart
  output$pieChart <- renderPlotly({
    plot_ly(data = filtered_data(), labels = ~sex, type = "pie", marker = list(colors = c("red", "blue"))) %>%
      layout(title = "Gender Distribution")
  })
  
  # 3D Scatter plot
  output$scatter3DPlot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~studytime, y = ~failures, z = ~G3, type = "scatter3d", mode = "markers", color = ~studytime) %>%
      layout(title = "Study Time vs Failures vs Grades")
  })
  
  # Association rules plots
  output$qualityPlot <- renderPlot({
    plot(rules@quality)
  })
  
  output$rulesPlot <- renderPlotly({
    plot(rules)
  })
  
  output$groupedRulesPlot <- renderPlot({
    plot(rules, method = "grouped")
  })
  
  output$rules_new_plot <- renderPlotly({
    rules_new <- apriori(data_new, parameter = list(conf = 0.87), appearance = list(rhs = c("higher=yes"), lhs = c("schoolsup=yes"), default = "none"))
    plot(rules_new)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
