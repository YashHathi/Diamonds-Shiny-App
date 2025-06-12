#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
# We will load the tidyverse to access libraries such as ggplot, dplyr, and magrittr
library(tidyverse)

# Define UI for application that draws a histogram
ui <- navbarPage( 
    title = "Diamonds App",
    # This is the opening page for our website, introducing us. Here I just wanted some simple text.
    tabPanel(title="Hello World!",
             verbatimTextOutput(
               "text"
             )
    ),
# So this page will host the crux of this project. This will include the statistical analysis and the visualization.
    tabPanel(title = "Statistical Testing",
# We start out with a basic textual introduction
             verbatimTextOutput("text3"),
             titlePanel(title = "Dynamic Linear modeling"),
# This is for our drop downs. The user will be able to choose the independent and dependent variables for a linear model.
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "ind",
                   label = "Independent Variable:",
                   choices = names(Filter(is.numeric,diamonds))
                   
                 ),
                 selectInput(
                   inputId = "dep",
                   label = "Dependent Variable:",
# I am filtering it so for the dependent variable, we only see numeric variables.Think of this as my data cleaning :).
                   choices = names(Filter(is.numeric,diamonds))
               ),
                 selectInput(
                   inputId = "group",
                   label = "Group Factor:",
                   choices = names(Filter(is.factor, diamonds))
                 )
             ),
# Finally they will be able to see a scatter plot of the two variables to visualize their correlation.
             mainPanel(
               verbatimTextOutput("summary"),
               plotOutput("statistical_plot")
               
             ) 
          )
    ),
    # This tab will be for our additional visualization. The goal is for the user to input the color and cut they want, and it'll output a frequency bar chart of the diamonds of each clarity type. 
    tabPanel(title = "Extra Visualization",
             verbatimTextOutput("text2"),
             titlePanel(title = "Diamonds"),
             sidebarLayout(
               sidebarPanel(
# So this is for the first drop down. Users will be able to choose the cut of diamond they want.
                selectInput(
                 inputId = "cut",
                 label = "Cut:",
                 choices = sort(unique(diamonds$cut))
                 ),
# This is the second drop down, where users can select the color they want. 
                selectInput(
                  inputId = "col",
                  label = "Color:",
                  choices = sort(unique(diamonds$color))
                )
               ),
# Finally this will output the plot, a frequency bar chart.
               mainPanel(
                 
                 plotOutput("plot")
              )
            )
    )
             
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
# The goal of this code block is to work under the hood to create a frequency bar chart that dynamically changes based on user preferences.
  output$plot <- renderPlot({
# First the data needs to be cleaned to focus specifically on color and cut
    cleaning <- diamonds %>%
      filter(cut == input$cut) %>%
      filter(color == input$col) %>%
# Now we group by clarity, as that is what we are plotting, the frequency of each clarity based on color and cut.
      group_by(clarity)%>%
# This is how we count. I am using a Dplyr function called n()
      summarise(Count = n()) %>%
# Here we are mutating the clarity column to focus on the 8 specific clarity types.
      mutate(clarity = factor(clarity, levels = unique(clarity))) 
# This is to create the plot using ggplot
    ggplot(data = cleaning, aes(x = clarity, y = Count, fill = clarity)) + 
    geom_bar(stat = "identity") + theme_minimal()
  })
# This just outputs the text on the opening page
    output$text <- renderText ({
    "Welcome to the Diamonds App! Click the tabs to see more!"
  })
# This outputs the text we see on the second page
  output$text2 <- renderText({
    "I had made this visualization before I understand the true scope of the project. 
So please don't think this is the visualization for a grade. Think of it like a little easter egg or something."
  })
# This is for our statistical analysis were we want to output the summary statistics for a linear regression model.
  output$text3 <- renderText({
    "Select a independent variable, and a depedent variable, 
to see how the strength of the relationship between the two variables!"
  })
# We are making the model reactive, so the output will change based on the independent and dependent variable chosen.
  model <- reactive({
    model <- lm(as.formula(paste(input$dep,"~", input$ind)), data=diamonds)
  })
# This will print the summary
  output$summary <- renderPrint({
    summary(model())
  })
# This is for the output of our plot for the statistical test. It will output a scatter plot.
  output$statistical_plot<- renderPlot ({
    ggplot(data = diamonds, aes(x = get(input$ind), y = get(input$dep), color = factor(get(input$group)))) + 
      geom_point() + geom_smooth(method="lm")+ theme_classic() + labs(x=input$ind, y=input$dep, color=input$group, title=paste(input$dep, "vs", input$ind)) + theme(plot.title = element_text(size = 20), axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
  })
}

  


# Run the application 
shinyApp(ui = ui, server = server)
