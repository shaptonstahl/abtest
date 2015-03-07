library(shiny)

# initialize the cohort list
cohort.list <- LETTERS

# Define UI for application that draws a histogram
fluidPage(
  # Application title
  titlePanel("Cohort Testing: Total Daily Listening Per User"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("cohort1", "Choose a baseline cohort:",
                  choices=cohort.list,
                  selected=cohort.list[1]),
      selectInput("cohort2", "Choose a comparison cohort:",
                  choices=cohort.list,
                  selected=cohort.list[2]),
      numericInput("min.meaningful.effect", 
                   label="Minimum meaningful effect (seconds):", 
                   "30",
                   min=0),
      
      tags$hr(),
      sliderInput("confidence.percent", 
                  "How confident do you want to be (percent)?",
                  min=51,
                  max=99,
                  value=95,
                  step=1),
      tags$hr(),
      textOutput("obs.needed"),
      submitButton("Update Analysis")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
      #,textOutput("debug")
    )
  ),
  helpText(HTML("<b>If the mean is positive, the baseline cohort did better; if the mean is negative, the comparison cohort did better.</b>")),
  helpText(HTML("<b>Horizontal axis:</b> Represents effect of the comparison cohort over the baseline cohort, specifically the value of the comparison cohort minus the value of the baseline cohort.")),
  helpText(HTML("<b>Vertical axis:</b> Represents the likelihood that the effect is that value. The higher the curve, the more likely the effect is there. (For those with a stats background, this is like a 'pdf' = probability density function.)")),
  helpText(HTML("<b>Minimal meaningful effect:</b> Represented by the vertical dotted lines, this is set to a value such that if the effect is smaller, then you would conclude that there is no meaningful difference between the cohorts. If most of the curve is between the dotted lines then there is no meaningful effect.")),
  helpText(HTML("<b>What if none of the percentages are close to 95% and it says I need a lot more data?</b> You could reconsider what constitutes a minimum meaningful effect.")),
  helpText(HTML("<b>Access this page via:</b> http://polimath.shinyapps.io/ab-boot-demo")),
  helpText(HTML('<b>Read about it on my blog at:<b/> <a href="http://haptonstahl.org/polimath/?p=330" target="_blank">http://haptonstahl.org/polimath/</a>')),
  helpText(HTML('<b>Access the source code via:</b> <a href="https://github.com/shaptonstahl/abtest/tree/master/ab-boot-demo" target="_blank">https://github.com/shaptonstahl/abtest/tree/master/ab-boot-demo</a>'))
)
