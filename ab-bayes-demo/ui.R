library(shiny)

cohort.list <- LETTERS

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("A/B Test via Bayesian Difference of Means"),
  helpText('This simulates a set of A/B tests where listening time in seconds is the metric to maximize.'),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Cohorts',
          tags$p(),
          selectInput("cohort1", "Choose a baseline cohort:",
                      choices=cohort.list,
                      selected=cohort.list[1]),
          selectInput("cohort2", "Choose a comparison cohort:",
                      choices=cohort.list,
                      selected=cohort.list[2])
        ),
        tabPanel('Hypotheses',
          tags$p(),
          numericInput("min.meaningful.effect", 
                       label="Minimum meaningful effect (seconds):", 
                       "30",
                       min=0),
      
          tags$p(),
          sliderInput("confidence", 
                      "How confident do you want to be (percent)?",
                      min=51,
                      max=99,
                      value=95,
                      step=1)
      
        )
      ),
      tags$p(),
      textOutput("obs.needed"),
      tags$p(),
      submitButton("Update Analysis")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  helpText(HTML("<b>If the mean is positive, the baseline cohort did better; if the mean is negative, the comparison cohort did better.</b>")),
  helpText(HTML("<b>Horizontal axis:</b> Represents effect of the comparison cohort over the baseline cohort, specifically the value of the comparison cohort minus the value of the baseline cohort.")),
  helpText(HTML("<b>Vertical axis:</b> Represents the likelihood that the effect is that value. The higher the curve, the more likely the effect is there. (For those with a stats background, this is a 'pdf' = probability density function.)")),
  helpText(HTML("<b>Minimal meaningful effect:</b> Represented by the vertical dotted lines, this is set to a value such that if the effect is smaller, then you would conclude that there is no meaningful difference between the cohorts. If most of the curve is between the dotted lines then there is no meaningful effect.")),
  helpText(HTML("<b>What if none of the percentages are close to 95% and it says I need a lot more data?</b> You could reconsider what constitutes a minimum meaningful effect.")),
  helpText(HTML("<b>Demo of significant difference:</b> A and B")),
  helpText(HTML("<b>Demo of non-significant difference:</b> A and W"))
))
