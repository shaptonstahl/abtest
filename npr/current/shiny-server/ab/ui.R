library(shiny)

cohort.list <- sort(read.table("http://172.31.2.98/shiny-data/ab-cohorts.php")[,1])

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Cohort Testing: Total Daily Listening Per User"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Source',
          selectInput("cohort1", "Choose a baseline cohort:",
                      choices=cohort.list,
                      selected=cohort.list[length(cohort.list)-1]),
          selectInput("cohort2", "Choose a comparison cohort:",
                      choices=cohort.list,
                      selected=cohort.list[length(cohort.list)]),
          tags$hr(),
          tags$hr(),
          tags$hr(),
          tags$a('Definitions of the cohorts', 
            href='http://confluence.npr.org/pages/viewpage.action?pageId=46956803',
            target='_blank')
        ),
        tabPanel('Format',
          dateInput("start.date", "Choose a start date:", value="2014-01-01"),
          dateInput("end.date", "Choose an end date:", value=Sys.Date()),
      
          tags$hr(),
          numericInput("min.meaningful.effect", 
                       label="Minimum meaningful effect (seconds):", 
                       "30",
                       min=0),
      
          tags$hr(),
          sliderInput("confidence", "How confident do you want to be (percent)?",
                      min=51,
                      max=99,
                      value=95,
                      step=1)
        )
      ),
      tags$hr(),
      textOutput("obs.needed"),
      submitButton("Update View")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
#      ,plotOutput("densityPlot")
    )
  ),
  helpText(HTML("<b>If the mean is positive, the comparison cohort did better; if the mean is negative, the baseline cohort did better.</b>")),
  helpText(HTML("<b>Horizontal axis:</b> Represents effect of the comparison cohort over the baseline cohort, specifically the value of the comparison cohort minus the value of the baseline cohort.")),
  helpText(HTML("<b>Vertical axis:</b> Represents the likelihood that the effect is that value. The higher the curve, the more likely the effect is there. (For those with a stats background, this is a 'pdf' = probability density function.)")),
  helpText(HTML("<b>Minimal meaningful effect:</b> Represented by the vertical dotted lines, this is set to a value such that if the effect is smaller, then you would conclude that there is no meaningful difference between the cohorts. If most of the curve is between the dotted lines then there is no meaningful effect.")),
  helpText(HTML("<b>What if none of the percentages are close to 95% and it says I need a lot more data?</b> You could reconsider what constitutes a minimum meaningful effect.")),
  helpText(HTML("<b>Data source:</b> gracchus.npr.org")),
  helpText(HTML("<b>Access this page via:</b> http://bit.ly/nprabtesting"))
))
