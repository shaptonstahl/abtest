library(shiny)

cohort.list <- as.character(read.csv('http://dev-shaptonstahl.npr.org/shiny-data/ab-cohorts.csv')$cohort)
platform.list <- as.character(read.csv('http://dev-shaptonstahl.npr.org/shiny-data/ab-platforms.csv')$platform)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cohort Testing: Total Daily Listening Per User"),
  helpText("May take 1-2 minutes to update"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Cohorts',
          tags$p(),
          selectInput("cohort1", "Choose a baseline cohort:",
                      choices=cohort.list,
                      selected=ifelse(any(grepl('A', cohort.list)), 'A', cohort.list[1])),
          selectInput("cohort2", "Choose a comparison cohort:",
                      choices=cohort.list,
                      selected=ifelse(any(grepl('B', cohort.list)), 'B', cohort.list[2])),
          dateRangeInput("start.end.dates",
                         label="Choose a date range:",
                         start=Sys.Date()-10),
          tags$p(),
          tags$a('Definitions of the cohorts', 
            href='http://confluence.npr.org/pages/viewpage.action?pageId=46956803',
            target='_blank')
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
                      step=1),
          tags$p(),
          numericInput("min.seconds", 
                       label="Minimum seconds per day to include:", 
                       "10",
                       min=0)
      
        ),
        tabPanel('Platforms',
          tags$p(),
          checkboxGroupInput("platforms.selected", 
                             "Choose platforms to include:",
                             platform.list,
                             selected=c("IPHONE","ANDROID","WINDOWPH"))
        ),
        tabPanel('Server',
          tags$p(),
          selectInput("server.selected", 
                      label="Choose the server to query:",
                      choices=c("live (nero)"="nero-infinite", 
                                "live (tiberius)"="tiberius-infinite", 
                                "stage 1"="stage1-public_user",
                                "stage 4"="stage4-infinite"),
                      selected="stage 4"),
          tags$p(),
          tags$p()
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
  helpText(HTML("<b>Vertical axis:</b> Represents the likelihood that the effect is that value. The higher the curve, the more likely the effect is there. (For those with a stats background, this is like a 'pdf' = probability density function.)")),
  helpText(HTML("<b>Minimal meaningful effect:</b> Represented by the vertical dotted lines, this is set to a value such that if the effect is smaller, then you would conclude that there is no meaningful difference between the cohorts. If most of the curve is between the dotted lines then there is no meaningful effect.")),
  helpText(HTML("<b>What if none of the percentages are close to 95% and it says I need a lot more data?</b> You could reconsider what constitutes a minimum meaningful effect.")),
  helpText(HTML("<b>Access this page via:</b> http://bit.ly/nprabtesting"))
))
