library(shiny)
library(RMySQL)
library(MCMCpack)

# initialize database connection
mysql.driver <- dbDriver("MySQL")
robo.ids <- c(11987982,
  13174734, 1186881532, 1087617437, 1138038188, 9906791, 7456245, 8943206, 
  12526723, 2019184, 100042, 11727645, 10853163, 11657281, 11420786)

DifferenceOfMedians <- function(x, y, frac.dropped=.2, n.iter=1000, silent=FALSE, session) {
  #' Given two vectors return the distribution of difference of means
  #' calculated using bootstrapping
  
  OneDraw <- function() {
    boot.x <- sample(x, round((1-frac.dropped)*length(x)), replace=TRUE)
    boot.y <- sample(y, round((1-frac.dropped)*length(y)), replace=TRUE)
    return(median(boot.x) - median(boot.y))
  }
  
  draws <- numeric(0)
    for(i in 1:n.iter) {
      draws <- c(draws, OneDraw())
    }
  
  if(!silent) {
    cat("Difference of Medians test via the bootstrap\n")
    cat("Median of x:", median(x), "\n")
    cat("Median of y:", median(y), "\n")
    cat("Probability that median(x) > median(y):", round(mean(draws > 0), 2), "\n")
  }
  return(list(prob.x.greater=mean(draws > 0),
              draws.x.greater=draws))
}

Probs <- function(draws, mme) {
  return(c(negative=mean(draws < -mme),
           near.zero=mean(draws < mme & draws > -mme),
           positive=mean(draws > mme)))
}

SampleSizeNeeded <- function(draws, n.current, mme, certainty.fraction) {
  #' Given draws from the posterior of mean(x) - mean(y),
  #' the total size of the current sample, the minimum meaningful 
  #' effect, and the desired level of certainty, returns an estimate
  #' of the sample size required to reach that level of certainty assuming
  #' that the shape of the posterior remains the same.
  if(max(Probs(draws, mme)) >= certainty.fraction) return(n.current)
  
  if(mean(draws) < -mme) {
    try(out <- ((quantile(draws, certainty.fraction) - mean(draws)) / (-mme - mean(draws)))^2 * n.current)
  } else if(mean(draws) > mme) {
    try(out <- ((mean(draws) - quantile(draws, 1 - certainty.fraction)) / (mean(draws) - mme))^2 * n.current)
  } else {
    try(f <- function(v) mean(draws < v & draws > -v) - certainty.fraction)
    try(ur <- uniroot(f, lower=0, upper=max(abs(draws))+1))
    try(out <- ((ur$root - abs(mean(draws))) / (mme - abs(mean(draws))))^2 * n.current)
  }
  out <- round(out)
  names(out) <- NULL
  return(out)
}
                                                
ObservationsNeededMessage <- function(current.obs, needed.obs, desired.certainty) {
  if(current.obs >= needed.obs) {
    return(paste("You (should) already have already exceeded ", desired.certainty, "% certainty. If not, you're very close.", sep=""))
  } else {
    return(paste("If the mean so far is correct, we estimate that you will need a total of ", needed.obs,
                 " (", needed.obs-current.obs, " additional) observations to reach ", 
                 desired.certainty, "% certainty.",
                 sep=""))
  }
}

PlotCohortEffect <- function(draws, cohort1, cohort2, mme, certainty.fraction, n.x, n.y) {
  if(mean(draws) > 0) {
    winning.cohort <- cohort1
    losing.cohort <- cohort2
    legend.offset <- .1  # fraction of x range legend is from the left
  } else {
    winning.cohort <- cohort2
    losing.cohort <- cohort1
    legend.offset <- .7
  }
  probs <- Probs(draws, mme)
  
  if(max(probs) < certainty.fraction) {
    title <- paste(winning.cohort, " is ahead of ", losing.cohort, ", but it is not yet conclusive", sep="")
  } else {
    if(probs['positive'] > certainty.fraction | probs['negative'] > certainty.fraction) {
      title <- paste(winning.cohort, " beat ", losing.cohort, sep="")
    } else {
      title <- "It's a tie"
    }
  }
  
  xlim <- c(min(c(-mme, draws)), max(c(mme, draws)))
  
  xlab <- paste("Sample size = ", n.x, " + ", n.y, 
                ".  The difference between cohorts is about ", round(mean(abs(draws))), " seconds", sep="")
  
  h <- hist(draws, 50, xlim=xlim,
            main=title, 
            xlab=xlab,
            axes=FALSE,
            ylab='')
  axis(1)
  abline(v=c(mme, -mme), lty=2, lwd=2)
  legend(x=legend.offset * (xlim[2] - xlim[1]) + xlim[1],
         y=.8 * max(h$counts),
         legend=c(paste(cohort1, " wins: ", round(probs['positive']*100), "%", sep=""),
                  paste("It's a tie: ", round(probs['near.zero']*100), "%", sep=""),
                  paste(cohort2, " wins: ", round(probs['negative']*100), "%", sep="")),
         title="Probabilities")
}

ReadX <- function(start.date, end.date, cohort, platforms, server, min.seconds) {
  cohort.sql <- paste("SELECT ratings_cohort, SUM(ratings_elapsed) AS listen_time_seconds ",
                      "FROM infinite.user_ratings ",
                      "WHERE ratings_platform IN ('", paste(platforms, collapse="', '"), "') ",
                      "AND DATE(ratings_timestamp) >= '", start.date, "' ",
                      "AND DATE(ratings_timestamp) <= '", end.date, "' ",
                      "AND ratings_rating IN ('SKIP','COMPLETED') ",
                      "AND ratings_cohort = '", cohort, "' ",
                      "AND ratings_user_id NOT IN (", paste(robo.ids, collapse=", "), ") ",
                      "GROUP BY ratings_user_id, DATE(ratings_timestamp)",
                      sep="")
  withProgress(message='Retrieving data', detail='This will take a minute', value=0, {
    setProgress(value=0, detail='making connection')
    con <- dbConnect(mysql.driver, group=server)
    setProgress(value=0, detail='sending query')
    rs <- dbSendQuery(con, cohort.sql)
    setProgress(value=0, detail='getting data')
    x <- fetch(rs, n=-1)[,2]
    dbDisconnect(con)
  })

  # filter out times more than 1 day
  x <- x[x < 60 * 60 * 24]
  
  # filter out zeros
  x <- x[x >= min.seconds]
  
  return(x)
}  

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Read the data
  x1 <- reactive({
    ReadX(input$start.end.dates[1], 
          end.date=input$start.end.dates[2], 
          cohort=input$cohort1, 
          platforms=input$platforms.selected,
          server=input$server.selected,
          min.seconds=input$min.seconds)
  })
  x2 <- reactive({
    ReadX(input$start.end.dates[1], 
          end.date=input$start.end.dates[2], 
          cohort=input$cohort2, 
          platforms=input$platforms.selected,
          server=input$server.selected,
          min.seconds=input$min.seconds)
  })
  
  # Perform bootstrap calculation
  bootstrap.result <- reactive({
    DifferenceOfMedians(x1(), x2(), silent=TRUE, session=session)
  })
  
  output$obs.needed <- renderText({
    needed.obs <- SampleSizeNeeded(bootstrap.result()$draws.x.greater,
                                   n.current=length(x1())+length(x2()),
                                   mme=input$min.meaningful.effect, 
                                   certainty.fraction=input$confidence/100)
    ObservationsNeededMessage(current.obs=length(x1()) + length(x2()), 
                              needed.obs=needed.obs,
                              desired.certainty=input$confidence)
  })

  output$distPlot <- renderPlot({
    PlotCohortEffect(bootstrap.result()$draws.x.greater, input$cohort1, input$cohort2, 
                     input$min.meaningful.effect, input$confidence/100,
                     length(x1()), length(x2()))
  })
})
