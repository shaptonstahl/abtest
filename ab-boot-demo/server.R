library(shiny)

DifferenceOfMedians <- function(x, y, frac.dropped=.2, n.iter=1000, silent=FALSE) {
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
  if(median(draws) < -mme) {
    out <- (quantile(draws, certainty.fraction) - median(draws)) / (-mme - median(draws)) * n.current
  } else if(median(draws) > mme) {
    out <- (median(draws) - quantile(draws, 1 - certainty.fraction)) / (median(draws) - mme) * n.current
  } else {
    f <- function(v) mean(draws < v & draws > -v) - certainty.fraction
    ur <- uniroot(f, lower=0, upper=max(abs(draws))+1)
    out <- (ur$root - median(draws)) / (mme - median(draws)) * n.current
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

ReadX <- function(cohort) {
  #' This function is written for the demo data.
  #' You can write this to hit a SQL database, read the output of a PHP file, etc.
  x <- read.delim(paste('data_', cohort, '.tsv', sep=''))[,1]
  return(x)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Read the data
  x1 <- reactive({
    ReadX(cohort=input$cohort1)
  })
  x2 <- reactive({
    ReadX(cohort=input$cohort2)
  })
  
  # Perform bootstrap calculation
  bootstrap.result <- reactive({
    DifferenceOfMedians(x1(), x2(), silent=TRUE)
  })
  
  output$obs.needed <- renderText({
    needed.obs <- SampleSizeNeeded(bootstrap.result()$draws.x.greater,
                                   n.current=length(x1())+length(x2()),
                                   mme=input$min.meaningful.effect, 
                                   certainty.fraction=input$confidence.percent/100)
    ObservationsNeededMessage(current.obs=length(x1()) + length(x2()), 
                              needed.obs=needed.obs,
                              desired.certainty=input$confidence.percent)
  })

  output$distPlot <- renderPlot({
    PlotCohortEffect(draws=bootstrap.result()$draws.x.greater, 
                     cohort1=input$cohort1, 
                     cohort2=input$cohort2, 
                     mme=input$min.meaningful.effect, 
                     certainty.fraction=input$confidence.percent/100,
                     n.x=length(x1()), 
                     n.y=length(x2()))
  })
  
})
