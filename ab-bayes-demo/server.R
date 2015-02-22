library(shiny)
library(MCMCpack)

BayesianDifferenceOfMeans <- function(x, y, minimum.meaningful.effect,
                                      burnin=500,
                                      n.draws=500,
                                      return.draws=FALSE) {
  #' Given two vectors of possibly different lengths
  #' returns the probability that the mean of x is greater than
  #' the mean of y and draws from the posterior distribution of
  #' mean(x) - mean(y).  
  #' 
  #' These draws can be used to answer questions like "What is the probability
  #' that the difference of means is between -30 and 30 seconds (ie close to 
  #' zero)?"
  #' 
  #' set the response variable to all the values
  mcmc.y <- c(x, y)
  #' set the dummy variable to caputre 'in group x'
  mcmc.x <- c(rep(1, length(x)), rep(0, length(y)))
  #' define prior precision (1/var) to go with mean zero for the difference
  #' assuming prior belief that there is only a 5% chance of an effect
  #' greater than minimum.meaningful.effect
  diff.prior.sd <- - minimum.meaningful.effect / qnorm(.05 / 2)
  diff.prior.precision <- 1 / diff.prior.sd ^ 2
  #' regress
  posterior  <- MCMCregress(mcmc.y ~ mcmc.x, 
                            burnin=burnin, mcmc=n.draws,
                            b0=0, 
                            B0 = matrix(c(0, 0, 0, diff.prior.precision), ncol=2))
  #' get draws for difference of means
  draws <- posterior[,2]
  #' return probability mean(x) > mean(y) given the data
  out <- list(prob.x.greater=mean(draws > 0))
  #' return distribution of mean(x) - mean(y)
  if(return.draws) out <- c(out, list(draws.x.minus.y=draws))
  return(out)
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

ReadX <- function(cohort) {
  #' This function is written for the demo data.
  #' You can write this to hit a SQL database, read the output of a PHP file, etc.
  x <- read.delim(paste('data_', cohort, '.tsv', sep=''))[,1]
  return(x)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Read the data
  x1 <- reactive({
    ReadX(cohort=input$cohort1)
  })
  x2 <- reactive({
    ReadX(cohort=input$cohort2)
  })
  
  # Perform Bayes calculation
  bayes.result <- reactive({
    BayesianDifferenceOfMeans(x=x1(), 
                              y=x2(), 
                              minimum.meaningful.effect=input$min.meaningful.effect, 
                              return.draws=TRUE)
  })
  
  output$obs.needed <- renderText({
    needed.obs <- SampleSizeNeeded(bayes.result()$draws.x.minus.y,
                                   n.current=length(x1())+length(x2()),
                                   mme=input$min.meaningful.effect, 
                                   certainty.fraction=input$confidence/100)
    ObservationsNeededMessage(current.obs=length(x1()) + length(x2()), 
                              needed.obs=needed.obs,
                              desired.certainty=input$confidence)
  })

  output$distPlot <- renderPlot({
    PlotCohortEffect(bayes.result()$draws.x.minus.y, input$cohort1, input$cohort2, 
                     input$min.meaningful.effect, input$confidence/100,
                     length(x1()), length(x2()))
  })
})
