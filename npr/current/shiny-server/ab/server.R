library(shiny)

CohortEffect <- function(x1,
                         x2,
                         min.meaningful.effect) {
  dat <- data.frame(y=c(x1,x2),
                    d2=c(rep(0, length(x1)), rep(1, length(x2))))
  res <- lm(y ~ d2, data=dat)
  coefs <- summary(res)$coefficients
  
  effect.mean <- coefs[2,1]
  effect.sd <- coefs[2,2]
  
  xmin <- min(c(-min.meaningful.effect, effect.mean - 3*effect.sd))
  xmax <- max(c(min.meaningful.effect, effect.mean + 3*effect.sd))
  ymax <- max(dnorm(0, sd=effect.sd))
  
  prob.near.zero <- pnorm(min.meaningful.effect, mean=effect.mean, sd=effect.sd) -
    pnorm(-min.meaningful.effect, mean=effect.mean, sd=effect.sd)
  prob.positive <- 1 - pnorm(min.meaningful.effect, mean=effect.mean, sd=effect.sd)
  prob.negative <- pnorm(-min.meaningful.effect, mean=effect.mean, sd=effect.sd)
  
  return(list(effect.mean=effect.mean,
              effect.sd=effect.sd,
              min.meaningful.effect=min.meaningful.effect,
              plot.xmin=xmin,
              plot.xmax=xmax,
              plot.ymax=ymax,
              prob.near.zero=prob.near.zero,
              prob.positive=prob.positive,
              prob.negative=prob.negative,
              n=length(x1)+length(x2)))
}

PlotCohortEffect <- function(cohort.effect,
                             col="RoyalBlue",
                             xlab="seconds") {
  
  plot(0,0, type='n', 
       main=paste("Cohort Effects:", ifelse(cohort.effect$effect.mean > 0, "Comparison", "Baseline"), "Cohort Did Better"),
       sub=paste("n =", cohort.effect$n, "   mean =", signif(cohort.effect$effect.mean, 2)),
       xlim=c(cohort.effect$plot.xmin, cohort.effect$plot.xmax),
       ylim=c(0, cohort.effect$plot.ymax),
       xlab=xlab,
       ylab="",
       yaxt='n')
  d1 <- function(x) dnorm(x, mean=cohort.effect$effect.mean, sd=cohort.effect$effect.sd)
  polygon(x=c(cohort.effect$plot.xmin, 
              seq(from=cohort.effect$plot.xmin, to=cohort.effect$plot.xmax, length.out=101), 
              cohort.effect$plot.xmax),
          y=c(0, d1(seq(from=cohort.effect$plot.xmin, to=cohort.effect$plot.xmax, length.out=101)), 0),
          col=col)
  curve(d1, 
        from=cohort.effect$plot.xmin, 
        to=cohort.effect$plot.xmax, 
        add=TRUE,
        lwd=2)
  
  abline(v=c(-cohort.effect$min.meaningful.effect, cohort.effect$min.meaningful.effect),
         lwd=2,
         lty=2)
  
  legend(.98 * cohort.effect$plot.xmin + .02 * cohort.effect$plot.ymax, 
         .8*cohort.effect$plot.ymax,
         c(paste("positive: ", round(100*cohort.effect$prob.positive), "%", sep=""),
           paste("near zero: ", round(100*cohort.effect$prob.near.zero), "%", sep=""),
           paste("negative: ", round(100*cohort.effect$prob.negative), "%", sep="")))
}

ObservationsNeeded <- function(x1, x2, min.meaningful.effect, confidence) {
  stopifnot(confidence > .5)
  
  y <- c(x1, x2)
  x <- c(rep(0, length(x1)), rep(1, length(x2)))
  res <- lm(y ~ x)
  
  post.mean <- summary(res)$coefficients[2,1]
  data.sd <- summary(res)$coefficients[2,2] * sqrt(length(x1) + length(x2))
  
  #return(paste("data.sd:", data.sd))
  #return(paste("post.mean:", post.mean))
  #return(paste("confidence:", confidence))

  if(post.mean > min.meaningful.effect) {
    # case: positive effect
    # return("DEBUG: positive effect")
    f <- function(N) pnorm((post.mean - min.meaningful.effect) * sqrt(N) / data.sd) - confidence / 100
  } else if(post.mean < -min.meaningful.effect) {
    # case: negative effect
    #  return("DEBUG: negative effect")
    f <- function(N) pnorm((-min.meaningful.effect - post.mean) * sqrt(N) / data.sd) - confidence / 100
  } else {
    # case: no meaningful effect
    # return("DEBUG: no meaningful effect")
    f <- function(N) pnorm((min.meaningful.effect - post.mean) * sqrt(N) / data.sd) - 
      pnorm((-min.meaningful.effect - post.mean) * sqrt(N) / data.sd) - confidence / 100
  }
  root.res <- uniroot(f, interval=c(2, 1e10))
  # root.res <- tryCatch(uniroot(f, interval=c(1, 1e10)), error=function(e) NA)
  if(is.na(root.res)) return("Unable to estimate number of needed observations.")
  if(root.res$estim.prec > 1e-3) {
    warning("Unable to estimate number of needed observations")
    return(Inf)
  } else {
    return(ceiling(root.res$root))
  }
}

ObservationsNeededMessage <- function(current.obs, needed.obs, desired.certainty) {
  if(current.obs >= needed.obs) {
    return(paste("You (should) already have already exceeded ", desired.certainty, "% certainty. If not, you're very close.", sep=""))
  } else {
    return(paste("If the mean so far is correct, you will need a total of ", needed.obs,
                 " (", needed.obs-current.obs, " additional) observations to reach ", 
                 desired.certainty, "% certainty.",
                 sep=""))
  }
}

ReadX <- function(start.date, end.date, cohort) {
  x <- read.table(
    paste("http://dev-shaptonstahl.npr.org/shiny-data/ab-data.php?start=", start.date,
          "&end=", end.date,
          "&cohort=", cohort, sep=""))[,1]
  # filter out times more than 1 day
  x <- x[x < 60 * 60 * 24]
  return(x)
}  

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$obs.needed <- renderText({
    # Read the data
    x1 <- ReadX(input$start.date, end.date=input$end.date, cohort=input$cohort1)
    x2 <- ReadX(input$start.date, end.date=input$end.date, cohort=input$cohort2)

    needed.obs <- ObservationsNeeded(x1, x2, input$min.meaningful.effect, input$confidence)
    ObservationsNeededMessage(current.obs=length(x1) + length(x2), 
                              needed.obs=ObservationsNeeded(x1, x2, input$min.meaningful.effect, input$confidence),
                              desired.certainty=input$confidence)
  })

  output$distPlot <- renderPlot({
    # Read the data
    x1 <- ReadX(input$start.date, end.date=input$end.date, cohort=input$cohort1)
    x2 <- ReadX(input$start.date, end.date=input$end.date, cohort=input$cohort2)

    ce <- CohortEffect(x1, x2, input$min.meaningful.effect)
    PlotCohortEffect(ce)
  })
  
#  output$densityPlot <- renderPlot({
#    # Read the data
#    x1 <- ReadX(input$start.date, end.date=input$end.date, cohort=input$cohort1)
#    x2 <- ReadX(input$start.date, end.date=input$end.date, cohort=input$cohort2)
#    
#    plot(1, 1, 
#      type='n', 
#      xlim=c(min(c(x1, x2)), max(c(x1, x2))), 
#      ylim=c(0, max(c(density(x1)$y, density(x2)$y))),
#      main="Distributions of times for the Cohorts",
#      xlab="seconds",
#      ylab="")
#    lines(density(x1), lwd=3)
#    lines(density(x2), lwd=3, lty=2)
#    legend(.6 * max(c(x1, x2)),
#           max(c(density(x1)$y, density(x2)$y)),
#           legend=paste("cohort", c(input$cohort1, input$cohort2)),
#           lwd=3, lty=1:2)
#  })

})
