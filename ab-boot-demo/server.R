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
#  withProgress(message='Calculating', detail=paste('part 0 of', n.iter), value=0, max=n.iter, {
    for(i in 1:n.iter) {
#      setProgress(i, detail=paste('part', i, 'of', n.iter))
      draws <- c(draws, OneDraw())
    }
#  })
  
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
  if(median(draws) > 0) {
    win.cohort <- cohort1
    lose.cohort <- cohort2
    win.draws <- draws
  } else {
    win.cohort <- cohort2
    lose.cohort <- cohort1
    win.draws <- -draws
  }
  win.probs <- Probs(win.draws, mme)
  
  if(max(win.probs) < certainty.fraction) {
    title <- paste(win.cohort, " is ahead of ", lose.cohort, ", but it is not yet conclusive", sep="")
  } else {
    if(win.probs['positive'] > certainty.fraction) {
      title <- paste(win.cohort, " beat ", lose.cohort, sep="")
    } else {
      title <- "It's a tie"
    }
  }
  
  xlim <- c(min(-mme, min(win.draws)), max(mme, max(win.draws)))
  
  xlab <- paste("Sample size = ", n.x, " + ", n.y, 
                ".  The difference between cohorts is about ", round(median(win.draws)), " seconds", sep="")
  
  h <- hist(win.draws, 50, xlim=xlim,
            main=title, 
            xlab=xlab)
  abline(v=c(mme, -mme), lty=2, lwd=2)
  legend(x=.1 * (xlim[2] - xlim[1]) + xlim[1],
         y=.8 * max(h$counts),
         legend=c(paste(win.cohort, " wins: ", round(win.probs['positive']*100), "%", sep=""),
                  paste("It's a tie: ", round(win.probs['near.zero']*100), "%", sep=""),
                  paste(lose.cohort, " wins: ", round(win.probs['negative']*100), "%", sep="")),
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
  
  output$debug <- renderText({
    paste(paste(x1(), collapse=", "), " |-----| ", paste(x2(), collapse=", "))
  })
})
