# demo data for Shiny app ab-boot-demo

n.a <- 200
n.b <- 300
mean.a <- 125
mean.b <- 125.5

n.rows <- max(c(n.a,n.b))

data <- data.frame(cohort.a=rep(NA_integer_, n.rows), 
                   cohort.b=rep(NA_integer_, n.rows))
data$cohort.a[1:n.a] <- rnorm(n.a, mean=mean.a)
data$cohort.b[1:n.b] <- rnorm(n.b, mean=mean.b)

write.table(data, 
            file="~/Box Sync/work/R/ab-boot-demo/cohorts_a_and_b.txt",
            quote=FALSE,
            sep='\t',
            row.names=FALSE,
            col.names=TRUE)

