#' Download and save cohorts and platforms for quick lookup by A/B testing shiny app

library(RMySQL)

# initialize database connection
mysql.driver <- dbDriver("MySQL")
con <- dbConnect(mysql.driver, group="stage4-infinite")

# initialize the cohort list
cohort.sql <- "SELECT ratings_cohort  FROM infinite.user_ratings  GROUP BY ratings_cohort"
rs <- dbSendQuery(con, cohort.sql)
cohort.list.mysql <- fetch(rs, n=-1)
if(0==ncol(cohort.list.mysql)) {
  cohort.list <- ""
} else {
  cohort.list <- cohort.list.mysql[,1]
}

# initialize platform list
platform.sql <- "SELECT ratings_platform FROM user_ratings GROUP BY ratings_platform"
rs <- dbSendQuery(con, platform.sql)
platform.list.mysql <- fetch(rs, n=-1)
if(0==ncol(platform.list.mysql)) {
  platform.list <- ""
} else {
  platform.list <- platform.list.mysql[,1]
}

cohort.list <- cohort.list[!is.na(cohort.list)]
platform.list <- platform.list[!is.na(platform.list)]

write.csv(data.frame(cohort=cohort.list), 
          file='~/www/shiny-data/ab-cohorts.csv',
          quote=TRUE,
          row.names=FALSE)

write.csv(data.frame(platform=platform.list),
          file='~/www/shiny-data/ab-platforms.csv',
          quote=TRUE,
          row.names=FALSE)
