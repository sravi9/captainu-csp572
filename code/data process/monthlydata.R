library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "chunkdb",
                 host = "captainu.c72j1qnfp0sw.us-east-1.rds.amazonaws.com", port = 5432,
                 user = "csp572", password = "captainUcsp572")
data <- dbGetQuery(con, "SELECT * from captainu.subscription_months")
write.csv(data,"C:/Users/sshss-pc/Desktop/monthlydata.csv")
