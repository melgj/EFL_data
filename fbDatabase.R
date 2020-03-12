library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)
library(uuid)

# get URL

urlPath <- readline(prompt = "Enter URL to file: ")

# read csv file

d <- read_csv(urlPath, col_names = T)

# convert to json format and write to file

js <- d %>% jsonlite::toJSON()

write_json(js, "footballdata.json")

# If Time column is missing, replace with default value

if(!("Time" %in% colnames(d))) {
  d$Time <- c("15:00:00")
}

# paste Date and Time into one variable

d$Date_Time <- paste(d$Date, d$Time)

# Convert date_time into datetime object

d$Date_Time <- lubridate::dmy_hms(d$Date_Time)

# remove redundant vars

fbData <- d %>% select(-c(Date, Time))

# convert colnames to lower case and generate uuid's

colnames(fbData) <- tolower(colnames(fbData))

id <- sapply(1:nrow(fbData), UUIDgenerate)

# create id var and add to df

fbData$id <- id

# rename cols

fbData <- fbData %>% 
  rename(home_team = hometeam, away_team = awayteam)  %>% 
  select(id, div, date_time, home_team, away_team, fthg, ftag)

#head(fbData)

# uncomment below to write to csv file

# write_csv(fbData, "fbData.csv")

# Load postgresql library

library(RPostgreSQL)

# get postgres log in data

username <- readline(prompt = "Enter postgresql Username: ")
dbpw <- readline(prompt = "Enter postgresql password: ")

# loads Postgres driver
dbd <- dbDriver("PostgreSQL")

con <- dbConnect(dbd, dbname = "football_db",
                 host = "localhost", port = 5432,
                 user = username, password = dbpw)
rm(dbpw) # removes the password

# create table
fbtable <- "CREATE TABLE football(
    id          UUID          NOT NULL,
    div         VARCHAR(50)   NOT NULL,
    date_time   TIMESTAMP     NOT NULL,
    home_team   VARCHAR(50)   NOT NULL,
    away_team   VARCHAR(50)   NOT NULL,
    fthg        INT           DEFAULT(0),
    ftag        INT           DEFAULT(0),
    CONSTRAINT football_pk PRIMARY KEY (id));"

dbGetQuery(con, fbtable)

# check table created
dbExistsTable(con, "football")    

# write fbData dataframe to postgres football table

dbWriteTable(con, "football", value = fbData, append = TRUE, row.names = FALSE)    

#check it works

pgcheck <- dbGetQuery(con, "SELECT * from football")

print(pgcheck)

dbDisconnect(con)
dbUnloadDriver(dbd)

  
