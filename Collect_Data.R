library(tidyverse)

# Download data for top four divisions from 2010-2020 inc.

yrSlug1 <- 10:19
yrSlug2 <- 11:20

t <- as.vector(paste0(yrSlug1, yrSlug2))

divSlug <- c("E0", "E1", "E2", "E3")

divSlug

baseURL <- "https://www.football-data.co.uk/mmz4281/"

for (ssn in seq_along(t)){
  for (div in seq_along(divSlug)){
    urlPath <- paste0(baseURL,t[ssn],"/",divSlug[div],".csv")
    snData <- read_csv(urlPath, col_names = T)
    snData$Season <- t[ssn]
    write_csv(snData, paste0(t[ssn],divSlug[div],".csv"))
  }
}

allCSV <- list.files(pattern="*.csv") %>%
  map_df(~read_csv(., col_types = cols(.default = "c")))

colnames(allCSV)

# Write data to csv file

write_csv(allCSV, "FootballData_AllSeasons.csv")



