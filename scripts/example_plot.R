library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(config)

# get database configuration info from the config.yml file
dsn <- get("gsint")
# connect to the database
con <- dbConnect(odbc::odbc(),driver = dsn$driver, database = dsn$database , timeout = 10)

# combined is a database view that has the table joins already specified
combined_tbl <- tbl(con, "combined")

# query the intensities for a marker
combined_tbl %>% filter(name == "1KG_1_14106394") %>% ggplot(aes(x = x, y = y, colour = gtype)) + geom_point()
