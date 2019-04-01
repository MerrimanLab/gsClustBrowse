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


#query intensities for a marker
combined_tbl %>% filter(name =="1KG_1_14106394") %>% head()

# plot function for a single marker
plot_intensities <- function(marker){
  combined_tbl %>% filter(name == marker) %>%
    ggplot(aes(x = x, y = y, colour = gtype)) +
    geom_point() +
    ggtitle(marker) +
    theme_bw() +
    expand_limits(x = c(0,1), y = c(0,1))
}


# plot the intensities for a marker
plot_intensities(marker = "1KG_1_14106394")

# plot multiple markers
markers <- c("1KG_1_14106394", "exm101", "exm104")
purrr::map(markers, ~ plot_intensites(marker = .x))
