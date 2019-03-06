#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(config)

# get database configuration info from the config.yml file
dsn <- get("gsint")
# connect to the database
con <- dbConnect(odbc::odbc(),driver = dsn$driver, database = dsn$database , timeout = 10)

onStop(function() {
    dbDisconnect(con)
})

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Browse Intensities"),
    mainPanel(
        tabsetPanel(
            tabPanel(title = "Plots",
                     # Sidebar with a slider input for number of bins
                     sidebarLayout(
                         sidebarPanel(
                             radioButtons("filter_by", "Use Marker Name or Position",
                                          c("Marker name","Chr and position")),
                             textInput("markername", label = "Marker name", value = "1KG_1_14106394"),
                             numericInput("chr", label = "Chr", value = 1),
                             numericInput("pos", label = "Position", value = 14106394)

                         ),

                         # Show a plot of the generated distribution
                         mainPanel(
                             plotOutput("intensityPlot")
                         )
                     )
            ),
            tabPanel(title = "Marker Table",
                     mainPanel(
                         numericInput("markerinfo_chr", value = 1, min = 1, label = "Chr"),
                         dataTableOutput("marker_info")
                     )
            )
        )

    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    # marker table
    markerinfo_tbl <- tbl(con, "marker_info")
    # joined dataset
    combined_tbl <- tbl(con, "combined")

    # return the marker table from the db so that markers can be browsed
    output$marker_info <- renderDataTable(
        datatable({markerinfo_tbl %>%
            select(chr, position, name) %>%
            filter(chr == input$markerinfo_chr) %>%
            arrange(position) %>%
            as_tibble() }))

    filtered_data <- reactive(
        if(input$filter_by == "Marker name"){
        combined_tbl %>% filter(name == input$markername) %>% as_tibble()
    } else { # has to be position
        combined_tbl %>% filter(chr == input$chr & position == input$pos) %>% as_tibble()
    }
    )

    # PLot of the intensities for the chosen marker
    output$intensityPlot <- renderPlot({


        plot_title <- filtered_data()[["name"]][1]
        filtered_data()  %>% ggplot(aes(x = x, y = y, colour = gtype)) + geom_point() + ggtitle(plot_title)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
