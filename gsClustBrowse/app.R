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
                         dataTableOutput("markerinfo"),
                         verbatimTextOutput(outputId = "cell")
                     )
            )
        )

    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    # marker table
    markerinfo_tbl <- reactive(tbl(con, "marker_info") %>%  select(chr, position, name) %>%
                                   filter(chr == input$markerinfo_chr) %>%
                                   arrange(position) #%>% as_tibble()
    )
    # joined dataset
    combined_tbl <- tbl(con, "combined")

    # return the marker table from the db so that markers can be browsed
    output$markerinfo <- renderDataTable(
        datatable({markerinfo_tbl() %>% collect()}, selection = list( target = "row"),
                  rownames = FALSE))

    filtered_data <- reactive({
        coords <- c(chr = input$chr, pos = input$pos)

        if(input$filter_by == "Marker name"){
            tmp_coords <- markerinfo_tbl() %>% filter(name == input$markername) %>% collect()
            #coords <- c(chr = tmp_coords$chr[[1]], pos = tmp_coords$pos[[1]])
        } # has to be position
        combined_tbl %>% filter(chr == coords[["chr"]] & position == coords[["pos"]]) %>% as_tibble()

    })

    # PLot of the intensities for the chosen marker
    output$intensityPlot <- renderPlot({
        plot_title <- filtered_data()[["name"]][1]
        filtered_data()  %>% ggplot(aes(x = x, y = y, colour = gtype)) + geom_point() + ggtitle(plot_title)
    })

    observeEvent(input$markerinfo_cell_clicked, {

        cell <- input$markerinfo_cell_clicked
        if(!is.null(cell$value)){
            if(colnames(markerinfo_tbl())[[cell$col +1 ]] == "name"){

                output$cell <- renderPrint(cell)
            }}
    })


}

# Run the application
shinyApp(ui = ui, server = server)
