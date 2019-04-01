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

pops <- c("East Polynesian","West Polynesian", "European", "Asian")
batch <- 1:13

onStop(function() {
    dbDisconnect(con)
})

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Browse Intensities"),
    sidebarPanel(
        h3("Filter Markers"),
        numericInput("markerinfo_chr", value = 1, min = 1, label = "Chr"),
        numericInput("markerinfo_start", value = 1, min = 1, label = "Start"),
        numericInput("markerinfo_end", value = 1e6, min = 1, label = "End"),
        hr(),
        p("Currently not 'plugged-in:'"),
        h3("Filter Data"),
        radioButtons("sex_filter", label = "Sex", choices = list(All = "all", Male = "male", Female = "female")  ),
        selectInput("ancestry_filter", label = "Ancestry", choices = pops, selected = NULL, multiple = TRUE),
        selectInput("batch_filter", label = "QC Batch", choices = batch, selected = NULL, multiple = TRUE),

        h3("Plot Options"),
        checkboxGroupInput("plot_options", label = "Facet By", choices = c("Ancestry", "Batch", "Sex"))
    ),

    mainPanel(

        p("Click on a marker name"),
        fluidRow(
            dataTableOutput("markerinfo")),
        #dataTableOutput("testTable")
        fluidRow(
            plotOutput("intensityPlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    # marker table
    markerinfo_tbl <- reactive(tbl(con, "marker_info") %>%  select(chr, position, name) %>%
                                   filter(chr == input$markerinfo_chr, between(position, input$markerinfo_start, input$markerinfo_end)) %>%  select(name, chr, position) %>%
                                   arrange(position) #%>% as_tibble()
    )
    # joined dataset
    combined_tbl <- tbl(con, "combined")
    #coord <- str_replace_all(input$coord, "-|:", " ") %>% str_split(., " ") %>% unlist() %>% set_names(., c("chr", "start", "end"))


    # return the marker table from the db so that markers can be browsed
    output$markerinfo <- renderDataTable(
        datatable({markerinfo_tbl() %>%collect()}, selection = list( target = "row", mode = "single"),
                  rownames = FALSE))

    filtered_data <- reactive({

        marker <- "exm101"
        if(!is.null(input$markerinfo_cell_clicked$col)){
            #     output$text <- renderPrint(colnames(markerinfo_tbl()))
            if(colnames(markerinfo_tbl())[input$markerinfo_cell_clicked$col +1 ] == "name"){
                marker <- input$markerinfo_cell_clicked$value
            }
        }
        #output$text <- renderPrint(cell)

        marker_detail <- markerinfo_tbl() %>% filter(name == marker) %>% collect()
        output$text <- renderPrint(marker_detail)
        combined_tbl %>% filter(chr %in% marker_detail$chr, position %in% marker_detail$position) %>% collect()



    })

    output$testTable <- renderDataTable({DT::datatable(filtered_data() %>% head())})

    # PLot of the intensities for the chosen marker
    output$intensityPlot <- renderPlot({
        dat <- filtered_data()
        plot_title <- dat[["name"]][1]
        filtered_data()  %>% ggplot(aes(x = x, y = y, colour = gtype)) + geom_point() + ggtitle(plot_title) + theme_bw() + expand_limits(x = c(0,1), y = c(0,1))
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
