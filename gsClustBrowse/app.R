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

# define genotype colour scale
gt_colours <- scale_color_manual( values=c("NC" = "grey", "BB" = "green", "AB" = "blue", "AA" ="red"))
sex_colours <- scale_color_manual(values = c("Female" = "orange", "Male" = "purple", "Missing" = "lightgrey", "Unknown" = "lightgrey"))

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

        h3("Filter Data"),
        radioButtons("sex_filter", label = "Sex", choices = list(All = "all", Male = "Male", Female = "Female")  ),
        selectInput("ancestry_filter", label = "Ancestry", choices = pops, selected = NULL, multiple = TRUE),
        selectInput("batch_filter", label = "QC Batch", choices = batch, selected = NULL, multiple = TRUE),

        h3("Plot Options"),
        radioButtons("coord_options", label = "Axes", choices = list(`Theta/R` = "theta_r", `X/Y` = "xy", `X/Y Raw` = "xy_raw")  ),
        radioButtons("facet_options", label = "Facet By", choices = list(None = "none", Ancestry = "ancestry", `Reported Sex` = "reported_sex", `Genetic Sex` = "genetic_sex")),
        radioButtons("colour_options", label = "Colour", choices = list(Gtype = "gtype", `Reported Sex` = "reported_sex", `Genetic Sex` = "genetic_sex", Ancestry = "ancestry"),   )
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
        out_dat <- combined_tbl %>% filter(chr %in% marker_detail$chr, position %in% marker_detail$position) %>% collect()
        if(input$sex_filter != "all"){
            out_dat <- out_dat %>% filter(genetic_sex == input$sex_filter | reported_sex == input$sex_filter)
        }
        out_dat %>% mutate(reported_sex = case_when(reported_sex == "Male" ~ "Male", reported_sex == "Female"~ "Female", is.na(reported_sex) | reported_sex == "" ~ "Missing"), genetic_sex = case_when(genetic_sex == "Male" ~ "Male", genetic_sex == "Female"~ "Female", (is.na(genetic_sex) | genetic_sex == "" | genetic_sex == "Unknown" | is.null(genetic_sex)) ~ "Unknown"))

    })

    output$testTable <- renderDataTable({DT::datatable(filtered_data())})

    # PLot of the intensities for the chosen marker
    output$intensityPlot <- renderPlot({
        dat <- filtered_data()#%>% mutate(gtype = forcats::as_factor(gtype) %>% forcats::lvls_expand(.,c("AA","AB","BB", "NC")))

        if(!is.null(input$batch_filter)){
            dat <- dat %>% filter(batchid %in% input$batch_filter)
        }
        selected_colour <- input$colour_options
        plot_title <- dat[["name"]][1]

        # coordinate plotting options
        p <- dat  %>% ggplot(aes(x = theta, y = r))
        if(input$coord_options == "xy"){
            p <-  dat  %>% ggplot(aes(x = x, y = y))
        } else if(input$coord_options == "xy_raw"){
            p <-  dat  %>% ggplot(aes(x = x_raw, y = y_raw))
        }

        # facetting
        if(input$facet_options == "none"){
            p <- p + facet_wrap(~ batchid)
        } else {
            facet <- paste0(input$facet_options, "~ batchid")
           p <- p + facet_grid( facet)
        }

        # colours
        if(input$colour_options == "gtype"){
            p <- p + geom_point(aes_string(colour = input$colour_options), alpha = 0.7) + gt_colours
        } else {
           p <-p + geom_point(aes_string(colour = input$colour_options), alpha = 0.7) + sex_colours
        }

        p <- p + ggtitle(plot_title) + theme_bw() + expand_limits(x = c(0,1), y = c(0,1))

        p
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
