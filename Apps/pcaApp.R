#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


# original code from App https://github.com/MerrimanLab/PCAplotting
# Made by Ruth Topless, adapted by Murray Cadzow

library(shiny)
library(shinythemes)
library(utils)
library(ggrepel) # label points
library(RColorBrewer)
library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(config)

# get database configuration info from the config.yml file
dsn <- get("psql_ro")
# connect to the database
con <- dbConnect(odbc::odbc(),
                 driver = dsn$driver,
                 database = dsn$database,
                 servername = dsn$server,
                 port = dsn$port,
                 UID = dsn$user,
                 PWD = dsn$password,
                 timeout = 100)


ggplot2::theme_set(theme_bw())

PCAglobal <- tbl(con, "sample") %>% filter(!withdrawn) %>% select(samplecode, ends_with("id"),contains("ethnicity"), contains("ancestry"), starts_with("pc")) %>% collect()
PCAlist <- c("pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10")
ancestrybroad <- unique(PCAglobal$ancestry_broad)
ancestrybroad_colours <- RColorBrewer::brewer.pal(length(ancestrybroad), "Set1")
names(ancestrybroad_colours) <- ancestrybroad


##################################
# Define UI
ui = fluidPage(

  theme = shinytheme("flatly"),
  headerPanel(h1('PCA clustering', align = "center")),
  sidebarPanel(
    fluidRow(
      column(width = 4,
             selectInput('xcol', 'X Variable Plot1', PCAlist),
             selectInput('ycol', 'Y Variable Plot1', PCAlist, selected = PCAlist[2]),
             selectInput('ancestryclass', 'Choose ancestral groups to include', choices = names(ancestrybroad), multiple = TRUE ),

             hr(),
             textInput("highlight_subjects", "Type in subject IDs to highlight on plot (comma separated)")
      ))),
  column(width = 8,
         plotOutput('plot1', width = "80%", height = "900px",
                    click = clickOpts(id = "plot1_click"),
                    brush = brushOpts(id = "plot1_brush")
         ),
  fluidRow(

    h4("Highlighted subject data"),
    tableOutput("subjectdata"),

    hr(),
    h4("Points near click in plot1"),
    tableOutput("click_info"),



         textInput("filenameplot1", "File name for download of Plot1", placeholder = "Plot1"),
         downloadButton('downloadDataPlot1', 'Download Data from Plot1'),
         downloadButton('downloadImagePlot1', 'Download Image from Plot1')
  )
),
hr(),
fluidRow(
  column(width = 12,
         h4("Brushed points from plot1 which are fed into plot2"),
         dataTableOutput("brush_info")
  )
),

hr(),


fluidRow(
  column(width = 4,
         selectInput('xcol2', 'X Variable Plot2', PCAlist, selected = PCAlist[3]),
         selectInput('ycol2', 'Y Variable Plot2', PCAlist, selected = PCAlist[4]),
         hr(),
         h4("Points near click for plot2"),
         tableOutput("click_info2")
  ),
  column(width = 8,
         plotOutput('plot2', width = "80%", height = "900px",
                    click = clickOpts(id = "plot2_click"),
                    brush = brushOpts(id = "plot2_brush")
         ),
         textInput("filenameplot2", "File name for download of Plot2", placeholder = "Plot2"),
         downloadButton('downloadDataPlot2', 'Download Data from Plot2'),
         downloadButton('downloadImagePlot2', 'Download Image from Plot2')
  )
),

fluidRow(
  column(width = 12,
         h4("Brushed points from plot2"),
         dataTableOutput("brush_info2")
  )
)
)



####################################################################################################
server <- function(input, output, session) {


  # define the colour scale to be used for plotting
  ancs_col_scale <- scale_colour_manual(values = ancestrybroad_colours,
                                       name = "Ancestries (Broad)"
  )

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    ancs <- input$ancestryclass
    if (is.null(ancs)) ancs <- ancestrybroad

    PCAglobal %>%
      select(assigned_uniqueid, ethnicity_specific, ethnicity_broad, !!input$xcol, !!input$ycol, ancestry_broad, ancestry_specific) %>%
      filter(ancestry_broad %in% ancs)

  })


  highlight_sub <- reactive({
    subj <- gsub(pattern = " ", replacement = "", input$highlight_subjects)
    subjectlist <- as.data.frame(strsplit(subj, ","))
    colnames(subjectlist)[1] <- "assigned_uniqueid"

    PCAglobal %>%
      select(assigned_uniqueid, ethnicity_specific, ethnicity_broad, !!input$xcol, !!input$ycol, ancestry_specific) %>%
      filter(assigned_uniqueid %in% subjectlist$assigned_uniqueid)

  })

  output$subjectdata <- renderTable({highlight_sub()}, bordered = TRUE)

  plotInput <- reactive({
    dat <- selectedData() %>% rename(xcol = input$xcol, ycol = input$ycol)

    subj <- gsub(pattern = " ", replacement = "", input$highlight_subjects)

    subjectlist <- as.data.frame(strsplit(subj, ","))

    colnames(subjectlist)[1] <- "assigned_uniqueid"

    highlight <- dat %>% filter(assigned_uniqueid %in% subjectlist$assigned_uniqueid)

    dat %>%
      ggplot(., aes(x = xcol, y = ycol, colour = ancestry_broad)) +
      geom_point() +
      ancs_col_scale + # the colour scale defined further up
      xlab(input$xcol) +
      ylab(input$ycol) +
      ylim(c(min(PCAglobal[, input$ycol]), max(PCAglobal[, input$ycol]) )) +
      xlim(c(min(PCAglobal[, input$xcol]), max(PCAglobal[, input$xcol]) )) +
      theme(legend.position = 'bottom',
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.7))) +
      geom_point(data = highlight, shape = 18, colour = 'black', size = 5) +
      geom_text_repel(data = highlight,aes(x = xcol, y = ycol, label = assigned_uniqueid),
                      colour = "black",
                      fontface = 'bold',
                      nudge_x = 0.002,
                      nudge_y = 0.002 )
  })

  output$plot1 <- renderPlot({
    plotInput()
  })

  output$downloadDataPlot1 <- downloadHandler(
    filename = function() {paste(input$filenameplot1,input$xcol,input$ycol, '.csv', sep = '')},
    content = function(file) {write.csv(selectedData(), file)})

  output$downloadImagePlot1 <- downloadHandler(
    filename = function() { paste(input$filenameplot1,input$xcol,input$ycol, '.png', sep = '') },
    content = function(file) {ggsave(file,plotInput(), device = 'png', width = 12, height = 12, units = "in", dpi = 300)})

  output$click_info <- renderTable({
    dat <- selectedData()
    nearPoints(dat[,c("assigned_uniqueid",input$xcol,input$ycol,"ethnicity_specific", "ethnicity_broad","ancestry_broad","ancestry_specific")], input$plot1_click, xvar = input$xcol, yvar = input$ycol, addDist = FALSE)},
    bordered = TRUE
  )

  plot1brushselected <- reactive({
    dat <- selectedData()
    brushedPoints(dat[,c("assigned_uniqueid",input$xcol,input$ycol, "ethnicity_specific", "ethnicity_broad","ancestry_broad","ancestry_specific")], input$plot1_brush, xvar = input$xcol, yvar = input$ycol)})

  output$brush_info <- renderDataTable({plot1brushselected()},
                                       #options = list(lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All')), pageLength = 10, autoWidth=FALSE)
                                       options = list(scrollY = "300px", scrollCollapse = TRUE, paging = FALSE, searching = TRUE) #for vertical scrolling
  )


  plot1brushselected2 <- reactive({
    dat <- selectedData()
    dat2 <- PCAtestdata %>% filter(assigned_uniqueid %in% dat$assigned_uniqueid)
    return(
      brushedPoints(dat2[,c("assigned_uniqueid",input$xcol,input$ycol,input$xcol2,input$ycol2, "ethnicity_specific", "ethnicity_broad","ancestry_broad","ancestry_specific")],
                    input$plot1_brush,
                    xvar = input$xcol,
                    yvar = input$ycol))
  })

  plotInput2 <- reactive({
    dat3 <- plot1brushselected2() %>% rename(xcol = input$xcol2, ycol = input$ycol2)
    subj <- gsub(pattern = " ", replacement = "", input$highlight_subjects)
    subjectlist <- as.data.frame(strsplit(subj, ","))
    colnames(subjectlist)[1] <- "assigned_uniqueid"
    highlight <- dat3 %>% filter(assigned_uniqueid %in% subjectlist$assigned_uniqueid)
    dat3 %>%
      ggplot(., aes(x = xcol, y = ycol, colour = ancestry_broad) ) +
      geom_point() +
      xlab(input$xcol2) +
      ylab(input$ycol2) +
      ancs_col_scale +
      ylim(c(min(PCAtestdata[, input$ycol2]), max(PCAtestdata[, input$ycol2]) )) +
      xlim(c(min(PCAtestdata[, input$xcol2]), max(PCAtestdata[, input$xcol2]) )) +
      theme(legend.position = 'right',
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.7))) +
      geom_point(data = highlight, shape = 18, colour = 'black', size = 5) +
      geom_text_repel(data = highlight,
                      aes(x = xcol, y = ycol,  label = assigned_uniqueid),
                      colour = "black",
                      fontface = 'bold',
                      nudge_x = 0.002,
                      nudge_y = 0.002)
  })

  output$plot2 <- renderPlot({
    plotInput2()
  })

  output$downloadDataPlot2 <- downloadHandler(
    filename = function() {paste(input$filenameplot2,input$xcol2,input$ycol2, '.csv', sep = '')},
    content = function(file) {write.csv(plot1brushselected2(), file)})

  output$downloadImagePlot2 <- downloadHandler(
    filename = function() { paste(input$filenameplot2,input$xcol2,input$ycol2, '.png', sep = '') },
    content = function(file) {ggsave(file,plotInput2(), device = 'png', width = 12, height = 12, units = "in", dpi = 300)})


  output$click_info2 <- renderTable({
    dat3 <- plot1brushselected2()
    nearPoints(dat3[,c("assigned_uniqueid",input$xcol2,input$ycol2,"ethnicity_specific", "ethnicity_broad","ancestry_broad","ancestry_specific")],
               input$plot2_click,
               xvar = input$xcol2,
               yvar = input$ycol2,
               addDist = FALSE)
  }, bordered = TRUE)

  plot2brushselected <- reactive({
    dat3 <- plot1brushselected2()
    brushedPoints(dat3[,c("assigned_uniqueid",input$xcol2,input$ycol2,"ethnicity_specific", "ethnicity_broad","ancestry_broad","ancestry_specific")],
                  input$plot2_brush,
                  xvar = input$xcol2,
                  yvar = input$ycol2)
  })


  output$brush_info2 <- renderDataTable({plot2brushselected() },
                                        options = list(lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All')),
                                                       pageLength = 10,
                                                       autoWidth = FALSE)
  )

} #end of server

# Run the application
shinyApp(ui = ui, server = server)
