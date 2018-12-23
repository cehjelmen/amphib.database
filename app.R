library(shiny)
library(shinythemes)
library(viridis)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "sandstone.css",
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
   sidebarLayout(
      sidebarPanel(
        width = 3,
        img(src = "./amphibs.png", width = 150, align = "center"),
        h4("Amphibian Karyotype"),
        h4("Database"),
        br(),
        tags$style(type = "text/css", "label { font-size: 10px; }"),        
        selectInput("ord", "Select an order:", 
                    c("All orders", "Anura",
                      "Gymnophiona", "Urodela"),selected="All orders"),
        uiOutput("fam"),
        uiOutput("gen"),
        br(),
        uiOutput("totals")
      ),
      mainPanel(
        tabsetPanel(
          
          tabPanel("Information", div(HTML("
                                           <br>
                                           <br>
                                           A karyotype is a highly variable and complex trait that can reveal changes in genome organization, uncover phylogenetic history, and distinguish cryptic species. The current database contains 1,947 Amphibian karyotypes which we have compiled from 194 publications. This website allows you to retrieve data for any order, family, or genus of interest. Once you have selected your dataset, you can either plot it interactively or build a customized table of your results.  In the plot tab, use the drop-down menus to select which variables to plot and subset the data. In the table tab, use the checkboxes to include each type of data in the table and the download button to retrieve a CSV file containing the selected data.
                                           <br>
                                           <br>
                                           <b>Submitting Data:</b> If you are aware of any available records that should be added to the database please email us and we will incorporate the missing data.
                                           <a href='mailto:hblackmon@bio.tamu.edu'>Contact Us</a><br>
                                           <br>
                                           Data taken from the database must not be reproduced in published lists, online databases, or other
                                           formats, nor redistributed without permission. The information in this database is provided
                                           solely for personal and academic use, and must not be used for the purposes of financial gain.
                                           <br>
                                           <br>
                                           <b>The database should be cited as follows:</b>
                                           <br>
                                           <a href='website' target='_blank'> Perkins R, R.H. Adams, H. Blackmon. The Amphibian Karyotype Database. XXXXX. XX:XX. XX-XX</a>
                                           <br>
                                           <br>
                                           Current version of the database is 0.1 last updated 17 October 2018."), style = "font-size:100%")),

          tabPanel("Plot", 
                   uiOutput("Xplot", inline=T), 
                   uiOutput("Yplot", inline=T), 
                   uiOutput("Colby", inline=T), 
                   plotOutput("distPlot")
                   ), 
          
          tabPanel("Table",
                   checkboxGroupInput(inputId = "col.pic", label = "Choose data to include", 
                                      choices = c("Taxonomy",
                                                  "Diploid number",
                                                  "Fundamental number",
                                                  "Sex chromosomes",
                                                  "Ploidy level",
                                                  "B chromosomes",
                                                  "Microchromosomes",
                                                  "Notes",
                                                  "Other names",
                                                  "Citation"
                                      ), selected = c("Taxonomy","Diploid number","Fundamental number"),
                                      inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   downloadButton("downloadData", "Download"),
                   div(tableOutput("selTable"), 
                       style = "font-size:80%"))
          
          

          
        )
      )
   )
)

# Define server logic 
server <- function(input, output) {
  # read in the data
  dat <- read.csv("amphib.csv", as.is=T, check.names=F)
   
  # subsets data based on order selection
  target.dat <- reactive({
     a <- dat
     if(input$ord != "All orders") a <- dat[dat$Order == input$ord, ]
     return(a)
   })
  # generates a vector of current family names
  fam <- reactive(sort(unique(target.dat()$Family)))
  
  # subsets data based on family selection
  target.dat2 <- reactive({
    a <- target.dat()
    if(input$fam != "All families") a <- a[a$Family == input$fam, ]
    return(a)
  })
  # generates a vector of current genus names
  gen <- reactive(sort(unique(target.dat2()$genus)))
  
  # this makes the family dropdown menu
  output$fam <- renderUI({
    selectInput("fam", "Select a family:", choices = c("All families", fam()))
  })
  
  # this makes the genus dropdown menu
  output$gen <- renderUI({
    selectInput("gen", "Select a genus:", choices = c("All genera", gen()))
  })
  
  # make final data selection function
  final.dat <- reactive({
    a <- target.dat()
    if(input$fam != "All families") a <- a[a$Family == input$fam, ]
    if(input$gen != "All genera") a <- a[a$genus == input$gen, ]
    return(a)
  })
  
  output$totals <- renderText({
    paste(nrow(final.dat()),"records available")
  })
  
  col.dat <- reactive({
    x <- 5
    if("Taxonomy" %in% input$col.pic) x <- c(1, 2, x)
    if("Diploid number" %in% input$col.pic) x <- c(x, 6)
    if("Fundamental number" %in% input$col.pic) x <- c(x, 7)
    if("Sex chromosomes" %in% input$col.pic) x <- c(x, 8)
    if("Ploidy level" %in% input$col.pic) x <- c(x, 9)
    if("B chromosomes" %in% input$col.pic) x <- c(x, 10)
    if("Microchromosomes" %in% input$col.pic) x <- c(x, 11)
    if("Notes" %in% input$col.pic) x <- c(x, 12)
    if("Other names" %in% input$col.pic) x <- c(x, 13)
    if("Citation" %in% input$col.pic) x <- c(x, 14)
    return(x)
  })
  
  col.plot <- reactive({
    x <- vector()
    if("Diploid number" %in% input$Xvar) x <- 6
    if("Fundamental number" %in% input$Xvar) x <- 7
    if("Sex chromosomes" %in% input$Xvar) x <- 8
    if("Ploidy level" %in% input$Xvar) x <- 9
    if("B chromosomes" %in% input$Xvar) x <- 10
    if("Microchromosomes" %in% input$Xvar) x <- 11
    if("Diploid number" %in% input$Yvar) x <- c(x, 6)
    if("Fundamental number" %in% input$Yvar) x <- c(x, 7)
    if("Ploidy level" %in% input$Yvar) x <- c(x, 9)
    if("B chromosomes" %in% input$Yvar) x <- c(x, 10)
    if("Microchromosomes" %in% input$Yvar) x <- c(x, 11)
    if("Order" %in% input$Colby) x <- c(x, 1)
    if("Family" %in% input$Colby) x <- c(x, 2)
    if("Sex Chromosomes" %in% input$Colby) x <- c(x, 8)
    return(x)
  })
  
  
  output$Xplot <- renderUI({
    selectInput("Xvar", "choose X variable", 
                choices=c("Diploid number",
                          "Fundamental number",
                          "Sex chromosomes",
                          "Ploidy level",
                          "B chromosomes",
                          "Microchromosomes"), 
                selected="Diploid number")
  })
  
  output$Yplot <- renderUI({
    selectInput("Yvar", "choose Y variable", 
                choices = c("None",
                            "Diploid number",
                            "Fundamental number",
                            "Ploidy level",
                            "B chromosomes",
                            "Microchromosomes"))
  })
  
  
  output$Colby <- renderUI({
    selectInput("Colby", "color plot by", 
                choices = c("Order",
                            "Family",
                            "Sex Chromosomes"), 
                selected = "Order")
  })
  
  
  
   # this just makes a simple histogram of selected data
   output$distPlot <- renderPlot({
     dat <- final.dat()[,col.plot()] 
     dat <- dat[complete.cases(dat),]
     if(ncol(dat)==2){
       if(input$Xvar=="Sex chromosomes"){
         dat <- dat[dat[, 1] != "", ]
         g <- ggplot(dat, aes(x=dat[,1], fill=dat[,2])) +
           geom_bar() +
           xlab(input$Xvar) + 
           guides(fill=guide_legend(title=input$Colby))
       }else{
       g <- ggplot(dat, aes(x=dat[,1], fill=dat[,2])) + 
         geom_histogram() +
         xlab(input$Xvar) + 
         guides(fill=guide_legend(title=input$Colby))
       }
     }
     if(ncol(dat)==3){
       g <- ggplot(dat, aes(x=dat[,1], y=dat[,2],color=dat[,3])) +
         geom_jitter() +
         ylab(input$Yvar) + 
         xlab(input$Xvar) + 
         guides(color=guide_legend(title=input$Colby))
     }
     g
   })
   
   output$selTable <- renderTable(final.dat()[,col.dat()],
                                  na = "",
                                  striped = T)
   
   
   
   output$downloadData <- downloadHandler(
     filename = paste("amphib-", Sys.Date(), ".csv", sep=""),
     content = function(file) {
       write.csv(final.dat()[,col.dat()], file, row.names = FALSE)
     }
   )

}

# Run the application 
shinyApp(ui = ui, server = server)






