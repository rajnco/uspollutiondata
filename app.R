library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(lubridate)
library(reshape2)

ReadDataFile <- function(){
    datDf <- read.csv(unz( "uspollution.zip", "pollution_us_2000_2016.csv"), header = TRUE, stringsAsFactors = FALSE)
    #datDf <- read.csv("/home/raj/altoona.csv", header = TRUE, stringsAsFactors = FALSE)
    datDf %<>% mutate(DateAsDate = as.Date(Date.Local))
    return(datDf)
}

GroupData <- function(datDf=datDf){    # group by City and Date
    datDf %<>% group_by(City, DateAsDate) %>% summarise(NO2.Mean=mean(NO2.Mean), SO2.Mean=mean(SO2.Mean), CO.Mean=mean(CO.Mean), O3.Mean=mean(O3.Mean))
    return(datDf)
}

SplitByCityYear<- function(datDf=datDf, City, MinYear, MaxYear){
    x <- datDf %>% filter(datDf$City==City && year(datDf$Date.Local) >= MinYear && year(datDf$Date.Local) <= MaxYear)
    return(x)
}

datDf <- ReadDataFile()   
datDf <- GroupData(datDf=datDf)
City <- datDf['City'] %>% distinct()
datDf$CO.Mean <- datDf$CO.Mean * 1000
datDf$O3.Mean <- datDf$O3.Mean * 1000
MinDate <- min(datDf$DateAsDate)
MaxDate <- max(datDf$DateAsDate)
MinYear <- year(MinDate)
MaxYear <- year(MaxDate)



ui <- fluidPage(
    titlePanel("US Air Quality - 2000 to 2016"),
    sidebarLayout(
        sidebarPanel(
            hr(),         
            sliderInput(inputId = "sliderYear", label = "Years", min = MinYear, max = MaxYear, value = c(MinYear,MinYear+1),  step = 1, sep=""),
            hr(),
            selectInput(inputId = "selectCity", label = "City", choices = City,  selected = NULL),
            hr(),
            checkboxGroupInput(inputId = "pollutantId", label = "Pollutant", 
                               choices = list("NO2"="NO2.Mean", "SO2"="SO2.Mean", "CO"="CO.Mean", "O3"="O3.Mean"),
                               selected = list("NO2"="NO2.Mean")),
            hr()
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", 
                         uiOutput("DateRange"),
                         verbatimTextOutput("Summary"),
                         textOutput("TextDisplay"), 
                         #textOutput("TextDisplay2"), 
                         #textOutput("TextDisplay3"),
                         #textOutput("TextDisplay4"),
                         #textOutput("Variable"), 
                         #textOutput("Value"),
                         dataTableOutput("ViewData"),
                         tableOutput("ViewTable")
                         
                         ),
                tabPanel("Graph GGPlot ",
                         uiOutput("ggSelectCity"),
                         plotOutput("ggplot2"),
                         plotOutput("ggplot2SelectedCity")
                         #plotOutput("ggplot2lm")
                         
                         ),
                tabPanel("Graph Plotly",
                         uiOutput("pltSelectCity"),
                         plotlyOutput("pltyggplot2"),
                         plotlyOutput("pltyggplot2SelectedCity")
                         #plotlyOutput("pltyggplot2lm")
                         ),
                tabPanel("Box Plot - Year",
                         plotOutput("BoxPlotTab4")
                )
            )
        )
    )
)



 

SummaryTab <- function(datDf=datDf, input, output, session){
    output$DateRange <- renderUI({
        dateRangeInput(inputId = "dateId", label = "Date Range", start = paste(c(input$sliderYear[1], 01, 01), collapse = '-'), end = paste(c(input$sliderYear[2], 12, 31), collapse = '-'))
    })
    
    #output$TextDisplay  <- renderText({ input$sliderYear })
    #output$TextDisplay2 <- renderText({ input$selectCity })
    #output$TextDisplay3 <- renderText({ paste(as.character(input$dateId ), collapse = " To ") })
    #output$TextDisplay4 <- renderText({ input$pollutantId })
    
    
    #citylist = reactive({ paste(Data['City'] %>% distinct(), Data['City'] %>% distinct(), sep="=", collapse = ",") })
    pd <- reactive({ datDf %>% select(City, DateAsDate, input$pollutantId[!is.na(input$pollutantId)])  %>% filter(City == input$selectCity & DateAsDate >= as.Date(as.character(input$dateId[1])) & DateAsDate <= as.Date(as.character(input$dateId[2])))     })
    
    output$Summary <- renderPrint({summary(pd())})
    #output$TextDisplay  <- renderText({ summary(pd()) })
    output$ViewData <- renderDataTable(pd())
    #output$ViewTable <- renderTable(summary(pd()), rownames = TRUE,  outputArgs = list(input$pollutantId[!is.na(input$pollutantId)]))
    #output$ViewTable <- renderTable({ summary(pd()) })
    
}

ScatterPlot <- function(datDf=datDf, input, output, session){
    
    output$ggSelectCity  <- renderUI({ 
        selectInput("ggCity2Compare", label = "City to Compare", choices = City, selected = City[1] )
    }) 
    output$pltSelectCity <- renderUI({ 
        selectInput("pltCity2Compare", label = "City to Compare", choices = City, selected = City[1] ) 
    })
    
    ggCityDat <- reactive({ datDf %>% select(City, DateAsDate, input$pollutantId[!is.na(input$pollutantId)]) %>% filter(City == input$ggCity2Compare & DateAsDate >= as.Date(as.character(input$dateId[1])) & DateAsDate <= as.Date(as.character(input$dateId[2])))  })
    pltCityDat <- reactive({ datDf %>% select(City, DateAsDate, input$pollutantId[!is.na(input$pollutantId)]) %>% filter(City == input$pltCity2Compare & DateAsDate >= as.Date(as.character(input$dateId[1])) & DateAsDate <= as.Date(as.character(input$dateId[2])))  })
    
    #output$TextDisplay  <- renderText({ input$pollutantId })
    
    pd <-  reactive({   datDf %>% select(City, DateAsDate, input$pollutantId[!is.na(input$pollutantId)])  %>% filter(City == input$selectCity & DateAsDate >= as.Date(as.character(input$dateId[1])) & DateAsDate <= as.Date(as.character(input$dateId[2])))     })
    
    d <- reactive({ melt(as.data.frame(pd()), id.vars = c("City", "DateAsDate") ) })
    d0 <- reactive({ melt(as.data.frame(pd()), id.vars = c("City", "DateAsDate") ) })
    d1 <- reactive({ melt(as.data.frame(ggCityDat()), id.vars = c("City", "DateAsDate") ) })
    d2 <- reactive({ melt(as.data.frame(pltCityDat()), id.vars = c("City", "DateAsDate") ) })

    #output$Variable <- renderText(d1()$variable)
    #output$Variable <- renderText(input$ggCity2Compare)
    #output$Value    <- renderText(d1()$value)
    #output$ViewTable <- renderTable(ggCityDat())
    
    #q <- reactive ({ 
    #    ggplot(data=d(), aes(DateAsDate, value, col=variable, colour=input$pollutantId[!is.na(input$pollutantId)]))  + 
    #        geom_point() + 
    #        labs(x=" Date ", y = "Pollutant")
    #    })
    
    q <- reactive({
        ggplot(data=d(), aes(x = DateAsDate, y = d()$value, color = d()$variable))  +
            labs(title = paste("Scatter Plot - ", input$selectCity, collapse = " "), subtitle = "Date Vs Pollutants", x = "Date", y = "Pollutant", colour = "Gas") +
            geom_point() #+ #+ geom_smooth() 
            #geom_point(data = d1(), aes(x = DateAsDate, y = d1()$value, color=d1()$variable)) 
    })
    
    q0 <- reactive({
        ggplot(data=d0(), aes(x = DateAsDate, y = d0()$value, color = d0()$variable))  +
            labs(title = paste("Scatter Plot - ", input$selectCity, collapse = " "), subtitle = "Date Vs Pollutants", x = "Date", y = "Pollutant", colour = "Gas") +
            geom_point() #+ #+ geom_smooth() 
        #geom_point(data = d1(), aes(x = DateAsDate, y = d1()$value, color=d1()$variable)) 
    })
    
    r <- reactive({
        ggplot(data=d1(), aes(x = DateAsDate, y = d1()$value, color = d1()$variable))  +
            labs(title = paste("Scatter Plot - ", input$ggCity2Compare, collapse = " "), subtitle = "Date Vs Pollutants", x = "Date", y = "Pollutant", colour = "Gas") +
            geom_point() #+ #+ geom_smooth() 
        #geom_point(data = d1(), aes(x = DateAsDate, y = d1()$value, color=d1()$variable)) 
    })
    
    s <- reactive({
        ggplot(data=d2(), aes(x = DateAsDate, y = d2()$value, color = d2()$variable))  +
            labs(title = paste("Scatter Plot - ", input$pltCity2Compare, collapse = " "), subtitle = "Date Vs Pollutants", x = "Date", y = "Pollutant", colour = "Gas") +
            geom_point() #+ #+ geom_smooth() 
        #geom_point(data = d1(), aes(x = DateAsDate, y = d1()$value, color=d1()$variable)) 
    })
    
    
    output$ggplot2 <- renderPlot(q()) 
    output$ggplot2SelectedCity <- renderPlot(r()) 
    #output$ggplot2lm <- renderPlot(q() + geom_smooth(method = lm, colour = "black"))
    
    
    
    output$pltyggplot2 <- renderPlotly({ ggplotly(q0() )  })
    output$pltyggplot2SelectedCity <- renderPlotly({ ggplotly(s() )  })
    #output$pltyggplot2lm <- renderPlotly({ ggplotly(q() + geom_smooth(method = lm, colour = "black"))  })
    
}


BoxPlotTab4 <- function(datDf=datDf, input, output, session) {
    pd <-  reactive({   datDf %>% select(City, DateAsDate, input$pollutantId[!is.na(input$pollutantId)])  %>% filter(City == input$selectCity & DateAsDate >= as.Date(as.character(input$dateId[1])) & DateAsDate <= as.Date(as.character(input$dateId[2])))     })
    
    d <- reactive({ melt(as.data.frame(pd()), id.vars = c("City", "DateAsDate") ) })
    
    #dat <- reactive({ SplitByCityYear(datDf=datDf, input$selectCity, MinYear, MaxYear)  })
    #output$TextDisplay4 <- renderText({ dat() })
    #output$ViewTable <- renderTable({ dat() })
    #output$ViewTable <- renderTable({ datDf })
    q <- reactive({
            ggplot(data=d(), aes(input$pollutantId[!is.na(input$pollutantId)], d()$value), color = d()$variable) +
            geom_boxplot()  
    })
    
    output$BoxPlotTab4 <- renderPlot(q())
}


server <- function(input, output, session) {
    SummaryTab(datDf=datDf, input, output, session)
    ScatterPlot(datDf=datDf, input, output, session)
    BoxPlotTab4(datDf=datDf, input, output, session)
}

shinyApp(ui, server)
