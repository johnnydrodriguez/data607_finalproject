#COMBINED_DF
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#https://mastering-shiny.org/basic-app.html

library(shiny)
library(data.table)
library(tidyverse)
library(DT)
library(RCurl)
library(janitor)

#Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("2022 Vehicle Fuel Cost Comparison"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("annualmiles",
                        "Number of annual miles:",
                        min = 1000,
                        max = 40000,
                        step = 1000,
                        value = 15000),
            
            #Price of Gas/Gal input
            numericInput("gasprice", 
                         "Set price per gallon of gasoline", 
                         value = 3.72,
                         width = NULL),
            
            #Price of KwH input
            numericInput("kwhprice",
                         "Set price per KwH", 
                         value = 0.21,
                         width = NULL),
            
            #Car 1 details
            selectInput("gascar",
                        "Select Gas Powered Vehicle",
                        choices=NULL,
                        selected =NULL),
            
            #Car 2 details
            selectInput("evcar",
                        "Select Electrical Vehicle",
                        choices=NULL,
                        selected =NULL),
            
            #Car 3 details
            selectInput("phevcar",
                        "Select PHEV Vehicle",
                        choices=NULL,
                        selected =NULL),
        ),
        
        # Show a plot and table
        mainPanel(
            plotOutput("barPlot"),
            plotOutput("linePlot"),
            DT::dataTableOutput("carselectiontable"),
            #DT::dataTableOutput("cartable")
        )
    )
)

# Define server logic required to load data and draw plots
server <- function(input, output, session) {
    # shiny::runApp(display.mode="showcase")
    
    #Load vehicle data
    urlfile <- 'https://raw.githubusercontent.com/johnnydrodriguez/data607_finalproject/main/combined_df.csv'
    data <- read_csv(url(urlfile))
    
    #Wrangling
    data_cln <- data |> 
        unite(col="model",c("division","carline","transmission","epa_fe_label_dataset_id"),sep= " ") |>
        clean_names()
    
    #split data into types of fuel
    lst <- split(data_cln, f = data_cln$vehicle_type) #creates a list of dataframes segmented by fuel type
    
    ##Extract each table individually
    gas_df <- lst[["Gas"]]
    ev_df <- lst[["Ev"]]
    phev_df <- lst[["Phev"]]
    
    #Send options to each select
    updateSelectInput(session, "gascar", choices=gas_df$model)
    updateSelectInput(session, "evcar", choices=ev_df$model)
    updateSelectInput(session, "phevcar", choices=phev_df$model)
    
    ############################################################################
    dfInput <- reactive({
        data_cln %>%
            filter(model==input$gascar | model==input$evcar | model==input$phevcar) |>
            mutate(units_fuel_annual = if_else(vehicle_type == "Ev",
                                               (input$annualmiles/100)*comb_fe_guide_conventional_fuel,
                                               input$annualmiles/comb_fe_guide_conventional_fuel
                                               ),
                   annual_fuel_cost = if_else(vehicle_type == "Ev",
                                              units_fuel_annual*input$kwhprice,
                                              units_fuel_annual*input$gasprice
                                              ),
                   year1 = annual_fuel_cost,
                   year2=annual_fuel_cost*2,
                   year3=annual_fuel_cost*3,
                   year4=annual_fuel_cost*4,
                   year5=annual_fuel_cost*5,
                   year6=annual_fuel_cost*6,
                   year7=annual_fuel_cost*7,
                   year8=annual_fuel_cost*8,
                   year9=annual_fuel_cost*9
            ) |>
            pivot_longer(cols = starts_with("year"), names_to = "fuel_cum_year", values_to = "cum_annual_cost")
    })
    
    #creating bar plot
    output$barPlot <- renderPlot({
        df1 <- dfInput()
        ggplot(df1) +
            geom_bar(mapping = aes(x=model, 
                                    y = annual_fuel_cost),
                     stat = "identity") +
            labs (x = "Vehicle", y = "Annual Cost", title = "Annual Fuel Consumption Cost") +
            theme(legend.key.size = unit(2, 'cm')) +
            theme(text = element_text(size=15))
    })
    
    #creating line plot
    output$linePlot <- renderPlot({
        df2 <- dfInput()
        ggplot(df2) +
            geom_line(mapping = aes(x=fuel_cum_year, 
                                    y = cum_annual_cost,
                                    group=model, 
                                    colour= model),
                      size = 1) + 
            #scale_colour_discrete(name = "model") + 
            labs (x = "Year", y = "Cumulative Annual Cost", title = "Fuel Consumption Cost Over Time") +
            theme(legend.key.size = unit(2, 'cm')) +
            theme(text = element_text(size=15))
    })
    
    #Render car selection table
    output$carselectiontable <- DT::renderDataTable({
        req(dfInput())
        data1 <- dfInput()
        data1
    })
    
    # #Render raw table
    # output$cartable <- DT::renderDataTable({
    #     req(data)
    #     data2 <- data
    #     data2
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
