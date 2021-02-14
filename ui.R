library(shiny)
library(tidyverse)

## Reading in the data (already clean)
data.df <- read.csv2("data_vis_final.csv", stringsAsFactors = F)

## Defining the countries I want for my analysis 
# Could be expanded, the dataset contains more countries
all.countries.vec <- c("United States", "France", "Japan", "Poland", "Austria", "Germany", "Croatia", "Spain", "Switzerland")

## Defining the UI
ui <- fluidPage(
  
  tags$h1("Why is US life expectancy an outlier among rich countries?"),
  
  sidebarLayout(
    
    ##Inputs
    sidebarPanel(
      
      checkboxGroupInput(inputId = "checkbox",
                         label = "Countries",
                         choices = all.countries.vec,
                         selected = c("United States", "France", "Japan", "Germany", "Switzerland")),
      
      
      sliderInput(inputId = "dateSlider",
                  label = "Years",
                  min = 1990,
                  max = 2017,
                  value = c(1990, 2017),
                  step = 1,
                  round = T,
                  sep = "")
      
    ),
    
    ##Outputs
    
    mainPanel(
      
      #Graph1
      tags$h2("Life expectancy vs. Health Expenditure"),
      tags$p("The US clearly stands out. It spends more on health than any other country, yet the life expectancy of it's population is far shorter."),
      plotOutput("graph1"),
      
      #Graph2
      tags$h2("Sales of cigarettes"),
      tags$p("Sales of cigarettes have been dropping in the US even faster than in other countries, although we could expect a lag of ~30 years between cigarettes sold and deaths caused by smoking."),
      plotOutput("graph2"),
      
      #Graph3
      tags$h2("Deaths by smoking"),
      tags$p("The US has a slightly higher death rate from smoking than other countries."), 
      plotOutput("graph3"),
      
      #Graph4
      tags$h2("Deaths by obesity"),
      tags$p("Obesity causes much more deaths in the US than in any other country."),
      plotOutput("graph4"),
      
      #Graph5
      tags$h2("Deaths by homicide"),
      tags$p("The US stands out significantly when considering deaths coused by interpersonal violence."),
      plotOutput("graph5"),
      
      #Graph6
      tags$h2("Deaths by opioid overdoses"),
      tags$p("The same goes for opioid overdoses, the rate of which rises faster and faster. It should be a major concern for US citizens."),
      plotOutput("graph6"),
      
      #Graph7
      tags$h2("Deaths by suicide"),
      tags$p("In terms of suicide the US is on the lower end of spectrum, although their rate is growing since the late 90s, as opposed to other countries."),
      plotOutput("graph7"),
      
      #Graph8
      tags$h2("Deaths from road accidents"),
      tags$p("Deaths caused by road accidents are much more common in the US. Additionally most of the Americans who die in road accidents are young which has a great impact on the life expectancy."),
      plotOutput("graph8"),
      
      #Graph9
      tags$h2("Infant mortality rate in the most recent year"),
      tags$p("There is an abnormally high infant mortality rate in the US. The cause could be attributed to high inequality among the US citizens. Poorer Americans are worse off during the first year of their life. Lower life expectancy of poor Americans plays a big role in lowering average life expectancy in the US as a whole."),
      plotOutput("graph9"),
      
      #Graph 10
      tags$h2("Health insurance coverage vs GDP per capita"),
      tags$h3("(Most recent data available)"),
      tags$p("The US stands out among the richer countries, as it doesn't have universal access to health insurance."),
      plotOutput("graph10"),
      
      
      
    )
  ),
  tags$hr(style = "border-top: 0.5px solid #000000;"),
  tags$p("Data sources:"),
  tags$a("http://ghdx.healthdata.org/gbd-results-tool",
         href = "http://ghdx.healthdata.org/gbd-results-tool",
         style="text-align: justify;"),
  tags$br(),
  tags$a("http://www.pnlee.co.uk/ISS.htm",
         href = "http://www.pnlee.co.uk/ISS.htm",
         style="text-align: justify;"),
  tags$br(),
  tags$a("http://data.worldbank.org/data-catalog/world-development-indicators",
         href = "http://data.worldbank.org/data-catalog/world-development-indicators",
         style="text-align: justify;")
)
