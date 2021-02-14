library(shiny)
library(tidyverse)

## Reading in the data (already clean)
data.df <- read.csv2("data_vis_final.csv", stringsAsFactors = F)

## Defining the server function
server <- function(input, output) {
  
  output$graph1 <- renderPlot({
    
    graph1_data.df <- data.df %>% 
      select(entity, year, life_expectancy_at_birth_total_years, health_expenditure_and_financing_per_capita_oec_dstat_2017) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph1_data.df, aes(x = as.numeric(health_expenditure_and_financing_per_capita_oec_dstat_2017), y = as.numeric(life_expectancy_at_birth_total_years))) +
      geom_line(aes(color = entity)) +
      xlab("Health expenditure per capita [PPP]") +
      ylab("Life Expectancy at birth [years]") +
      labs(color = "Countries") +
      theme_bw() +
      theme(legend.text=element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic"))
            
    
  })
  
  output$graph2 <- renderPlot({
    
    graph2_data.df <- data.df %>% 
      select(entity, year, sales_of_cigarettes_per_adult_per_day_international_smoking_statistics_2017) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph2_data.df, aes(x = as.numeric(year), y = as.numeric(sales_of_cigarettes_per_adult_per_day_international_smoking_statistics_2017))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Cigarettes sold per adult, per day") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph3 <- renderPlot({
    
    graph3_data.df <- data.df %>% 
      select(entity, year, deaths_smoking_sex_both_age_age_standardized_rate) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph3_data.df, aes(x = as.numeric(year), y = as.numeric(deaths_smoking_sex_both_age_age_standardized_rate))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Death rate from smoking [deaths per 100k individuals]") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph4 <- renderPlot({
    
    graph4_data.df <- data.df %>% 
      select(entity, year, deaths_high_body_mass_index_sex_both_age_age_standardized_rate) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph4_data.df, aes(x = as.numeric(year), y = as.numeric(deaths_high_body_mass_index_sex_both_age_age_standardized_rate))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Death rate from obesity [deaths per 100k individuals]") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph5 <- renderPlot({
    
    graph5_data.df <- data.df %>% 
      select(entity, year, deaths_interpersonal_violence_sex_both_age_all_ages_rate) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph5_data.df, aes(x = as.numeric(year), y = as.numeric(deaths_interpersonal_violence_sex_both_age_all_ages_rate))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Homicide rate [deaths per 100k individuals]") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph6 <- renderPlot({
    
    graph6_data.df <- data.df %>% 
      select(entity, year, deaths_opioid_use_disorders_sex_both_age_age_standardized_rate) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph6_data.df, aes(x = as.numeric(year), y = as.numeric(deaths_opioid_use_disorders_sex_both_age_age_standardized_rate))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Opioid overdose rate [deaths per 100k individuals]") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph7 <- renderPlot({
    
    graph7_data.df <- data.df %>% 
      select(entity, year, deaths_self_harm_sex_both_age_all_ages_rate) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph7_data.df, aes(x = as.numeric(year), y = as.numeric(deaths_self_harm_sex_both_age_all_ages_rate))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Suicide rate [deaths per 100k individuals]") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph8 <- renderPlot({
    
    graph8_data.df <- data.df %>% 
      select(entity, year, deaths_road_injuries_sex_both_age_age_standardized_rate) %>% 
      filter(entity %in% input$checkbox,
             between(year, input$dateSlider[1], input$dateSlider[2])) 
    
    ggplot(data = graph8_data.df, aes(x = as.numeric(year), y = as.numeric(deaths_road_injuries_sex_both_age_age_standardized_rate))) +
      geom_line(aes(color = entity)) +
      xlab("Year") +
      ylab("Death rate from road accidents \n [deaths per 100k individuals]") +
      labs(color = "Countries") +
      scale_x_continuous(breaks = seq(input$dateSlider[1], input$dateSlider[2], by = 5)) +
      theme_bw() +
      theme(legend.text = element_text(color = "white", size = 15),
            legend.title = element_text(color = "white", size = 15),
            legend.key = element_rect(fill = "white"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(color = "white")))
    
  })
  
  output$graph9 <- renderPlot({
    
    graph9_data.df <- data.df %>% 
      select(entity, year, mortality_rate_infant_per_1_000_live_births) %>% 
      filter(entity %in% input$checkbox, 
             year == input$dateSlider[2]) 
    
    ggplot(data = graph9_data.df, aes(x=entity, y=mortality_rate_infant_per_1_000_live_births)) +
      geom_bar(stat='identity', aes(fill = entity)) +
      coord_flip() +
      xlab("Countries") +
      ylab("Number of infants who died before 1 year of age per 1000 live births") +
      labs(fill = "Countries") +
      theme_bw() +
      theme(legend.text=element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) 
    
  })
  
  output$graph10 <- renderPlot({
    
    graph10_data.df <- data.df %>% 
      select(entity, year, total_population_gapminder_hyde_un_x, share_of_population_covered_by_health_insurance_ilo_2014, gdp_per_capita_ppp_constant_2011_international) %>% 
      filter(entity %in% input$checkbox,
             year <= input$dateSlider[2],
             !is.na(share_of_population_covered_by_health_insurance_ilo_2014)) %>% 
      group_by(entity) %>% 
      filter(year == max(year)) %>% 
      ungroup() %>% 
      mutate(entity = paste(entity, " (", year, ")"))
    
    ggplot(data = graph10_data.df, aes(x = gdp_per_capita_ppp_constant_2011_international, 
                                      y = share_of_population_covered_by_health_insurance_ilo_2014, 
                                      size = total_population_gapminder_hyde_un_x,
                                      color = entity)) +
      geom_point(alpha = 0.5) +
      scale_size(range = c(1, 24), name = "Population") +
      labs(color = "Countries") +
      xlab("GDP per capita [PPP]") +
      ylab("Share of population covered by health insurance [%]") +
      theme_bw() +
      theme(legend.text=element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="italic")) +
      geom_text(aes(label = entity), size = 5)
    
  })
  
}
