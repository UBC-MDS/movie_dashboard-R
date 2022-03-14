# add theme

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(dash)
library(purrr)

j_data <- read_csv("data/processed/jasmine_df.csv")

j_data <- j_data %>% filter(is.na(j_data$cast_count) == FALSE)

cast_data <- j_data %>% 
  group_by(release_year) %>% 
  summarize(mean_cast_count = mean(cast_count))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


app$layout(
  dbcContainer(list(
    
    # title card 
    dbcRow(
      dbcCol(
        htmlDiv("Netflix Movie Dashboard: Visualize movie trends on the world's most popular streaming platform!")
      ),
    ),
    
    # first row
    dbcRow(
      
      # Jasmine column
      dbcCol(
        dbcCard(
          dbcCardBody(
            htmlDiv(
              list(
                dccGraph(id='scatter'),
                htmlLabel('Year Range'),
                dccSlider(
                  id='xslider',
                  min=1942,
                  max=2019,
                  marks = list(
                    '1942' = '1942',
                    '1962' = '1962',
                    '1982' = '1982',
                    '2002' = '2002',
                    '2019'= '2019'
                  ),
                  value=2002 
                )
              )
            ) # html 
          ) # card body 
        ) # card
        ,
        md=6), # first column
      
      # Masha column 
      dbcCol(
        dbcCard(
          dbcCardBody(
            htmlDiv(
              list(
                htmlLabel('Is this working?')
              ) # list
            ) # htmlDiv
          ) # card body 
        ), # card
        
        md=6) # second column
      
    ), # second row
    
    # Sufang card
    dbcRow(
      dbcCard(
        dbcCardBody(
          htmlDiv(
            list(
              htmlLabel('Sufang plot')
            ) # list
          ) # htmlDiv
        ) # CardBody 
      ) # card
    ) # second row 
    
  ) # list 
  ) # container  
)

app$callback(
  output('scatter', 'figure'),
  list(input('xslider', 'value')),
  function(xcol) {
    
    p <- cast_data %>% 
      ggplot(aes(x=release_year,
                 y=mean_cast_count)) + 
      geom_point(color= "red2") +
      ggtitle("Average Cast Size Per Year") +
      xlab("Release Year") +
      ylab("Avg. Cast Size") +
      xlim(1942, xcol) + 
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(size = 0.5, linetype = "solid"),
      )
    ggplotly(p+ aes(text = release_year), tooltip = 'release_year')
    
  }
)

app$run_server(host = '0.0.0.0')
# app$run_server(debug = T) # use when running locally
