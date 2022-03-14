library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(dash)
library(purrr)

j_data <- read_csv("data/jasmine_df.csv")

j_data <- j_data %>% filter(is.na(j_data$cast_count) == FALSE)

cast_data <- j_data %>% 
  group_by(release_year) %>% 
  summarize(mean_cast_count = mean(cast_count))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


app$layout(
  dbcContainer(
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
        value=2002 # add start range at 1942 
      )
    )
  )
)

app$callback(
  output('scatter', 'figure'),
  list(input('xslider', 'value')),
  function(xcol) {
    
    p <- cast_data %>% 
      ggplot(aes(x=release_year,
                 y=mean_cast_count)) + 
      geom_point() +
      ggtitle("Average Cast Size Per Year") +
      xlab("Release Year") +
      ylab("Avg. Cast Size") +
      xlim(1942, xcol)
    ggplotly(p+ aes(text = release_year), tooltip = 'release_year')
    
  }
)


# app$run_server(debug = T)

app$run_server(host = '0.0.0.0')
# app$run_server(debug = T) # use when running locally

