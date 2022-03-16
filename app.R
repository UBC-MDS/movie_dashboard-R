library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(dash)
library(purrr)

suf_data <- read.csv(file = 'data/processed/sufang_clean_df.csv')
j_data <- read_csv("data/processed/jasmine_df.csv")
data <- read_csv("data/processed/clean_df.csv")

# drop down list
drop_list <- list(
  "TV-G",
  "TV-14",
  "TV-MA",
  "TV-PG",
  "R",
  "TV-Y7",
  "TV-Y",
  "PG",
  "G",
  "PG-13",
  "NR",
  "UR",
  "TV-Y7-FV",
  "NC-17")


j_data <- j_data %>% filter(is.na(j_data$cast_count) == FALSE)

cast_data <- j_data %>% 
  group_by(release_year) %>% 
  summarize(mean_cast_count = mean(cast_count))

app <- Dash$new(external_stylesheets = dbcThemes$DARKLY)


app$layout(
  dbcContainer(
    list(
      
      # title card 
      dbcRow(
        list(
          dbcCard(
            dbcCardBody(
              list(h4("Netflix Movie Dashboard: Visualize movie trends on the world's most popular streaming platform!", className = "card-title")),
              dbcCol(
                style=list("font-weight"="bold", 
                           "font-size"="85%",
                           "font-family"= "Garamond"),
              ), # dbcCol
            ), # dbcCardBody
            color ="dark", 
            inverse=TRUE
          ) #dbcCard
        ), # list
      ), # dbcRow
      
      # first row
      dbcRow(
        list(
          
          # Jasmine's plot
          dbcCol( 
            dbcCard(
              dbcCardBody(
                div(
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
                      value=2002)
                  )
                  
                ) # html
              ), # dbcCardBody
              color="dark"
            ), # dbcCard
            md=6,
          ), # Jasmine dbcCol
          
          # Mahsa plot
          
          dbcCol(
            dbcCard(
              dbcCardBody(
                div(
                  list(
                    dccGraph(id='plot_line'),
                    
                    dccDropdown(
                      id='rating-select',
                      options = drop_list %>%
                        purrr::map(function(rating,pop) list(label = rating, value = rating)),
                      value=list("TV-G","TV-MA", "TV-14","TV-Y7"),
                      multi=TRUE
                    ),
                    
                    dccRangeSlider(
                      id='my-range-slider',
                      min=1942,
                      max=2020,
                      marks =
                        list(
                          '1942' = '1942',
                          '1960' = '1960',
                          '1980' = '1980',
                          '2000' = '2000',
                          '2020' = '2020'
                        ),
                      value=list(2003, 2020)
                    )
                  ) # list
                ) # div
              ), # dbcCardBody
              color="dark"
            ), # dbcCard
            md=6), # close dbcCol
          
          dbcRow(
            dbcCol(
              dbcCard(
                dbcCardBody(
                  div(
                    
                    ##
                    
                    list(
                      dccGraph(id='plot-area'),
                      htmlLabel("Year"),
                      dccRangeSlider(id='year3',
                                     min = min(suf_data$release_year),
                                     max= max(suf_data$release_year),
                                     value=list(1995, 2020),
                                     marks=list(
                                       '1970'= "1970",
                                       '1975'= "1975",
                                       '1980'= "1980",
                                       '1985'= "1985",
                                       '1990'= "1990",
                                       '1995'= "1995",
                                       '2000'= "2000",
                                       '2005'= "2005",
                                       '2010'= "2010",
                                       '2015'= "2015",
                                       '2020'= "2020"
                                     )
                      ),
                      htmlLabel("Duration"),
                      dccRangeSlider(id='duration3',
                                     min = min(suf_data$duration),
                                     max = max(suf_data$duration),
                                     value=list(60, 120),
                                     marks=list(
                                       '10'=  "10 min",
                                       '30'= "30 min",
                                       '50'= "50 min",
                                       '70'= "70 min",
                                       '90'= "90 min",
                                       '110'= "110 min",
                                       '130'= "130 min",
                                       '150'= "150 min",
                                       '170'= "170 min",
                                       '190'= "190 min",
                                       '210'= "210 min",
                                       '230'= "230 min")
                      )
                    )
                    
                    ###
                  ) # div
                ), # dbcCardBody
                color="dark"
              ) # dbcCard
            )
          )
          
        ) # close list
      ) # first row
    ), # close list
    style=list(backgroundColor = "#000000")
  ) # container  
) 

# jasmine callback 
app$callback(
  output('scatter', 'figure'),
  list(input('xslider', 'value')),
  function(xcol) {
    
    p <- cast_data %>%
      ggplot(aes(x=release_year,
                 y=mean_cast_count)) +
      geom_point(fill= "red2", color = "black", shape = 21, size = 3) +
      ggtitle("Average Cast Size Per Year") +
      xlab("Release Year") +
      ylab("Avg. Cast Size") +
      xlim(1942, xcol) +
      theme(plot.title = element_text(hjust = 0.5, color = "white"),
            panel.background = element_blank(),
            panel.grid = element_line(color = "#7a7979"),
            axis.line = element_line(colour = "black"),
            plot.background = element_rect(fill = '#171614', colour = '#171614'),
            axis.text = element_text(color="white"),
            axis.title.x = element_text(color="white"),
            axis.title.y = element_text(color="white")
      )
    ggplotly(p + aes(text = release_year), tooltip = 'release_year')  %>% layout(plot_bgcolor = '000000')
  }
)

# mahsa callback

app$callback(
  output('plot_line', 'figure'),
  list(input('rating-select', 'value'),
       input('my-range-slider','value')),
  function(ratings_range,year_range) {
    df <- na.omit(data) %>% 
      filter(release_year > year_range[1],release_year < year_range[2]) %>%
      filter(rating %in% ratings_range) %>%
      group_by(release_year,rating) %>% 
      summarise(count = length(rating))
    
    plot  <- ggplot(df ,aes(x = release_year, y = count, color = rating)) +
      geom_line()+      
      scale_size(range = c(2, 12)) +
      ggtitle('Movie Rating in Netflix Over Time') +
    labs(x = 'Years', y= "Number of movie", color="white") +
      scale_color_manual(name="Movie Rating", 
                         values=c("#CE2626", 
                                  "#C864ED", 
                                  "#FBBA72", 
                                  "#EFAAC4", 
                                  "#A56124", 
                                  "#D30C7B",
                                  "#ff0067", 
                                  "#FF3C38", 
                                  "#FF8C42", 
                                  "#F991CC", 
                                  "#A05BFA", 
                                  "#9F2042",
                                  "#E63946",
                                  "#F58549")) +
      theme(plot.title = element_text(hjust = 0.5, color ="white"),
            panel.background = element_blank(),
            panel.grid = element_line(color = "#7a7979"),
            axis.line = element_line(colour = "black"),
            plot.background = element_rect(fill = '#171614', colour = '#171614'),
            legend.background = element_rect(fill="#171614"),
            legend.text = element_text(color="white"),
            legend.title = element_text(color="white"),
            axis.text = element_text(color="white"),
            axis.title.x = element_text(color="white"),
            axis.title.y = element_text(color="white")
            ) 
    
    ggplotly(plot)  %>% layout(plot_bgcolor = '000000')
  }
)

# sufang callback

app$callback(
  output('plot-area', 'figure'),
  list(input('year3', 'value'),
       input('duration3', 'value')),
  function(year_range,duration_range){
    data_filt <- suf_data %>%
      dplyr::filter((release_year > year_range[1] & release_year < year_range[2]) & (duration > duration_range[1] & duration < duration_range[2]))
    p1 <- ggplot(data_filt, aes(x = forcats::fct_infreq(country)),text = name) +
      geom_bar(stat = 'count', color = "black", fill = "red2") +
      ggtitle('Which Countries Makes the Most Movies?') +
      labs(x = 'Country', color="white") +
      labs(y = 'Number of Movies Produced', color="white") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(color="white"),
            plot.title = element_text(hjust = 0.5, color ="white"),
            panel.background = element_blank(),
            panel.grid = element_line(color = "#7a7979"),
            axis.line = element_line(colour = "black"),
            plot.background = element_rect(fill = '#171614', colour = '#171614'),
            axis.title.x = element_text(color="white"),
            axis.title.y = element_text(color="white")
            )
    ggplotly(p1) %>% layout(plot_bgcolor = '000000')
  }
)

app$run_server(host = '0.0.0.0')
# app$run_server(debug = T) # use when running locally
