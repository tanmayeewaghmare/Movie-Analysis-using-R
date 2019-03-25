#1. Importing libraries ----
library(dplyr)
library(readxl)
library(writexl) 
library(tidyquant)
library(tidyverse)
library(forcats)
library(magrittr)
#2. Loading the data ----
setwd('C:/Users/Tanmayee Waghmare/Desktop/Projects/Film_Locations')

film <- read.csv('Film_Locations_in_San_Francisco.csv')
#3. Scanning the data ---
dim(film)
#We have 1622 rows and 11 columns. Since the number of columns is large, we will use glimpse() function to
#take a look at the columns all at once.
glimpse(film)

#4. Missing data ----
sum(is.na(film))
#It reports only 4 missing values whereas there are clearly more blank cells
#So we use mutate_all function to make all blank cells as NAs. Doing so would make it easy to remove or replace them at once.
film <- film %>% mutate_all(na_if,"")
sum(is.na(film)) #1913 NAs

colSums(is.na(film))
# Title       Release.Year          Locations          Fun.Facts Production.Company        Distributor 
#0                  0                 54               1187                  2                100 
#Director             Writer            Actor.1            Actor.2            Actor.3 
#0                  1                  4                 93                472

#Dealing with Missing Data ---
film[c('Locations', 'Fun.Facts', 'Production.Company', 'Distributor', 'Writer')] <- as.character(film[c('Locations', 'Fun.Facts', 'Production.Company', 'Distributor', 'Writer')])
film[c('Locations', 'Fun.Facts', 'Production.Company', 'Distributor', 'Writer')][is.na(film[c('Locations', 'Fun.Facts', 'Production.Company', 'Distributor', 'Writer')])] <- 'Unknown'

film[c('Actor.1', 'Actor.2', 'Actor.3')] <- as.character(film[c('Actor.1', 'Actor.2', 'Actor.3')])
film[c('Actor.1', 'Actor.2', 'Actor.3')][is.na(film[c('Actor.1', 'Actor.2', 'Actor.3')])] <- 'None'

#We have taken care of all the Missing value and appropriately renamed them

#5. Wrangling the data----
# Renaming the columns
film_wrangled <- film %>%
  set_names(names(.)%>% str_replace_all("\\.", "_")) %>%
#Re-ordering the columns
  select(Title, Release_Year, Locations, Director, Writer, everything()) %>%
  glimpse()

#6. Data Manipulation----
 film_tbl <- film_wrangled %>%
   #Choosing the columns to focus on
   select(Release_Year, Title, Locations, Director, Distributor) %>%
   glimpse()
 


#Analyzing which years had most movies shot in San Francisco 
 popular_by_year <- film_tbl %>%
                    group_by(Release_Year) %>%
                    summarize(Number_of_movies = n_distinct(Title)) %>%
                    filter(Number_of_movies > 5) %>%
                    arrange(desc(Number_of_movies)) 
                    
 #It seems like the year 2015 saw most movies shot in SF followed by the year 2016.
 #Before these two years, most movie shootings in SF took place in the year 1996. 
   
  popular_locations <- film_tbl %>%
                       group_by(Locations) %>%
                       summarize(Number_of_movies = n_distinct(Title)) %>%
                       filter(Number_of_movies > 5) %>%
                       filter(Number_of_movies < 54) %>%  
                       arrange(desc(Number_of_movies))
# The data tells us that the most famous location to shoot a movie is Golden Gate Bridge (No surprises here!)
# It is hotly contested with other popular locations such as City Hall, Fairmont Hotel at 950 Mason Street,
  # Treasure Island and Coit Tower.
  
  directors <- film_tbl %>%
               group_by(Director) %>%
               summarize(Number_of_movies = n_distinct(Title)) %>%
               filter(Number_of_movies > 3) %>%
               arrange(desc(Number_of_movies))
  # Director Andrew Haigh have shot a whopping 11 films in this City by the Bay followed by
  # Alfred Hitchcock, Chris Columbus, Garry Marshall and Philip Kaufman who have
  # shot 4 films each there.
  
  film_tbl %>%
    select(Title, Distributor) %>%
    group_by(Distributor) %>%
    summarise(Number_of_movies = n_distinct(Title)) %>%
    arrange(desc(Number_of_movies))

#7. Visualization ----
  popular_by_year %>%
    ggplot(aes(x = Release_Year, y = Number_of_movies)) +
    
    #Adding geometries
    geom_col(fill = "#18BC9C") +
    
    #Formatting
    geom_text(aes(label= Number_of_movies), vjust = 0 ) +
    theme_tq() +
    labs(
         title = "Number of movies shot in San Francisco by release year",
         subtitle = "The City was the clear favourite of movie-makers in 2015 ",
         x = "Release Year",
         y = "Number of Movies"
    ) 
    
    
    directors %>%
    ggplot(aes(x = Director, y = Number_of_movies )) +
    
    #Adding Geometries  
    geom_col(fill = "#0055AA" ) +
    
    #Formatting  
    theme_tq() +
    geom_text(aes(label= Number_of_movies), vjust = 0 ) +
    labs (
         title = "San Francisco is a top choice of the Directors",
         subtitle = "Andrew Haigh directed the most number of movies shot in the City",
         y = "Number of Movies"
    )
    
    popular_locations %>%
      ggplot(aes(x = reorder(Locations, -Number_of_movies), y = Number_of_movies )) + 
      
      #Adding Geometries  
      geom_col(fill = "#C40003" ) +
      
      #Formatting  
      theme_tq() +
      geom_text(aes(label= Number_of_movies), hjust = 0 ) +
      labs (
        title = "Popular film locations in San Francisco",
        subtitle = "Golden Gate Bridge is most popular destination ",
        x = "Number of Movies",
        y = "Locations"
      ) +
      coord_flip()
    
    
    #Names of the Movies shot at Golden Gate Bridge
    film_tbl %>%
      select(Title, Locations, Release_Year) %>%
      filter(grepl("Golden Gate Bridge", Locations)) %>%
      arrange(desc(Release_Year))

    
