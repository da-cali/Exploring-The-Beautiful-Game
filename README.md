Exploring The Beautiful Game
================

I came across [this soccer
dataset](https://www.kaggle.com/hugomathien/soccer/data) and decided to
explore it using R.

We begin by loading the data.

``` r
library(DBI)
library(RSQLite)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Database connection.
con <- dbConnect(SQLite(),"database.sqlite")

# Names of tables.
names <- as.data.frame(dbListTables(con))

# Tables.
player <- dbReadTable(con,'Player')
playerAttr <- dbReadTable(con,'Player_Attributes')

# Disconnecting from the database.
dbDisconnect(con)
```

Let’s arrange the players by rating and find out who were the 50 players
with the highest FIFA ratings during this period.

``` r
# Returns the name of player p.
getName <- function(p) {filter(player,p$player_api_id==player_api_id)$player_name}

# Top 50 distinct player attributes ordered by FIFA ratings.
top50 <- distinct(na.omit(arrange(playerAttr,desc(overall_rating))),player_api_id,.keep_all=TRUE)[1:50,]

# Show the names of the 50 players with the highest rankings in descending order.
sapply(c(1:nrow(top50)), function(i) paste(i,"-",getName(top50[i,]),"\n")) %>% message
```

    ## 1 - Lionel Messi 
    ## 2 - Cristiano Ronaldo 
    ## 3 - Gianluigi Buffon 
    ## 4 - Wayne Rooney 
    ## 5 - Gregory Coupet 
    ## 6 - Xavi Hernandez 
    ## 7 - Alessandro Nesta 
    ## 8 - Andres Iniesta 
    ## 9 - Iker Casillas 
    ## 10 - John Terry 
    ## 11 - Ronaldinho 
    ## 12 - Thierry Henry 
    ## 13 - Arjen Robben 
    ## 14 - David Trezeguet 
    ## 15 - Francesco Totti 
    ## 16 - Franck Ribery 
    ## 17 - Frank Lampard 
    ## 18 - Kaka 
    ## 19 - Luis Suarez 
    ## 20 - Manuel Neuer 
    ## 21 - Neymar 
    ## 22 - Radamel Falcao 
    ## 23 - Ze Roberto 
    ## 24 - Zlatan Ibrahimovic 
    ## 25 - Adriano 
    ## 26 - Carles Puyol 
    ## 27 - Cesc Fabregas 
    ## 28 - Cris 
    ## 29 - David Villa 
    ## 30 - Eden Hazard 
    ## 31 - Julio Cesar 
    ## 32 - Luca Toni 
    ## 33 - Lucio 
    ## 34 - Nemanja Vidic 
    ## 35 - Petr Cech 
    ## 36 - Robin van Persie 
    ## 37 - Samuel Eto'o 
    ## 38 - Steven Gerrard 
    ## 39 - Andrea Pirlo 
    ## 40 - Bastian Schweinsteiger 
    ## 41 - Carlos Tevez 
    ## 42 - David Silva 
    ## 43 - Dida 
    ## 44 - Didier Drogba 
    ## 45 - Diego 
    ## 46 - Fernando Torres 
    ## 47 - Gerard Pique 
    ## 48 - Hernan Crespo 
    ## 49 - Jamie Carragher 
    ## 50 - Joaquin

As expected, Messi and Ronaldo occupy positions 1 and 2.

Which of these players are over 190 centimeters tall?

``` r
# Show players over 190 cm. tall.
filter(player, player_api_id%in%(top50$player_api_id) & height>190)[,-c(1,4,5)]
```

    ##   player_api_id        player_name height weight
    ## 1         30728    David Trezeguet 190.50    176
    ## 2         30720               Dida 195.58    187
    ## 3         37482       Gerard Pique 193.04    187
    ## 4         30717   Gianluigi Buffon 193.04    201
    ## 5         30709          Luca Toni 193.04    194
    ## 6         27299       Manuel Neuer 193.04    203
    ## 7         30865      Nemanja Vidic 190.50    194
    ## 8         30859          Petr Cech 195.58    198
    ## 9         35724 Zlatan Ibrahimovic 195.58    209

Which of these players weigh under 160 pounds?

``` r
# Show players under 160 pounds.
filter(player, player_api_id%in%(top50$player_api_id) & weight<160)[,-c(1,4,5)]
```

    ##    player_api_id      player_name height weight
    ## 1          30731     Andrea Pirlo 177.80    150
    ## 2          30955   Andres Iniesta 170.18    150
    ## 3          38817     Carlos Tevez 172.72    157
    ## 4          37459      David Silva 170.18    148
    ## 5          30909      David Villa 175.26    152
    ## 6          30924    Franck Ribery 170.18    159
    ## 7          30981     Lionel Messi 170.18    159
    ## 8          19533           Neymar 175.26    150
    ## 9          22543   Radamel Falcao 177.80    159
    ## 10         30843 Robin van Persie 187.96    157
    ## 11         39854   Xavi Hernandez 170.18    148
    ## 12         38843       Ze Roberto 172.72    159

Surprisingly, Robin van Persie is among these players.

What is the average height and weight of the top 50 highest ranked
players?

``` r
# Show mean height and weight.
summarise(filter(player,player_api_id%in%(top50$player_api_id)),mean(height),mean(weight))
```

    ##   mean(height) mean(weight)
    ## 1     182.4228       175.02

What is the preferred foot ratio?

``` r
# Show number of left and right footed players.
count(top50,preferred_foot)
```

    ## # A tibble: 2 x 2
    ##   preferred_foot     n
    ##   <chr>          <int>
    ## 1 left              10
    ## 2 right             40

Everything looks reasonable so far, but to further evaluate the quality
of the data lets test the idea that similar players play at similar
positions by grouping these players into four clusters (representing the
four main positions in the game: goalkeeper, defender, midfielder, and
striker).

``` r
# K-means object.
clusters <- kmeans(top50[,10:42],centers=4,iter.max=20,nstart=100)

# Returns the names of the players of the nth cluster.
clusterNames <- function(n) {
   clust <- as.data.frame(split(top50,clusters$cluster)[n])
   names(clust) <- names(top50)
   sapply(c(1:nrow(clust)), function(i) getName(clust[i,]))
}

# Matrix of names of players per cluster.
playersPerCluster <- sapply(c(1:length(clusters$size)),clusterNames)

# Show names of players per cluster.
sapply(c(1:length(clusters$size)), function(i) (playersPerCluster[i]))
```

    ## [[1]]
    ## [1] "Gianluigi Buffon" "Gregory Coupet"   "Iker Casillas"    "Manuel Neuer"    
    ## [5] "Julio Cesar"      "Petr Cech"        "Dida"            
    ## 
    ## [[2]]
    ##  [1] "Wayne Rooney"           "Xavi Hernandez"         "Andres Iniesta"        
    ##  [4] "Ronaldinho"             "Thierry Henry"          "Francesco Totti"       
    ##  [7] "Frank Lampard"          "Kaka"                   "Ze Roberto"            
    ## [10] "Cesc Fabregas"          "David Villa"            "Samuel Eto'o"          
    ## [13] "Steven Gerrard"         "Andrea Pirlo"           "Bastian Schweinsteiger"
    ## [16] "Diego"                  "Joaquin"               
    ## 
    ## [[3]]
    ## [1] "Alessandro Nesta" "John Terry"       "Carles Puyol"     "Cris"            
    ## [5] "Lucio"            "Nemanja Vidic"    "Gerard Pique"     "Jamie Carragher" 
    ## 
    ## [[4]]
    ##  [1] "Lionel Messi"       "Cristiano Ronaldo"  "Arjen Robben"      
    ##  [4] "David Trezeguet"    "Franck Ribery"      "Luis Suarez"       
    ##  [7] "Neymar"             "Radamel Falcao"     "Zlatan Ibrahimovic"
    ## [10] "Adriano"            "Eden Hazard"        "Luca Toni"         
    ## [13] "Robin van Persie"   "Carlos Tevez"       "David Silva"       
    ## [16] "Didier Drogba"      "Fernando Torres"    "Hernan Crespo"

We can see that two of these clusters are composed exclusively by
goalkeepers and defenders, while the other two also seem to distinguish
between midfielders and strikers.
