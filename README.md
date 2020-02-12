Exploring The Beautiful Game
================

I came across [this soccer
dataset](https://www.kaggle.com/hugomathien/soccer/data) and decided to
explore it using R.

We begin loading the data.

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
# Database connection
con <- dbConnect(SQLite(),"database.sqlite")

# Names of tables
names <- as.data.frame(dbListTables(con))

# Tables
player <- dbReadTable(con,'Player')
playerAttr <- dbReadTable(con,'Player_Attributes')

# Disconnecting from the database
dbDisconnect(con)
```

Let’s order the players by rating and find out who were the 50 players
with the highest FIFA ratings during this period.

``` r
# Distinct player attributes ordered by FIFA ratings.
topRatings <- distinct(na.omit(arrange(playerAttr,desc(overall_rating))),player_api_id,.keep_all=TRUE)

# Returns the name of player p.
getName <- function(p) {filter(player,p$player_api_id==player_api_id)$player_name}

# Print the names of the 50 players with the highest rankings in descending order.
for (i in 1:50) {message(paste(i,"-",getName(topRatings[i,])))}
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

Which of these players are over 190 centimeters
tall?

``` r
filter(player, player_api_id%in%(topRatings[1:50,]$player_api_id) & height > 190)
```

    ##      id player_api_id        player_name player_fifa_api_id            birthday
    ## 1  2426         30728    David Trezeguet               5984 1977-10-15 00:00:00
    ## 2  2597         30720               Dida               3712 1973-10-07 00:00:00
    ## 3  3775         37482       Gerard Pique             152729 1987-02-02 00:00:00
    ## 4  3826         30717   Gianluigi Buffon               1179 1978-01-28 00:00:00
    ## 5  6265         30709          Luca Toni              23015 1977-05-26 00:00:00
    ## 6  6556         27299       Manuel Neuer             167495 1986-03-27 00:00:00
    ## 7  7846         30865      Nemanja Vidic             140601 1981-10-21 00:00:00
    ## 8  8580         30859          Petr Cech              48940 1982-05-20 00:00:00
    ## 9 11057         35724 Zlatan Ibrahimovic              41236 1981-10-03 00:00:00
    ##   height weight
    ## 1 190.50    176
    ## 2 195.58    187
    ## 3 193.04    187
    ## 4 193.04    201
    ## 5 193.04    194
    ## 6 193.04    203
    ## 7 190.50    194
    ## 8 195.58    198
    ## 9 195.58    209

Which of these players weigh under 160
pounds?

``` r
filter(player, player_api_id%in%(topRatings[1:50,]$player_api_id) & weight < 160)
```

    ##       id player_api_id      player_name player_fifa_api_id            birthday
    ## 1    706         30731     Andrea Pirlo               7763 1979-05-19 00:00:00
    ## 2    742         30955   Andres Iniesta                 41 1984-05-11 00:00:00
    ## 3   1581         38817     Carlos Tevez             143001 1984-02-05 00:00:00
    ## 4   2413         37459      David Silva             168542 1986-01-08 00:00:00
    ## 5   2430         30909      David Villa             113422 1981-12-03 00:00:00
    ## 6   3514         30924    Franck Ribery             156616 1983-04-07 00:00:00
    ## 7   6176         30981     Lionel Messi             158023 1987-06-24 00:00:00
    ## 8   7867         19533           Neymar             190871 1992-02-05 00:00:00
    ## 9   8704         22543   Radamel Falcao             167397 1986-02-10 00:00:00
    ## 10  9097         30843 Robin van Persie               7826 1983-08-06 00:00:00
    ## 11 10861         39854   Xavi Hernandez              10535 1980-01-25 00:00:00
    ## 12 11039         38843       Ze Roberto              28765 1974-07-06 00:00:00
    ##    height weight
    ## 1  177.80    150
    ## 2  170.18    150
    ## 3  172.72    157
    ## 4  170.18    148
    ## 5  175.26    152
    ## 6  170.18    159
    ## 7  170.18    159
    ## 8  175.26    150
    ## 9  177.80    159
    ## 10 187.96    157
    ## 11 170.18    148
    ## 12 172.72    159

Surprisingly, Robin van Persie is among these players.

What is the average height and weight of the top 50 highest ranked
players?

``` r
summarise(filter(player,player_api_id%in%(topRatings[1:50,]$player_api_id)),mean(height),mean(weight))
```

    ##   mean(height) mean(weight)
    ## 1     182.4228       175.02

What is the preferred foot ratio?

``` r
count(topRatings[1:50,],preferred_foot)
```

    ## # A tibble: 2 x 2
    ##   preferred_foot     n
    ##   <chr>          <int>
    ## 1 left              10
    ## 2 right             40

Everything seems reasonable so far, but to further test the data lets
test the idea that similar players play at similar positions by grouping
these players into four clusters representing the four main positions in
the game (goalkeeper, defender, midfielder, and striker).

``` r
# Lets create clusters of the players...
clusters <- kmeans(topRatings[1:50,10:42],centers=4,iter.max=20,nstart=100)

# Printing the names of players of each cluster.
for (n in 1:4) {
   c <- as.data.frame(split(topRatings[1:50,],clusters$cluster)[n])
   names(c) <- names(topRatings)
   message(paste("CLUSTER ",n,":"))
   for (i in 1:nrow(c)) {message(getName(c[i,]))}
   message("-----------------")
}
```

    ## CLUSTER  1 :

    ## Gianluigi Buffon

    ## Gregory Coupet

    ## Iker Casillas

    ## Manuel Neuer

    ## Julio Cesar

    ## Petr Cech

    ## Dida

    ## -----------------

    ## CLUSTER  2 :

    ## Lionel Messi

    ## Cristiano Ronaldo

    ## Arjen Robben

    ## David Trezeguet

    ## Franck Ribery

    ## Luis Suarez

    ## Neymar

    ## Radamel Falcao

    ## Zlatan Ibrahimovic

    ## Adriano

    ## Eden Hazard

    ## Luca Toni

    ## Robin van Persie

    ## Carlos Tevez

    ## David Silva

    ## Didier Drogba

    ## Fernando Torres

    ## Hernan Crespo

    ## -----------------

    ## CLUSTER  3 :

    ## Alessandro Nesta

    ## John Terry

    ## Carles Puyol

    ## Cris

    ## Lucio

    ## Nemanja Vidic

    ## Gerard Pique

    ## Jamie Carragher

    ## -----------------

    ## CLUSTER  4 :

    ## Wayne Rooney

    ## Xavi Hernandez

    ## Andres Iniesta

    ## Ronaldinho

    ## Thierry Henry

    ## Francesco Totti

    ## Frank Lampard

    ## Kaka

    ## Ze Roberto

    ## Cesc Fabregas

    ## David Villa

    ## Samuel Eto'o

    ## Steven Gerrard

    ## Andrea Pirlo

    ## Bastian Schweinsteiger

    ## Diego

    ## Joaquin

    ## -----------------
