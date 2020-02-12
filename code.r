library(DBI)
library(RSQLite)
library(dplyr)

# Database connection
con <- dbConnect(SQLite(),"database.sqlite")

# Names of tables
names <- as.data.frame(dbListTables(con))

# Tables
player <- dbReadTable(con,'Player')
playerAttr <- dbReadTable(con,'Player_Attributes')

# Disconnecting from the database
dbDisconnect(con)

# Distinct player attributes ordered by FIFA ratings.
topRatings <- distinct(na.omit(arrange(playerAttr,desc(overall_rating))),player_api_id,.keep_all=TRUE)

# Returns the name of player p.
getName <- function(p) {filter(player,p$player_api_id==player_api_id)$player_name}

# Print the names of the 50 players with the highest rankings in descending order.
for (i in 1:50) {message(paste(i,"-",getName(topRatings[i,])))}

filter(player, player_api_id%in%(topRatings[1:50,]$player_api_id) & height > 190)

filter(player, player_api_id%in%(topRatings[1:50,]$player_api_id) & weight < 160)

summarise(filter(player,player_api_id%in%(topRatings[1:50,]$player_api_id)),mean(height),mean(weight))

count(topRatings[1:50,],preferred_foot)

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