library(DBI)
library(RSQLite)
library(dplyr)

# Database connection.
con <- dbConnect(SQLite(),"database.sqlite")

# Names of tables.
names <- as.data.frame(dbListTables(con))

# Tables.
player <- dbReadTable(con,'Player')
playerAttr <- dbReadTable(con,'Player_Attributes')

# Disconnecting from the database.
dbDisconnect(con)

# Returns the name of player p.
getName <- function(p) {filter(player,p$player_api_id==player_api_id)$player_name}

# Top 50 distinct player attributes ordered by FIFA ratings.
top50 <- distinct(na.omit(arrange(playerAttr,desc(overall_rating))),player_api_id,.keep_all=TRUE)[1:50,]

# Show the names of the 50 players with the highest rankings in descending order.
sapply(c(1:nrow(top50)), function(i) paste(i,"-",getName(top50[i,]),"\n")) %>% message

# Show players over 190 cm. tall.
filter(player, player_api_id%in%(top50$player_api_id) & height>190)[,-c(1,4,5)]

# Show players under 160 pounds.
filter(player, player_api_id%in%(top50$player_api_id) & weight<160)[,-c(1,4,5)]

# Show mean height and weight.
summarise(filter(player,player_api_id%in%(top50$player_api_id)),mean(height),mean(weight))

# Show number of left and right footed players.
count(top50,preferred_foot)

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

