#Required libraries:
require(data.table)
require(ggplot2)
require(maps)
# The multiplot function has been copied from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

#Getting previously saved data and changing boolean values by numeric ones:
load("./TwitterData/002_iii_rda_and_stopwords/allthetweets.rda")
tweets.dt <- as.data.table(tweets.dt)
tweets.dt$geo_enabled <- ifelse(tweets.dt$geo_enabled,1,0)
tweets.dt$verified <- ifelse(tweets.dt$verified,1,0)
tweets.dt$retweeted <- ifelse(tweets.dt$retweeted,1,0)

#Table creation to display summarized information from the data set.
tweets.dt.statistics <- (matrix(rep(NaN,24),nrow = 8,ncol = 3))


colnames(tweets.dt.statistics) <- c("Jobs_Mean","Original_Mean","JobsVsOriginal")
rownames(tweets.dt.statistics) <- c( "statuses_count","retweet_count"
                                      ,"followers_count","friends_count"
                                      ,"geo_enabled","verified"
                                      ,"favourites_count","number_observations")

tweets.dt.statistics["statuses_count","Original_Mean"] <- mean(tweets.dt$statuses_count)
tweets.dt.statistics["retweet_count","Original_Mean"] <- mean(tweets.dt$retweet_count)
tweets.dt.statistics["followers_count","Original_Mean"] <- mean(tweets.dt$followers_count)
tweets.dt.statistics["friends_count","Original_Mean"] <- mean(tweets.dt$friends_count)
tweets.dt.statistics["geo_enabled","Original_Mean"] <- mean(tweets.dt$geo_enabled)
tweets.dt.statistics["verified","Original_Mean"] <- mean(tweets.dt$verified)
tweets.dt.statistics["favourites_count","Original_Mean"] <- mean(tweets.dt$favourites_count)
tweets.dt.statistics["number_observations","Original_Mean"] <- nrow(tweets.dt)

tweets.dt.original <- tweets.dt

#Memory released.
rm(tweets.dt)

#Second data set retrieval:
load("./TwitterData/002_iv_rda/alljobtweets.rda")
tweets.dt <- as.data.table(tweets.dt)
#Boolean values converted to numeric values.
tweets.dt$geo_enabled <- ifelse(tweets.dt$geo_enabled,1,0)
tweets.dt$verified <- ifelse(tweets.dt$verified,1,0)
tweets.dt$retweeted <- ifelse(tweets.dt$retweeted,1,0)

#Jobs dataset statistics inserted into statistics table.
tweets.dt.statistics["statuses_count","Jobs_Mean"] <- mean(tweets.dt$statuses_count)
tweets.dt.statistics["retweet_count","Jobs_Mean"] <- mean(tweets.dt$retweet_count)
tweets.dt.statistics["followers_count","Jobs_Mean"] <- mean(tweets.dt$followers_count)
tweets.dt.statistics["friends_count","Jobs_Mean"] <- mean(tweets.dt$friends_count)
tweets.dt.statistics["geo_enabled","Jobs_Mean"] <- mean(tweets.dt$geo_enabled)
tweets.dt.statistics["verified","Jobs_Mean"] <- mean(tweets.dt$verified)
tweets.dt.statistics["favourites_count","Jobs_Mean"] <- mean(tweets.dt$favourites_count)
tweets.dt.statistics["number_observations","Jobs_Mean"] <- nrow(tweets.dt)

#Arithmetic comparison between comumns 1 and 2.
tweets.dt.statistics[,3] <- round(tweets.dt.statistics[,1]/tweets.dt.statistics[,2],2)
#Numeric rounding to enhance visibility
tweets.dt.statistics[,1] <- round(tweets.dt.statistics[,1],2)
tweets.dt.statistics[,2] <- round(tweets.dt.statistics[,2],2)

tweets.dt.jobs <- tweets.dt
#Memroy release
rm(tweets.dt)

## == At this point there are two separated data sets (one for jobs another for original one).
## == A third table contains the summarized data.
## == Both jobs and original tables are merged in one sole table:
tweets.dt <- as.data.table(rbind(tweets.dt.original,tweets.dt.jobs))
tweets.dt$dataset <- c(rep("original", nrow(tweets.dt.original)), rep("jobs", nrow(tweets.dt.jobs)))

#The next graphs are not included on the wiki page, but they show how wide is the actual
#data scale. For this reason, the final graphs display logarithmic transformation.
boxplot (tweets.dt.jobs$statuses_count)
boxplot (tweets.dt.original$statuses_count)

#Mann-Whitney-Wilcoxon Test. If p-value<0.05, groups are statistically different.
#No normal distribution is required.
wilcox.test(as.numeric(tweets.dt$favourites_count) ~ tweets.dt$dataset)

tweets.dt <- data.table(tweets.dt)

#Creating box plots for different twitter features.
#Multiplot functions creates a grid and puts them within.
bp1 <- ggplot(aes(y = log10(retweet_count+1), x = dataset), data = tweets.dt) + 
  geom_boxplot() + 
  ggtitle("Number of Retweets\nper Message")

bp2 <- ggplot(aes(y = log10(followers_count+1), x = dataset), data = tweets.dt) + 
  geom_boxplot() + ggtitle("Number of Account Followers\nper Message")
  
bp3 <- ggplot(aes(y = log10(friends_count+1), x = dataset), data = tweets.dt) + 
  geom_boxplot() + ggtitle("Number of User Friends\nper Message")

bp4 <- ggplot(aes(y = log10(statuses_count+1), x = dataset), data = tweets.dt) + 
  geom_boxplot() + ggtitle("Number of Total User Messages\nper message")

multiplot(bp1, bp2, bp3, bp4, cols=2)

#How scaled and non-scaled distributions look like.
dp2 <- ggplot(tweets.dt.jobs, aes(x=log10(retweet_count+1))) + 
  geom_density(fill="blue", alpha=0.3) +
  ggtitle("Number of Retweets\nper Message")

dp1 <- ggplot(tweets.dt, aes(x=scale(log10(statuses_count+1)))) + 
  geom_density(aes(group=dataset, fill=dataset), alpha=0.3) +
  ggtitle("Number of Total User Messages\nper message")

multiplot(dp1,dp2,cols = 2)

#Shaphiro tests p-value<0.05 indicates that distributions are not statistically normal.
shapiro.test(sample(scale(tweets.dt$statuses_count),5000))
shapiro.test(sample(scale(tweets.dt$retweet_count),5000))

#Memory release
dp1
rm(bp1)
rm(bp2)
rm(bp3)
rm(bp4)
rm(dp1)
rm(dp2)

#Getting sample users by activiy, popularity and verified and geo-coded features.
tweets.dt.sample <- (matrix(rep(NaN,25),nrow = 25,ncol = 8))
tweets.dt.sample <- data.table(tweets.dt.sample)
colnames(tweets.dt.sample) <-
  c("most_followed","most_retweeted","most_listed","max_tweets","max_favourites",
    "max_friends","verfied_sample","geo_enabled_sample")

tweets.dt.sample[,1] <- head(unique(tweets.dt.jobs[order(-followers_count)]$name),n=25)
tweets.dt.sample[,2] <- head(unique(tweets.dt.jobs[order(-retweet_count)]$name),n=25L)
tweets.dt.sample[,3] <- head(unique(tweets.dt.jobs[order(-listed_count)]$name),n=25L)
tweets.dt.sample[,4] <- head(unique(tweets.dt.jobs[order(-statuses_count)]$name),n=25L)
tweets.dt.sample[,5] <- head(unique(tweets.dt.jobs[order(-favourites_count)]$name),n=25L)
tweets.dt.sample[,6] <- head(unique(tweets.dt.jobs[order(-friends_count)]$name),n=25L)
setkey(tweets.dt.jobs,id_str)
set.seed(1235)
tweets.dt.sample[,7] <- sample(unique(tweets.dt.jobs[tweets.dt.jobs$verified==T,]$name),size = 25)
set.seed(123)
tweets.dt.sample[,8] <- sample(unique(tweets.dt.jobs[tweets.dt.jobs]$name),size = 25)

#Backup:
save(file="./TwitterData/002_iv_rda/tweets.dt.sample.rda", x=tweets.dt.sample, compress = T)
#load("./tweets.dt.sample.rda")

#Memory release.
rm(tweets.dt.sample)
rm(tweets.dt.jobs)
rm(tweets.dt.original)
rm(tweets.dt.statistics)
rm(multiplot)

#UTF-8 Characters not handled easily on data.frames.
#Matrix objects offer immediate UFT8 conversion.
as.matrix(tweets.dt.sample)


#optional :load("./TwitterData/002_iii_rda_and_stopwords/allthetweets.rda")
#isolate geo-located tweets.
tweets.dt.geolocated <- as.data.table(
unique(tweets.dt[(!is.na(tweets.dt$lat)) & !is.na(tweets.dt$lon) ,c("user_id_str","lat","lon","location"), with=F])
)

locations <- as.data.frame(tweets.dt.geolocated)
# Not included on wiki:
with(locations, plot(lon, lat))

#Map generation adapted from https://gist.github.com/dsparks/4329876

#Generate world-map with tweets sources as points.
worldMap <- map_data("world")
mapPlot <- ggplot(worldMap)
#Drawing map
mapPlot <- mapPlot + geom_path(aes(x = long, y = lat, group = group), 
                       colour = gray(2/3), lwd = 1/3)
#Plot points on map:
mapPlot <- mapPlot + geom_point(data = locations,
                        aes(x = lon, y = lat),
                        colour = "RED", alpha = 1/2, size = 1)
mapPlot <- mapPlot + coord_equal()  #Projections
#mapPlot <- mapPlot + theme_minimal()  # Drop background gray color.
print(mapPlot)

#Note:
#A backup of the multiplot function has been kept on this location:
# https://onedrive.live.com/redir?resid=E6AE5490ACB22C0A!15984&authkey=!AAZ62Ey90DRm7ng&ithint=file%2ctxt
