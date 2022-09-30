# script to determine occupancy

library(tidyverse)
library(cowplot)
library(ggmap)
#library(rgdal)
library(rgeos)
library(maptools)

#rgeos/rgdal being depricated, so trying sf in the future
#library(sf)

#read in input file to pull in temperature values for each of patches. this does not vary across scenarios.

setwd("../../inputs/AFS_conference/")

patchvars <- read.csv("patchvars_desert.csv")

#need to subset for left_join memory issues
patchvars <- patchvars %>%
  select(X, Y, GrowthTemperatureBack)

#then a little renaming
patchvars$XCOORD <- patchvars$X
patchvars$YCOORD <- patchvars$Y
patchvars$Y <- NULL
patchvars$X <- NULL

setwd("./afs_outputs")

#also make sure no summary files still present

output_files = list.files(pattern = paste(".csv", sep=''), 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=paste(dirname((x)),basename(x),sep="/")))


#data_df <- head(output_files) this is useful for testing below when running lots of replicates
data_df <- output_files

#create year and run column from file name after deleting unneccessary rows
data_df$ID <- NULL
data_df$sex <- NULL
data_df$size <- NULL
data_df$mature <- NULL
data_df$newmature <- NULL
data_df$layeggs <- NULL
data_df$capture <- NULL
data_df$infection <- NULL
data_df$Hindex <- NULL
data_df$Species <- NULL
data_df$recapture <- NULL
data_df$SubPatchID <- NULL
data_df$age <- NULL
data_df$CDist <- NULL

data_df <- separate(data = output_files, col = filename, into = c('junk', 'replicate', 'year'), sep = "/")

data_df <- separate(data = data_df, col = year, into = c('ind', 'year'), sep = "d")
data_df <- separate(data = data_df, col = year, into = c('year', 'junk3'), sep = ".cs")


#remove rows from the initialization, initial pop
data_df <- subset(data_df, data_df$year != -1)
data_df$junk <- NULL
data_df$junk3 <- NULL
data_df$ind <- NULL
data_df$replicate <- as.factor(data_df$replicate)
data_df$year <- as.numeric(data_df$year)
data_df$XCOORD <- as.numeric(data_df$XCOORD)
data_df$YCOORD <- as.numeric(data_df$YCOORD)

saveRDS(data_df, file = "data_df_AFS.Rds")

#summarize for maps
for_viz <- data_df %>%
group_by(PatchID, XCOORD, YCOORD, year, replicate, .drop = FALSE) %>% 
  summarise(n = n())

for_viz1 <- for_viz %>%
  group_by(PatchID, XCOORD, YCOORD, year, .drop = FALSE) %>%
  summarize(m = mean(n), sdev = sd(n))

#bring in temperature column from patchvars
for_viz1 <- merge(x=for_viz1, y=patchvars, by=c("XCOORD", "YCOORD"))

saveRDS(for_viz1, file = "for_viz_AFS.Rds")

for_stat <- data_df %>%
  group_by(year, replicate, .drop = FALSE) %>% 
  summarise(n = n())

saveRDS(for_stat, file = "for_stat_AFS.Rds")

pop <- data_df %>%
  group_by(year, replicate) %>%
  tally()

#summary stats more useful for plot
pop1 <- pop %>%
  group_by(year, .drop = FALSE) %>%
  summarize(m = mean(n), sdev = sd(n))

saveRDS(pop1, file = "pop1_AFS.Rds")

#LOAD summary info
pop <- readRDS('pop1_AFS.RDS')
#LOAD by site
for_viz <- readRDS('for_viz_AFS.RDS')

#from here down can be done locally (although data_df may be too large)

#Jacks
#115.8251393°W 42.8174436°N  TR
#116.3219521°W 42.3691728°N  BL

map_desert <- get_map(c(left = -116.3219521, bottom = 42.3691728, top = 42.8174436, right = -115.8251393))

p_desert <- ggmap(map_desert)


#change UTM to LONG LAT and add back in n
coordinates(for_viz) <- ~XCOORD+YCOORD #similar to SpatialPoints
data_viz_UTM <- SpatialPoints(for_viz, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84"))
data_viz_LONGLAT <- spTransform(data_viz_UTM, CRS("+proj=longlat +datum=WGS84")) #can also be done with spTransform but being depricated
data_viz_ll_df <- as.data.frame(data_viz_LONGLAT)
data_viz_ll_df$m <- for_viz$m
data_viz_ll_df$year <- for_viz$year
data_viz_ll_df$loc <- for_viz$loc
data_viz_ll_df$replicate <- for_viz$replicate
data_viz_ll_df$GrowthTemperatureBack <- for_viz$GrowthTemperatureBack

#sep by location (from old code, useed to be a filter step)
data_viz_ll_df_desert <- data_viz_ll_df

#one problem with points is that it is hard to see due to overlap. remove points
#data_viz_ll_df_desert <- data_viz_ll_df_desert %>%
#  sample_frac(0.2, replace = FALSE)


#next, we are going to single out the first (10) and last years (100) of the model
data_viz_ll_df_desert <- data_viz_ll_df_desert %>% 
  filter(year == 10 | year == 100)



#now to split the temp up based on the input requirements
#some of the inputs have " | " and some have "|" and some have "  |  ". Also, seem to glitch when separating by | so change to "/" before that step.

data_viz_ll_df_desert$GrowthTemperatureBack <- gsub(pattern="|", replacement=" | ", data_viz_ll_df_desert$GrowthTemperatureBack, fixed = TRUE)
data_viz_ll_df_desert$GrowthTemperatureBack <- gsub(pattern="  |  ", replacement=" | ", data_viz_ll_df_desert$GrowthTemperatureBack, fixed = TRUE)
data_viz_ll_df_desert$GrowthTemperatureBack <- gsub(pattern=" | ", replacement="/", data_viz_ll_df_desert$GrowthTemperatureBack, fixed = TRUE)
data_viz_ll_df_desert<- separate(data_viz_ll_df_desert, col = GrowthTemperatureBack, into = c('C10', 'mid', 'C100'), sep = "/")


#change to numeric and delete unwanted column
data_viz_ll_df_desert$mid <- NULL
data_viz_ll_df_desert$C10 <- as.numeric(data_viz_ll_df_desert$C10)
data_viz_ll_df_desert$C100 <- as.numeric(data_viz_ll_df_desert$C100)

#next step is to then give the numeric ranges the threshold values
data_viz_ll_df_desert$C10_thresh  <- cut(data_viz_ll_df_desert$C10, breaks = c(-50, 18, 20, 50), labels = c("Cold", "Trigger", "Avoid"))
data_viz_ll_df_desert$C100_thresh <- cut(data_viz_ll_df_desert$C100, breaks = c(-50, 18, 20, 50), labels = c("Cold", "Trigger", "Avoid"))

#bring in stream data
#add in stream layer data
stream <- rgdal::readOGR("../../../data/NorWeST_PredictedStreamTempLines_MiddleSnake.shp")
stream_repro <- spTransform(stream, CRS("+proj=longlat +datum=WGS84"))
stream_fort <- fortify(stream_repro)
saveRDS(stream_fort, file = "stream_fort_AFS.Rds")

#Now to make the maps for the filtered years. see loop on bottom, but problem is getting alignment/only using certain combinations of C and Year.

data_viz_ll_df_desert_20 <- data_viz_ll_df_desert %>%
  filter(year == 20)

JacksCreek10 <- p_desert + 
  geom_path(data = stream_fort, aes(long, lat, group = group), color = "steelblue2") +
  geom_point(data = data_viz_ll_df_desert_10, aes(x = XCOORD, y = YCOORD, size = m, fill = C10_thresh), color = "black", shape = 21) +
  scale_size_continuous(range = c(2, 6)) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  scale_alpha_continuous(range = c(0.15, .85)) +
  theme(legend.position = "none") +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle(paste("Jack's Creek","20",sep = " ")) +
  theme_bw(base_size = 14)+
  theme(legend.position = "none")

ggsave(
  JacksCreek10, filename=paste("JacksCreek","20",".png",sep=""),dpi = 400, width = 10, height = 8, units = "in")

data_viz_ll_df_desert_100 <- data_viz_ll_df_desert %>%
  filter(year == 100)

JacksCreek100 <-  p_desert + 
  geom_path(data = stream_fort, aes(long, lat, group = group), color = "steelblue2") +
  geom_point(data = data_viz_ll_df_desert_100, aes(x = XCOORD, y = YCOORD, size = m, fill = C100_thresh), color = "black", shape = 21) +
  scale_size_continuous(range = c(2, 6)) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  scale_alpha_continuous(range = c(0.15, .85)) +
  theme(legend.position = "none") +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle(paste("Jack's Creek","100",sep = " ")) +
  theme_bw(base_size = 14)+
  theme(legend.position = "none")

ggsave(JacksCreek100, filename=paste("JacksCreek","100",".png",sep=""),dpi = 400, width = 10, height = 8, units = "in")
