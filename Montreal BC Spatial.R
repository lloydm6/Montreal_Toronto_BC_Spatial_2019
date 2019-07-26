#setup####

library(readxl)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(table1)
library(stargazer)
library(formattable)
library(purrr)
library(reshape)
library(broom)
library(tidyverse)
library(BMA)
library(ggpmisc)
library(miscTools)
library(MASS)
library(leaps)
library(googleway)
library(rJava)
library(caret)
library(sf)
library(rgdal)
library(raster)
library(ggplot2)
library(gridExtra)
library(stringr)
library(ape)

#xlsx was working but now it doesn't. It says it need rJava to work. I tried to get rJava to work (~5hrs of internet wormhole trial and error). I made some progress (changed the type of errors I got), but it's still not working. 
#the rJava bug has been fixed for now, but the underlying Java 12 problem has not been solved, so other packages that require Java do not work. 
#library(xlsx)
#library(glmulti)
#install.packages('rJava',,'http://www.rforge.net/', type = "source")
#install.packages("helloJavaWorld")
#library(helloJavaWorld)
#options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/jre")
#ImportExport requires xlxs, which requires Java
#library(ImportExport)



setwd("/Users/macbook/Documents/McGill School/Practicum/Montreal_Toronto_BC_Spatial_2019")

#data read in####

########### Read in all the data
#columns AB and AC in the spread sheet are the outcomes of interest. AD also indicates if the data is good or not (eliminate the = 0)
m.t.s.outcomes.raw <- read.csv("Montreal and Toronto SPATIAL STUDY FILTER DATA and Chain of Custody 29-05-2019 SR.csv")

#different columns in this one, I check it lower down in this code. 
m.w.outcomes.raw <- read_xlsx("Montreal winter spatial study filter database 13 May (1).xlsx")
str(m.w.outcomes.raw)
str(m.t.s.outcomes.raw)

m.s.determinants <- read_xlsx("Variable Data Montreal 20-06-2019 ML.xlsx", sheet = 2)
t.s.determinants <- read_xlsx("Variable Data Toronto.xlsx", sheet = 2)

m.s.wb.determinants <- read.csv("woodburning buffers.csv")


#the 3 new determinants had blanks instead of zeros
#check to see if any others had NAs
sum(is.na(filter(m.s.determinants, Filter_ID != "MTL_space_114" & Filter_ID != "MTL_space_115" & Filter_ID != "MTL_space_131")))
#they are all in these rows
sum(is.na(filter(m.s.determinants, Filter_ID == "MTL_space_114" | Filter_ID == "MTL_space_115" | Filter_ID == "MTL_space_131")))
sum(is.na(m.s.determinants))
#NAs begone!
m.s.determinants[is.na(m.s.determinants)] <- 0

#add wood burning to the m.s.determinants
setdiff(sort(m.s.determinants$Filter_ID), sort(as.character(m.s.wb.determinants$filter)))
setdiff(sort(as.character(m.s.wb.determinants$filter)), sort(m.s.determinants$Filter_ID))
filter(m.s.data.all, f.id == "MTL_space_41")
#MTL_space_41 is missing from the wb.data, though it is bad data, so it wouldn't be kept anyways. Didn't want to add to book,just adding here
#note, this is one way code and shouldn't be run if the the csv gets changed. There is a bit of a sanity check, it will input "character(0)" if there is not differences between
m.s.wb.determinants <- add_row(m.s.wb.determinants)
m.s.wb.determinants$filter <- as.character(m.s.wb.determinants$filter)
m.s.wb.determinants[nrow(m.s.wb.determinants), 3] <- setdiff(sort(m.s.determinants$Filter_ID), sort(as.character(m.s.wb.determinants$filter)))
tail(m.s.wb.determinants)
m.s.wb.determinants$filter <- as.factor(m.s.wb.determinants$filter)
str(m.s.wb.determinants)
tail(m.s.wb.determinants)
summary(m.s.wb.determinants)
#looks good, now to bind together in a matched way with the other determinants

m.s.wb.determinants$filter <- as.character(m.s.wb.determinants$filter)
m.s.wb.determinants <- arrange(m.s.wb.determinants, filter)
m.s.determinants$Filter_ID == m.s.wb.determinants$filter
#all true, it's good
data.frame(m.s.wb.determinants$filter, m.s.determinants$Filter_ID)
#jut a visual
str(m.s.determinants)
#turns out the m.s.determinants are characters, not factors. 
#want to use full_join, so need the column name to be the same
#also tidy up the column names to make sure they are in line with the other variales
colnames(m.s.wb.determinants)

colnames(m.s.wb.determinants)[3:9] <- c(colnames(m.s.determinants)[1], "d_woodburn", "woodburn_100m", "woodburn_200m", "woodburn_500m", "woodburn_750m", "woodburn_1000m")
m.s.determinants <- full_join(m.s.determinants, dplyr::select(m.s.wb.determinants, -latitude, -longitude), by = "Filter_ID")
str(m.s.determinants[, c(1, 150:160)])
#they're in!

#this is to link the site id of winter to summer to the determinants (which are listed by summer site)
m.w.id.key <- read_xlsx("Winter and summer sites.xlsx")

#this is an explanation of each of the determinants
det.legend <- read_xlsx("Variable Data Montreal 20-06-2019 ML.xlsx", sheet = 1)


#######inspection
#compare headings fromt the two outcomes, this is FYI for incorporating the winter data into the summer data. Note: cbind repeats entries of shorter vector
cbind(colnames(m.t.s.outcomes.raw), colnames(m.w.outcomes.raw))
#for m.t.s.outcomes, I care about columns 1, 28, and 29 annnnnnd 30
#for m.w.outcomes, I care about columns 1, 11, and 12

#noticed that there are more rows of city = Montreal in the outcomes file than there are rows for the Montreal determinants file. 3 more
nrow(subset(m.t.s.outcomes.raw, city == "Montreal"))
nrow(m.s.determinants)
#compare them
setdiff(as.character(subset(m.t.s.outcomes.raw, city == "Montreal")$filter), as.character(m.s.determinants$Filter_ID))
#rows MTL_space_114,115 (bc value looks regular) and 131 (big bc value and NA latlong) are in the montreal outcomes (bc) but not the montreal determinants (ArcGIS variables in Variable Data Montreal)
#Now it's been solved, we have data for those 3 points. 


#comparing summer vs winter montreal outcomes
length(subset(m.t.s.outcomes.raw, city == "Montreal")$filter)
nrow(m.w.outcomes.raw)
#they are different, there is only 83 winter observations. Also, some of the montreal winter observations have Toronto labels. And the montreal labels say "Winter" instead of "space". There are no such entries in the "Variable Data Montreal"

#now check the Toronto outcome vs determinants rows. 
nrow(subset(m.t.s.outcomes.raw, city == "Toronto"))
#110
nrow(t.s.determinants)
#100
setdiff(as.character(subset(m.t.s.outcomes.raw, city == "Toronto")$filter), as.character(t.s.determinants$Filter_ID))
#MTL_wood_58, 60, TO_space_02, 108, 37, 41, 51, 55, 81, and 85 (all NA bc values) are in the outcomes but not in the determinants. No biggie. 

str(m.s.determinants)
#121 obs of 154 variables
#now it's 124 obervations, hurray! Annnnd 160 variables

str(t.s.determinants)
#100 observations of 152 variabales

str(m.t.s.outcomes.raw)
#275 observations o 30 variables

#create a data frame with just montreal determinants and outcomes. First figure out which filters are used 
str(m.t.s.outcomes.raw$filter)

str(m.s.determinants$Filter_ID)


#getting elevation
#need lats and longs. initially used the lats and longs from Chain of Custody 29-05-2019 SR.csv, but there were some missing values
#got sent a file from a later date that was more correct with lats and longs
(m.t.s.outcomes.raw[,c(2:3, 73:74)])
mtl.to.latlong <- read.csv("Montreal and Toronto SPATIAL STUDY FILTER DATA and Chain of Custody 03-07-2019 all.csv")
mtl.to.latlong <- mtl.to.latlong[,c(1,2,5,6)]
head(mtl.to.latlong)
str(mtl.to.latlong)
describe(mtl.to.latlong)

#first time I did it it read it with a couple of them had positive longs, that can't be right. Change to negative. Now it's okay
#mtl.to.latlong$longitude <- ifelse(mtl.to.latlong$longitude > 0, mtl.to.latlong$longitude*-1, mtl.to.latlong$longitude )

#the googleway function just wants a simple df of lat and long
mtl.to.just.latlong <-mtl.to.latlong[,3:4]

#this is my API key from google
key <- "AIzaSyBuZ4BbvdZNQbej19sGQddogYu4_FNAEVQ"

#function can't handle NAs, filtered them out
#mtl.to.elevation.googleway <- google_elevation(filter(mtl.to.just.latlong, !is.na(latitude) & !is.na(longitude)), location_type = "individual", key = key)
#I printed out a couple and cross referenced on google maps. The elevations appear to be goood. 
print(mtl.to.just.latlong[200,], digits = 10)
#write the googleway output to a file to make sure I'm not running it every time. I'm not sure what would consitute enough usage to warrant a charge to my API, but I don't want to find out the hard way. 
#note, have to put $results if writing from new googleway output
#write.csv(mtl.to.elevation.googleway$results, file = "Cleaned Data csv/mtl.to.elevation.googleway.csv")
mtl.to.elevation.googleway <- read.csv("Cleaned Data csv/mtl.to.elevation.googleway.csv")
str(mtl.to.elevation.googleway)


#get the list of complete cases, paste elevations to it
mtl.to.latlong.cc <- filter(mtl.to.latlong, !is.na(latitude) & !is.na(longitude))
#when reading this from csv .googleway, then the below is good. For from the actual googleway, you need a $results in there
mtl.to.latlong.cc$elevation <- mtl.to.elevation.googleway$elevation
#check to make sure with googleway output
formattable(mtl.to.latlong.cc)
#it's good, now join them
mtl.to.latlong.elevation <- full_join(mtl.to.latlong, mtl.to.latlong.cc, by = "filter")
str(mtl.to.latlong.elevation)

#now sneak it at the end of the determinants....Note: at this point, m.w.determinants hasn't been created yet, it gets created from m.s.determinants 
colnames(m.s.determinants)
colnames(t.s.determinants)
colnames(mtl.to.latlong.elevation)[1] <- colnames(m.s.determinants)[1]
colnames(mtl.to.latlong.elevation)
mtl.to.latlong.elevation$Filter_ID <- as.character(mtl.to.latlong.elevation$Filter_ID)
#full join is DANGEROUS! I've got mtl and to data all mixed in elevation. Need to left join
m.s.determinants <- left_join(m.s.determinants, dplyr::select(mtl.to.latlong.elevation, Filter_ID, elevation, latitude.x, longitude.x), by = "Filter_ID")
t.s.determinants <- left_join(t.s.determinants, dplyr::select(mtl.to.latlong.elevation, Filter_ID, elevation, latitude.x, longitude.x), by = "Filter_ID")
str(m.s.determinants)
str(t.s.determinants)
describe(t.s.determinants$elevation)
describe(m.s.determinants$elevation)
#it looks like they both kept the same number of observations.....soooo I think it worked
#there is 1 missing Montreal, which is it?
is.na(m.s.determinants$elevation)
m.s.determinants[85,]
m.s.determinants[84:86, c(1,160:163)]
NCOL(m.s.determinants)
#missing MTL_space_41


describe(filter(m.s.data, good_data == 1)$elevation)
describe(filter(t.s.data, good_data == 1)$elevation)
describe(filter(m.a.data)$elevation)
describe(filter(t.s.data, good_data == 1)$elevation)
length(m.a.data$elevation)

nrow(filter(m.s.data, good_data == 1))
nrow(filter(t.s.data, good_data == 1))

#this was just a check to make sure the lats and longs are the same for the two files. 
#it was from when they lats and longs were taken from determinants.raw
#there are some missing lats and longs, there is another file with them so check to see what it's like
mtl.to.latlong.2 <- read.csv("Montreal and Toronto SPATIAL STUDY FILTER DATA and Chain of Custody 03-07-2019 all.csv")
mtl.to.latlong.1and2 <- left_join(mtl.to.latlong, mtl.to.latlong.2[,1:6], by = "filter")
mtl.to.latlong.1and2$latitude.x == mtl.to.latlong.1and2$latitude.y
mtl.to.latlong.1and2$longitude.x == mtl.to.latlong.1and2$longitude.y
tail(dplyr::filter(mtl.to.latlong.1and2, city.x == "Montreal"))
#so they are all true or NA. No falses. I think that means we are good. I took a peek and .2 has the missing MTLs
#so should be using a different 

#data clean#####
#########Cleaning
m.s.determinants$Filter_ID <- as.factor(m.s.determinants$Filter_ID)
t.s.determinants$Filter_ID <- as.factor(t.s.determinants$Filter_ID)

#I saw that m.dets only had MTL_space labelled filter, so keep only those ones for m.s.outcomes
m.s.outcomes <- filter(m.t.s.outcomes.raw, grepl('MTL_space', filter))
nrow(m.s.outcomes)
length(m.s.outcomes$filter)
length(m.s.determinants$Filter_ID)


#there are still some MTL_space labelled filters that weren't used, need to figre out which ones and remmove them. Probably could have just done this in one step.
m.s.outcomes <- filter(m.s.outcomes, !(filter %in% setdiff(m.s.outcomes$filter, m.s.determinants$Filter_ID)))
nrow(m.s.outcomes)
length(m.s.outcomes$filter)
length(m.s.determinants$Filter_ID)
#looking good! for numbers, but there are more leves of fators for outcomes. I'll try to reset it.

#repeat with tonoront summer
t.s.outcomes <- subset(m.t.s.outcomes.raw, city == "Toronto")
nrow(t.s.outcomes)
t.s.outcomes <- filter(t.s.outcomes, !(filter %in% setdiff(t.s.outcomes$filter, t.s.determinants$Filter_ID)))
nrow(t.s.outcomes)
#looking grrrreat!


str(m.s.outcomes$filter)
str(as.factor(as.character(m.s.outcomes$filter)))
m.s.outcomes$filter <- as.factor(as.character(m.s.outcomes$filter))
m.s.outcomes$filter == m.s.determinants$Filter_ID
#they are all the same and in the same order, I can mush them together. 
t.s.outcomes$filter <- as.factor(as.character(t.s.outcomes$filter))
t.s.outcomes$filter == t.s.determinants$Filter_ID

as.numeric(m.s.outcomes[ , 28])
as.numeric(as.character(m.s.outcomes[ , 28]))
length(as.factor(m.s.outcomes[ , 30]))

#bring them all together. The m.s.determinants come in with everything except the first column, so I can keep adding variables to the end if I want
m.s.data.all <- data.frame(f.id = m.s.determinants$Filter_ID, bc_conc = as.numeric(m.s.outcomes[ , 28]), uvpm_conc = as.numeric(as.character(m.s.outcomes[ , 29])), good_data = as.factor(m.s.outcomes[ , 30]), m.s.determinants[ , -1])

slice(m.s.data.all, 40:50)

#just checking to make sure they are the correct length
length(m.s.determinants$Filter_ID)
length(as.numeric(m.s.outcomes[ , 28]))
length(as.numeric(as.character(m.s.outcomes[ , 29])))
nrow(m.s.determinants[ , -1])
head(m.s.data.all)
str(m.s.data.all)

#now take out the bad data
m.s.data <- subset(m.s.data.all, good_data == 1)

nrow(m.s.data)
nrow(m.s.data.all)
describe(m.s.data.all$good_data)
#gooooooood looking mr. cooking
describe(m.s.data$d_woodburn)
#there are 9 missing d_woodburn for Mtl Summer

#and for toronto. Recall that it is mtl winter that is different, so 26 and 27 are still the correct columns. 
head(t.s.outcomes)
str(t.s.outcomes)
as.numeric(t.s.outcomes[ , 28])
as.numeric(as.character(t.s.outcomes[ , 29]))

#i'll keep them separate for the unpooled unis. I'm sure I could fancy code this, but not tonight cowboy. Will mash togther for the pooled. 
#bring them all together. The t.s.determinants come in with everything except the first column, so I can keep adding variables to the end if I want
t.s.data.all <- data.frame(f.id = t.s.determinants$Filter_ID, bc_conc = as.numeric(t.s.outcomes[ , 28]), uvpm_conc = as.numeric(as.character(t.s.outcomes[ , 29])), good_data = as.factor(t.s.outcomes[ , 30]), t.s.determinants[ , -1])

#just checking to make sure they are the correct length
length(t.s.determinants$Filter_ID)
length(as.numeric(t.s.outcomes[ , 26]))
length(as.numeric(as.character(t.s.outcomes[ , 27])))
nrow(t.s.determinants[ , -1])
head(t.s.data.all[,1:4 ])
str(t.s.data.all)
t.s.data.all[,155:158]

#take out bad data
t.s.data <- subset(t.s.data.all, good_data == 1)

nrow(t.s.data)
nrow(t.s.data.all)
describe(t.s.data.all$good_data)

# Now do MTL Winter
#there is a key to link the site ID of m.outcomes with the filter ID
#need to link m.w.outcomes.raw Site ID with m.s.determinants Filter_ID
#note that all the data is good quality data, 
str(m.w.outcomes.raw)
str(m.w.id.key)
str(m.t.s.outcomes.raw)
str(m.s.determinants)

#add Summer Site from id.key to the m.w.outcomes. also, make m.w.outcomes small to be easire to inspect
m.w.outcomes <- m.w.outcomes.raw[ , c(1,11,12,4)]
m.w.outcomes$SITE
nrow(m.w.outcomes)
nrow(m.w.id.key)
nrow(subset(m.w.outcomes, SITE != "BLANK"))

#no W16, insert a row with summer = NA. this chunk below is 1 way code and will keep making the matrix bigger. If that's a problem, play around with the ##out code below.
#ifelse(m.w.id.key[16,2] == "W16", m.w.id.key[16,2], insertRow(as.matrix(m.w.id.key), v = c(NA, "W16"), r = 16))
m.w.id.key <- insertRow(as.matrix(m.w.id.key), v = c(NA, "W16"), r = 16)
m.w.id.key[16,]
nrow(m.w.id.key)
nrow(subset(m.w.outcomes, SITE != "BLANK"))
#now we're good, same number of rows

m.w.outcomes <- subset(m.w.outcomes, SITE != "BLANK")
nrow(m.w.outcomes)

m.w.outcomes$winter.id <- as.factor(m.w.id.key[ ,2])
m.w.outcomes$summer.id <- as.factor(m.w.id.key[ ,1])

length(m.w.outcomes$summer.id)
length(m.s.outcomes$SITE_ID)
str(m.w.outcomes$summer.id)
str(m.s.outcomes$SITE_ID)

#now try getting m.s.outcomes with site ID and Filter ID. And pulling out only those that are in m.w.outcomes
setdiff(m.s.outcomes$SITE_ID, m.w.outcomes$summer.id)
#57 in m.s that aren't in m.w
setdiff(m.w.outcomes$summer.id, m.s.outcomes$SITE_ID)
#3 in m.w that aren't in m.s, that includes the NA. This is weird. These are MTL_space_114 and MTL_space_115 those are the ones that are strangely missing from the determinnants. Susannah is looking into this. 
#now there are no differences, we've got those 2 back!
length(m.s.outcomes$SITE_ID)
length(m.w.outcomes$summer.id)

m.w.sum.site.sumfilt.id <- m.s.outcomes[m.s.outcomes$SITE_ID %in% m.w.outcomes$summer.id, ]
m.w.sum.site.sumfilt.id <- data.frame(id.summer = m.w.sum.site.sumfilt.id$SITE_ID, filter = m.w.sum.site.sumfilt.id$filter)

m.w.sum.site.sumfilt.id <- m.w.sum.site.sumfilt.id[order(m.w.sum.site.sumfilt.id$id.summer),]
m.w.outcomes <- m.w.outcomes[order(m.w.outcomes$summer.id),]
m.w.outcomes[c(-NA),]

m.w.outcomes.w.id.convert <- data.frame(m.w.outcomes[! m.w.outcomes$summer.id %in% c(NA),], m.w.sum.site.sumfilt.id)
#aw shit yeah, got the summer ids lined up! now should just spot a couple of them

#next steps: sort the id.convert df by filter, then tack on the outcomes. 
m.w.outcomes.w.id.convert <- m.w.outcomes.w.id.convert[order(m.w.outcomes.w.id.convert$filter),]
m.w.determinants <-  m.s.determinants[m.s.determinants$Filter_ID %in% m.w.outcomes.w.id.convert$filter, ]
all.equal(m.w.determinants$Filter_ID, m.w.outcomes.w.id.convert$filter)
#if TRUE, we good. It's true. We good.


m.w.data <- data.frame(m.w.outcomes.w.id.convert, m.w.determinants)
#spot check
slice(m.w.data[ , 1:10])
#spot checked end, W60 (what was missing), and a couple other randos in the middle. We good!
str(m.w.data)
#need to change colnames
colnames(m.w.data)[2:3] <- c("bc_conc", "uvpm_conc")


####MTL Annual
#need to bring m.s.data and m.w.data together
str(m.s.data[ , 1:5])
str(m.w.data[ , 1:10])

nrow(filter(m.s.data, f.id %in% m.w.data$Filter_ID))
nrow(m.w.data)
#there's two rows in m.w.data that aren't in m.s.data
setdiff(m.w.data$Filter_ID, m.s.data$f.id)
#they are space_71 and 94, both are "bad quality data"
subset(m.s.data.all, f.id == "MTL_space_71")
subset(m.s.data.all, f.id == "MTL_space_94")
#take those two out of m.w.data, save as annnual, then add column of summer data that matches the winter

m.a.s.data <- filter(m.s.data, f.id %in% m.w.data$Filter_ID)
nrow(m.a.s.data)

m.a.w.data <- filter(m.w.data, Filter_ID %in% m.a.s.data$f.id)
nrow(m.a.w.data)
nrow(m.w.data)

#label the columns before combining in order to keep track
colnames(m.a.w.data)[1:3] <- c("f.id.winter", "w.bc_conc", "w.uvpm_conc")
colnames(m.a.s.data)[1:3] <- c("f.id.summer", "s.bc_conc", "s.uvpm_conc")
colnames(m.a.w.data)[1:10]
colnames(m.a.s.data)[1:10]

#jsut working on the outcomes, add determinants later. No need for quality data column. 
m.a.data <- data.frame(f.id.summer = m.a.s.data$f.id.summer, m.a.w.data$Filter_ID, m.a.w.data[ ,1:3], m.a.s.data[2:3])
head(m.a.data)
all.equal(m.a.data[,1], m.a.data[,2])
#filter.id summmer names line up and match, so we can delete the duplicate column 

m.a.data <- dplyr::select(m.a.data, -2)
head(m.a.data)


m.a.data <- mutate(m.a.data, a.bc_conc = (w.bc_conc + s.bc_conc)/2, d.bc = round((w.bc_conc - s.bc_conc)/w.bc_conc*100, 0), a.uvpm_conc = (w.uvpm_conc + s.uvpm_conc)/2, d.uvpm = round((w.uvpm_conc - s.uvpm_conc)/w.uvpm_conc*100, 0))
# I added d. columns that are the % change in concentration between summer and winter. There are some really big values. 
#can take away those when making the "long" data frame for regressions. 

#slap the determinants in there
#make sure they are all the same
all.equal(m.a.s.data[ , 5:ncol(m.a.s.data)], m.a.w.data[ , 10:ncol(m.a.w.data)])

m.a.data <- data.frame(m.a.data, m.a.s.data[ , 5:ncol(m.a.s.data)])
str(m.a.data)
str(m.a.data[ , 160:172])
#woodburn is in there

m.s.v.w.change <- m.a.data[, 1:10]
colnames(m.s.v.w.change)[c(8,10)] <- c("BC % Change", "UVPM % Change")
formattable(m.s.v.w.change)

write.csv(m.s.v.w.change, "m.s.v.w.change.csv")

#Outlier Inspection #####

which.max(m.s.data$bc_conc)
formattable(m.s.data[42,])
#one of the new MTL points is very large, new outlier

which.max(t.s.data$bc_conc)
t.s.data[75, ]
formattable(t.s.data[75,])

which.max(t.s.outcomes$BC_ng_m3)
t.outlier <- t.s.outcomes[97,]
formattable(t.s.outcomes[97,])

formattable(subset(m.s.data, bc_conc > 4000))
formattable(subset(m.s.data.stan, bc_conc > 4000))
describe(subset(m.s.data, bc_conc > 4000))
which.max(m.s.data.stan$woodburn_500m)
max(m.s.data.stan$woodburn_500m)
max(m.s.data$woodburn_500m)
max(m.s.data$woodburn_750m)
max(m.s.data$woodburn_1000m)
head(m.s.outcomes)
formattable(subset(m.s.outcomes, BC_ng_m3 > 10000))
#we see later that woodburn_500m is the only wb buffer that shows up anywhere and it only shows up when all MTL summer data is around. 
#It's the 12k data point that has the max woodburn_500m value
#Note that it doesn't have a max value for any of the other woodburns, but has high _750 and _1000

max(m.a.data$a.bc_conc, na.rm = T)
which.max(m.a.data$a.bc_conc)
formattable(filter(m.a.data.stan, bc_conc > 4000))
#no big ones with large woodburn in winter. The 12k from summer doesn't have a winter value

formattable(subset(m.s.outcomes, BC_ng_m3 > 4000))
str(t(subset(m.s.outcomes, BC_ng_m3 > 4000)))

m.s.outlier <- subset(m.s.outcomes[ ,-1], BC_ng_m3 > 4000)
formattable(m.s.out)

write.csv(t.outlier, "t.outlier.csv")
write.csv(m.s.outlier, "m.outlier.csv")

length(colnames(m.s.determinants))
length(colnames(t.s.determinants))
nrow(m.s.data)
describe(m.s.data$bc_conc)
describe(m.a.data$a.bc_conc)
describe(m.w.data$bc_conc)
describe(t.s.data$bc_conc)
describe(mts.data.pool$bc_conc)
#so many Toronto sites without BC measures. Is that okay? Double check to make sure
looky <- read.csv("Montreal and Toronto SPATIAL STUDY FILTER DATA and Chain of Custody 29-05-2019 SR.csv")
formattable(dplyr::filter(looky, city == "Toronto"))
nrow(dplyr::filter(looky, city == "Toronto"))
nrow(dplyr::filter(looky, city == "Toronto" & !is.na(BC_ng_m3)))
nrow(dplyr::filter(looky, city == "Toronto" & !is.na(BC_ng_m3) & Good_quality == 1))
#yep, it's okay
describe(t.s.outcomes$Good_quality)
dplyr::filter(looky, BC_ng_m3 > 30000)
dplyr::filter(t.s.determinants, Filter_ID == "TO_space_94")



###Toronto actually has some wierd IVs:
#4 observations that have Nox_1000m of 0 but a Nox_750m that is non-zero. Three of them have "good data" that makes it through and becomes part of the analysis. 
filter(t.s.data, tot_Nox_1000m == 0)
filter(t.s.data, tot_Nox_750m == 0)

#toronto has crazy low number of bus stops
describe(t.s.data$bus_stop_1000m)

#toronto also has an observation of inter_1000m = 0......so how did they drive there?!
filter(t.s.data, inter_1000m == 0)




# MTL S Histograms ####

####determinants 

#note that uvpm_conc was a factor and I changed to numeric. Blank and "too dark" entries are now NA.
#note: majrd and mjrd are same categories
#note: no bus_stop_50m category
#Note: NPRI_PM25_1000m vs NPRI_PM_750m
#note:  Missing NPRI_Nox_100m and 50m
#note: missing pop_300m, 200m, 100m, and 50m
#note: MTL_space_23 has a huge BC and UVPM values
#what other info from the outcomes should I keep in the data frame?

###MTL Histograms
#take a quick gander at some of the distributions
hist(m.s.data$bc_conc, breaks = 100)
hist(m.s.data$bc_conc, breaks = 100, xlim = c(0, 4000))
#With the bad data taken out, the one out at 30k (MTL_space_23) is not longer there. Looks normallish under 3000. 
#another little clump of 4 observations out at ~8000, note that TO has many values under 10k. That's not an unreasonable amount of bc
#bulk of it is under 5k
#one of the 3 new data points is very large. It's supposed to be good data. 

#look at log histogram
hist(log(m.s.data$bc_conc), breaks = 100)

hist(m.s.data$uvpm_conc, breaks = 100)
hist(m.s.data$bc_conc, breaks = 100, xlim = c(0, 4000))
#One out at 22k (MTL_space_23). Looks normallish under 3000. 
describe(m.s.data$uvpm_conc)
describe(m.s.data$bc_conc)

hist(m.s.data$d_woodburn)
describe(m.s.data$d_woodburn)
describe(m.s.data$woodburn_500m)

hist(m.s.data$woodburn_100m)
hist(m.s.data$woodburn_200m)
hist(m.s.data$woodburn_500m)
hist(m.s.data$woodburn_750m)
hist(m.s.data$woodburn_1000m)

hist(m.s.data$build_1000m)
hist(m.s.data$build_750m)
hist(m.s.data$build_500m)
hist(m.s.data$build_300m)
hist(m.s.data$build_200m)
hist(m.s.data$build_100m)
hist(m.s.data$build_50m)
#looks like linear decline for 1000m, then looks more and more like -exp()

hist(m.s.data$com_1000m)
hist(m.s.data$com_1000m, breaks = 100, xlim = c(10000, 500000), ylim = c(0, 10))
hist(m.s.data$com_500m)
hist(m.s.data$com_50m)
#a lot of small values

hist(m.s.data$gov_1000m)
hist(m.s.data$gov_500m)
hist(m.s.data$gov_50m)
# a lot of small values...mkay, they are all looking pretty similar. Assume that no comment means a lot of small values

hist(m.s.data$open_1000m)
hist(m.s.data$open_500m)
hist(m.s.data$open_50m)

hist(m.s.data$resid_1000m)
hist(m.s.data$resid_750m)
hist(m.s.data$resid_500m)
hist(m.s.data$resid_300m)
hist(m.s.data$resid_200m)
hist(m.s.data$resid_100m)
hist(m.s.data$resid_50m)
#more high values. The 1000m looks normalish. The 50 m is mostly large values, but has a nice chunk of very small

hist(m.s.data$ind_1000m)
hist(m.s.data$ind_500m)
hist(m.s.data$ind_50m)

hist(m.s.data$water_1000m)
hist(m.s.data$water_500m)
hist(m.s.data$water_50m)

hist(m.s.data$highway_1000m)
hist(m.s.data$highway_750m)
hist(m.s.data$highway_500m)
hist(m.s.data$highway_50m)

hist(m.s.data$majrd_1000m)
hist(m.s.data$mjrd_750m)
hist(m.s.data$mjrd_500m)
hist(m.s.data$mjrd_300m)
hist(m.s.data$mjrd_50m)
#more evenly distributed. 

hist(m.s.data$road_1000m)
hist(m.s.data$road_750m)
hist(m.s.data$road_500m)
hist(m.s.data$road_300m)
hist(m.s.data$road_200m)
hist(m.s.data$road_100m)
hist(m.s.data$road_50m)
#more highr numbers, closer to looking normal. Makes sense, there are roads in cities....

hist(m.s.data$d_highway)

hist(m.s.data$d_majrd)

hist(m.s.data$bus_1000m)
hist(m.s.data$bus_750m)
hist(m.s.data$bus_500m)
hist(m.s.data$bus_300m)
hist(m.s.data$bus_200m)
hist(m.s.data$bus_100m)
hist(m.s.data$bus_50m)
#the peak is at larger values for 1000m and slowly moves down until it is at zero/low numbers for 300m

hist(m.s.data$bus_stop_1000m)
hist(m.s.data$bus_stop_750m)
hist(m.s.data$bus_stop_500m)
hist(m.s.data$bus_stop_300m)
hist(m.s.data$bus_stop_200m)
hist(m.s.data$bus_stop_100m)
#more normal looking until 300m, though that is still a straight decline. 200m and 100m are mostly low values. 
#note there is no bus_stop_50m

hist(m.s.data$inter_1000m)
hist(m.s.data$inter_750m)
hist(m.s.data$inter_500m)
hist(m.s.data$inter_300m)
hist(m.s.data$inter_200m)
hist(m.s.data$inter_100m)
hist(m.s.data$inter_50m)
#pretty normal looking until 200m. Then typical shape. Note that for 50 m, there are only 3 values (0,1,2), which makes sense

hist(m.s.data$traffic_1000m)
hist(m.s.data$traffic_500m)
hist(m.s.data$traffic_50m)

hist(m.s.data$tot_traffic_1000m)
hist(m.s.data$tot_traffic_750m)
hist(m.s.data$tot_traffic_50m)

hist(m.s.data$Nox_1000m)
hist(m.s.data$Nox_750m)
hist(m.s.data$Nox_500m)
hist(m.s.data$Nox_50m)

hist(m.s.data$tot_Nox_1000m)
hist(m.s.data$tot_Nox_500m)
hist(m.s.data$tot_Nox_50m)

hist(m.s.data$NPRI_PM25_1000m)
hist(m.s.data$NPRI_PM_750m)
hist(m.s.data$NPRI_PM_500m)
hist(m.s.data$NPRI_PM_300m)
hist(m.s.data$NPRI_PM_200m)
hist(m.s.data$NPRI_PM_100m)
hist(m.s.data$NPRI_PM_50m)
#this is number of chimneys. It's an integer. Most are zero, a couple of 1s. Only 1000m has anything over 2 (and not much)

hist(m.s.data$NPRI_Nox_1000m)
hist(m.s.data$NPRI_Nox_750m)
hist(m.s.data$NPRI_Nox_500m)
hist(m.s.data$NPRI_Nox_200m)
#this is number of chimneys, integer. Most are zero. Fewer than PM chimneys. Missing NPRI_Nox_100m and 50m

hist(m.s.data$d_NPRI_Nox)
#normalish is a long right tail

hist(m.s.data$d_NPRI_PM)
#normalish is a long right tail

hist(m.s.data$d_airport)
#normal, bit of a tail

hist(m.s.data$d_railline)

hist(m.s.data$d_port)
#normal

hist(m.s.data$d_shore)

hist(m.s.data$pop_1000m)
hist(m.s.data$pop_750m)
hist(m.s.data$pop_500m)
#missing pop_300m, 200m, 100m, and 50m

hist(m.s.data$rail_1000m)
hist(m.s.data$rail_500m)
hist(m.s.data$rail_50m)

######End MTL Histograms






# MTL W Histograms ####
#take a quick gander at some of the distributions
hist(m.w.data$bc_conc, breaks = 100)
hist(m.w.data$bc_conc, breaks = 100, xlim = c(0, 7000))
#pretty good looking!

#check log 
hist(log(m.w.data$bc_conc), breaks = 100)

hist(m.w.data$uvpm_conc, breaks = 100)
hist(m.w.data$uvpm_conc, breaks = 100, xlim = c(0, 8000))
#One out at 35k (). Looks normallish under 7000, bit of a right tail
describe(m.w.data$uvpm_conc)
describe(m.w.data$bc_conc)


hist(m.w.data$build_1000m)
hist(m.w.data$build_750m)
hist(m.w.data$build_500m)
hist(m.w.data$build_300m)
hist(m.w.data$build_200m)
hist(m.w.data$build_100m)
hist(m.w.data$build_50m)

hist(m.w.data$com_1000m)
hist(m.w.data$com_1000m, breaks = 100, xlim = c(10000, 500000), ylim = c(0, 10))
hist(m.w.data$com_500m)
hist(m.w.data$com_50m)
#a lot of small values

hist(m.w.data$gov_1000m)
hist(m.w.data$gov_500m)
hist(m.w.data$gov_50m)


hist(m.w.data$open_1000m)
hist(m.w.data$open_750m...25)
hist(m.w.data$open_750m...18)
hist(m.w.data$open_500m)
hist(m.w.data$open_50m)
#open 500 doesn't exist.
#there's also a weird thing wih 750m, it shows up twice. I'm guessing it might be 500m?

hist(m.w.data$resid_1000m)
hist(m.w.data$resid_750m)
hist(m.w.data$resid_500m)
hist(m.w.data$resid_300m)
hist(m.w.data$resid_200m)
hist(m.w.data$resid_100m)
hist(m.w.data$resid_50m)
#more high values. Chunky normal, almost uniformish. The 50 m is mostly large values, but has a lil chunk of very small

hist(m.w.data$ind_1000m)
hist(m.w.data$ind_500m)
hist(m.w.data$ind_50m)

hist(m.w.data$water_1000m)
hist(m.w.data$water_500m)
hist(m.w.data$water_50m)

hist(m.w.data$highway_1000m)
hist(m.w.data$highway_750m)
hist(m.w.data$highway_500m)
hist(m.w.data$highway_50m)

hist(m.w.data$majrd_1000m)
hist(m.w.data$mjrd_750m)
hist(m.w.data$mjrd_500m)
hist(m.w.data$mjrd_300m)
hist(m.w.data$mjrd_50m)
#more evenly distributed. Still a sharp decline

hist(m.w.data$road_1000m)
hist(m.w.data$road_750m)
hist(m.w.data$road_500m)
hist(m.w.data$road_300m)
hist(m.w.data$road_200m)
hist(m.w.data$road_100m)
hist(m.w.data$road_50m)
#closer to looking normal. Makes sense, there are roads in cities.....The MTL of this had more large numbers (I think)

hist(m.w.data$d_highway)

hist(m.w.data$d_majrd)

hist(m.w.data$bus_1000m)
hist(m.w.data$bus_750m)
hist(m.w.data$bus_500m)
hist(m.w.data$bus_300m)
hist(m.w.data$bus_200m)
hist(m.w.data$bus_100m)
hist(m.w.data$bus_50m)
#the peak is in the middle for 1000m (was at larger values for MTL S) and slowly moves down (MTL S was nearly all at zero/low numbers for 300m and below)

hist(m.w.data$bus_stop_1000m)
hist(m.w.data$bus_stop_750m)
hist(m.w.data$bus_stop_500m)
hist(m.w.data$bus_stop_300m)
hist(m.w.data$bus_stop_200m)
hist(m.w.data$bus_stop_100m)
#more normal looking until 300m, though that is still a straight decline. 200m and 100m are mostly low values. 
#note there is no bus_stop_50m

hist(m.w.data$inter_1000m)
hist(m.w.data$inter_750m)
hist(m.w.data$inter_500m)
hist(m.w.data$inter_300m)
hist(m.w.data$inter_200m)
hist(m.w.data$inter_100m)
hist(m.w.data$inter_50m)
#pretty normal looking until 200m. Then typical shape. 
#Note that for 50 m, there are only 3 values (0,1,2), which makes sense

hist(m.w.data$traffic_1000m)
hist(m.w.data$traffic_500m)
hist(m.w.data$traffic_50m)

hist(m.w.data$tot_traffic_1000m)
hist(m.w.data$tot_traffic_750m)
hist(m.w.data$tot_traffic_50m)

hist(m.w.data$Nox_1000m)
hist(m.w.data$Nox_750m)
hist(m.w.data$Nox_500m)
hist(m.w.data$Nox_50m)

hist(m.w.data$tot_Nox_1000m)
hist(m.w.data$tot_Nox_500m)
hist(m.w.data$tot_Nox_50m)

hist(m.w.data$NPRI_PM25_1000m)
hist(m.w.data$NPRI_PM_750m)
hist(m.w.data$NPRI_PM_500m)
hist(m.w.data$NPRI_PM_300m)
hist(m.w.data$NPRI_PM_200m)
#this is number of chimneys. It's an integer. Most are zero, a couple of 1s. Only 1000m has anything over 2 (and not much). 300m and 200m are uniform nothing (ie: all zero). There is no 100m or 50m value

hist(m.w.data$NPRI_Nox_1000m)
hist(m.w.data$NPRI_Nox_750m)
hist(m.w.data$NPRI_Nox_500m)
hist(m.w.data$NPRI_Nox_300m)
hist(m.w.data$NPRI_Nox_200m)
#this is number of chimneys, integer. Most are zero. Fewer than PM chimneys. Missing NPRI_Nox_50m.


hist(m.w.data$d_NPRI_Nox)
#Chunky
#MTL S:normalish is a long right tail

hist(m.w.data$d_NPRI_PM)
#normalish is a long right tail

hist(m.w.data$d_airport)
#normal, bit of a tail

hist(m.w.data$d_railline)

hist(m.w.data$d_port)
#chunky normal

hist(m.w.data$d_shore)

hist(m.w.data$pop_1000m)
hist(m.w.data$pop_750m)
hist(m.w.data$pop_500m)
#missing pop_300m, 200m, 100m, and 50m

hist(m.w.data$rail_1000m)
hist(m.w.data$rail_500m)
hist(m.w.data$rail_300m)
hist(m.w.data$rail_200m)
hist(m.w.data$rail_100m)
hist(m.w.data$rail_50m)
#50m is all zeros




#MTL A Histograms #####

hist(m.a.data$a.bc_conc, breaks = 100)
hist(m.a.data$a.bc_conc, breaks = 100, xlim = c(0, 7000))






#TO Histograms ####
#take a quick gander at some of the distributions
hist(t.s.data$bc_conc, breaks = 100)
hist(t.s.data$bc_conc, breaks = 100, xlim = c(0, 7000))
#One out at 40k (XXX). Looks normallish under 8000, bit of a right tail

#check log
hist(log(t.s.data$bc_conc), breaks = 100)

hist(t.s.data$uvpm_conc, breaks = 100)
hist(t.s.data$uvpm_conc, breaks = 100, xlim = c(0, 8000))
#One out at 35k (). Looks normallish under 7000, bit of a right tail
describe(t.s.data$uvpm_conc)
describe(t.s.data$bc_conc)


hist(t.s.data$build_1000m)
hist(t.s.data$build_750m)
hist(t.s.data$build_500m)
hist(t.s.data$build_300m)
hist(t.s.data$build_200m)
hist(t.s.data$build_100m)
hist(t.s.data$build_50m)
#looks like linear decline for 1000m, then looks more and more like -exp()

hist(t.s.data$com_1000m)
hist(t.s.data$com_1000m, breaks = 100, xlim = c(10000, 500000), ylim = c(0, 10))
hist(t.s.data$com_500m)
hist(t.s.data$com_50m)
#a lot of small values

hist(t.s.data$gov_1000m)
hist(t.s.data$gov_500m)
hist(t.s.data$gov_50m)
# a lot of small values...mkay, they are all looking pretty similar. Assume that no comment means a lot of small values

hist(t.s.data$open_1000m)
hist(t.s.data$open_500m)
hist(t.s.data$open_50m)

hist(t.s.data$resid_1000m)
hist(t.s.data$resid_750m)
hist(t.s.data$resid_500m)
hist(t.s.data$resid_300m)
hist(t.s.data$resid_200m)
hist(t.s.data$resid_100m)
hist(t.s.data$resid_50m)
#more high values. The 1000m looks normalish. The 50 m is mostly large values, but has a lil chunk of very small

hist(t.s.data$ind_1000m)
hist(t.s.data$ind_500m)
hist(t.s.data$ind_50m)

hist(t.s.data$water_1000m)
hist(t.s.data$water_500m)
hist(t.s.data$water_50m)

hist(t.s.data$highway_1000m)
hist(t.s.data$highway_750m)
hist(t.s.data$highway_500m)
hist(t.s.data$highway_50m)

hist(t.s.data$majrd_1000m)
hist(t.s.data$mjrd_750m)
hist(t.s.data$mjrd_500m)
hist(t.s.data$mjrd_300m)
hist(t.s.data$mjrd_50m)
#more evenly distributed. Still a sharp decline

hist(t.s.data$road_1000m)
hist(t.s.data$road_750m)
hist(t.s.data$road_500m)
hist(t.s.data$road_300m)
hist(t.s.data$road_200m)
hist(t.s.data$road_100m)
hist(t.s.data$road_50m)
#closer to looking normal. Makes sense, there are roads in cities.....The MTL of this had more large numbers (I think)

hist(t.s.data$d_highway)

hist(t.s.data$d_majrd)

hist(t.s.data$bus_1000m)
hist(t.s.data$bus_750m)
hist(t.s.data$bus_500m)
hist(t.s.data$bus_300m)
hist(t.s.data$bus_200m)
hist(t.s.data$bus_100m)
hist(t.s.data$bus_50m)
#the peak is in the middle for 1000m (was at larger values for MTL) and slowly moves down (MTL was nearly all at zero/low numbers for 300m and below)

hist(t.s.data$bus_stop_1000m)
hist(t.s.data$bus_stop_750m)
hist(t.s.data$bus_stop_500m)
hist(t.s.data$bus_stop_300m)
hist(t.s.data$bus_stop_200m)
hist(t.s.data$bus_stop_100m)
#mostly small values, not like MTL at all
#MTL was: more normal looking until 300m, though that is still a straight decline. 200m and 100m are mostly low values. 
#note there is no bus_stop_50m

hist(t.s.data$inter_1000m)
hist(t.s.data$inter_750m)
hist(t.s.data$inter_500m)
hist(t.s.data$inter_300m)
hist(t.s.data$inter_200m)
hist(t.s.data$inter_100m)
hist(t.s.data$inter_50m)
#pretty normal looking until 200m. Then typical shape. 
#MTL had: Note that for 50 m, there are only 3 values (0,1,2), which makes sense

hist(t.s.data$traffic_1000m)
hist(t.s.data$traffic_500m)
hist(t.s.data$traffic_50m)

hist(t.s.data$tot_traffic_1000m)
hist(t.s.data$tot_traffic_750m)
hist(t.s.data$tot_traffic_50m)

hist(t.s.data$Nox_1000m)
hist(t.s.data$Nox_750m)
hist(t.s.data$Nox_500m)
hist(t.s.data$Nox_50m)

hist(t.s.data$tot_Nox_1000m)
hist(t.s.data$tot_Nox_500m)
hist(t.s.data$tot_Nox_50m)

hist(t.s.data$NPRI_PM25_1000m)
hist(t.s.data$NPRI_PM_750m)
hist(t.s.data$NPRI_PM_500m)
hist(t.s.data$NPRI_PM_300m)
hist(t.s.data$NPRI_PM_200m)
hist(t.s.data$NPRI_PM_100m)
hist(t.s.data$NPRI_PM_50m)
#this is number of chimneys. It's an integer. Most are zero, a couple of 1s. Only 1000m has anything over 2 (and not much)

hist(t.s.data$NPRI_Nox_1000m)
hist(t.s.data$NPRI_Nox_750m)
hist(t.s.data$NPRI_Nox_500m)
hist(t.s.data$NPRI_Nox_300m)
hist(t.s.data$NPRI_Nox_200m)
hist(t.s.data$NPRI_Nox_100m)
#this is number of chimneys, integer. Most are zero. Fewer than PM chimneys. Missing NPRI_Nox_50m. 300, 200, and 100 are completely zero.


hist(t.s.data$d_NPRI_Nox)
#Chunky
#MTL:normalish is a long right tail

hist(t.s.data$d_NPRI_PM)
#normalish is a long right tail

hist(t.s.data$d_airport)
#normal, bit of a tail

hist(t.s.data$d_railline)

hist(t.s.data$d_port)
#chunky normal, MtL might be more normal looking

hist(t.s.data$d_shore)

hist(t.s.data$pop_1000m)
hist(t.s.data$pop_750m)
hist(t.s.data$pop_500m)
#missing pop_300m, 200m, 100m, and 50m

hist(t.s.data$rail_1000m)
hist(t.s.data$rail_500m)
hist(t.s.data$rail_50m)

###end TO histograms





str(m.s.data[,1:6])
str(m.w.data[,1:6])
str(t.s.data[,1:6])
all.data <- bind_rows(mutate(m.s.data[,1:3], data.set = rep("ms", nrow(m.s.data))), 
                      mutate(m.w.data[,1:3], data.set = rep("mw", nrow(m.w.data))), 
                      mutate(t.s.data[,1:3], data.set = rep("ts", nrow(t.s.data))))
ggplot(data = all.data, aes(x = bc_conc, color = data.set)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge") +
  labs(title = "Histogram of All Data", x = "BC Concentration")

ggplot(data = subset(all.data, bc_conc < 10000), aes(x = bc_conc, color = data.set)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge") +
  labs(title = "Histogram of BC < 10,000", x = "BC Concentration")

ggplot(data = all.data, aes(x = uvpm_conc, color = data.set)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge") +
  labs(title = "Histogram of All Data", x = "UVPM Concentration")

ggplot(data = subset(all.data, uvpm_conc < 10000), aes(x = bc_conc, color = data.set)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge") +
  labs(title = "Histogram of BC < 10,000", x = "UVPM Concentration")


str(all.data)

# Standardizing ####
###MTL Summer
#time to standardize it
#this is how to standardize just part of the data frame
NCOL(m.s.data)
colnames(m.s.data)[c(-1,-2,-3,-4,-165,-166)]
summary(scale(m.s.data[ , c(-1,-2,-3,-4,-165,-166)]))
m.s.data.stan <- data.frame(m.s.data[ , 1:4], m.s.data[ , 165:166], scale(m.s.data[ , c(-1,-2,-3,-4,-165,-166)]))
summary(m.s.data.stan)
#can see in the summary that all the means are zero
apply(m.s.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!


##MTL Winter
NCOL(m.w.data)
colnames(m.w.data)[c(-1:-9, -170, -171)]
summary(scale(m.w.data[ , c(-1:-9, -170, -171)]))
m.w.data.stan <- data.frame(m.w.data[ , 1:9], m.w.data[ , 170:171], scale(m.w.data[ , c(-1:-9, -170, -171)]))
summary(m.w.data.stan)
#can see in the summary that all the means are zero
#note:NPRI_PM_300m, 200m, and rail_50m are NA, need to remove them for the regressions 

apply(m.w.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!

describe(m.w.data$NPRI_PM_300m)
describe(m.w.data$NPRI_PM_200m)
describe(m.w.data$rail_50m)

#they are entirely zero, take em out!
m.w.data.stan <- subset(m.w.data.stan, select = -c(NPRI_PM_300m, NPRI_PM_200m, rail_50m))
summary(m.w.data.stan)
#looks like MTL W data is all good now. Note that I only modified the standardized data frame. If I do stuff with the non-stan, may consider removing those variables there too
str(m.w.data.stan)

#alsooo, there are the extra columns I used when lining up the filter IDs. Pull those out just to make it a bit shorter. Leave in the winter and summer filter IDs, but pull every else.
head(m.w.data.stan[,1:12])
head(m.w.data.stan[,10:11])
m.w.data.stan <- dplyr::select(m.w.data.stan, 1, 9, 2:3, 10:11, 12:ncol(m.w.data.stan))
str(m.w.data.stan)

####MTL Annual
colnames(m.a.data)[c(1:12)]
colnames(m.a.data)[c(-1:-10)]
summary(scale(m.a.data[ , c(-1:-10)]))
ncol(m.a.data)
m.a.data.stan <- data.frame(m.a.data[ , 1:10], m.a.data[ , 171:172], scale(m.a.data[ , c(-1:-10, -171, -172)]))
summary(m.a.data.stan)

#NAs for NPRI_PM_300m  NPRI_PM_200m rail_50m, need to remove them for the regressions 

apply(m.a.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!

describe(m.a.data$NPRI_PM_300m)
describe(m.a.data$NPRI_PM_200m)
describe(m.a.data$rail_50m)

#they are entirely zero, take em out!
m.a.data.stan <- subset(m.a.data.stan, select = -c(NPRI_PM_300m, NPRI_PM_200m, rail_50m))
summary(m.a.data.stan)
#looks like MTL A data is all good now. Note that I only modified the standardized data frame. If I do stuff with the non-stan, may consider removing those variables there too
str(m.a.data.stan)

#alsooo, there are the extra columns I used for calculations and out of interest. Pull those out just to make it a bit shorter. Leave in the winter and summer filter IDs, but pull every else.
head(m.a.data.stan[,1:12])
m.a.data.stan <- dplyr::select(m.a.data.stan, 1:2, 7, 9, 11, 12, 13:ncol(m.a.data.stan))



###TO
#time to standardize it
#this is how to standardize just part of the data frame
colnames(t.s.data)[c(-1,-2,-3,-4)]
summary(scale(t.s.data[ , c(-1,-2,-3,-4, -157, -158)]))
ncol(t.s.data)
t.s.data.stan <- data.frame(t.s.data[ , 1:4], t.s.data[ , 157:158], scale(t.s.data[ , c(-1,-2,-3,-4, -157, -158)]))
summary(t.s.data.stan)
#can see in the summary that all the means are zero
apply(t.s.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!

#same for TO
which.max(t.s.data$bc_conc)
t.s.data[75, 2]
formattable(t.s.data.stan[75, ])
formattable(t.s.data[75, ])
#The TO max has some NaNs for NPRI_Nox_
describe(t.s.data$NPRI_Nox_300m)
describe(t.s.data$NPRI_Nox_200m)
describe(t.s.data$NPRI_Nox_100m)
summary(t.s.data)
#NPRI_NOx_300m, 200m, and 100m are entirely zero, need to do something about it to make sure it doesn't scale them. Actually, don't need em at all! Take em out!
t.s.data.stan <- subset(t.s.data.stan, select = -c(NPRI_PM_300m, NPRI_PM_200m, NPRI_PM_100m, NPRI_Nox_300m, NPRI_Nox_200m, NPRI_Nox_100m))
summary(t.s.data.stan)
#looks like TO data is all good now. Note that I only modified the standardized data frame. If I do stuff with the non-stan, may consider removing those variables there too

#Pooled standardize is down in it's regression. Not sure why. Might reorganize later. 



#### TO+MTL S Pooled

ncol(m.s.data.stan)
ncol(t.s.data.stan)
ncol(m.s.data)
ncol(t.s.data)
#they don't have the same number of variables. Recall that I removed 6 columns from the t.s.data.stan because all the values within the column were the same. Need to keep that column in now that Montreal will add variability ya?. 

#these are in m.s.data, but not in t.s.data (4 vars)
setdiff(colnames(m.s.data.stan), colnames(t.s.data))
#these are in t.s.data, but not in m.s.data (2 vars)
setdiff(colnames(t.s.data), colnames(m.s.data))
#visual check if I want
cbind(colnames(m.s.data), colnames(t.s.data))

#take out the non-matching columns for each one to set up the data that wil be merged for the pooled data frame. 
m.s.data.pool <- m.s.data
m.s.data.pool$water_50m <- NULL
m.s.data.pool$highway_50m <- NULL
m.s.data.pool$bus_stop_200m <- NULL
m.s.data.pool$bus_stop_100m <- NULL


t.s.data.pool <- t.s.data
t.s.data.pool$NPRI_PM_100m <- NULL
t.s.data.pool$NPRI_Nox_100m <- NULL
t.s.data.pool$d_woodburn <- rep(NA, nrow(t.s.data))
t.s.data.pool$woodburn_1000m <- rep(NA, nrow(t.s.data))
t.s.data.pool$woodburn_750m <- rep(NA, nrow(t.s.data))
t.s.data.pool$woodburn_500m <- rep(NA, nrow(t.s.data))
t.s.data.pool$woodburn_200m <- rep(NA, nrow(t.s.data))
t.s.data.pool$woodburn_100m <- rep(NA, nrow(t.s.data))
describe(t.s.data.pool$d_woodburn)
str(t.s.data.pool)
str(t.s.data.pool[, 150:162])  
#woodburn is in there. 

mts.data.pool <- as.data.frame(rbind(m.s.data.pool, t.s.data.pool))
head(mts.data.pool[,1:4])
mts.data.pool$city <- c(rep("MTL", nrow(m.s.data.pool)), rep("TO", nrow(t.s.data.pool)))
describe(mts.data.pool$city)
describe(mts.data.pool)

#standardize them
ncol(mts.data.pool)
colnames(mts.data.pool)[c(-1,-2,-3,-4, -161, -162, -ncol(mts.data.pool))]
summary(scale(mts.data.pool[ , c(-1,-2,-3,-4, -161, -162,-ncol(mts.data.pool))]))
mts.data.pool.stan <- data.frame(mts.data.pool[ , 1:4], city =mts.data.pool$city, latitude.x = mts.data.pool$latitude.x, longitude.x = mts.data.pool$longitude.x,scale(mts.data.pool[ , c(-1,-2,-3,-4, -161, -162, -ncol(mts.data.pool))]))
summary(mts.data.pool.stan)
head(mts.data.pool.stan[ ,1:7])
tail(mts.data.pool.stan[ ,1:7])
#TOs are coded TO and MTLs are coded MTL

#can see in the summary that all the means are zero
apply(mts.data.pool.stan, 2, sd)







# MTL Summer Uni Regressions #####

#####MTL UNI Regressions
str(m.s.data.stan)
long.m.s.data.stan <- melt(m.s.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc", "good_data", "latitude.x", "longitude.x"))
str(long.m.s.data.stan)

#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#check to make sure nothing is too squirrelly
summary(long.m.s.data.stan)
nrow(long.m.s.data.stan)/nrow(m.s.data.stan)
str(m.s.determinants)
#determinants had 163 columns, filter_id, lat, long, and 160 others. Now the data frame is 160 times longer, I think we're good to go
slice(long.m.s.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.m.s.data.stan, 200:210)

####Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution
#https://stackoverflow.com/questions/51567914/hundreds-of-linear-regressions-that-run-by-group-in-r

#do simple regressions on bc_conc for each of the determinants
m.s.bc.uni.beta.p <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.bc.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.bc.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))

options(scipen=999)
#put em together 
m.s.bc.uni.reg <- data.frame(m.s.bc.uni.beta.p[ , c(1,2)], lapply(m.s.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.bc.uni.r2[ ,2:3], 5), m.s.bc.uni.beta.p[ , c(3,4)])
m.s.bc.uni.reg$Beta <- as.numeric(m.s.bc.uni.reg$Beta)
str(m.s.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
m.s.uvpm.uni.beta.p <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.s.uvpm.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.s.uvpm.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$uvpm_conc))))


nrow(m.s.data.stan)
#to see how many observations there are. Cross check this with the dof output. It's goooood. dof = n - k - 1; k is # of parameters, 1 for all of these here unis. 

m.s.uvpm.uni.reg <- data.frame(m.s.uvpm.uni.beta.p[ , c(1,2)], lapply(m.s.uvpm.uni.cis[ , c(2,3)], as.numeric), m.s.uvpm.uni.r2[ ,2:3], m.s.uvpm.uni.beta.p[ , c(3,4)])
m.s.uvpm.uni.reg$Beta <- as.numeric(m.s.uvpm.uni.reg$Beta)

#write them. Now I can use these excels as a quick load for the data if I want
write.csv(m.s.bc.uni.reg, file = "MTL_S_BC_Uni_Regressions.csv")
write.csv(m.s.uvpm.uni.reg, file = "MTL_S_UVPM_Uni_Regressions.csv")
write.csv(m.s.data, file = "montreal_summer_data.csv")
write.csv(m.s.data.stan, file = "montreal_summer_standardized_data.csv")

formattable(m.s.bc.uni.reg)
formattable(m.s.uvpm.uni.reg)

#note: no bus_stop_50m category; it doesn't look like bus_stop was getting there (bus_stop_100m p = 0.742)
#note:  Missing NPRI_Nox_100m and 50m; it doesn't look like the NPRI_Nox that are there are heading towards significance (NPRI_Nox_200m p = 0.760, though the 200m and 300m are the same. Is that a result of data source?)
#note: missing pop_300m, 200m, 100m, and 50m; it doesn't look like pop_ is heading to sig (pop_500m p = 0.669)

#I saw two of the NPRI_Nox had same values in the automated regression. Wanted to make sure the automation worked correctly
summary(lm(data = m.s.data.stan, bc_conc ~ NPRI_Nox_300m))
summary(lm(data = m.s.data.stan, bc_conc ~ NPRI_Nox_200m))
#all the values are the same
all_equal(m.s.data.stan$NPRI_Nox_300m, m.s.data.stan$NPRI_Nox_200m)
all_equal(m.s.data$NPRI_Nox_300m, m.s.data$NPRI_Nox_200m)

#check one of each to make sure it worked
summary(lm(data = m.s.data.stan, formula = bc_conc ~ rail_200m))
summary(lm(data = m.s.data.stan, formula = uvpm_conc ~ pop_500m))
#tried to figure out how to get more info out of the regressions. Not sure if I need more, but curious. The code turns the regression into a tibble like this:
tidy(lm(data = m.s.data.stan, formula = uvpm_conc ~ pop_500m))
tidy(confint(lm(data = m.s.data.stan, formula = uvpm_conc ~ pop_500m)))

#MTL Summer Uni Reg -Outliers #####

#based on the graphs below, we see that there is between 1 to 5 outliers that really drive the fits. I'll take a quick look at them to see what's going on. 
#They may be bad data points depending on the volumes used or time run. Or maybe they are points that we don't want in our model (eg: right on a train, we want to describe city living, not train living) 
#one of teh outliers was a "bad data", but the other four are considered good data
describe(subset(m.s.data.stan, bc_conc > 4000))
describe(subset(m.s.outcomes, BC_ng_m3 > 4000))
formattable(subset(m.s.outcomes, Monitor_type == "Harvard" & BC_ng_m3 > 4000))
formattable(subset(m.s.outcomes, Monitor_type == "UPAS" & BC_ng_m3 > 4000))

#the one realy bad one has been removed, but not the other 4

#duty cycle is na or 76, same with flow check
#calculated sample runtime is ~15k or missing, same with calculated sample volume, 
#sample volume NA or ~15 // Harv outlier has 0
#an outlier note has "run interupted" (Harv)
#an outlier has a stop date of June 8th (0 or 1 day after run started), that is different than most (Harv)
#1 outlier is harvard, others are upas. UPAS don't need volume_m3 (see note on next line)
#4 outliers have volume_m3 missing, one outlier has volume 1.16 (Harvard monitor, that's the crazy high one), most others have 100 (same for st_vol_m3)
#outlier PM2.5_mg seems possible/reasonable
#outlier PM2.5_ug seems possible/reasonable, except one might be unusually LOW (Harv)
#PM2.5_ug_m3 seems possible/reasonable, except one might be unusually high (Harv)
#BC_ng/m3 ad UVPM_ng.m3 are very high. (that's how these outliers were selected)
#1 outlier did not run for 10 days

#summary: 
#largest outlier is a Harvard monitor that only ran for 1 day and a volumer_m3 of 1.16 (vice ~100) . it has BC_ng_m3 of 30103 (vice under 5000k)
#The other four large outliers are not evidently wrong. Not sure what's going on there after my initial glance. Maybe these monitors worked perfectly fine and they are just very distintct places in the city?
  #could take a look at the determinants of these four outliers
#here are the names of them
str(subset(m.s.outcomes, BC_ng_m3 > 45000)$filter)
subset(m.s.outcomes, BC_ng_m3 > 4000 & BC_ng_m3 < 10000)$filter
subset(m.s.data.stan, bc_conc < 4000)

#do unis without all 4 outliers
#do simple regressions on bc_conc for each of the determinants

m.s.outlier.level <- 4000
m.s.u4k.bc.uni.beta.p <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.u4k.bc.uni.cis <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.u4k.bc.uni.r2 <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
m.s.u4k.bc.uni.reg <- data.frame(m.s.u4k.bc.uni.beta.p[ , c(1,2)], lapply(m.s.u4k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.u4k.bc.uni.r2[ ,2:3], 5), m.s.u4k.bc.uni.beta.p[ , c(3,4)])
m.s.u4k.bc.uni.reg$Beta <- as.numeric(m.s.u4k.bc.uni.reg$Beta)

nrow(subset(m.s.u4k.bc.uni.reg, P.Value < 0.05))
nrow(subset(m.s.bc.uni.reg, P.Value < 0.05))
#8 with all data and 41 with the 4 over 5k outliers cut out
#with new observations, it's 10 and 39. d_woodburn doesn't change it. 
#one of the woodburns buffers shows up for the whole data set, not the u4k data. 

formattable(m.s.u4k.bc.uni.reg)
formattable(subset(m.s.u4k.bc.uni.reg, P.Value <= 0.05))



#repeat for uvpm
m.s.u4k.uvpm.uni.beta.p <- subset(long.m.s.data.stan, uvpm_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.u4k.uvpm.uni.cis <- subset(long.m.s.data.stan, uvpm_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.u4k.uvpm.uni.r2 <- subset(long.m.s.data.stan, uvpm_conc < m.s.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$uvpm_conc))))

#put em together 
m.s.u4k.uvpm.uni.reg <- data.frame(m.s.u4k.uvpm.uni.beta.p[ , c(1,2)], lapply(m.s.u4k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(m.s.u4k.uvpm.uni.r2[ ,2:3], 5), m.s.u4k.uvpm.uni.beta.p[ , c(3,4)])
m.s.u4k.uvpm.uni.reg$Beta <- as.numeric(m.s.u4k.uvpm.uni.reg$Beta)

nrow(subset(m.s.u4k.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.uvpm.uni.reg, P.Value <= 0.05))
#3 with all data and 33 with the 4 over 4k outliers cut out
#with new observations, it's 7 and 38

formattable(m.s.u4k.uvpm.uni.reg)
formattable(subset(m.s.u4k.uvpm.uni.reg, P.Value <= 0.05))



write.csv(m.s.u4k.bc.uni.reg, file = "MTL_S_u4k_BC_Uni_Regressions.csv")
write.csv(m.s.u4k.uvpm.uni.reg, file = "MTL_S_u4k_UVPM_Uni_Regressions.csv")






###try cutting out the zeros and the over 4k


m.s.outlier.level <- 4000
m.s.o0u4k.bc.uni.beta.p <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level & bc_conc > 0) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.o0u4k.bc.uni.cis <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level & bc_conc > 0) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.o0u4k.bc.uni.r2 <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level & bc_conc > 0) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))
nrow(m.s.o0u4k.bc.uni.beta.p)
nrow(m.s.o0u4k.bc.uni.cis)
nrow(m.s.o0u4k.bc.uni.r2)

#rail_50 is NA, it gets included in the CIs and R2, but not in the beta.p for some reason. Need to cut it out. 
setdiff(m.s.o0u4k.bc.uni.beta.p$variable, m.s.o0u4k.bc.uni.cis$variable)
setdiff(m.s.o0u4k.bc.uni.cis$variable, m.s.o0u4k.bc.uni.beta.p$variable)
describe(m.s.data.stan$rail_50m)
#it's all zeros and 1 high value. Not an informative variable.
filter(m.s.o0u4k.bc.uni.beta.p, variable == "rail_50m")
filter(m.s.o0u4k.bc.uni.cis, variable == "rail_50m")
filter(m.s.o0u4k.bc.uni.r2, variable == "rail_50m")

#for bringing it all together, it's better to insert a row
m.s.o0u4k.bc.uni.beta.p <- add_row(m.s.o0u4k.bc.uni.beta.p, variable = setdiff(m.s.o0u4k.bc.uni.cis$variable, m.s.o0u4k.bc.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.s.o0u4k.bc.uni.cis$`2.5%`)))

#put em together 
m.s.o0u4k.bc.uni.reg <- data.frame(m.s.o0u4k.bc.uni.beta.p[ , c(1,2)], lapply(m.s.o0u4k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.o0u4k.bc.uni.r2[ ,2:3], 5), m.s.o0u4k.bc.uni.beta.p[ , c(3,4)])
m.s.o0u4k.bc.uni.reg$Beta <- as.numeric(m.s.o0u4k.bc.uni.reg$Beta)

nrow(subset(m.s.o0u4k.bc.uni.reg, P.Value < 0.05))
nrow(subset(m.s.u4k.bc.uni.reg, P.Value < 0.05))
#so there's more. Does it matter?

formattable(m.s.o0u4k.bc.uni.reg)
formattable(subset(m.s.o0u4k.bc.uni.reg, P.Value <= 0.05))

#compared to just the u4k, add ind_1000m, resid_750m, d_railline and remove d_NPRI_PM and rail_50m (I think those get removed anyways). Do a quick check on the plots:
plot(m.s.data.stan$bc_conc, m.s.data.stan$ind_1000m)
?plot
ggplot(data = filter(m.s.data.stan, bc_conc > 0 & bc_conc < 4000), aes(x = d_railline, y = bc_conc)) + 
  geom_point() +   
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#checked those three on three separate plots. They all look pretty decent. ind_1000m looks really good. Try some quick regressions:
m.s.o0u4k.full.multi.lm <- lm(data = filter(m.s.data.stan, bc_conc < 4000 & bc_conc > 0), bc_conc ~ ind_1000m + resid_750m + d_railline + build_1000m + mjrd_750m + d_majrd + bus_50m + bus_stop_750m + traffic_750m + 
                              tot_traffic_750m + Nox_750m + tot_Nox_750m + d_NPRI_Nox + d_airport)
summary(m.s.o0u4k.full.multi.lm)
#0.34 with the u4k data and the o0u4k selected variables
#0.405 with the o0u4k data and the o0u4k selected variables
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000 & bc_conc > 0), bc_conc ~ build_1000m + mjrd_750m + d_majrd + bus_50m + bus_stop_750m + traffic_750m + 
             tot_traffic_750m + Nox_750m + tot_Nox_750m + d_NPRI_Nox + d_airport))
#0.32 for u4k data with the u4k selected variables. 
#0.38 for o0u4k data with the u4k selected variables. 

#conclusion: it's not just the zero values that make Montreal Summer tough to model. There is something else there that we weren't able to capture

# MTL Winter Uni Regressions #####

#####MTL Winter UNI Regressions
str(m.w.data.stan)
colnames(m.w.data.stan)[1:2] <- c("f.id.winter", "f.id")
long.m.w.data.stan <- melt(m.w.data.stan, id.vars = c("f.id", "f.id.winter", "bc_conc", "uvpm_conc", "latitude.x", "longitude.x"))
str(long.m.w.data.stan)

#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#check to make sure nothing is too squirrelly
summary(long.m.w.data.stan)
ncol(m.w.data.stan)
nrow(long.m.w.data.stan)/nrow(m.w.data.stan)
#data.stan had 163 columns, 2 filter ids, 2 outcomes, lat and long, and 157 determinants. Now the data frame is 157 times longer, I think we're good to go
#new woodburning data in there, so numbers changed
slice(long.m.w.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.m.w.data.stan, 200:210)

####Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution
#https://stackoverflow.com/questions/51567914/hundreds-of-linear-regressions-that-run-by-group-in-r

#do simple regressions on bc_conc for each of the determinants
m.w.bc.uni.beta.p <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.w.bc.uni.cis <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.w.bc.uni.r2 <- long.m.w.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.w.data.stan$bc_conc))))


#put em together 
m.w.bc.uni.reg <- data.frame(m.w.bc.uni.beta.p[ , c(1,2)], lapply(m.w.bc.uni.cis[ , c(2,3)], as.numeric), round(m.w.bc.uni.r2[ ,2:3], 5), m.w.bc.uni.beta.p[ , c(3,4)])
m.w.bc.uni.reg$Beta <- as.numeric(m.w.bc.uni.reg$Beta)
str(m.w.bc.uni.reg)


#do simple regressions on uvpm_conc for each of the determinants
m.w.uvpm.uni.beta.p <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.w.uvpm.uni.cis <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.w.uvpm.uni.r2 <- long.m.w.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.w.data.stan$uvpm_conc))))

m.w.uvpm.uni.reg <- data.frame(m.w.uvpm.uni.beta.p[ , c(1,2)], lapply(m.w.uvpm.uni.cis[ , c(2,3)], as.numeric), m.w.uvpm.uni.r2[ ,2:3], m.w.uvpm.uni.beta.p[ , c(3,4)])
m.w.uvpm.uni.reg$Beta <- as.numeric(m.w.uvpm.uni.reg$Beta)

write.csv(m.w.bc.uni.reg, file = "MTL_W_BC_Uni_Regressions.csv")
write.csv(m.w.uvpm.uni.reg, file = "MTL_W_UVPM_Uni_Regressions.csv")
write.csv(m.w.data, file = "montreal_winter_data.csv")
write.csv(m.w.data.stan, file = "montreal_winter_standardized_data.csv")

formattable(m.w.bc.uni.reg)
formattable(m.w.uvpm.uni.reg)


#I saw two of the NPRI_Nox had same values in the automated regression. Wanted to make sure the automation worked correctly
summary(lm(data = m.w.data.stan, bc_conc ~ NPRI_Nox_300m))
summary(lm(data = m.w.data.stan, bc_conc ~ NPRI_Nox_200m))
#all the values are the same
all_equal(m.w.data.stan$NPRI_Nox_300m, m.w.data.stan$NPRI_Nox_200m)
all_equal(m.s.data$NPRI_Nox_300m, m.s.data$NPRI_Nox_200m)

#check one of each to make sure it worked
summary(lm(data = m.w.data.stan, formula = bc_conc ~ rail_200m))
summary(lm(data = m.w.data.stan, formula = uvpm_conc ~ pop_500m))
#tried to figure out how to get more info out of the regressions. Not sure if I need more, but curious. The code turns the regression into a tibble like this:
tidy(lm(data = m.w.data.stan, formula = uvpm_conc ~ pop_500m))
tidy(confint(lm(data = m.w.data.stan, formula = uvpm_conc ~ pop_500m)))

#see how many of each are p < 0.05
nrow(subset(m.w.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.w.uvpm.uni.reg, P.Value <= 0.05))
#58 BC and 45 uvpm
#now 55 and 42, d_woodburn shows up for BC! But none of the woodburn buffers

formattable(subset(m.w.bc.uni.reg, P.Value <= 0.05))
formattable(subset(m.w.uvpm.uni.reg, P.Value <= 0.05))




#MTL Winter Uni Reg -Outliers #####

hist(m.w.data.stan$bc_conc)
hist(m.w.data.stan$uvpm_conc)
#nothing too squirelly. Don't even bother with this. 












# MTL Annual Uni Regressions ######
str(m.a.data.stan)
colnames(m.a.data.stan)[3:4] <- c("bc_conc", "uvpm_conc")
long.m.a.data.stan <- melt(m.a.data.stan, id.vars = c("f.id.summer", "f.id.winter", "bc_conc", "uvpm_conc", "latitude.x", "longitude.x"))
str(long.m.a.data.stan)

#check to make sure nothing is too squirrelly
summary(long.m.a.data.stan)
ncol(m.a.data.stan)
nrow(long.m.a.data.stan)/nrow(m.a.data.stan)
#data.stan had 163 columns, 2 filter ids, 2 outcomes, lat and long, and 157 determinants. Now the data frame is 157 times longer, I think we're good to go
#new variables added, so different number, but still looks correct
slice(long.m.a.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.m.a.data.stan, 200:210)

#do simple regressions on bc_conc for each of the determinants
m.a.bc.uni.beta.p <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.bc.uni.cis <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.bc.uni.r2 <- long.m.a.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.a.data.stan$bc_conc))))

str(m.a.bc.uni.r2)

#com_50 is NA, it gets included in the CIs and R2, but not in the beta.p for some reason. Need to cut it out. 
setdiff(m.a.bc.uni.beta.p$variable, m.a.bc.uni.cis$variable)
setdiff(m.a.bc.uni.cis$variable, m.a.bc.uni.beta.p$variable)
describe(m.a.data.stan$com_50m)
#it's all zeros and 1 high value. Not an informative variable.
filter(m.a.bc.uni.beta.p, variable == "com_50m")
filter(m.a.bc.uni.cis, variable == "com_50m")
filter(m.a.bc.uni.r2, variable == "com_50m")
#get rid of those rows
#m.a.bc.uni.cis <- filter(m.a.bc.uni.cis, !is.na(`2.5%`))
#m.a.bc.uni.r2 <- filter(m.a.bc.uni.r2, r.squared != 0)

#for bringing it all together, it's better to insert a row
m.a.bc.uni.beta.p <- add_row(m.a.bc.uni.beta.p, variable = setdiff(m.a.bc.uni.cis$variable, m.a.bc.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.bc.uni.cis$`2.5%`)))

#put em together 
m.a.bc.uni.reg <- data.frame(m.a.bc.uni.beta.p[ , c(1,2)], lapply(m.a.bc.uni.cis[ , c(2,3)], as.numeric), round(m.a.bc.uni.r2[ ,2:3], 5), m.a.bc.uni.beta.p[ , c(3,4)])
m.a.bc.uni.reg$Beta <- as.numeric(m.a.bc.uni.reg$Beta)
str(m.a.bc.uni.reg)



#do simple regressions on uvpm_conc for each of the determinants
m.a.uvpm.uni.beta.p <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.a.uvpm.uni.cis <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.a.uvpm.uni.r2 <- long.m.a.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.a.data.stan$uvpm_conc))))


#com_50 is NA, it gets included in the CIs and R2, but not in the beta.p for some reason. Need to cut it out. 
setdiff(m.a.uvpm.uni.beta.p$variable, m.a.uvpm.uni.cis$variable)
setdiff(m.a.uvpm.uni.cis$variable, m.a.uvpm.uni.beta.p$variable)
describe(m.a.data.stan$com_50m)
#it's all zeros and 1 high value. Not an informative variable.
filter(m.a.uvpm.uni.beta.p, variable == "com_50m")
filter(m.a.uvpm.uni.cis, variable == "com_50m")
filter(m.a.uvpm.uni.r2, variable == "com_50m")
#get rid of those rows
#m.a.uvpm.uni.cis <- filter(m.a.uvpm.uni.cis, !is.na(`2.5%`))
#m.a.uvpm.uni.r2 <- filter(m.a.uvpm.uni.r2, r.squared != 0)
#for bringing it all ogether later, it's better to insert a row
m.a.uvpm.uni.beta.p <- add_row(m.a.uvpm.uni.beta.p, variable = setdiff(m.a.uvpm.uni.cis$variable, m.a.uvpm.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.uvpm.uni.cis$`2.5%`)))

m.a.uvpm.uni.reg <- data.frame(m.a.uvpm.uni.beta.p[ , c(1,2)], lapply(m.a.uvpm.uni.cis[ , c(2,3)], as.numeric), m.a.uvpm.uni.r2[ ,2:3], m.a.uvpm.uni.beta.p[ , c(3,4)])
m.a.uvpm.uni.reg$Beta <- as.numeric(m.a.uvpm.uni.reg$Beta)

write.csv(m.a.bc.uni.reg, file = "MTL_A_BC_Uni_Regressions.csv")
write.csv(m.a.uvpm.uni.reg, file = "MTL_A_UVPM_Uni_Regressions.csv")
write.csv(m.a.data, file = "montreal_annual_data.csv")
write.csv(m.a.data.stan, file = "montreal_annual_standardized_data.csv")

formattable(m.a.bc.uni.reg)
formattable(m.a.uvpm.uni.reg)


#I saw two of the NPRI_Nox had same values in the automated regression. Wanted to make sure the automation worked correctly
summary(lm(data = m.a.data.stan, bc_conc ~ NPRI_Nox_300m))
summary(lm(data = m.a.data.stan, bc_conc ~ NPRI_Nox_200m))
#all the values are the same
all_equal(m.a.data.stan$NPRI_Nox_300m, m.a.data.stan$NPRI_Nox_200m)
all_equal(m.a.data$NPRI_Nox_300m, m.a.data$NPRI_Nox_200m)

#check one of each to make sure it worked
summary(lm(data = m.a.data.stan, formula = bc_conc ~ rail_200m))
summary(lm(data = m.a.data.stan, formula = uvpm_conc ~ pop_500m))
#tried to figure out how to get more info out of the regressions. Not sure if I need more, but curious. The code turns the regression into a tibble like this:
tidy(lm(data = m.a.data.stan, formula = uvpm_conc ~ pop_500m))
tidy(confint(lm(data = m.a.data.stan, formula = uvpm_conc ~ pop_500m)))

#see how many of each are p < 0.05
nrow(subset(m.a.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.uvpm.uni.reg, P.Value <= 0.05))
#6 BC and 5 uvpm
#now 6 and 6

#if I just blindly take them....
summary(lm(data = m.a.data.stan, bc_conc ~ com_300m + bus_stop_300m + NPRI_PM_500m + d_airport))
#thought maybe those could make a model, but looking at the xy plots, com and bus_stop are not legit. Take them out and it's a 2 variable model, withouth NOx, that has an R2 of 0.2


formattable(subset(m.a.bc.uni.reg, P.Value <= 0.05))
formattable(subset(m.a.uvpm.uni.reg, P.Value <= 0.05))
#sooooo not a lot. Can compare. Maybe these are the most important? We'll see.....



# MTL Annual Uni Regressionts - Outliers ########

filter(m.a.data, a.bc_conc > 4000)
#3 outliers above 4000, MTL_space_106, 112, 134

#do unis without all 3 outliers
#do simple regressions on bc_conc for each of the determinants

m.a.outlier.level <- 4000
m.a.u4k.bc.uni.beta.p <- subset(long.m.a.data.stan, bc_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.u4k.bc.uni.cis <- subset(long.m.a.data.stan, bc_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.u4k.bc.uni.r2 <- subset(long.m.a.data.stan, bc_conc < m.a.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/(sum(!is.na(m.a.data.stan$bc_conc) ) - 3) ) )

#com_50 is NA, it gets included in the CIs and R2, but not in the beta.p for some reason. Need to cut it out. 
setdiff(m.a.u4k.bc.uni.beta.p$variable, m.a.u4k.bc.uni.cis$variable)
setdiff(m.a.u4k.bc.uni.cis$variable, m.a.u4k.bc.uni.beta.p$variable)
describe(m.a.data.stan$com_50m)
#it's all zeros and 1 high value. Not an informative variable.
filter(m.a.u4k.bc.uni.beta.p, variable == "com_50m")
filter(m.a.u4k.bc.uni.cis, variable == "com_50m")
filter(m.a.u4k.bc.uni.r2, variable == "com_50m")
#get rid of those rows
#m.a.u4k.bc.uni.cis <- filter(m.a.u4k.bc.uni.cis, !is.na(`2.5%`))
#m.a.u4k.bc.uni.r2 <- filter(m.a.u4k.bc.uni.r2, r.squared != 0)
#better to insert instead
m.a.u4k.bc.uni.beta.p <- add_row(m.a.u4k.bc.uni.beta.p, variable = setdiff(m.a.u4k.bc.uni.cis$variable, m.a.u4k.bc.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.u4k.bc.uni.cis$`2.5%`)))

#put em together 
#something was going wierd with the build of the data frame. Com_50m and  Gov_50m were swithcing CIs. Rewrote it using dplpyr. I think the output is the smae. Way better with dplyr. Made this change to Toronto Summer too. 
summary(full_join(m.a.u4k.bc.uni.beta.p, m.a.u4k.bc.uni.cis, by = "variable") %>% full_join(m.a.u4k.bc.uni.r2, by = "variable") %>% transmute(variable = variable, Beta = as.numeric(Beta), "2.5%" = as.numeric(`2.5%`), "97.5" = as.numeric(`97.5%`), r.squared, RMSE, SE, `P Value`))

m.a.u4k.bc.uni.reg <- full_join(m.a.u4k.bc.uni.beta.p, m.a.u4k.bc.uni.cis, by = "variable") %>% 
                                full_join(m.a.u4k.bc.uni.r2, by = "variable") %>% 
                                transmute(variable = variable, Beta = as.numeric(Beta), "2.5%" = as.numeric(`2.5%`), "97.5" = as.numeric(`97.5%`), r.squared, RMSE, SE, `P Value`)
summary(m.a.u4k.bc.uni.reg)

nrow(subset(m.a.u4k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.bc.uni.reg, P.Value <= 0.05))
#6 with all data and 25 with the 3 over 4k outliers cut out
#with 3 new obs, it's 6 and 32, d_woodburn doesn't show up

formattable(m.a.u4k.bc.uni.reg)
formattable(subset(m.a.u4k.bc.uni.reg, P.Value <= 0.05))





#repeat for uvpm
m.a.u4k.uvpm.uni.beta.p <- subset(long.m.a.data.stan, uvpm_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.u4k.uvpm.uni.cis <- subset(long.m.a.data.stan, uvpm_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.u4k.uvpm.uni.r2 <- subset(long.m.a.data.stan, uvpm_conc < m.a.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.a.data.stan$uvpm_conc))))

#com_50 is NA, it gets included in the CIs and R2, but not in the beta.p for some reason. Need to cut it out. 
setdiff(m.a.u4k.uvpm.uni.beta.p$variable, m.a.u4k.uvpm.uni.cis$variable)
setdiff(m.a.u4k.uvpm.uni.cis$variable, m.a.u4k.uvpm.uni.beta.p$variable)
describe(m.a.data.stan$com_50m)
#it's all zeros and 1 high value. Not an informative variable.
filter(m.a.u4k.uvpm.uni.beta.p, variable == "com_50m")
filter(m.a.u4k.uvpm.uni.cis, variable == "com_50m")
filter(m.a.u4k.uvpm.uni.r2, variable == "com_50m")
#get rid of those rows
#m.a.u4k.uvpm.uni.cis <- filter(m.a.u4k.uvpm.uni.cis, !is.na(`2.5%`))
#m.a.u4k.uvpm.uni.r2 <- filter(m.a.u4k.uvpm.uni.r2, r.squared != 0)
#better for later to add a row with NA
m.a.u4k.uvpm.uni.beta.p <- add_row(m.a.u4k.uvpm.uni.beta.p, variable = setdiff(m.a.u4k.uvpm.uni.cis$variable, m.a.u4k.uvpm.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.u4k.uvpm.uni.cis$`2.5%`)))


#put em together 
m.a.u4k.uvpm.uni.reg <- data.frame(m.a.u4k.uvpm.uni.beta.p[ , c(1,2)], lapply(m.a.u4k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(m.a.u4k.uvpm.uni.r2[ ,2:3], 5), m.a.u4k.uvpm.uni.beta.p[ , c(3,4)])
m.a.u4k.uvpm.uni.reg$Beta <- as.numeric(m.a.u4k.uvpm.uni.reg$Beta)

nrow(subset(m.a.u4k.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.uvpm.uni.reg, P.Value <= 0.05))
#5 with all data and 18 with the 3 over 4k outliers cut out
#6 and 25 now

formattable(m.a.u4k.uvpm.uni.reg)
formattable(subset(m.a.u4k.uvpm.uni.reg, P.Value <= 0.05))


write.csv(m.a.u4k.bc.uni.reg, file = "MTL_A_u4k_BC_Uni_Regressions.csv")
write.csv(m.a.u4k.uvpm.uni.reg, file = "MTL_A_u4k_UVPM_Uni_Regressions.csv")














#MTL Summer Uni log Regression ########
#####MTL UNI Regressions Log on outcome
#Don't focus on this too much right now

#there are some 0 values for bc that go to -Inf when logged. 
log(long.mts.pool.data.stan$bc_conc)
describe(long.mts.pool.data.stan$bc_conc)
log(long.m.s.data.stan$bc_conc)
describe(long.m.s.data.stan$bc_conc)
#can do a log(bc_conc + 1) transformation
#could also do + mean or + 100. ask them about that.

#do simple regressions on log(bc_conc + 1) for each of the determinants
m.s.log.bc.uni.beta.p <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.log.bc.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.log.bc.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
m.s.log.bc.uni.reg <- data.frame(m.s.log.bc.uni.beta.p[ , c(1,2)], lapply(m.s.log.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.log.bc.uni.r2[ ,2:3], 5), m.s.log.bc.uni.beta.p[ , c(3,4)])
m.s.log.bc.uni.reg$Beta <- as.numeric(m.s.log.bc.uni.reg$Beta)
str(m.s.log.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
m.s.log.uvpm.uni.beta.p <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(uvpm_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.s.log.uvpm.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(uvpm_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.s.log.uvpm.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(uvpm_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$uvpm_conc))))

m.s.log.uvpm.uni.reg <- data.frame(m.s.log.uvpm.uni.beta.p[ , c(1,2)], lapply(m.s.log.uvpm.uni.cis[ , c(2,3)], as.numeric), m.s.log.uvpm.uni.r2[ ,2:3], m.s.log.uvpm.uni.beta.p[ , c(3,4)])
m.s.log.uvpm.uni.reg$Beta <- as.numeric(m.s.log.uvpm.uni.reg$Beta)

write.csv(m.s.log.bc.uni.reg, file = "MTL_S_log_BC_Uni_Regressions.csv")
write.csv(m.s.log.uvpm.uni.reg, file = "MTL_S_log_UVPM_Uni_Regressions.csv")

formattable(m.s.log.bc.uni.reg)
formattable(m.s.log.uvpm.uni.reg)

nrow(subset(m.s.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.bc.uni.reg, P.Value <= 0.05))
#9 with the log transform, 8 without it
# 9 and 12

#MTL Summer Uni log Reg - BC = 0 #####
#probably not necessary to take out the BC > 4000, the log takes care of the outliers. 
#do log unis without all 4 big outliers
#it has trouble with the BC = 0 values

nrow(subset(m.s.data.stan, bc_conc > 0))
m.s.log.o0.bc.uni.beta.p <- subset(long.m.s.data.stan, bc_conc > 0) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.log.o0.bc.uni.cis <- subset(long.m.s.data.stan, bc_conc > 0) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.log.o0.bc.uni.r2 <- subset(long.m.s.data.stan, bc_conc > 0) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))

setdiff(m.s.log.o0.bc.uni.r2$variable, m.s.log.o0.bc.uni.beta.p$variable)
#rail_50m is missing
m.s.log.o0.bc.uni.beta.p <- add_row(m.s.log.o0.bc.uni.beta.p, variable = setdiff(m.s.log.o0.bc.uni.r2$variable, m.s.log.o0.bc.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.s.log.o0.bc.uni.cis$`2.5%`)))

#put em together 
m.s.log.o0.bc.uni.reg <- data.frame(m.s.log.o0.bc.uni.beta.p[ , c(1,2)], lapply(m.s.log.o0.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.log.o0.bc.uni.r2[ ,2:3], 5), m.s.log.o0.bc.uni.beta.p[ , c(3,4)])
m.s.log.o0.bc.uni.reg$Beta <- as.numeric(m.s.log.o0.bc.uni.reg$Beta)

nrow(subset(m.s.log.o0.bc.uni.reg, P.Value < 0.05))
nrow(subset(m.s.u4k.bc.uni.reg, P.Value < 0.05))
nrow(subset(m.s.log.bc.uni.reg, P.Value < 0.05))
#9 with all data and 17 with the BC = 0 taken out
#with extra data and variables: 9 with all and 25 with BC = 0 removed
#no woodburn

setdiff(filter(m.s.log.o0.bc.uni.reg, P.Value < 0.05)$variable, filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable)
setdiff(filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable, filter(m.s.log.o0.bc.uni.reg, P.Value < 0.05)$variable)



# MTL Annual Uni log Reg ####











#do simple regressions on log(bc_conc + 1) for each of the determinants
m.a.log.bc.uni.beta.p <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.log.bc.uni.cis <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.log.bc.uni.r2 <- long.m.a.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))

#different lengths. 
setdiff(m.a.log.bc.uni.cis$variable, m.a.log.bc.uni.beta.p$variable)
#gosh darn com_50m again.....I mean, obviously. Shoulda seen that coming
#insert a row
m.a.log.bc.uni.beta.p <- add_row(m.a.log.bc.uni.beta.p, variable = setdiff(m.a.log.bc.uni.cis$variable, m.a.log.bc.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.log.bc.uni.cis$`2.5%`)))


#put em together 
m.a.log.bc.uni.reg <- data.frame(m.a.log.bc.uni.beta.p[ , c(1,2)], lapply(m.a.log.bc.uni.cis[ , c(2,3)], as.numeric), round(m.a.log.bc.uni.r2[ ,2:3], 5), m.a.log.bc.uni.beta.p[ , c(3,4)])
m.a.log.bc.uni.reg$Beta <- as.numeric(m.a.log.bc.uni.reg$Beta)
str(m.a.log.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
m.a.log.uvpm.uni.beta.p <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(uvpm_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.a.log.uvpm.uni.cis <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(uvpm_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.a.log.uvpm.uni.r2 <- long.m.a.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(uvpm_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$uvpm_conc))))

#different lengths. 
setdiff(m.a.log.uvpm.uni.cis$variable, m.a.log.uvpm.uni.beta.p$variable)
#gosh darn com_50m again.....I mean, obviously. Shoulda seen that coming
#insert a row
m.a.log.uvpm.uni.beta.p <- add_row(m.a.log.uvpm.uni.beta.p, variable = setdiff(m.a.log.uvpm.uni.cis$variable, m.a.log.uvpm.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.log.uvpm.uni.cis$`2.5%`)))



m.a.log.uvpm.uni.reg <- data.frame(m.a.log.uvpm.uni.beta.p[ , c(1,2)], lapply(m.a.log.uvpm.uni.cis[ , c(2,3)], as.numeric), m.a.log.uvpm.uni.r2[ ,2:3], m.a.log.uvpm.uni.beta.p[ , c(3,4)])
m.a.log.uvpm.uni.reg$Beta <- as.numeric(m.a.log.uvpm.uni.reg$Beta)

write.csv(m.a.log.bc.uni.reg, file = "MTL_A_log_BC_Uni_Regressions.csv")
write.csv(m.a.log.uvpm.uni.reg, file = "MTL_A_log_UVPM_Uni_Regressions.csv")

formattable(m.a.log.bc.uni.reg)
formattable(m.a.log.uvpm.uni.reg)

nrow(subset(m.a.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.bc.uni.reg, P.Value <= 0.05))
#11 with the log transform, 6 without it
#now 16 and 6


#try with the BC > 4000 outliers removed
nrow(filter(m.a.data.stan, bc_conc > 4000))
#do simple regressions on log(bc_conc + 1) for each of the determinants
m.a.log.u4k.bc.uni.beta.p <- filter(long.m.a.data.stan, bc_conc < 4000) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.log.u4k.bc.uni.cis <- filter(long.m.a.data.stan, bc_conc < 4000) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.log.u4k.bc.uni.r2 <- filter(long.m.a.data.stan, bc_conc < 4000) %>%
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))

#different lengths. 
setdiff(m.a.log.u4k.bc.uni.cis$variable, m.a.log.u4k.bc.uni.beta.p$variable)
#gosh darn com_50m again.....I mean, obviously. Shoulda seen that coming
#insert a row
m.a.log.u4k.bc.uni.beta.p <- add_row(m.a.log.u4k.bc.uni.beta.p, variable = setdiff(m.a.log.u4k.bc.uni.cis$variable, m.a.log.u4k.bc.uni.beta.p$variable), Beta = NA, SE = NA, `P Value` = NA, .after = which(is.na(m.a.log.u4k.bc.uni.cis$`2.5%`)))


#put em together 
m.a.log.u4k.bc.uni.reg <- data.frame(m.a.log.u4k.bc.uni.beta.p[ , c(1,2)], lapply(m.a.log.u4k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.a.log.u4k.bc.uni.r2[ ,2:3], 5), m.a.log.u4k.bc.uni.beta.p[ , c(3,4)])
m.a.log.u4k.bc.uni.reg$Beta <- as.numeric(m.a.log.u4k.bc.uni.reg$Beta)
str(m.a.log.u4k.bc.uni.reg)

nrow(subset(m.a.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.log.u4k.bc.uni.reg, P.Value <= 0.05))
#11 with the log transform, 22 with log transform and BC > 4,000 removed
#now 16 and 26





#TO Uni Regressions #####

#####TO UNI Regressions
#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#this first step makes the wide data long to set up the code further down
str(t.s.data.stan)
long.t.s.data.stan <- melt(t.s.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc", "good_data", "latitude.x", "longitude.x"))
str(long.t.s.data.stan)

#check to make sure nothing is too squirrelly
summary(long.t.s.data.stan)
nrow(long.t.s.data.stan)/nrow(t.s.data.stan)
ncol(t.s.data.stan)
str(t.s.data.stan)
#we had 78 rows for 152 columns, all but 6 got staked underneath. We should now have a df that is 146 times longer. CHECK!

slice(long.t.s.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.t.s.data.stan, 200:210)

####Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution
#https://stackoverflow.com/questions/51567914/hundreds-of-linear-regressions-that-run-by-group-in-r

#do simple regressions on bc_conc for each of the determinants
to.bc.uni.beta.p <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.bc.uni.cis <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.bc.uni.r2 <- long.t.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(t.s.data.stan$bc_conc))))


#put em together 
to.bc.uni.reg <- data.frame(to.bc.uni.beta.p[ , c(1,2)], lapply(to.bc.uni.cis[ , c(2,3)], as.numeric), round(to.bc.uni.r2[ ,2:3], 5), to.bc.uni.beta.p[ , c(3,4)])
to.bc.uni.reg$Beta <- as.numeric(to.bc.uni.reg$Beta)
str(to.bc.uni.reg)



#do simple regressions on uvpm_conc for each of the determinants
to.uvpm.uni.beta.p <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
to.uvpm.uni.cis <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
to.uvpm.uni.r2 <- long.t.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(t.s.data.stan$uvpm_conc))))

to.uvpm.uni.reg <- data.frame(to.uvpm.uni.beta.p[ , c(1,2)], lapply(to.uvpm.uni.cis[ , c(2,3)], as.numeric), to.uvpm.uni.r2[ ,2:3], to.uvpm.uni.beta.p[ , c(3,4)])
to.uvpm.uni.reg$Beta <- as.numeric(to.uvpm.uni.reg$Beta)

write.csv(to.bc.uni.reg, file = "TO_BC_Uni_Regressions.csv")
write.csv(to.uvpm.uni.reg, file = "TO_UVPM_Uni_Regressions.csv")
write.csv(t.s.data, file = "toronto_data.csv")
write.csv(t.s.data.stan, file = "toronto_standardized_data.csv")

formattable(to.bc.uni.reg)
formattable(to.uvpm.uni.reg)

nrow(subset(to.bc.uni.reg, P.Value <= 0.05))
#aawwwwww yeah! 63 

#note: no bus_stop_50m category; it doesn't look like bus_stop was getting there (bus_stop_100m p = 0.742)
#note:  Missing NPRI_Nox_100m and 50m; it doesn't look like the NPRI_Nox that are there are heading towards significance (NPRI_Nox_200m p = 0.760, though the 200m and 300m are the same. Is that a result of data source?)
#note: missing pop_300m, 200m, 100m, and 50m; it doesn't look like pop_ is heading to sig (pop_500m p = 0.669)

#I saw two of the NPRI_Nox had same values in the automated regression. Wanted to make sure the automation worked correctly
summary(lm(data = t.s.data.stan, bc_conc ~ NPRI_Nox_300m))
summary(lm(data = t.s.data.stan, bc_conc ~ NPRI_Nox_200m))
#all the values are the same
all_equal(t.s.data.stan$NPRI_Nox_300m, t.s.data.stan$NPRI_Nox_200m)
all_equal(t.s.data$NPRI_Nox_300m, t.s.data$NPRI_Nox_200m)

#check one of each to make sure it worked
summary(lm(data = t.s.data.stan, formula = bc_conc ~ rail_200m))
summary(lm(data = t.s.data.stan, formula = uvpm_conc ~ pop_500m))
#tried to figure out how to get more info out of the regressions. Not sure if I need more, but curious. The code turns the regression into a tibble like this:
tidy(lm(data = t.s.data.stan, formula = uvpm_conc ~ pop_500m))
tidy(confint(lm(data = t.s.data.stan, formula = uvpm_conc ~ pop_500m)))

#TO Uni Reg - Outliers ######

#The toronto regressions seem to run semi okay even with this outlier, but we can take a look to see what happens when we take it out. 
describe(subset(t.s.outcomes, BC_ng_m3 > 10000))
#it's TO_space_94, it's a UPAS, it ran for 10 days
t.outlier.level<- 10000
to.u10k.bc.uni.beta.p <- subset(long.t.s.data.stan, bc_conc < t.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.u10k.bc.uni.cis <- subset(long.t.s.data.stan, bc_conc < t.outlier.level)  %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.u10k.bc.uni.r2 <- subset(long.t.s.data.stan, bc_conc < t.outlier.level)  %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/(sum(!is.na(t.s.data.stan$bc_conc))-1)))    #note that there is a -1 because of the 1 outlier removed. 

summary(full_join(to.u10k.bc.uni.beta.p, to.u10k.bc.uni.cis, by = "variable") %>% 
                                full_join(to.u10k.bc.uni.r2, by = "variable") %>% 
                                transmute(variable = variable, Beta = as.numeric(Beta), "2.5%" = as.numeric(`2.5%`), "97.5" = as.numeric(`97.5%`), r.squared, RMSE, SE, `P Value`))

#put em together 
#montreal annual has issues, so I changed to dplyr code
to.u10k.bc.uni.reg <- full_join(to.u10k.bc.uni.beta.p, to.u10k.bc.uni.cis, by = "variable") %>% 
                                full_join(to.u10k.bc.uni.r2, by = "variable") %>% 
                                transmute(variable = variable, Beta = as.numeric(Beta), "2.5%" = as.numeric(`2.5%`), "97.5" = as.numeric(`97.5%`), r.squared, RMSE, SE, `P Value`)

summary(to.u10k.bc.uni.reg)

formattable(to.u10k.bc.uni.reg)
nrow(subset(to.bc.uni.reg, P.Value <= 0.05))
nrow(subset(to.u10k.bc.uni.reg, P.Value <= 0.05))
#removing the outlier takes it from 63 pvals, ot 73 pvals


#repeat for uvpm
to.u10k.uvpm.uni.beta.p <- subset(long.t.s.data.stan, uvpm_conc < t.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.u10k.uvpm.uni.cis <- subset(long.t.s.data.stan, uvpm_conc < t.outlier.level)  %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.u10k.uvpm.uni.r2 <- subset(long.t.s.data.stan, uvpm_conc < t.outlier.level)  %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(t.s.data.stan$uvpm_conc))))


#put em together 
to.u10k.uvpm.uni.reg <- data.frame(to.u10k.uvpm.uni.beta.p[ , c(1,2)], lapply(to.u10k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(to.u10k.uvpm.uni.r2[ ,2:3], 5), to.u10k.uvpm.uni.beta.p[ , c(3,4)])
to.u10k.uvpm.uni.reg$Beta <- as.numeric(to.u10k.uvpm.uni.reg$Beta)
str(to.u10k.uvpm.uni.reg)

formattable(to.u10k.uvpm.uni.reg)
nrow(subset(to.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(to.u10k.uvpm.uni.reg, P.Value <= 0.05))
#removing the outlier takes it from 54 pvals, ot 61 pvals

write.csv(to.u10k.bc.uni.reg, file = "TO_u10k_BC_Uni_Regressions.csv")
write.csv(to.u10k.uvpm.uni.reg, file = "TO_u10k_UVPM_Uni_Regressions.csv")



# TO Uni log Regressions ######

#do simple regressions on log(bc_conc + 1) for each of the determinants
to.log.bc.uni.beta.p <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.log.bc.uni.cis <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.log.bc.uni.r2 <- long.t.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
to.log.bc.uni.reg <- data.frame(to.log.bc.uni.beta.p[ , c(1,2)], lapply(to.log.bc.uni.cis[ , c(2,3)], as.numeric), round(to.log.bc.uni.r2[ ,2:3], 5), to.log.bc.uni.beta.p[ , c(3,4)])
to.log.bc.uni.reg$Beta <- as.numeric(to.log.bc.uni.reg$Beta)
str(to.log.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
to.log.uvpm.uni.beta.p <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(uvpm_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
to.log.uvpm.uni.cis <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(uvpm_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
to.log.uvpm.uni.r2 <- long.t.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(uvpm_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$uvpm_conc))))


to.log.uvpm.uni.reg <- data.frame(to.log.uvpm.uni.beta.p[ , c(1,2)], lapply(to.log.uvpm.uni.cis[ , c(2,3)], as.numeric), to.log.uvpm.uni.r2[ ,2:3], to.log.uvpm.uni.beta.p[ , c(3,4)])
to.log.uvpm.uni.reg$Beta <- as.numeric(to.log.uvpm.uni.reg$Beta)

write.csv(to.log.bc.uni.reg, file = "TO_log_BC_Uni_Regressions.csv")
write.csv(to.log.uvpm.uni.reg, file = "TO_log_UVPM_Uni_Regressions.csv")

nrow(subset(to.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(to.bc.uni.reg, P.Value <= 0.05))
#38 with the log transform, 63 without it



#there is 1 BC = 0, 1 BC = 52, and 1 BC = 10k
length(filter(t.s.data.stan, bc_conc == 0)$bc_conc)
#do an o0, an u10k, and an o0u10k

#do simple regressions on log(bc_conc + 1) for each of the determinants
to.log.o0.bc.uni.beta.p <- filter(long.t.s.data.stan, bc_conc > 0) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.log.o0.bc.uni.cis <- filter(long.t.s.data.stan, bc_conc > 0) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.log.o0.bc.uni.r2 <- filter(long.t.s.data.stan, bc_conc > 0) %>%
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
to.log.o0.bc.uni.reg <- data.frame(to.log.o0.bc.uni.beta.p[ , c(1,2)], lapply(to.log.o0.bc.uni.cis[ , c(2,3)], as.numeric), round(to.log.o0.bc.uni.r2[ ,2:3], 5), to.log.o0.bc.uni.beta.p[ , c(3,4)])
to.log.o0.bc.uni.reg$Beta <- as.numeric(to.log.o0.bc.uni.reg$Beta)
str(to.log.o0.bc.uni.reg)

# 100 < BC < 10000
#do simple regressions on log(bc_conc + 1) for each of the determinants
to.log.o100u10k.bc.uni.beta.p <- filter(long.t.s.data.stan, bc_conc > 100 & bc_conc < 10000) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.log.o100u10k.bc.uni.cis <- filter(long.t.s.data.stan, bc_conc > 100 & bc_conc < 10000) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.log.o100u10k.bc.uni.r2 <- filter(long.t.s.data.stan, bc_conc > 100 & bc_conc < 10000) %>%
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
to.log.o100u10k.bc.uni.reg <- data.frame(to.log.o100u10k.bc.uni.beta.p[ , c(1,2)], lapply(to.log.o100u10k.bc.uni.cis[ , c(2,3)], as.numeric), round(to.log.o100u10k.bc.uni.r2[ ,2:3], 5), to.log.o100u10k.bc.uni.beta.p[ , c(3,4)])
to.log.o100u10k.bc.uni.reg$Beta <- as.numeric(to.log.o100u10k.bc.uni.reg$Beta)
nrow(filter(to.log.o100u10k.bc.uni.reg, P.Value < 0.05))
nrow(filter(testy, P.Value < 0.05))

#I ran the above code with bc > 0 and saved it as testy. then I checked to see what variables changes 
setdiff(filter(to.log.o100u10k.bc.uni.reg, P.Value < 0.05)$variable, filter(testy, P.Value < 0.05)$variable)
#[1] "com_1000m"    "resid_1000m"  "resid_750m"   "resid_500m"   "resid_300m"   "com_100m"     "com_50m"      "d_highway"    "NPRI_PM_750m"
#[10] "d_airport"  
setdiff(filter(testy, P.Value < 0.05)$variable, filter(to.log.o100u10k.bc.uni.reg, P.Value < 0.05)$variable)
#[1] "parks_1000m" "parks_750m"  "parks_500m"  "parks_300m"  "d_railline" 
# I decided to make the low end cut at bc 1000 because if you look at the residuals, the bc = 52 point is further off than the bc = 40k, so if we are cuting the 40k for log, then we can probably justify cutting the 52. 
#I've already done a bc =/= 0, if there is another trim to be had, it's on both sides of the dist



nrow(filter(to.u10k.bc.uni.reg, P.Value < 0.05))
nrow(filter(to.log.bc.uni.reg, P.Value < 0.05))
nrow(filter(to.log.o0.bc.uni.reg, P.Value < 0.05))
#73 for linear u10k, 38 for log-linear, 80 for log-linear o0

setdiff(filter(to.log.o0.bc.uni.reg, P.Value < 0.05)$variable, filter(to.u10k.bc.uni.reg, P.Value < 0.05)$variable)
setdiff(filter(to.u10k.bc.uni.reg, P.Value < 0.05)$variable, filter(to.log.o0.bc.uni.reg, P.Value < 0.05)$variable)







#MTL+TO Summer Pool Uni Reg #####


#now run all the uni regressions. Not sure if the pooled will be an MTL annual average pooled with TO or MTL summer pooled with TO. THis is MTL summer with TO
long.mts.pool.data.stan
str(mts.data.pool.stan)
mts.data.pool.stan$d_woodburn
long.mts.pool.data.stan <- melt(mts.data.pool.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc", "good_data", "city", "latitude.x", "longitude.x"))
str(long.mts.pool.data.stan)
tail(long.mts.pool.data.stan$variable)

#check to make sure nothing is too squirrelly
summary(long.mts.pool.data.stan)
nrow(long.mts.pool.data.stan)/nrow(mts.data.pool.stan)

###Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution

#do simple regressions on bc_conc for each of the determinants
mts.pool.bc.uni.beta.p <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.bc.uni.cis <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.bc.uni.r2 <- long.mts.pool.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(mts.data.pool.stan$bc_conc))))

#put em together 
mts.pool.bc.uni.reg <- data.frame(mts.pool.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.bc.uni.r2[ ,2:3], 5), mts.pool.bc.uni.beta.p[ , c(3,4)])
mts.pool.bc.uni.reg$Beta <- as.numeric(mts.pool.bc.uni.reg$Beta)
str(mts.pool.bc.uni.reg)
formattable(mts.pool.bc.uni.reg)


#repat for uvpm
#do simple regressions on uvpm_conc for each of the determinants
mts.pool.uvpm.uni.beta.p <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.uvpm.uni.cis <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.uvpm.uni.r2 <- long.mts.pool.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(mts.data.pool.stan$uvpm_conc))))

#put em together 
mts.pool.uvpm.uni.reg <- data.frame(mts.pool.uvpm.uni.beta.p[ , c(1,2)], lapply(mts.pool.uvpm.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.uvpm.uni.r2[ ,2:3], 5), mts.pool.uvpm.uni.beta.p[ , c(3,4)])
mts.pool.uvpm.uni.reg$Beta <- as.numeric(mts.pool.uvpm.uni.reg$Beta)
str(mts.pool.uvpm.uni.reg)
formattable(mts.pool.uvpm.uni.reg)

write.csv(mts.pool.bc.uni.reg, file = "M_T_Pool_BC_Uni_Regressions.csv")
write.csv(mts.pool.uvpm.uni.reg, file = "M_T_Pool_UVPM_Uni_Regressions.csv")
write.csv(mts.data.pool, file = "mtl_to_pooled_summer_data.csv")
write.csv(mts.data.pool.stan, file = "mtl_to_pooled_summer_standardized_data.csv")








# MTL+TO Summer Pool Uni Reg -Outliers #######

#do unis without the 1 extreme outlier. The 4 MTL outliers are within the range of tonronto
#do simple regressions on bc_conc for each of the determinants
mts.pool.outlier.level <- 10000
mts.pool.u10k.bc.uni.beta.p <- subset(long.mts.pool.data.stan, bc_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.u10k.bc.uni.cis <- subset(long.mts.pool.data.stan, bc_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.u10k.bc.uni.r2 <- subset(long.mts.pool.data.stan, bc_conc < mts.pool.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(mts.data.pool.stan$bc_conc))))

#put em together 
mts.pool.u10k.bc.uni.reg <- data.frame(mts.pool.u10k.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.u10k.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.u10k.bc.uni.r2[ ,2:3], 5), mts.pool.u10k.bc.uni.beta.p[ , c(3,4)])
mts.pool.u10k.bc.uni.reg$Beta <- as.numeric(mts.pool.u10k.bc.uni.reg$Beta)
str(mts.pool.u10k.bc.uni.reg)
formattable(mts.pool.u10k.bc.uni.reg)

nrow(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.u10k.bc.uni.reg, P.Value <= 0.05))
#there are 52 with the outlier and 42 without the outlier
#now 54 and 44

formattable(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
formattable(subset(mts.pool.u10k.bc.uni.reg, P.Value <= 0.05))



#repeat for uvpm
mts.pool.u10k.uvpm.uni.beta.p <- subset(long.mts.pool.data.stan, uvpm_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.u10k.uvpm.uni.cis <- subset(long.mts.pool.data.stan, uvpm_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.u10k.uvpm.uni.r2 <- subset(long.mts.pool.data.stan, uvpm_conc < mts.pool.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(mts.data.pool.stan$uvpm_conc))))

#put em together 
mts.pool.u10k.uvpm.uni.reg <- data.frame(mts.pool.u10k.uvpm.uni.beta.p[ , c(1,2)], lapply(mts.pool.u10k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.u10k.uvpm.uni.r2[ ,2:3], 5), mts.pool.u10k.uvpm.uni.beta.p[ , c(3,4)])
mts.pool.u10k.uvpm.uni.reg$Beta <- as.numeric(mts.pool.u10k.uvpm.uni.reg$Beta)
str(mts.pool.u10k.uvpm.uni.reg)
formattable(mts.pool.u10k.uvpm.uni.reg)

nrow(subset(mts.pool.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.u10k.uvpm.uni.reg, P.Value <= 0.05))
#there are 51 with the outlier and 31 without the outlier
#now 47 and 32

formattable(subset(mts.pool.uvpm.uni.reg, P.Value <= 0.05))
formattable(subset(mts.pool.u10k.uvpm.uni.reg, P.Value <= 0.05))


write.csv(mts.pool.u10k.bc.uni.reg, file = "M_T_Pool_u10k_BC_Uni_Regressions.csv")
write.csv(mts.pool.u10k.uvpm.uni.reg, file = "M_T_Pool_u10k_UVPM_Uni_Regressions.csv")




#check keeping that 12k outiler in 
mts.pool.u13k.bc.uni.beta.p <- subset(long.mts.pool.data.stan, bc_conc < 13000) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.u13k.bc.uni.cis <- subset(long.mts.pool.data.stan, bc_conc < 13000) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.u13k.bc.uni.r2 <- subset(long.mts.pool.data.stan, bc_conc < 13000) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(mts.data.pool.stan$bc_conc))))

#put em together 
mts.pool.u13k.bc.uni.reg <- data.frame(mts.pool.u13k.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.u13k.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.u13k.bc.uni.r2[ ,2:3], 5), mts.pool.u13k.bc.uni.beta.p[ , c(3,4)])
mts.pool.u13k.bc.uni.reg$Beta <- as.numeric(mts.pool.u13k.bc.uni.reg$Beta)
str(mts.pool.u10k.bc.uni.reg)
formattable(mts.pool.u13k.bc.uni.reg)

nrow(subset(mts.pool.u10k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.u13k.bc.uni.reg, P.Value <= 0.05))
#44 u10k and 38 u13k....39 u13k when we look at woodburning. That one MTL has high BC and high woodburning. 

setdiff(filter(mts.pool.u13k.bc.uni.reg, P.Value < 0.05)$variable, filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)$variable)
#building and intersection. Let's see what the plots look like with both

formattable(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
formattable(subset(mts.pool.u10k.bc.uni.reg, P.Value <= 0.05))




# MTL + TO Summer Pool Uni log Regressions #######


#the 40k BC outlier is so clearly an outlier that I'm not even going to try to keep it in with everything. 
#do simple regressions on log(bc_conc + 1) for each of the determinants
mts.pool.log.bc.uni.beta.p <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.log.bc.uni.cis <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.log.bc.uni.r2 <- long.mts.pool.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
mts.pool.log.bc.uni.reg <- data.frame(mts.pool.log.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.log.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.log.bc.uni.r2[ ,2:3], 5), mts.pool.log.bc.uni.beta.p[ , c(3,4)])
mts.pool.log.bc.uni.reg$Beta <- as.numeric(mts.pool.log.bc.uni.reg$Beta)
str(mts.pool.log.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
mts.pool.log.uvpm.uni.beta.p <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(uvpm_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
mts.pool.log.uvpm.uni.cis <- long.mts.pool.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(uvpm_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
mts.pool.log.uvpm.uni.r2 <- long.mts.pool.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(uvpm_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$uvpm_conc))))


mts.pool.log.uvpm.uni.reg <- data.frame(mts.pool.log.uvpm.uni.beta.p[ , c(1,2)], lapply(mts.pool.log.uvpm.uni.cis[ , c(2,3)], as.numeric), mts.pool.log.uvpm.uni.r2[ ,2:3], mts.pool.log.uvpm.uni.beta.p[ , c(3,4)])
mts.pool.log.uvpm.uni.reg$Beta <- as.numeric(mts.pool.log.uvpm.uni.reg$Beta)

write.csv(mts.pool.log.bc.uni.reg, file = "M_T_Pool_log_BC_Uni_Regressions.csv")
write.csv(mts.pool.log.uvpm.uni.reg, file = "M_T_Pool_log_UVPM_Uni_Regressions.csv")

nrow(subset(mts.pool.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
#23 with the log transform, 52 without it
#now 23 and 54

subset(mts.pool.bc.uni.reg, P.Value < 0.05)$variable
filter(mts.pool.bc.uni.reg, P.Value < 0.05)$variable


# MTL + TO Summer Pool Uni log Regressions -Outliers (0s and 40k) #########
# try with BC < 10k
mts.pool.log.u10k.bc.uni.beta.p <- filter(long.mts.pool.data.stan, bc_conc < 10000) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.log.u10k.bc.uni.cis <- filter(long.mts.pool.data.stan, bc_conc < 10000) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.log.u10k.bc.uni.r2 <- filter(long.mts.pool.data.stan, bc_conc < 10000) %>%
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
mts.pool.log.u10k.bc.uni.reg <- data.frame(mts.pool.log.u10k.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.log.u10k.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.log.bc.uni.r2[ ,2:3], 5), mts.pool.log.u10k.bc.uni.beta.p[ , c(3,4)])
mts.pool.log.u10k.bc.uni.reg$Beta <- as.numeric(mts.pool.log.u10k.bc.uni.reg$Beta)
str(mts.pool.log.bc.uni.reg)

nrow(subset(mts.pool.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.log.u10k.bc.uni.reg, P.Value <= 0.05))
#23 with 2 10k+ and 18 without them



########remove the BC = 0 AND the BC = 10,000
nrow(filter(mts.data.pool.stan, bc_conc > 0, bc_conc < 10000))
nrow(filter(mts.data.pool.stan, bc_conc > 100, bc_conc < 10000))
describe(filter(mts.data.pool.stan, bc_conc > 0, bc_conc < 10000)$bc_conc)
#keep the 52, there are now some numbers between it and 600

#do simple regressions on log(bc_conc + 1) for each of the determinants
mts.pool.log.o0u13.bc.uni.beta.p <- filter(long.mts.pool.data.stan, bc_conc > 0, bc_conc < 13000) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  dplyr::select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.log.o0u13.bc.uni.cis <- filter(long.mts.pool.data.stan, bc_conc > 0, bc_conc < 13000) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  dplyr::select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.log.o0u13.bc.uni.r2 <- filter(long.mts.pool.data.stan, bc_conc > 0, bc_conc < 13000) %>%
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  dplyr::select(variable, r.squared, deviance) %>%
  transmute(variable, r.squared, RMSE = sqrt(deviance/sum(!is.na(m.s.data.stan$bc_conc))))


#put em together 
mts.pool.log.o0u13.bc.uni.reg <- data.frame(mts.pool.log.o0u13.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.log.o0u13.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.log.o0u13.bc.uni.r2[ ,2:3], 5), mts.pool.log.o0u13.bc.uni.beta.p[ , c(3,4)])
mts.pool.log.o0u13.bc.uni.reg$Beta <- as.numeric(mts.pool.log.o0u13.bc.uni.reg$Beta)
str(mts.pool.log.o0u13.bc.uni.reg)

nrow(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.log.u10k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(mts.pool.log.o0u13.bc.uni.reg, P.Value <= 0.05))
#52 linear (i think it doesn't have the 40k in it)
#log 23 with 10k and 18 without the 10k
#49 without the 0 and the 40k
#now 55, 23, 18, and 51





#MTL XY Fit Plots######

########MTL plots, start with just scatter plots of each variable vs bc. Add fit lines and p values and R2
str(long.m.s.data.stan)
describe(long.m.s.data.stan$bc_conc)

#####MTL Summer
#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name MTL.s.bc.v.var.plot.loess.alldata.png)
m.s.bc.xy.fit.plot.alldata <- ggplot(data = long.m.s.data.stan, aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Summer Variables vs BC") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#for automatically writing the png file. Not sure how to sort the facets by variable
#png('MTL.s.bc.v.var.plot.loess.alldata.png', width = 4000, height = 4000)
#m.s.bc.xy.fit.plot.alldata
#dev.off()

#repeat for uvpm, saved as MTL.s.uvpm.v.var.plot.loess.alldata.png
m.s.uvpm.xy.fit.plot.alldata <- ggplot(data = long.m.s.data.stan, aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal Summer Variables vs UVPM") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#png('MTL.s.uvpm.v.var.plot.loess.alldata.png', width = 4000, height = 4000)
#m.s.bc.xy.fit.plot.alldata
#dev.off()

#Those < 4k points are driving a lot of stuff. Saved as 4k x 4k and then look at the image (file name MTL.s.bc.v.var.plot.loess.u5k.png)
nrow(subset(long.m.s.data.stan, bc_conc > 4000))
m.s.bc.xy.fit.plot.u4k <- ggplot(data = subset(long.m.s.data.stan, bc_conc < 4000), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Summer Variables vs BC (4 outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTL.s.uvpm.v.var.plot.loess.u5k.png
m.s.uvpm.xy.fit.plot.u4k <- ggplot(data = subset(long.m.s.data.stan, uvpm_conc < 4000), aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal Summer Variables vs UVPM (4 outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#They all look pretty linear. Some have sparse data and outliers. 
#mjrd_200m is really weird. All the other mjrds have a slope, but that one is flat. 

#For initial variable selection, pull out all the p > 0.05, remove the 4 outlierts over 4k, and for now, just look at bc
nrow(subset(long.m.s.data.stan, bc_conc > 5000))
nrow(filter(long.m.s.data.stan, bc_conc > 5000))
#confiming I know how the %in% argument works
nrow(filter(long.m.s.data.stan, variable %in% m.s.bc.uni.reg$variable))
nrow(filter(long.m.s.data.stan, variable %in% filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable))/104
nrow(filter(long.m.s.data.stan, variable %in% "rail_50m"))

nrow(subset(long.m.s.data.stan, bc_conc > 5000))
nrow(subset(m.s.data.stan, bc_conc > 5000))
612/4

long.m.s.data.stan %>%
  filter(!is.na(bc_conc)) %>%
  filter(bc_conc < 4000) %>%
  nrow()
#testing the code before running it. 
long.m.s.data.stan %>%
  filter(!is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable) %>%
  describe()
describe(filter(long.m.s.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable))

##########MTL Summer plots unis with p < 0.05
m.s.bc.xy.fit.plot.u4k.pu5 <- ggplot(data = filter(long.m.s.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Summer p < 0.05 Variables vs BC (5 outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#saved as pu5_m_s_u4k_bc
nrow(filter(m.s.data, bc_conc > 4000))
#p > 0.05
m.s.bc.xy.fit.plot.u4k.po5 <- ggplot(data = filter(long.m.s.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.s.u4k.bc.uni.reg, P.Value >= 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Summer p > 0.05 Variables vs BC (5 outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


####MTL Summer Log
#now try to look at the log plots
m.s.log.bc.xy.fit.plot <- ggplot(data = long.m.s.data.stan, aes(x = value, y = log(bc_conc + 1))) +
  ggtitle("Montreal Summer Variables vs log(BC)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#See the log(0+1) values. There are about 6. The next lowest value is about 150, which log transformed is 5

#all the logs with all data p < 0.05
m.s.log.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.m.s.data.stan, !is.na(bc_conc) & variable %in% filter(m.s.log.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Summer p < 0.05 Variables vs log(BC+1)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#####MTL S log(BC + 1) plots unis for p < 0.05 AND BC > 0. 
m.s.log.o0.bc.uni.reg
m.s.log.o0.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.m.s.data.stan, !is.na(bc_conc) & bc_conc > 0 & variable %in% filter(m.s.log.o0.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = log(bc_conc + 1))) +
  ggtitle("Montreal Summer p < 0.05 Variables vs log(BC+1) (6 BC = 0 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
nrow(filter(m.s.data, bc_conc == 0))
m.s.log.o0.bc.xy.fit.plot.po5 <- ggplot(data = filter(long.m.s.data.stan, !is.na(bc_conc) & bc_conc > 0 & variable %in% filter(m.s.log.o0.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = log(bc_conc + 1))) +
  ggtitle("Montreal Summer p > 0.05 Variables vs log(BC+1) (6 BC = 0 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)






######MTL Winter

#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name MTL.w.bc.v.var.plot.fit.png)
m.w.bc.xy.fit.plot.alldata <- ggplot(data = long.m.w.data.stan, aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Winter Variables vs BC") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

m.w.uvpm.xy.fit.plot.alldata <- ggplot(data = long.m.w.data.stan, aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal Winter Variables vs UVPM") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#no outliers, no seperate plot with outliers removed. 

#just the p < 0.05 plots
m.w.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.m.w.data.stan, !is.na(bc_conc) & variable %in% filter(m.w.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Winter p < 0.05 Variables vs BC") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

m.w.bc.xy.fit.plot.po5 <- ggplot(data = filter(long.m.w.data.stan, !is.na(bc_conc) & variable %in% filter(m.w.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Winter p > 0.05 Variables vs BC") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

######MTL Annual

#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name MTL.a.bc.v.var.plot.loess.png)
m.a.bc.xy.fit.plot.alldata <- ggplot(data = long.m.a.data.stan, aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Annual Variables vs BC") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#(file name MTL.a.uvpm.v.var.plot.loess.png)
m.a.uvpm.xy.fit.plot.alldata <- ggplot(data = long.m.a.data.stan, aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal Annual Variables vs UVPM") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#three points above 4k are maybe driving relationships (file name MTL.a.bc.v.var.plot.loess.u4k.png)
nrow(subset(long.m.a.data.stan, bc_conc > 4000))
m.a.bc.xy.fit.plot.u4k <- ggplot(data = subset(long.m.a.data.stan, bc_conc < 4000), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Annual Variables vs BC (3 outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTL.a.uvpm.v.var.plot.loess.u4k.png
m.a.uvpm.xy.fit.plot.u4k <- ggplot(data = subset(long.m.a.data.stan, uvpm_conc < 4000), aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal Annual Variables vs UVPM (3 outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#just the p < 0.05 plots, with the outliers removed as well
m.a.bc.xy.fit.plot.u4k.pu5 <- ggplot(data = filter(long.m.a.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.a.u4k.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Annual p < 0.05 Variables vs BC (3 BC outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
nrow(filter(m.a.data.stan, bc_conc > 4000))

# the p > 0.05 plots, with the outliers removed as well
m.a.bc.xy.fit.plot.u4k.po5 <- ggplot(data = filter(long.m.a.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.a.u4k.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Annual p > 0.05 Variables vs BC (3 BC outliers over 4,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)



####MTL Annual Log
#now try to look at the log plots
m.a.log.bc.xy.fit.plot <- ggplot(data = long.m.a.data.stan, aes(x = value, y = log(bc_conc + 1))) +
  ggtitle("Montreal Annual Variables vs log(BC)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#just the p < 0.05
m.a.log.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.m.a.data.stan, !is.na(bc_conc) & variable %in% filter(m.a.log.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Annual p < 0.05 Variables vs log(BC + 1)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#that only gives 16 

m.a.log.bc.xy.fit.plot.po5 <- ggplot(data = filter(long.m.a.data.stan, !is.na(bc_conc) & variable %in% filter(m.a.log.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Annual p > 0.05 Variables vs log(BC + 1)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


#just the p < 0.05 with the BC > 4000 cut out
m.a.log.u4k.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.m.a.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.a.log.u4k.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Annual p < 0.05 Variables vs log(BC + 1) (3 BC > 4,000 outliers removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#check the p > 0.05
m.a.log.u4k.bc.xy.fit.plot.po5 <- ggplot(data = filter(long.m.a.data.stan, !is.na(bc_conc) & bc_conc < 4000 & variable %in% filter(m.a.log.u4k.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Annual p > 0.05 Variables vs log(BC + 1) (3 BC > 4,000 outliers removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)








#in case we want them on separate sheets. There were 121 rows for each variable when I wrote this, but now there are fewer. Check before running
#number of rws times 16 to make it a 4 x 4 grid of plots. Coulda probably made a loop for this, but I didn't. Need to learn loops. 
#####MTL S separate sheets
nnn <- nrow(m.s.data.stan)
ggtest <- ggplot(data = subset(long.m.s.data.stan[1:(nnn*16), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#png('MTL.s.bc.v.var.plot.loess.alldata.png', width = 4000, height = 4000)
#m.s.bc.xy.fit.plot.alldata
#dev.off()
#par()

ggplot(data = subset(long.m.s.data.stan[(nnn*16+1):(nnn*16*2), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


# TO XY Fit Plots #####

########TO plots, start with just scatter plots of each variable vs bc. Add fit lines and p values and R2
str(long.t.s.data.stan)
#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name TO.bc.v.var.plot.loess.alldata.png)
t.bc.xy.fit.plot.alldata <- ggplot(data = long.t.s.data.stan, aes(x = value, y = bc_conc)) +
  ggtitle("Toronto Summer Variables vs BC") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#that one 30k point is near zero for all determinants except rail. That's what's driving everything. 

#repeat for uvpm, saved as TO.uvpm.v.var.plot.loess.alldata.png
t.uvpm.xy.fit.plot.alldata <- ggplot(data = long.t.s.data.stan, aes(x = value, y = uvpm_conc)) +
  ggtitle("Toronto Summer Variables vs UVPM") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


#Remove 1 outlier. Saved as 4k x 4k and then look at the image (file name TO.bc.v.var.plot.loess.u10k.png)
nrow(subset(long.t.s.data.stan, bc_conc > 10000))
t.bc.xy.fit.plot.u10k <- ggplot(data = subset(long.t.s.data.stan, bc_conc < 10000), aes(x = value, y = bc_conc)) +
  ggtitle("Toronto Summer Variables vs BC (1 outlier removed") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#a little easier to see the spread of the data witht he 1 outlier removed. As noted in an earlier section, removal of the outlier gives 9 more pvals under 0.05

#repeat with UVPM saved as TO.uvpm.v.var.plot.loess.u10k.png
t.uvpm.xy.fit.plot.u10k <- ggplot(data = subset(long.t.s.data.stan, uvpm_conc < 10000), aes(x = value, y = uvpm_conc)) +
  ggtitle("Toronto Summer Variables vs UVPM (1 outlier removed") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


# just the p < 0.05 and the u10k
t.s.bc.xy.fit.plot.u10k.pu5 <- ggplot(data = filter(long.t.s.data.stan, !is.na(bc_conc) & bc_conc < 10000 & variable %in% filter(to.u10k.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Toronto Summer p < 0.05 Variables vs BC (1 outlier over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#the p > 0.05 and BC < 10k
t.s.bc.xy.fit.plot.u10k.po5 <- ggplot(data = filter(long.t.s.data.stan, !is.na(bc_conc) & bc_conc < 10000 & variable %in% filter(to.u10k.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("Toronto Summer p > 0.05 Variables vs BC (1 outlier over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)



to.log.o0.bc.uni.reg
####TO logs
#the p > 0.05 for BC > 0
t.s.log.o0.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.t.s.data.stan, !is.na(bc_conc) & bc_conc > 0 &
                                                variable %in% filter(to.log.o0.bc.uni.reg, P.Value < 0.05)$variable), 
                                                aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Toronto Summer p < 0.05 Variables vs log(BC+1) (1 BC = 0 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#the p > 0.05
t.s.log.o0.bc.xy.fit.plot.p05 <- ggplot(data = filter(long.t.s.data.stan, !is.na(bc_conc) & bc_conc > 0 &
                                                      variable %in% filter(to.log.o0.bc.uni.reg, P.Value > 0.05)$variable), 
                                      aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Toronto Summer p < 0.05 Variables vs log(BC+1) (1 BC = 0 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

describe(t.s.data$bc_conc)
#the p < 0.05 for 100 < BC < 10000
t.s.log.o100u10k.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.t.s.data.stan, !is.na(bc_conc) & bc_conc > 100 &
                                                         bc_conc < 10000 &
                                                      variable %in% filter(to.log.o100u10k.bc.uni.reg, P.Value < 0.05)$variable), 
                                      aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Toronto Summer p < 0.05 Variables vs log(BC+1) (2 BC < 100 and 1 BC > 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#the p > 0.05
t.s.log.o100u10k.bc.xy.fit.plot.po5 <- ggplot(data = filter(long.t.s.data.stan, !is.na(bc_conc) & bc_conc > 100 &
                                                         bc_conc < 10000 &
                                                      variable %in% filter(to.log.o100u10k.bc.uni.reg, P.Value > 0.05)$variable), 
                                      aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Toronto Summer p > 0.05 Variables vs log(BC+1)  (2 BC < 100 and 1 BC > 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)



# Pooled XY Fit Plots #######

########MTL plots, start with just scatter plots of each variable vs bc. Add fit lines and p values and R2
str(long.mts.pool.data.stan)
describe(long.mts.pool.data.stan$bc_conc)
#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name MTS.pool.bc.v.var.plot.loess.alldata.png)
mts.pool.bc.xy.fit.plot.alldata <- ggplot(data = long.mts.pool.data.stan, aes(x = value, y = bc_conc)) +
  ggtitle("Pooled Summer Variables vs BC") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTS.pool.uvpm.v.var.plot.loess.alldata.png
mts.pool.uvpm.xy.fit.plot.alldata <- ggplot(data = long.mts.pool.data.stan, aes(x = value, y = uvpm_conc)) +
  ggtitle("Pooled Summer Variables vs UVPM") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


#That one 40k point might be driving a lot of stuff. Saved as 4k x 4k and then look at the image (file name MTS.pool.bc.v.var.plot.loess.u10k.png)
nrow(subset(mts.data.pool.stan, bc_conc > 10000))
mts.pool.bc.xy.fit.plot.u10k <- ggplot(data = subset(long.mts.pool.data.stan, bc_conc < 13000), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal+Toronto Summer Pooled Variables vs BC (4 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTS.pool.uvpm.v.var.plot.loess.u10k.png
mts.pool.uvpm.xy.fit.plot.u10k <- ggplot(data = subset(long.mts.pool.data.stan, uvpm_conc < 10000), aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal+Toronto Summer Pooled Variables vs UVPM (4 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


#looks at the hists for NPRI_PM, NPRI_Nox, tot_traffic
mts.pool.u10k.bc.uni.reg
#only p < 0.05 and without outliers
mts.pool.xy.fit.plot.u10k.pu5 <- ggplot(data = filter(long.mts.pool.data.stan, !is.na(bc_conc) & bc_conc < 10000 & variable %in% filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("MTL + TO Pooled p < 0.05 Variables vs BC (2 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#ee what keeping that 1 12k point in does
mts.pool.xy.fit.plot.u13k.pu5 <- ggplot(data = filter(long.mts.pool.data.stan, !is.na(bc_conc) & bc_conc < 13000 & variable %in% filter(mts.pool.u13k.bc.uni.reg, P.Value < 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("MTL + TO Pooled p < 0.05 Variables vs BC (2 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

formattable(filter(mts.pool.u13k.bc.uni.reg, P.Value < 0.05))
filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)
#looking at the plots and the change in Betas, we can see that the 13k outlier has a lot of build_ and inter_ and it makes those appear or change a lot. On others, it'll make a 5% ~change
#cut it out for the non-log
#might need to make a residuals with it. 

#the p > 0.05
mts.pool.xy.fit.plot.u10k.po5 <- ggplot(data = filter(long.mts.pool.data.stan, !is.na(bc_conc) & bc_conc < 10000 & variable %in% filter(mts.pool.u10k.bc.uni.reg, P.Value > 0.05)$variable), aes(x = value, y = bc_conc)) +
  ggtitle("MTL + TO Pooled p > 0.05 Variables vs BC (2 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)





#### MTL TO Pooled log plots
length(filter(mts.data.pool.stan, !is.na(bc_conc) & bc_conc < 10000 & bc_conc > 0)$bc_conc)

mts.pool.log.o0u13.bc.xy.fit.plot.pu5 <- ggplot(data = filter(long.mts.pool.data.stan, !is.na(bc_conc) & bc_conc > 0 &
                                                                bc_conc < 13000 &
                                                                variable %in% filter(mts.pool.log.o0u13.bc.uni.reg, P.Value < 0.05)$variable), 
                                                aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Summer + Toronto Summer  Pooled p < 0.05 Variables vs log(BC) (7 BC = 0 removed and 1 BC > 13,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

mts.pool.log.o0u13.bc.xy.fit.plot.po5 <- ggplot(data = filter(long.mts.pool.data.stan, !is.na(bc_conc) & bc_conc > 0 &
                                                                bc_conc < 13000 &
                                                                variable %in% filter(mts.pool.log.o0u13.bc.uni.reg, P.Value > 0.05)$variable), 
                                                aes(x = value, y = log(bc_conc+1))) +
  ggtitle("Montreal Summer + Toronto Summer  Pooled p > 0.05 Variables vs log(BC) (7 BC = 0 removed and 1 BC > 13,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


nrow(filter(mts.pool.log.o0u13.bc.uni.reg, P.Value < 0.05))


# Residuals Histograms ####
  
##MTL Summer
m.s.outlier.level
resid_hist_m_s_u4k_bc <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>% 
    nest(-variable) %>% 
    mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
    unnest(resids) %>%
    ggplot(data = ., aes(x = resids)) + 
      geom_histogram() +  
      facet_wrap(~ variable, scales = "free") +
      ggtitle("Montreal Summer Residual Histograms for All Uni Regs (4 BC outliers > 4000 removed)")
#saved as resid_hist_m_s_u4k_bc.png, 4k x 4k
  
#check to confirm it is correct. First get that unnested data, trim everything except build_1000m and check it. Then just run my own lm() and check resids. 
hist.try.data <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>% 
    nest(-variable) %>% 
    mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
    unnest(resids)
nrow(filter(hist.try.data, variable == "build_1000m"))
filter(hist.try.data, variable == "build_1000m")
ggplot(data = filter(hist.try.data, variable == "build_1000m"), aes(x = resids)) + geom_histogram()
#yeah  it looks good!
#now just do it from simple lm()
m.s.data.stan %>%
    subset(bc_conc < 4000) %>%
    lm(data = ., bc_conc ~ build_1000m) %>%
    residuals() %>% 
    hist(breaks = 20)
#not the exact same, but the bin widths are different. Looks pretty dang similar. Run another one just to make sure
m.s.data.stan %>%
    subset(bc_conc < 4000) %>%
    lm(data = ., bc_conc ~ bus_100m) %>%
    residuals() %>% 
    hist(breaks = 25)
#i'd say they look pretty good

#MTL Summer only p < 0.05 plots
filter(long.m.s.data.stan, bc_conc < 4000 & variable %in% filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable)
resid_hist_pu5_m_s_u4k_bc <- filter(long.m.s.data.stan, bc_conc < m.s.outlier.level & variable %in% filter(m.s.u4k.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Summer Residual Histograms for Uni Regs p < 0.05 (4 BC outliers > 4000 removed)")
#saved as resid_hist_pu5_m_s_u4k_bc.png, 3k x 3k (41 plots)

#only the ones selected for the model.
#Note: needs code from below to be run, it has the m.s.u4k.lin.variables vector from the Variable Selection section 
resid_hist_selected_m_s_u4k_bc <- filter(long.m.s.data.stan, bc_conc < m.s.outlier.level & variable %in% m.s.u4k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Summer Residual Histograms for Uni Regs of Selected Variables (4 BC outliers > 4000 removed)")
#saved as resid_hist_selected_m_s_u4k_bc.png

#MTL Summer log Reg residuals hist, only p < 0.05 plots
filter(long.m.s.data.stan, variable %in% filter(m.s.log.bc.uni.reg, P.Value < 0.05)$variable)
resid_hist_pu5_m_s_log_bc <- filter(long.m.s.data.stan, variable %in% filter(m.s.log.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Summer Residual Histograms for log Uni Regs p < 0.05")

hist.log.try.data <- filter(long.m.s.data.stan, bc_conc > 0) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids)
ggplot(data = filter(hist.log.try.data, variable == "bus_stop_500m"), aes(x = resids)) + geom_histogram()
#it's those bc_conc = 0 values that are out there

#MTL Summer log Reg residuals hist, only p < 0.05 plots, BC = 0 filtered out
resid_hist_pu5_m_s_log_o0_bc <- filter(long.m.s.data.stan, bc_conc > 0, variable %in% filter(m.s.log.o0.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Summer Residual Histograms for log Uni Regs p < 0.05 (BC = 0 removed)")





##MTL Winter
resid_hist_m_w_bc <- long.m.w.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
    geom_histogram() +  
    facet_wrap(~ variable, scales = "free") +
    ggtitle("Montreal Winter Residual Histograms for All Uni Regs)")
#saved as resid_hist_m_w_bc.png
## MTL Winter only p < 0.05 plots
resid_hist_pu5_m_w_bc <- filter(long.m.w.data.stan, variable %in% filter(m.w.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Winter Residual Histograms for Uni Regs p < 0.05")
#saved as resid_hist_pu5_m_w_bc.png, 3k x 3k (58 plots)







##MTL Annual
resid_hist_m_a_u4k_bc <- subset(long.m.a.data.stan, bc_conc < m.a.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
    geom_histogram() +  
    facet_wrap(~ variable, scales = "free") +
    ggtitle("Montreal Annual Residual Histograms for All Uni Regs (3 BC outliers > 4000 removed)")
#saved as resid_hist_m_a_u4k_bc.png
#MTL Annual only p < 0.05 plots
resid_hist_pu5_m_a_u4k_bc <- filter(long.m.a.data.stan, bc_conc < m.a.outlier.level & variable %in% filter(m.a.u4k.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Annual Residual Histograms for Uni Regs p < 0.05 (3 BC outliers > 4000 removed)")
#saved as resid_hist_pu5_m_a_u4k_bc.png

#MTL Annual log Uni reg histogrm, p < 0.05
describe(filter(long.m.a.data.stan, variable %in% filter(m.a.log.bc.uni.reg, P.Value < 0.05)$variable))
resid_hist_pu5_m_a_log_bc <- filter(long.m.a.data.stan, variable %in% filter(m.a.log.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Annual Residual Histograms for log Uni Regs p < 0.05")
describe(m.a.data.stan$bc_conc)
#there are no bc_conc = 0 values for m.a., that would require winter and summer to have zero.

#MTL Annual log Uni reg histogrm, p < 0.05, BC > 4000 removed
describe(filter(long.m.a.data.stan, variable %in% filter(m.a.log.u4k.bc.uni.reg, P.Value < 0.05)$variable))
resid_hist_pu5_m_a_log_u4k_bc <- filter(long.m.a.data.stan, variable %in% filter(m.a.log.u4k.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal Annual Residual Histograms for log Uni Regs p < 0.05 (3 BC outliers > 4,000 removed)")
describe(m.a.data.stan$bc_conc)
#there are no bc_conc = 0 values for m.a., that would require winter and summer to have zero.

setdiff(filter(m.a.log.u4k.bc.uni.reg, P.Value < 0.05)$variable, filter(m.a.u4k.bc.uni.reg, P.Value < 0.05)$variable)
setdiff(filter(m.a.u4k.bc.uni.reg, P.Value < 0.05)$variable, filter(m.a.log.u4k.bc.uni.reg, P.Value < 0.05)$variable)







##TO Summer
resid_hist_t_s_u10k_bc <- subset(long.t.s.data.stan, bc_conc < t.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Toronto Summer Residual Histograms for All Uni Regs (1 BC outlier > 10,000 removed)")
#saved as resid_hist_t_s_u10k_bc.png
#TO only p < 0.05 plots
resid_hist_pu5_t_s_u10k_bc <- filter(long.t.s.data.stan, bc_conc < t.outlier.level & variable %in% filter(to.u10k.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Toronto Summer Residual Histograms for Uni Regs p < 0.05 (1 BC outlier > 10,000 removed)")
#saved as resid_hist_pu5_t_s_u10k_bc.png

#TO log uni reg resids, p < 0.05
filter(long.t.s.data.stan, variable %in% filter(to.log.bc.uni.reg, P.Value < 0.05)$variable)
resid_hist_pu5_to_log_bc <- filter(long.t.s.data.stan, variable %in% filter(to.log.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Toronto Residual Histograms for log Uni Regs p < 0.05")
describe(t.s.data.stan$bc_conc)
#the 0 and 52 are the low outlier and 40k is the high outlier. The 40k isn't that bad. 


resid_hist_pu5_to_log_o100_bc <- filter(long.t.s.data.stan, bc_conc > 0, variable %in% filter(to.log.o100.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Toronto Residual Histograms for log Uni Regs p < 0.05 (2 BC outliers < 100 removed)")
describe(t.s.data.stan$bc_conc)





long.mts.pool.data.stan
##MTL Summer + TO Summer Pooled
resid_hist_mts_pool_u10k_bc <- subset(long.mts.pool.data.stan, bc_conc < mts.pool.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal and Toronto Summer Pooled Residual Histograms for All Uni Regs (1 BC outlier > 10,000 removed)")
#saved as resid_hist_mts_pool_u10k_bc.png
#MTL TO pooled only p < 0.05 plots
resid_hist_pu5_mts_pool_u10k_bc <- filter(long.mts.pool.data.stan, bc_conc < mts.pool.outlier.level & variable %in% filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal and Toronto Summer Pooled Residual Histograms for Uni Regs p < 0.05 (1 BC outlier > 10,000 removed)")
#saved as resid_hist_pu5_mts_pool_u10k_bc.png

#quick check to see what happens if we cut out the 4 mtl outliers
filter(long.mts.pool.data.stan, bc_conc < 7000 & variable %in% filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal and Toronto Summer Pooled Residual Histograms for Uni Regs p < 0.05 (5 BC outliers > 7,000 removed)")
#still skewed

#log MTL TO pooled only p < 0.05 plots
resid_hist_pu5_mts_pool_log_bc <- filter(long.mts.pool.data.stan, variable %in% filter(mts.pool.log.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal and Toronto Summer Pooled Residual Histograms for log Uni Regs p < 0.05")
#saved as resid_hist_pu5_mts_pool_u10k_bc.png


describe(filter(long.mts.pool.data.stan, bc_conc > 0, bc_conc < 10000, variable %in% filter(mts.pool.log.o0u13.bc.uni.reg, P.Value < 0.05)$variable))
nrow(filter(mts.pool.log.o0.bc.uni.reg, P.Value < .05))
#log MTL TO pooled only p < 0.05 plots, BC =/= 0
resid_hist_pu5_mts_pool_log_o0u10_bc <- filter(long.mts.pool.data.stan, bc_conc > 0, bc_conc < 10000, variable %in% filter(mts.pool.log.o0.bc.uni.reg, P.Value < 0.05)$variable) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Montreal and Toronto Summer Pooled Residual Histograms for log Uni Regs p < 0.05 (BC = 0 and BC > 10,000 removed)")


setdiff(filter(mts.pool.log.o0u13.bc.uni.reg, P.Value < 0.05)$variable, filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)$variable)
setdiff(filter(mts.pool.u10k.bc.uni.reg, P.Value < 0.05)$variable, filter(mts.pool.log.o0u13.bc.uni.reg, P.Value < 0.05)$variable)

# Comparing All Regressions (with and without outliers)######

##### This is a dead section for now. I may pick pick it up later

#there are five groups of uni regressions:
  #MTL Summer
  #MTL Winter
  #MTL Annual
  #TO Summer
  #MTL+TO Summer Pooled
#all except winter also have some outliers for which I have seperate regressions with outliers removed. 
#all have uvpm
#each of the five has a different number of variable (recal some were removed due to all being same value)

#look at each of the five separately first, then look at them together
#made a udf to put into apply
outliers.3sd <- function(x) {
  length(which(abs(x) > 3))
}

#look at BC and UVPM with and without outliers, include "max" and number over 3sd from mean to give an idea of outliers
#the output is R2 value if p < 0.05, otherwise, it's NA
m.s.uni.var.sel <- data.frame(Predictor = colnames(m.s.data.stan[ , 5:ncol(m.s.data.stan)]), MTL_S_BC = ifelse(m.s.bc.uni.reg$P.Value < 0.05, round(m.s.bc.uni.reg$r.squared, 2), NA), 
            MTL_S_BC_u4k = ifelse(m.s.u4k.bc.uni.reg$P.Value < 0.05, round(m.s.u4k.bc.uni.reg$r.squared, 2), NA), 
            MTL_S_UVPM = ifelse(m.s.uvpm.uni.reg$P.Value < 0.05, round(m.s.uvpm.uni.reg$r.squared, 2), NA),
            MTL_S_UVPM_u4k = ifelse(m.s.u4k.uvpm.uni.reg$P.Value < 0.05, round(m.s.u4k.uvpm.uni.reg$r.squared, 2), NA),
            pred_max_val = round(apply(m.s.data.stan[ , 5:ncol(m.s.data.stan)], 2, max), 1),
            pred_over_3sd = apply(m.s.data.stan[ , 5:ncol(m.s.data.stan)], 2, outliers.3sd)
            )

m.w.uni.var.sel <- data.frame(Predictor = colnames(m.w.data.stan[ , 5:ncol(m.w.data.stan)]), 
                              MTL_W_BC = ifelse(m.w.bc.uni.reg$P.Value < 0.05, round(m.w.bc.uni.reg$r.squared, 2), NA), 
                                MTL_W_UVPM = ifelse(m.w.uvpm.uni.reg$P.Value < 0.05, round(m.w.uvpm.uni.reg$r.squared, 2), NA),
                              pred_max_val = round(apply(m.w.data.stan[ , 5:ncol(m.w.data.stan)], 2, max), 1),
                              pred_over_3sd = apply(m.w.data.stan[ , 5:ncol(m.w.data.stan)], 2, outliers.3sd)
                                )

m.a.uni.var.sel <- data.frame(Predictor = colnames(m.a.data.stan[ , 5:ncol(m.a.data.stan)]),
                              MTL_A_BC = ifelse(m.a.bc.uni.reg$P.Value < 0.05, round(m.a.bc.uni.reg$r.squared, 2), NA), 
                                MTL_A_BC_u4k = ifelse(m.a.u4k.bc.uni.reg$P.Value < 0.05, round(m.a.u4k.bc.uni.reg$r.squared, 2), NA), 
                                MTL_A_UVPM = ifelse(m.a.uvpm.uni.reg$P.Value < 0.05, round(m.a.uvpm.uni.reg$r.squared, 2), NA),
                                MTL_A_UVPM_u4k = ifelse(m.a.u4k.uvpm.uni.reg$P.Value < 0.05, round(m.a.u4k.uvpm.uni.reg$r.squared, 2), NA),
                              pred_max_val = round(apply(m.a.data.stan[ , 5:ncol(m.a.data.stan)], 2, max), 1),
                              pred_over_3sd = apply(m.a.data.stan[ , 5:ncol(m.a.data.stan)], 2, outliers.3sd)
                                )

to.uni.var.sel <- data.frame(Predictor = colnames(t.s.data.stan[ , 5:ncol(t.s.data.stan)]),
                             TO_S_BC = ifelse(to.bc.uni.reg$P.Value < 0.05, round(to.bc.uni.reg$r.squared, 2), NA), 
                                TO_S_BC_u10k = ifelse(to.u10k.bc.uni.reg$P.Value < 0.05, round(to.u10k.bc.uni.reg$r.squared, 2), NA), 
                                TO_S_UVPM = ifelse(to.uvpm.uni.reg$P.Value < 0.05, round(to.uvpm.uni.reg$r.squared, 2), NA),
                                TO_S_UVPM_u10k = ifelse(to.u10k.uvpm.uni.reg$P.Value < 0.05, round(to.u10k.uvpm.uni.reg$r.squared, 2), NA),
                               pred_max_val = round(apply(t.s.data.stan[ , 5:ncol(t.s.data.stan)], 2, max), 1),
                               pred_over_3sd = apply(t.s.data.stan[ , 5:ncol(t.s.data.stan)], 2, outliers.3sd)
                                )

mts.pool.uni.var.sel <- data.frame(Predictor = colnames(mts.data.pool.stan[ , 6:ncol(mts.data.pool.stan)]),
                                   M_T_BC = ifelse(mts.pool.bc.uni.reg$P.Value < 0.05, round(mts.pool.bc.uni.reg$r.squared, 2), NA), 
                             M_T_BC_u10k = ifelse(mts.pool.u10k.bc.uni.reg$P.Value < 0.05, round(mts.pool.u10k.bc.uni.reg$r.squared, 2), NA), 
                             M_T_UVPM = ifelse(mts.pool.uvpm.uni.reg$P.Value < 0.05, round(mts.pool.uvpm.uni.reg$r.squared, 2), NA),
                             M_T_UVPM_u10k = ifelse(mts.pool.u10k.uvpm.uni.reg$P.Value < 0.05, round(mts.pool.u10k.uvpm.uni.reg$r.squared, 2), NA),
                             pred_max_val = round(apply(mts.data.pool.stan[ , 6:ncol(mts.data.pool.stan)], 2, max), 1),
                             pred_over_3sd = apply(mts.data.pool.stan[ , 6:ncol(mts.data.pool.stan)], 2, outliers.3sd)
                              )
#function for the formattable bars, it just makes the bar width start with the smallest value in column instead of 0
sd.scale <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#MTL Summer, all and then the "all NA" filtered out
formattable(m.s.uni.var.sel)
formattable(filter(m.s.uni.var.sel, rowSums(is.na(m.s.uni.var.sel)) != 4),
            list(`pred_max_val` = color_bar("#FA614B", fun = sd.scale),
            `MTL_S_BC` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
            `MTL_S_BC_u4k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
            `MTL_S_UVPM` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
            `MTL_S_UVPM_u4k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue")))
            )
            )
#MTL Winter, all and then the "all NA" filtered out, note no outliers
formattable(m.w.uni.var.sel)
formattable(filter(m.w.uni.var.sel, rowSums(is.na(m.w.uni.var.sel)) != 2),
            list(`pred_max_val` = color_bar("#FA614B", fun = sd.scale),
                 `MTL_W_BC` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `MTL_W_UVPM` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue")))
                 )
                )
#MTL Annual, all and then the "all NA" filtered out
formattable(m.a.uni.var.sel)
formattable(filter(m.a.uni.var.sel, rowSums(is.na(m.a.uni.var.sel)) != 4),
            list(`pred_max_val` = color_bar("#FA614B", fun = sd.scale),
                 `MTL_A_BC` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `MTL_A_BC_u4k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `MTL_A_UVPM` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `MTL_A_UVPM_u4k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue")))
                )
              )

#TO Summer, all and then the "all NA" filtered out
formattable(to.uni.var.sel)
formattable(filter(to.uni.var.sel, rowSums(is.na(to.uni.var.sel)) != 4),
            list(`pred_max_val` = color_bar("#FA614B", fun = sd.scale),
                 `TO_S_BC` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `TO_S_BC_u10k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `TO_S_UVPM` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `TO_S_UVPM_u10k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue")))
                )
              )
#MTL+TO Summer Pooled, all and then the "all NA" filtered out
formattable(mts.pool.uni.var.sel)
formattable(filter(mts.pool.uni.var.sel, rowSums(is.na(mts.pool.uni.var.sel)) != 4),
            list(`pred_max_val` = color_bar("#FA614B", fun = sd.scale),
                 `M_T_BC` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `M_T_BC_u10k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `M_T_UVPM` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue"))),
                 `M_T_UVPM_u10k` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", "darkblue")))
                )
              )

#trying to put all of the variables together
str(m.s.determinants)
str(m.s.uni.var.sel)
str(to.uni.var.sel)
str(mts.pool.uni.var.sel)
m.s.determinants %>% ncol
t.s.determinants %>% ncol()
pred.vars <- as_tibble(colnames(m.s.determinants[,-1]))
colnames(pred.vars) <- "Predictor"
str(pred.vars)
pred.vars$Predictor <- as.factor(pred.vars$Predictor)
str(m.s.uni.var.sel$Predictor)
#this was working.....but now it isn't. Dayum. 
all.uni.reg.r2 <- full_join(pred.vars, m.s.uni.var.sel[,c(-1,-7,-8)], by = "Predictor") %>%
  full_join(m.w.uni.var.sel[ , c(-1, -5,-6)], by = "Predictor") %>%
  full_join(m.a.uni.var.sel[ ,c(-1, -7,-8)], by = "Predictor") %>%
  full_join(to.uni.var.sel[, c(-1, -7, -8)], by = "Predictor") %>%
  full_join(mts.pool.uni.var.sel[, c(-1, -7, -8)], by = "Predictor")


formattable(all.data.together)
#hashtaged out to not keep writing. 
#write.csv(all.uni.reg.r2, "all_uni_reg_r2.csv")








# Uni Results Tables #######
#as discussed, focus on BC, pull out the outliers, and don't bother with a Winter for now
#err on the side of keeping data in, so for mts.pool, the MTL S outliers aren't as huge so keep them in. 
#also looking for log() regressions. 
#this leaves: m.s.bc.u4k, m.a.bc.u4k, t.bc.u10k, mts.pool.bc.u10k......and then the logs

### MTL Annual
#change the names of variables to make it easier to split
tbl.m.a.u4k.bc.uni <- m.a.u4k.bc.uni.reg
tbl.m.a.u4k.bc.uni$variable <- tbl.m.a.u4k.bc.uni$variable %>%
  gsub("^NPRI_", "NPRI.", .) %>%
  gsub("^d.NPRI_", "d.NPRI.", .) %>%
  gsub("^d_", "Distanceto.", .) %>%
  gsub("^d.", "Distanceto.", .) %>%
  gsub("^tot_", "total ", .) %>%
  gsub("^bus_stop", "bus.stop", .) %>%
  gsub("0m$", "0", .) %>%
  str_to_title() %>%
  gsub("Npri", "NPRI", .) %>%
  gsub("nox", "NOx", .) %>%
  gsub("Nox", "NOx", .) %>%
  print()
#all of the variables
tbl.m.a.u4k.bc.uni %>%
  transmute(variable = variable, Beta = paste(Beta, " (", `2.5%`, ", ", `97.5`, ")", sep = ""), r.squared = r.squared, RMSE = RMSE) %>%
  separate(col = variable, into = c("Independent Variable", "Buffer Size"), sep = "_", remove = TRUE) %>%
  arrange(`Independent Variable`) %>%
  write.csv("Paper Tables/tbl.m.a.u4k.bc.uni.csv")

#split, remove the p > 0.05, and take out the SE and P.Value Columns. 
tbl.m.a.u4k.bc.uni %>%
  filter(P.Value < 0.05) %>%
  formattable()

m.a.u4k.bc.uni.reg
confint(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ com_50m))
confint(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ com_100m))
confint(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_airport))
confint(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ gov_50m))


#toronto table
#change the names of variables to make it easier to split
tbl.to.u10k.bc.uni <- to.u10k.bc.uni.reg
tbl.to.u10k.bc.uni$variable <- tbl.to.u10k.bc.uni$variable %>%
  gsub("^NPRI_", "NPRI.", .) %>%
  gsub("^d.NPRI_", "d.NPRI.", .) %>%
  gsub("^d_", "Distanceto.", .) %>%
  gsub("^d.", "Distanceto.", .) %>%
  gsub("^tot_", "total ", .) %>%
  gsub("^bus_stop", "bus.stop", .) %>%
  gsub("0m$", "0", .) %>%
  str_to_title() %>%
  gsub("Npri", "NPRI", .) %>%
  gsub("nox", "NOx", .) %>%
  gsub("Nox", "NOx", .) %>%
  print()
#all of the variables
tbl.to.u10k.bc.uni %>%
  transmute(variable = variable, Beta = paste(Beta, " (", `2.5%`, ", ", `97.5`, ")", sep = ""), r.squared = r.squared, RMSE = RMSE) %>%
  separate(col = variable, into = c("Independent Variable", "Buffer Size"), sep = "_", remove = TRUE) %>%
  arrange(`Independent Variable`) %>%
  write.csv("Paper Tables/tbl.to.u4k.bc.uni.csv")

#split, remove the p > 0.05, and take out the SE and P.Value Columns. 
tbl.to.u10k.bc.uni %>%
  filter(P.Value < 0.05) %>%
  formattable()


#others. No need for now 
#change the names of variables to make it easier to split
tbl.m.s.u4k.bc.uni <- m.s.u4k.bc.uni.reg
tbl.m.s.u4k.bc.uni$variable <- tbl.m.s.u4k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("^NPRI_", "NPRI.", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)

#split, remove the p > 0.05, and take out the SE and P.Value Columns. 
tbl.m.s.u4k.bc.uni %>%
  separate(col = variable, into = c("Independent Variable", "Buffer Size"), sep = "_", remove = TRUE) %>%
  filter(P.Value < 0.05) %>%
  dplyr::select(-SE, -P.Value) %>%
  formattable()


#change the names of variables to make it easier to split
tbl.mts.pool.u10k.bc.uni <- mts.pool.u10k.bc.uni.reg
tbl.mts.pool.u10k.bc.uni$variable <- tbl.mts.pool.u10k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("^NPRI_", "NPRI.", .) %>%
  gsub("^d.NPRI_", "d.NPRI.", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)

#split, remove the p > 0.05, and take out the SE and P.Value Columns. 
tbl.mts.pool.u10k.bc.uni %>%
  separate(col = variable, into = c("Independent Variable", "Buffer Size"), sep = "_", remove = TRUE) %>%
  filter(P.Value < 0.05) %>%
  dplyr::select(-SE, -P.Value) %>%
  formattable()



### Descriptives Tables ######

tbl.m.a.bc.desc 
tbl.m.a.bc.desc <- m.a.data.stan %>% 
  filter(bc_conc < 4000) %>%
  summarise(Minimum = min(bc_conc), "10th percentile" = quantile(bc_conc, 0.1), "First Quartile" = quantile(bc_conc, 0.25), Mean = mean(bc_conc), SD = sd(bc_conc), Median = median(bc_conc), "Third Quartile" = quantile(bc_conc, 0.75), "90th percentile" = quantile(bc_conc, 0.90), Maximum = max(bc_conc)) %>%
  mutate("Mean (SD)" = paste(round(Mean, 2), " (", round(SD, 2), ")", sep = "")) %>%
  dplyr::select(Minimum, `10th percentile`, `First Quartile`, `Mean (SD)`, Median, `Third Quartile`, `90th percentile`, Maximum)

tbl.t.s.bc.desc <- t.s.data.stan %>% 
  filter(bc_conc < 10000) %>%
  summarise(Minimum = min(bc_conc), "10th percentile" = quantile(bc_conc, 0.1), "First Quartile" = quantile(bc_conc, 0.25), Mean = mean(bc_conc), SD = sd(bc_conc), Median = median(bc_conc), "Third Quartile" = quantile(bc_conc, 0.75), "90th percentile" = quantile(bc_conc, 0.90), Maximum = max(bc_conc)) %>%
  mutate("Mean (SD)" = paste(round(Mean, 2), " (", round(SD, 2), ")", sep = "")) %>%
  dplyr::select(Minimum, `10th percentile`, `First Quartile`, `Mean (SD)`, Median, `Third Quartile`, `90th percentile`, Maximum)

tbl.m.a.t.s.bc.desc <- data.frame(Montreal = t(tbl.m.a.bc.desc), Toronto = t(tbl.t.s.bc.desc))
write.csv(tbl.m.a.t.s.bc.desc, "Paper Tables/tbl.m.a.t.s.bc.desc.csv")

library(table1)
library(tableone)

m.a.det.desc.data <- filter(m.a.data, a.bc_conc < 4000) %>% dplyr::select(11:(NCOL(m.a.data)-2))
nrow(m.a.det.desc.data)

m.a.det.desc.data
m.a.det.desc <- data.frame("I.V" = colnames(m.a.det.desc.data), Mean = t(summarise_all(m.a.det.desc.data, funs(mean))), SD = t(summarise_all(m.a.det.desc.data, funs(sd))), 
           Minimum = t(summarise_all(m.a.det.desc.data, funs(min))), Maximum = t(summarise_all(m.a.det.desc.data, funs(max))))  %>%
  mutate("Mean (SD)" = paste(round(Mean, 2), " (", round(SD, 2), ")", sep = "")) %>%
  dplyr::select(I.V, `Mean (SD)`, Minimum, Maximum) %>%
  transmute("I.V" = I.V  %>% gsub("^NPRI_", "NPRI.", .) %>%
              gsub("^d.NPRI_", "d.NPRI.", .) %>%
              gsub("^d_", "Distanceto.", .) %>%
              gsub("^d.", "Distanceto.", .) %>%
              gsub("^tot_", "total ", .) %>%
              gsub("^bus_stop", "bus.stop", .) %>%
              gsub("0m$", "0", .) %>%
              str_to_title() %>%
              gsub("Npri", "NPRI", .) %>%
              gsub("nox", "NOx", .) %>%
              gsub("Nox", "NOx", .),
            "Mean (SD)" = `Mean (SD)`, Minimum = round(Minimum, 2), Maximum = round(Maximum, 2)) %>%
  separate(col = I.V, into = c("Independent Variable", "Buffer Size"), sep = "_", remove = TRUE) %>%
  arrange(`Independent Variable`) %>%
  write.csv("Paper Tables/m.a.det.desc.csv")

summary(m.a.det.desc.data)                 

t.s.det.desc.data <- filter(t.s.data, bc_conc < 10000) %>% dplyr::select(5:(NCOL(t.s.data)-2))
nrow(t.s.det.desc.data)

t.s.det.desc <- data.frame("I.V" = colnames(t.s.det.desc.data), Mean = t(summarise_all(t.s.det.desc.data, funs(mean))), SD = t(summarise_all(t.s.det.desc.data, funs(sd))), 
                           Minimum = t(summarise_all(t.s.det.desc.data, funs(min))), Maximum = t(summarise_all(t.s.det.desc.data, funs(max))))  %>%
  mutate("Mean (SD)" = paste(round(Mean, 2), " (", round(SD, 2), ")", sep = "")) %>%
  dplyr::select(I.V, `Mean (SD)`, Minimum, Maximum) %>%
  transmute("I.V" = I.V  %>% gsub("^NPRI_", "NPRI.", .) %>%
              gsub("^d.NPRI_", "d.NPRI.", .) %>%
              gsub("^d_", "Distanceto.", .) %>%
              gsub("^d.", "Distanceto.", .) %>%
              gsub("^tot_", "total ", .) %>%
              gsub("^bus_stop", "bus.stop", .) %>%
              gsub("0m$", "0", .) %>%
              str_to_title() %>%
              gsub("Npri", "NPRI", .) %>%
              gsub("nox", "NOx", .) %>%
              gsub("Nox", "NOx", .),
            "Mean (SD)" = `Mean (SD)`, Minimum = round(Minimum, 2), Maximum = round(Maximum, 2)) %>%
  separate(col = I.V, into = c("Independent Variable", "Buffer Size"), sep = "_", remove = TRUE) %>%
  arrange(`Independent Variable`) %>%
  write.csv("Paper Tables/t.s.det.desc.csv")



# Variable Selection MTL S ######
#look at file m.s.bc.xy.fit.plot.u4k.pu5.png for the p < 0.05 graphs 


m.s.u4k.lin.variables <- c("build_1000m", "mjrd_750m", "d_majrd", "bus_50m", "bus_stop_750m", "traffic_750m", 
                           "tot_traffic_750m", "Nox_750m", "tot_Nox_750m", "d_NPRI_Nox", "d_airport")
m.s.u4k.nlin.variables <- NA


m.s.u4k.full.multi.lm <- lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + mjrd_750m + d_majrd + bus_50m + bus_stop_750m + traffic_750m + 
                              tot_traffic_750m + Nox_750m + tot_Nox_750m + d_NPRI_Nox + d_airport)
summary(m.s.u4k.full.multi.lm)
#woof. Small R^2, none p < 0.05
tidy(m.s.u4k.full.multi.lm)
glance(m.s.u4k.full.multi.lm)

summary(bic.glm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + mjrd_750m + d_majrd + bus_50m + bus_stop_750m + traffic_750m + 
                  tot_traffic_750m + Nox_750m + tot_Nox_750m + d_NPRI_Nox + d_airport, glm.family = gaussian))
#bic.glm suggests mjrd_750m + bus_stop_750m + d_NPRI_Nox for a R2 of 0.231
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ mjrd_750m + bus_stop_750m + d_NPRI_Nox))


#####checking possible non-linear and IV outlier influence while selecting vars
#d_majrd might be non-linear
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_majrd))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ I(d_majrd^2)))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_majrd + I(d_majrd^2)))
#the non-linear terms have p > 0.05, so d_majrd is only linear

#bus_50m looks like an outlier might be driving it, check without the outlier
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ bus_50m))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000 & bus_50m < 2), bc_conc ~ bus_50m))
#p is still under 0.05 and Beta is very similar, R^2 is similar too. I think it's worth keeping

#traffic_750m has 6 points above 2sd and 3 above 3sd. 
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ traffic_750m))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000 & traffic_750m < 2), bc_conc ~ traffic_750m))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000 & traffic_750m < 3), bc_conc ~ traffic_750m))
#holds

#tot_Nox_750m has 7 over 2sd and 1 over 3sd
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000 & tot_Nox_750m < 2), bc_conc ~ tot_Nox_750m))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000 & tot_Nox_750m < 3), bc_conc ~ tot_Nox_750m))
#all still under 0.05

#d_NPRI_Nox might be non-linear
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_NPRI_Nox))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ I(d_NPRI_Nox^2)))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_majrd + I(d_NPRI_Nox^2)))
#just lin

#d_airport might be non-linear
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_airport))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ I(d_airport^2)))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_majrd + I(d_airport^2)))
#just lin

######maybe check in the p > 0.05 for some non-linears?
#checked em, they don't look like much. All of the d_ are very flat.


#none of them has it
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_woodburn))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ I(d_woodburn^2)))
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ d_woodburn + I(d_woodburn^2)))
#nope

####Variable Selection MTL S log #######
log.m.s.data.stan <- m.s.data.stan
log.m.s.data.stan$logbc <- log(log.m.s.data.stan$bc_conc+1)

m.s.log.lin.variables <- c("bus_stop_750m")
m.s.log.nlin.variables <- NA

m.s.log.full.multi.lm <- lm(data = filter(m.s.data.stan), bc_conc ~ bus_stop_750m)
summary(m.s.log.full.multi.lm)
#

summary(lm(data = filter(log.m.s.data.stan), logbc ~ bus_stop_750m))
summary(lm(data = filter(log.m.s.data.stan, bus_stop_750m < 2), logbc ~ bus_stop_750m))
summary(lm(data = filter(log.m.s.data.stan), logbc ~ bus_stop_1000m))
summary(lm(data = filter(log.m.s.data.stan, bus_stop_1000m < 2), logbc ~ bus_stop_1000m))
summary(lm(data = filter(log.m.s.data.stan), logbc ~ bus_stop_300m))
summary(lm(data = filter(log.m.s.data.stan, bus_stop_300m < 2), logbc ~ bus_stop_300m))
#bust_stop_750m  holds





########Variable Selection MTL S log 0 < BC ######
m.s.log.o0.lin.variables <- c("build_200m", "ind_1000m", "mjrd_750m", "d_majrd", "bus_300m", "bus_stop_750m", 
                              "d_airport", "d_railline")


m.s.log.o0.nlin.variables <- NA

m.s.log.o0.full.multi.lm <- lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ build_200m + ind_1000m + mjrd_750m + d_majrd + bus_300m + bus_stop_750m + d_airport + d_railline)
summary(m.s.log.o0.full.multi.lm)
#woof. Small R^2 again, none p < 0.05


summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ build_200m))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0 & build_200m < 2), logbc ~ build_200m))
#goood! This is new with the 3 new obs


summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ ind_1000m))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0 & ind_1000m < 2), logbc ~ ind_1000m))
#goood.

summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ com_500m))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0 & com_500m < 2), logbc ~ com_500m))
#not good. Those outliers drive it all man

summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ mjrd_750m))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0 & mjrd_750m < 2), logbc ~ mjrd_750m))
#gooood

summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ bus_300m))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0 & bus_300m < 2), logbc ~ bus_300m))
#gooood


summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ inter_100m))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0 & inter_100m < 2), logbc ~ inter_100m))
#nope. _750m and  _1000m doesn't hold either

#d_majrd might be non-linear
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_majrd))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ I(d_majrd^2)))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_majrd + I(d_majrd^2)))
#just lin

summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_airport))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ I(d_airport^2)))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_airport + I(d_airport^2)))
#just lin

#d_raillline might be non-linear
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_railline))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ I(d_railline^2)))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_railline + I(d_railline^2)))
#nope
#check p > 0.05 for nlin
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_NPRI_PM))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ I(d_NPRI_PM^2)))
summary(lm(data = filter(log.m.s.data.stan, bc_conc > 0), logbc ~ d_NPRI_PM + I(d_NPRI_PM^2)))
#nope



# Variable Selection MTL W #####
# look at m.w.bc.xy.fit.plot.pu5.png
m.w.lin.variables <- c("build_1000m", "water_1000m", "open_300m", "ind_500m", "bus_1000m", "traffic_1000m", "tot_Nox_750m", "Nox_1000m", "d_airport", "d_NPRI_Nox", "d_woodburn")
m.w.nlin.variables <- data.frame(var = c("d_airport", "d_NPRI_Nox"), status = c("alone", "alone"))

describe(m.w.data.stan$bc_conc)
#recall there are no outliers for m.w.

m.w.full.multi.lm <- lm(data = filter(m.w.data.stan), bc_conc ~ build_1000m + water_1000m + open_300m + ind_500m + 
                          bus_1000m + traffic_1000m + tot_Nox_750m + Nox_1000m + d_woodburn + I(d_airport^2) + I(d_NPRI_Nox^2))
summary(m.w.full.multi.lm)
#not bad, R2 = 0.4685
#note that d_woodburn has a lot of NA in it. If we take woodburn out, the R2 goes up.

#this has R2 of 0.5206
summary(lm(data = filter(m.w.data.stan), bc_conc ~ build_1000m + open_300m + traffic_1000m + tot_Nox_750m + I(d_airport^2) + I(d_NPRI_Nox^2)))


summary(lm(data = filter(m.w.data.stan), bc_conc ~ build_1000m + open_300m + I(d_airport^2)))
#this has all p < 0.05 and an R2 of 0.4438

summary(lm(data = filter(m.w.data.stan), bc_conc ~ water_1000m))
summary(lm(data = filter(m.w.data.stan, water_1000m < 2), bc_conc ~ water_1000m))
#good

summary(lm(data = filter(m.w.data.stan), bc_conc ~ open_500m))
summary(lm(data = filter(m.w.data.stan, open_500m < 2), bc_conc ~ open_500m))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ open_300m))
summary(lm(data = filter(m.w.data.stan, open_300m < 2), bc_conc ~ open_300m))
#_300m is more robust. 

summary(lm(data = filter(m.w.data.stan), bc_conc ~ ind_500m))
summary(lm(data = filter(m.w.data.stan, ind_500m < 2), bc_conc ~ ind_500m))
#good

summary(lm(data = filter(m.w.data.stan), bc_conc ~ highway_1000m))
summary(lm(data = filter(m.w.data.stan, highway_1000m < 2), bc_conc ~ highway_1000m))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ highway_750m))
summary(lm(data = filter(m.w.data.stan, highway_750m < 2), bc_conc ~ highway_750m))
#nope. neither, I checked _500m too and it's also driven by outliers. 

summary(lm(data = filter(m.w.data.stan), bc_conc ~ traffic_1000m))
summary(lm(data = filter(m.w.data.stan, traffic_1000m < 2), bc_conc ~ traffic_1000m))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ traffic_750m))
summary(lm(data = filter(m.w.data.stan, traffic_750m < 2), bc_conc ~ traffic_750m))
#_1000m is better

summary(lm(data = filter(m.w.data.stan), bc_conc ~ tot_traffic_1000m))
summary(lm(data = filter(m.w.data.stan, tot_traffic_1000m < 2), bc_conc ~ tot_traffic_1000m))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ tot_traffic_750m))
summary(lm(data = filter(m.w.data.stan, tot_traffic_750m < 2), bc_conc ~ tot_traffic_750m))
#nope. 

summary(lm(data = filter(m.w.data.stan), bc_conc ~ tot_Nox_750m))
summary(lm(data = filter(m.w.data.stan, tot_Nox_750m < 2), bc_conc ~ tot_Nox_750m))
#good 

summary(lm(data = filter(m.w.data.stan), bc_conc ~ Nox_1000m))
summary(lm(data = filter(m.w.data.stan, Nox_1000m < 2), bc_conc ~ Nox_1000m))
#good

summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_NPRI_Nox))
summary(lm(data = filter(m.w.data.stan, d_NPRI_Nox < 2), bc_conc ~ d_NPRI_Nox))
summary(lm(data = filter(m.w.data.stan, d_NPRI_Nox < 2), bc_conc ~ I(d_NPRI_Nox^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ I(d_NPRI_Nox^2)))
#lin driven by 1 point, but non-lin good. Best as x^2

summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_airport))
summary(lm(data = filter(m.w.data.stan, d_airport < 2), bc_conc ~ d_airport))
summary(lm(data = filter(m.w.data.stan, d_airport < 2), bc_conc ~ I(d_airport^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ I(d_airport^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_airport + I(d_airport^2)))
#best as x^2

summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_railline))
summary(lm(data = filter(m.w.data.stan, d_railline < 3), bc_conc ~ d_railline))
summary(lm(data = filter(m.w.data.stan, d_railline < 2), bc_conc ~ I(d_railline^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ I(d_railline^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_railline + I(d_railline^2)))
#nope, driven by outliers

summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_shore))
summary(lm(data = filter(m.w.data.stan, d_shore < 2), bc_conc ~ d_shore))
summary(lm(data = filter(m.w.data.stan, d_shore < 2), bc_conc ~ I(d_shore^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ I(d_shore^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_shore + I(d_shore^2)))
#nope, driven by outliers

summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_woodburn))
summary(lm(data = filter(m.w.data.stan, d_woodburn < 2), bc_conc ~ d_woodburn))
summary(lm(data = filter(m.w.data.stan, d_woodburn < 2), bc_conc ~ I(d_woodburn^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ I(d_woodburn^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_woodburn + I(d_woodburn^2)))
#only lin, lin holds without outliers 

#d_majrd or d_highway might be non-linear
summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_highway))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ I(d_highway^2)))
summary(lm(data = filter(m.w.data.stan), bc_conc ~ d_highway + I(d_highway^2)))
#nope. tried d_majrd too

# Variable Selection MTL A #####
# look at file m.a.bc.xy.fit.plot.u4k.pu5.png

#see below for work, but best model looks like tot_Nox_750m + d_NPRI_Nox + I(d_shore^2) giving R2 of 0.474

formattable(tbl.m.a.u4k.bc.uni)
#look at file m.a.bc.xy.fit.plot.u4k.pu5.png for the p < 0.05 graphs and the tbl.m.a.u4k.bc.uni too
m.a.u4k.lin.variables <- c("build_1000m", "mjrd_1000m", "bus_stop_200m", "traffic_1000m", "tot_Nox_750m", 
                           "d_NPRI_Nox", "d_airport", "d_railline", "d_shore")
#see below for more info, but it looks like "mjrd_500m", "bus_stop_300m", "traffic_750m", and "tot_traffic_750m" are p < 0.05 becasue of a handful of outliers (~2-3 for each). Could throw them in, but I haven't. The R2 aren't huge for them anyways
#"d_airport", "d_railline", "d_shore" also show up when ^2

m.a.u4k.nlin.variables <- data.frame(var = c("d_airport", "d_shore"), status = rep("alone", 2))
#all three of these show up when ^2. They only show up on their own, not in combination with their non-squared terms. Also, d_railline^2 has a lower R^2 than the linear d_railline

#without the outlier driven variables
m.a.u4k.full.multi.lm <- lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + mjrd_1000m + 
                              bus_stop_200m + traffic_1000m + tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + d_railline + I(d_shore^2))
summary(m.a.u4k.full.multi.lm)
#take the worst performers out, still R2 > 0.5
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ traffic_1000m + tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + I(d_shore^2)))
#only the p < 0.05
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m + d_NPRI_Nox + I(d_shore^2)))
#do a model averaging
summary(bic.glm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + bus_stop_200m + traffic_1000m + tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + d_railline + I(d_shore^2), glm.family = gaussian))
#it picks the same ones we do. 




summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ ind_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & ind_750m < 2), bc_conc ~ ind_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ ind_1000m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & ind_1000m < 2), bc_conc ~ ind_1000m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ ind_500m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & ind_500m < 2), bc_conc ~ ind_500m))
#they are driven by outliers. Without them p > 0.05

#mjrd_500m looks like 2 outliers might be driving it, check without the outliers
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ mjrd_500m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & mjrd_500m < 2), bc_conc ~ mjrd_500m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ mjrd_1000m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & mjrd_1000m < 2), bc_conc ~ mjrd_1000m))
#mjrd_1000m holds

#bus_stop_300m looks like 2 outliers might be driving it, check without the outliers
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ bus_stop_300m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & bus_stop_300m < 2), bc_conc ~ bus_stop_300m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ bus_stop_200m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & bus_stop_200m < 2), bc_conc ~ bus_stop_200m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ bus_stop_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & bus_stop_750m < 2), bc_conc ~ bus_stop_750m))
#they are. Without them p > 0.05. Take bus_stop_200m, it holds without outliers 

#traffic_750m looks like 3 outliers might be driving it, check without the outliers
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ traffic_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & traffic_750m < 2), bc_conc ~ traffic_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ traffic_1000m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & traffic_1000m < 2), bc_conc ~ traffic_1000m))
#traffic_1000m holds and has higher R2 without outliers

summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ Nox_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & Nox_750m < 2), bc_conc ~ Nox_750m))
#nope

#tot_Nox_750m looks like 5 outliers might be driving it, check without the outliers
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000 & tot_Nox_750m < 2), bc_conc ~ tot_Nox_750m))
#yep, Without them p < 0.05. Non-linear?
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(tot_Nox_750m^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m + I(tot_Nox_750m^2)))
#nope

#####checking possible non-linear and IV outlier influence while selecting vars
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_NPRI_Nox))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(d_NPRI_Nox^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_NPRI_Nox + I(d_NPRI_Nox^2)))
#no non-linear component

#d_airport might be non-linear
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_airport))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(d_airport^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_airport + I(d_airport^2)))
#shows up as non-linear, but is that a hump instead of a decay? the x^2 has the highest r^2 value

summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_railline))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(d_railline^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_railline + I(d_railline^2)))
#the d_railline^2 has a p<0.05, but a lower r^2 value

summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_shore))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(d_shore^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_shore + I(d_shore^2)))
#the d_shore^2 looks best, it has the highest r^2 of the three

#checked the plots of p > 0.05 to see if any non-linear. Maybe d_majrd
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_majrd))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(d_majrd^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_majrd + I(d_majrd^2)))
#nope, not even close. 



#check the d_ p > 0.05 for non lin
#none of them has it
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_woodburn))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ I(d_woodburn^2)))
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ d_woodburn + I(d_woodburn^2)))
#nope



####What if I don't trim out those 3 points above 4k? What do I get?
summary(lm(data = m.a.data.stan, bc_conc ~ com_300m + bus_stop_300m + NPRI_PM_500m + d_airport))
summary(lm(data = m.a.data.stan, bc_conc ~ bus_stop_300m + d_airport))
summary(lm(data = m.a.data.stan, bc_conc ~ bus_stop_300m + d_airport + bus_stop_300m:d_airport))
#but if you check the plots, com_300m isn't legit. Neither is NPRI_PM_500m. Take them out and we have nothing. 



########Variable Selection MTL A log ########
log.m.a.data.stan <- m.a.data.stan
log.m.a.data.stan$logbc <- log(m.a.data.stan$bc_conc+1)

m.a.log.lin.variables <- c("bus_stop_300m", "d_airport")
m.a.log.nlin.variables <- NA

m.a.log.full.multi.lm <- lm(data = filter(log.m.a.data.stan), logbc ~ bus_stop_300m + d_airport)
summary(m.a.log.full.multi.lm)
#woof. very small R^2, 0.2702




summary(lm(data = filter(log.m.a.data.stan), logbc ~ build_200m))
summary(lm(data = filter(log.m.a.data.stan, build_200m < 2), bc_conc ~ build_200m))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ build_300m))
summary(lm(data = filter(log.m.a.data.stan, build_300m < 2), bc_conc ~ build_300m))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ build_500m))
summary(lm(data = filter(log.m.a.data.stan, build_500m < 2), bc_conc ~ build_500m))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ build_1000m))
summary(lm(data = filter(log.m.a.data.stan, build_1000m < 2), bc_conc ~ build_1000m))
#nope accross the board

summary(lm(data = filter(log.m.a.data.stan), logbc ~ ind_1000m))
summary(lm(data = filter(log.m.a.data.stan, ind_1000m < 2), bc_conc ~ ind_1000m))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ ind_750m))
summary(lm(data = filter(log.m.a.data.stan, ind_750m < 2), bc_conc ~ ind_750m))
#nope

summary(lm(data = filter(log.m.a.data.stan), logbc ~ mjrd_500m))
summary(lm(data = filter(log.m.a.data.stan, mjrd_500m < 2), bc_conc ~ mjrd_500m))
#nope

summary(lm(data = filter(log.m.a.data.stan), logbc ~ bus_stop_300m))
summary(lm(data = filter(log.m.a.data.stan, bus_stop_300m < 2), bc_conc ~ bus_stop_300m))
#good

summary(lm(data = filter(log.m.a.data.stan), logbc ~ d_airport))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ I(d_airport^2)))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ d_airport + I(d_airport^2)))
#lin is the best 

summary(lm(data = filter(log.m.a.data.stan), logbc ~ d_railline))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ I(d_railline^2)))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ d_railline + I(d_railline^2)))
summary(lm(data = filter(log.m.a.data.stan, d_railline < 3), logbc ~ d_railline))
summary(lm(data = filter(log.m.a.data.stan, d_railline < 3), logbc ~ I(d_railline^2)))
#x2 is the best, but they are driven by those two outliers above 3 sd

#check the d_ p > 0.05 for non lin
#none of them has it
summary(lm(data = filter(log.m.a.data.stan), logbc ~ d_woodburn))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ I(d_woodburn^2)))
summary(lm(data = filter(log.m.a.data.stan), logbc ~ d_woodburn + I(d_woodburn^2)))


########Variable Selection MTL A log BC < 4,000 #######
m.a.log.u4k.bc.uni.reg
m.a.log.u4k.lin.variables <- c("build_1000m", "mjrd_1000m", "traffic_750m", "tot_Nox_750m", 
                               "d_NPRI_Nox", "d_airport", "d_shore")
m.a.log.u4k.nlin.variables <- data.frame(var = c("d_airport", "d_shore"), status = rep("alone", 2))

m.a.log.u4k.full.multi.lm <- lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ build_1000m + mjrd_1000m + 
                                  traffic_750m + tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + I(d_shore^2))
summary(m.a.log.u4k.full.multi.lm)
#R2 of 0.5569
summary(bic.glm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ build_1000m + bus_stop_750m + traffic_500m + tot_traffic_500m + tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + d_railline + I(d_shore^2), glm.family = gaussian))

#this still gives an R2 > 0.5
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + I(d_shore^2)))


m.a.log.u4k.bc.uni.reg

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ build_1000m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & build_1000m < 2), bc_conc ~ build_1000m))
#good


summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ ind_1000m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & ind_1000m < 2), bc_conc ~ ind_1000m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ ind_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & ind_750m < 2), bc_conc ~ ind_750m))
#nope

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ mjrd_500m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & mjrd_500m < 2), bc_conc ~ mjrd_500m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ mjrd_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & mjrd_750m < 2), bc_conc ~ mjrd_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ mjrd_1000m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & mjrd_1000m < 2), bc_conc ~ mjrd_1000m))
#_1000m holds without oultiers and has teh highest R2

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ bus_stop_300m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & bus_stop_300m < 2), bc_conc ~ bus_stop_300m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ bus_stop_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & bus_stop_750m < 2), bc_conc ~ bus_stop_750m))
#nope

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ traffic_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & traffic_750m < 2), bc_conc ~ traffic_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ traffic_1000m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & traffic_1000m < 2), bc_conc ~ traffic_1000m))
#they both hold

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ tot_Nox_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & tot_Nox_750m < 2), bc_conc ~ tot_Nox_750m))
#goood

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ Nox_750m))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000 & Nox_750m < 2), bc_conc ~ Nox_750m))
#npe

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_NPRI_Nox))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_NPRI_Nox^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_NPRI_Nox + I(d_NPRI_Nox^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000, d_NPRI_Nox < 2), logbc ~ d_NPRI_Nox))
#just lin, holds wityhout the outlier

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_NPRI_PM))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_NPRI_PM^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_NPRI_PM + I(d_NPRI_PM^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000, d_NPRI_PM <2), logbc ~ d_NPRI_PM))
#just lin, but doesn't hold without that one point

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_airport))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_airport^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_airport + I(d_airport^2)))
#x^2 is the best, on it's own

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_railline))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_railline^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_railline + I(d_railline^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000, d_railline < 2), logbc ~ d_railline))
#lin is the best on it's own, but is just driven by those 4 outliers

#look at the d_ for p > 0.05

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_highway^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_highway + I(d_highway^2)))
#nope
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_majrd^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_majrd + I(d_majrd^2)))
#nope
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_shore))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_shore^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_shore + I(d_shore^2)))
#d_shore shows up non-linear! as x2 alone

summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_woodburn))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ I(d_woodburn^2)))
summary(lm(data = filter(log.m.a.data.stan, bc_conc < 4000), logbc ~ d_woodburn + I(d_woodburn^2)))
#nope






# Variable Selection TO S #######
#question: resid_50m has the highest R^2, but less variability, resid_100m and resid_200m are more spread out. Similar situation with ind_

#see below for work, but looks like best model is com_750m + ind_1000m + Nox_50m with an R2 of 0.628

formattable(tbl.to.u10k.bc.uni)
#look at file t.bc.xy.fit.plot.u10k.pu5.png for the p < 0.05 graphs and the tbl.to.u10k.bc.uni too

to.u10k.lin.variables <- c("build_200m", "com_750m", "resid_100m", "ind_1000m", "open_50m", "mjrd_100m", "road_50m", "d_highway", 
                           "d_majrd", "bus_50m", "inter_50m", "traffic_100m", "tot_traffic_100m", "tot_Nox_100m", "Nox_50m",
                           "d_NPRI_PM", "d_airport", "rail_1000m")


to.u10k.nlin.variables <- data.frame(var = c("d_NPRI_PM"), status = rep("together", 1))
#best as x + x^2

#bic.glm and a quick downselect pics com_750m + ind_1000m + Nox_50m, giving an R2 of 0.6284

#all the linear and non-linear together
to.u10k.full.multi.lm <- lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + 
                              open_50m + mjrd_100m + road_50m + d_highway + d_majrd + bus_50m + inter_50m + traffic_100m + tot_traffic_100m + 
             tot_Nox_100m + Nox_50m + d_NPRI_PM + d_airport + rail_1000m + I(d_NPRI_PM^2))
summary(to.u10k.full.multi.lm)

#note: have to take tot_Nox_100m out of it due to colinearity
summary(bic.glm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m +
                  mjrd_100m + road_50m + d_highway + d_majrd + bus_50m + inter_50m + traffic_100m + tot_traffic_100m +
                  Nox_50m + d_NPRI_PM + d_airport + rail_1000m + I(d_NPRI_PM^2), glm.family = gaussian))

#these three give R2 of 0.6284
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_750m + ind_1000m + Nox_50m))


#tot_Nox_750m looks like 3 outliers might be driving it, check without the outliers
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ build_200m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & build_200m < 2), bc_conc ~ build_200m))
#the outilers dampen, 
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ build_200m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(build_200m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ build_200m + I(build_200m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & build_200m < 2), bc_conc ~ build_200m + I(build_200m^2)))
#could add a non-linear component to try to capture the outliers, but that is solely to get the 4 points above 2sd. 

#com_ has some outliers driving it. _500m is the highest R^2, but check other ones to see if they have a non-outlier driven relationship
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_500m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & com_500m < 2), bc_conc ~ com_500m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_750m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & com_750m < 2), bc_conc ~ com_750m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_1000m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & com_1000m < 2), bc_conc ~ com_1000m))
#com_750m looks like a better candidate

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ resid_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & resid_50m < 0.9), bc_conc ~ resid_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ resid_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & resid_100m < 1), bc_conc ~ resid_100m))
#it still holds if we take out that clump around 1. Resid_100m has a nicer spread and a higher R2 without the clump

#ind_100m has the highest R2, but it doesn't have much spread. 
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ ind_1000m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & ind_1000m < 2), bc_conc ~ ind_1000m))
#holds pretty well without the outliers. R2 of .191
#check the other distances. 
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ ind_750m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & ind_750m < 2), bc_conc ~ ind_750m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ ind_500m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & ind_500m < 2), bc_conc ~ ind_500m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ ind_300m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & ind_300m < 2), bc_conc ~ ind_300m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ ind_200m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & ind_200m < 2), bc_conc ~ ind_200m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ ind_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & ind_100m < 2), bc_conc ~ ind_100m))
#nearly all of the other non-outlier ones have R2 below .191. I think I'll put the _1000m in because it has a better spread

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_750m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & com_750m < 1.9), bc_conc ~ com_750m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_500m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & com_500m < 1.9), bc_conc ~ com_500m))
#just driven by a couple of outliers. 

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ open_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & open_50m < 2), bc_conc ~ open_50m))
#good

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ mjrd_300m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & mjrd_300m < 2), bc_conc ~ mjrd_300m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ mjrd_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & mjrd_100m < 2), bc_conc ~ mjrd_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ mjrd_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & mjrd_50m < 2), bc_conc ~ mjrd_50m))
#_100m is the best

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ road_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & road_50m < 2), bc_conc ~ road_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ road_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(road_50m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ road_50m + I(road_50m^2)))
#non-linear has smaller R2

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_highway))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(d_highway^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_highway + I(d_highway^2)))
#nope, just keep the linear

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_majrd))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(d_majrd^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_majrd + I(d_majrd^2)))
#non-linear has smalleer R2

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ bus_200m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & bus_200m < 2), bc_conc ~ bus_200m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ bus_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & bus_100m < 2), bc_conc ~ bus_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ bus_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & bus_50m < 2), bc_conc ~ bus_50m))
#bus_50m looks good. 

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ inter_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & inter_50m < 2), bc_conc ~ inter_50m))
#gooood

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ traffic_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & traffic_100m < 2), bc_conc ~ traffic_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ traffic_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(traffic_100m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ traffic_100m + I(traffic_100m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000& traffic_100m < 2), bc_conc ~ traffic_100m + I(traffic_100m^2)))
#looks very good with the linear and ^2 component, high R2 value, but take out the outliers and it's gone. I don't think we should add a non-linear component just to catch 3 outliers
#lin relationship stays strong without outliers. 


summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ tot_traffic_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & tot_traffic_100m < 2), bc_conc ~ tot_traffic_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ tot_traffic_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(tot_traffic_100m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ tot_traffic_100m + I(tot_traffic_100m^2)))
#same deal as traffic. I think they are very similar data

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ tot_Nox_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & tot_Nox_100m < 2), bc_conc ~ tot_Nox_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ tot_Nox_100m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(tot_Nox_100m^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ tot_Nox_100m + I(tot_Nox_100m^2)))
#same deal as traffic. I think they are very similar data

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ Nox_50m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & Nox_50m < 2), bc_conc ~ Nox_50m))
#gooood


summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_NPRI_PM))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(d_NPRI_PM^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_NPRI_PM + I(d_NPRI_PM^2)))
#lin + on-lin look good together. 

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_airport))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(d_airport^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_airport + I(d_airport^2)))
#just the lin


summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ rail_1000m))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000 & rail_1000m < 2), bc_conc ~ rail_1000m))
#goooood

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_railline))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(d_railline^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_railline + I(d_railline^2)))
#this was a p > 0.05, checking the potential non-linear. They are not there. 

summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_NPRI_Nox))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ I(d_NPRI_Nox^2)))
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ d_NPRI_Nox + I(d_NPRI_Nox^2)))
#this was a p > 0.05, checking the potential non-linear. They are not there. 



# Variable Selection TO S log   0 < BC#####
log.t.s.data.stan <- t.s.data.stan
log.t.s.data.stan$logbc <- log(t.s.data.stan$bc_conc+1)


to.log.o0.lin.variables <- c("build_750m", "com_750m", "resid_100m", "ind_500m", "open_100m", "mjrd_300m", "road_100m", "d_highway",
                             "d_majrd", "traffic_100m", "tot_traffic_100m", "Nox_50m", "tot_Nox_100m", "d_railline", "rail_1000m")

to.log.o0.nlin.variables <- data.frame(var = c(NA), status = rep("NA", 1))



to.log.o0.full.multi.lm <- lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ build_750m + com_750m + resid_100m + ind_500m + open_100m + mjrd_300m + road_100m + d_highway +
           d_majrd + traffic_100m + tot_traffic_100m + Nox_50m + tot_Nox_100m + d_railline + rail_1000m)

summary(to.log.o0.full.multi.lm)
#0.57

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ build_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & build_750m < 2), logbc ~ build_750m))
#goooood

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ com_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & com_1000m < 2), logbc ~ com_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ com_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & com_750m < 1.9), logbc ~ com_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ com_500m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & com_500m < 2), logbc ~ com_500m))
#only _750m holds with outliers above 2 cut out and it doesn't hold when that one at 2 is cut. Basically 3 points are driving this. Select it for now, but not liekly a good one. 

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ parks_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & parks_750m < 2.5), logbc ~ parks_750m))
#nope. it's that one big outlier that creates the association

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ ind_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & ind_1000m < 2 & logbc < 10 & logbc > 5), logbc ~ ind_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ ind_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & ind_750m < 2 & logbc < 10 & logbc > 5), logbc ~ ind_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ ind_500m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & ind_500m < 2 & logbc < 10 & logbc > 5), logbc ~ ind_500m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ ind_300m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & ind_300m < 2 & logbc < 10 & logbc > 5), logbc ~ ind_300m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ ind_200m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & ind_200m < 2 & logbc < 10 & logbc > 5), logbc ~ ind_200m))
#ind_1000m has the most spread out, though a lower R2. Srta looks like _500m has the highest even with outliers removed. 

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ water_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & water_1000m < 2), logbc ~ water_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ water_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & water_750m < 2), logbc ~ water_750m))
#nope, just the outliers driving it

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ resid_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & resid_100m < 2 & logbc < 10 & logbc > 5), logbc ~ resid_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ resid_200m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & resid_200m < 2 & logbc < 10 & logbc > 5), logbc ~ resid_200m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ resid_300m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & resid_300m < 2 & logbc < 10 & logbc > 5), logbc ~ resid_300m))
#yep, _100m

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ open_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & open_100m < 2 & logbc < 10 & logbc > 5), logbc ~ open_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ open_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & open_50m < 2 & logbc < 10 & logbc > 5), logbc ~ open_50m))
#it holds. the _100m has a higher R2 with outliers removed. And a nicer spread

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ mjrd_300m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & mjrd_300m < 2 & logbc < 10 & logbc > 5), logbc ~ mjrd_300m))
#goood

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ road_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & road_100m < 2 & logbc < 10 & logbc > 5), logbc ~ road_100m))
#gets weaker, but holds

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_highway))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ I(d_highway^2)))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_highway + I(d_highway^2)))
#lin is best
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_highway))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & d_highway < 2 & logbc < 10 & logbc > 5), logbc ~ d_highway))
#good

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_majrd))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ I(d_majrd^2)))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_majrd + I(d_majrd^2)))
#lin is best. x^2 also works, but smaller R2. 
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_majrd))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & d_majrd < 2 & logbc < 10 & logbc > 5), logbc ~ d_majrd))
#good

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ bus_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & bus_50m < 2), logbc ~ bus_50m))
#nope

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ traffic_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & traffic_100m < 2 & logbc < 10 & logbc > 5), logbc ~ traffic_100m))
#good

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ tot_traffic_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & tot_traffic_100m < 2 & logbc < 10 & logbc > 5), logbc ~ tot_traffic_100m))
#good

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ Nox_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & Nox_50m < 2 & logbc < 10 & logbc > 5), logbc ~ Nox_50m))
#gooood

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ tot_Nox_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & tot_Nox_100m < 2 & logbc < 10 & logbc > 5), logbc ~ tot_Nox_100m))
#gooood

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_railline))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ I(d_railline^2)))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ d_railline + I(d_railline^2)))
#lin is best. 
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & logbc < 10 & logbc > 5), logbc ~ d_railline))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & d_railline < 2), logbc ~ d_railline))
#but it doesn't hold up without those outliers.....what do now? keep it for now, just know that the BC outliers are probably driving it. 

summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0), logbc ~ rail_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 0 & rail_1000m < 2 & logbc < 10 & logbc > 5), logbc ~ rail_1000m))
#_1000m keeps a higher R2 after outlier removal. 

#checked the d_ of p > 0.05 and none look like they have potential non-linear. Like not even close. All vey flat. 





# Variable Selection TO S log 100 < BC < 10k#####

to.log.o100u10k.lin.variables <- c("build_200m", "com_750m", "resid_100m", "ind_750m", "open_50m", "mjrd_100m", "road_50m", "d_highway", 
                                   "d_majrd", "bus_50m", "traffic_100m", "tot_traffic_100m", "Nox_50m", "tot_Nox_100m", "rail_1000m")
#almost the same as the o0 variables, just slightly differenty buffer sizes on some and bus_50m is added. 

to.log.o100u10k.nlin.variables <- data.frame(var = c(NA), status = rep("NA", 1))

to.log.o100u10k.full.multi.lm <- lm(data = filter(log.t.s.data.stan, bc_conc > 100 & bc_conc < 10000), logbc ~ build_200m + com_750m + resid_100m + ind_750m + open_50m + 
             mjrd_100m + road_50m + d_highway + d_majrd + bus_50m + traffic_100m + tot_traffic_100m + Nox_50m + tot_Nox_100m + rail_1000m)
summary(to.log.o100u10k.full.multi.lm)
#R2 is 0.6132

#the step down p < 0.05, still gives R2 = 0.5743
summary(lm(data = filter(log.t.s.data.stan, bc_conc > 100 & bc_conc < 10000), logbc ~ ind_750m +  
             mjrd_100m + road_50m + d_highway + Nox_50m))



summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ build_200m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & build_200m < 2), logbc ~ build_200m))
#gooood

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ com_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & com_1000m < 2), logbc ~ com_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ com_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & com_750m < 2), logbc ~ com_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ com_500m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & com_500m < 2), logbc ~ com_500m))
#_750m has highest R2 after removal of outliers. 

#resid_200m, _100m and _50m all look great. Not big outliers, _50m has the highest R2 but the most clumped
nrow(filter(t.s.data.stan, resid_200m > 0.9))/nrow(t.s.data.stan)
nrow(filter(t.s.data.stan, resid_100m > 0.9))/nrow(t.s.data.stan)
nrow(filter(t.s.data.stan, resid_50m > 0.9))/nrow(t.s.data.stan)
#I guess just make a judgement call and take resid_100m, it's a .029 improvement in R2 with 5% more on the line, whereas _50m is a 0.025 for 8%

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ ind_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & ind_1000m < 2), logbc ~ ind_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ ind_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & ind_750m < 2), logbc ~ ind_750m))
#R2 of _750m without outliers is bigger

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ open_100m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 10000 & bc_conc > 100 & open_100m < 2), logbc ~ open_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ open_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 10000 & bc_conc > 100 & open_50m < 2), logbc ~ open_50m))
#opne_50m holds

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ mjrd_300m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & mjrd_300m < 2), logbc ~ mjrd_300m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ mjrd_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & mjrd_100m < 2), logbc ~ mjrd_100m))
#_100m has higher R2 with outliers removed. 

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ road_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & road_100m < 2), logbc ~ road_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ road_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & road_50m < 2), logbc ~ road_50m))
#_50m is gooood 

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ d_highway))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ I(d_highway^2)))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ d_highway + I(d_highway^2)))
#lin is best. 

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ d_majrd))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ I(d_majrd^2)))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ d_majrd + I(d_majrd^2)))
#lin is the best. x^2 works, but has lower R2

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ bus_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & bus_100m < 2), logbc ~ bus_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ bus_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & bus_50m < 2), logbc ~ bus_50m))
#_50m is clumpd, but note that they are all pretty toit. it ahs teh highest R2 with outliers removed. 

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ inter_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & inter_50m < 2), logbc ~ inter_50m))
#nope

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ traffic_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & traffic_100m < 2), logbc ~ traffic_100m))
#goood

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ tot_traffic_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & tot_traffic_100m < 2), logbc ~ tot_traffic_100m))
#goood

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ Nox_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & Nox_100m < 2), logbc ~ Nox_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ Nox_50m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & Nox_50m < 2), logbc ~ Nox_50m))
#Nox_100m has higher R2 with outliers cut out, but that's becasue these outliers are actually dampening the relationship. I like _50m better even though it's a bit clumpy

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ tot_Nox_200m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & tot_Nox_200m < 2), logbc ~ tot_Nox_200m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ tot_Nox_100m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & tot_Nox_100m < 2), logbc ~ tot_Nox_100m))
#_100m has higher R2

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ d_airport))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ I(d_airport^2)))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ d_airport + I(d_airport^2)))
#lin is the best.
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & d_airport < 2), logbc ~ d_airport))
#is driven by the 2 outliers above 2 sd

summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ rail_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & rail_1000m < 2), logbc ~ rail_1000m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ rail_750m))
summary(lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100 & rail_750m < 2), logbc ~ rail_750m))
#_1000m is higher

#check the d_ p > 0.05 for possible non linear, nope, they are all pretty flat. 


# Variable Selection MTL S + TO Pooled ######
#question: mjrd_300m vs 100m or 50m. The smaller distances have a higher R2, but 300m is nice and spread out. When trimming values above 2 sds, the 300m has a higher R2
#question: traffic_50m has a crazy outlier. What do with that? 

#see below for work, but looks like best model is com_750m + ind_1000m + Nox_50m with an R2 of 0.628



formattable(tbl.mts.pool.u10k.bc.uni)
#look at file mts.pool.xy.fit.plot.u10k.pu5.png for the p < 0.05 graphs and the tbl.mts.pool.u10k.bc.uni too

mts.pool.u10k.lin.variables <- c("build_200m", "com_750m", "resid_750m", "ind_500m", "open_50m", "mjrd_300m", "road_50m", "d_majrd", "bus_50m", "inter_50m",
                                 "traffic_50m", "tot_Nox_100m", "d_NPRI_PM", "d_airport", "d_port", "d_shore", "elevation")


mts.pool.u10k.nlin.variables <- c(NA)

#all the linear and non-linear together
mts.pool.u10k.full.multi.lm <- lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ build_200m + com_750m + resid_750m + ind_500m + open_50m + mjrd_300m + road_50m + 
             d_majrd + bus_50m + inter_50m + traffic_50m + tot_Nox_100m + d_NPRI_PM + d_airport + d_port + d_shore + elevation)
summary(mts.pool.u10k.full.multi.lm)
#gives 0.3297

summary(bic.glm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ com_750m + resid_750m + ind_500m + open_50m + mjrd_300m + road_50m + 
                  d_majrd + bus_50m + inter_50m + traffic_50m + tot_Nox_100m + d_NPRI_PM + d_airport + d_port + d_shore + elevation, glm.family = gaussian))
describe(mts.data.pool.stan$elevation)
#there are missing values for elevation, can't run bic.glm with it. 

#eliminating the highest p values 1 by 1, we have this that has an R2 of 0.333, compared to the everything that has R2 0.3241
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ com_750m + resid_750m + mjrd_300m + road_50m + 
             d_majrd + inter_50m + tot_Nox_100m + d_airport + d_shore + elevation))
#this still has R2 of 0.299
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ resid_750m + road_50m + d_majrd + tot_Nox_100m + d_airport))

#bic.glm suggests this, which gives R2 of 0.2662
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ road_50m + d_majrd + d_airport))


summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ build_200m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & build_200m < 2), bc_conc ~ build_200m))
#good, new!

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ com_500m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & com_500m < 2), bc_conc ~ com_500m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & com_500m < 3), bc_conc ~ com_500m))
#five points beyond 2, four of which are beyond 3. p > 0.05 when cutting the out. 
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ com_750m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & com_750m < 2), bc_conc ~ com_750m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & com_750m < 3), bc_conc ~ com_750m))
#com_750m is more robust

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ resid_750m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & resid_750m < 2), bc_conc ~ resid_750m))
#gooood

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ ind_500m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & ind_500m < 2), bc_conc ~ ind_500m))
#good

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ open_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & open_50m < 2), bc_conc ~ open_50m))
#goood. super weak, but good. 

#mjrd_300 has a much nicer spread. a little less R2
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ mjrd_300m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & mjrd_300m < 2), bc_conc ~ mjrd_300m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ mjrd_100m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & mjrd_100m < 2), bc_conc ~ mjrd_100m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ mjrd_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & mjrd_50m < 2), bc_conc ~ mjrd_50m))
#_300m has a higher R2 when we cut out points above 2sds. 

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ road_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & road_50m < 2), bc_conc ~ road_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ road_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(road_50m^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ road_50m + I(road_50m^2)))
#linear has the highest R2. Not worried about outliers on this one. 

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_majrd))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(d_majrd^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_majrd + I(d_majrd^2)))
#only linear keeps p < 0.05

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ bus_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & bus_50m < 2), bc_conc ~ bus_50m))
#gooooood

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ inter_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & inter_50m < 2), bc_conc ~ inter_50m))
#goooood

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ traffic_50m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & traffic_50m < 2), bc_conc ~ traffic_50m))
#goooood. It's actually there in spite of the outlier. THe outlier dampens. 

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ tot_Nox_100m))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000 & tot_Nox_100m < 2), bc_conc ~ tot_Nox_100m))
#goood. 

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_NPRI_PM))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(d_NPRI_PM^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_NPRI_PM + I(d_NPRI_PM^2)))
#just linear

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_airport))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(d_airport^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_airport + I(d_airport^2)))
#linear has a higher R2. The combined doesn'thave x^2 p < 0.05

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_port))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(d_port^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_port + I(d_port^2)))
#only linear

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_shore))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(d_shore^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_shore + I(d_shore^2)))
#linear has a higher R2. The combined doesn'thave x^2 p < 0.05

summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_highway))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ I(d_highway^2)))
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ d_highway + I(d_highway^2)))
#nope accross the board/ 








# Variable Selection MTL S + TO Pooled log 0 < BC < 10k########
#question: traffic_50m has a huge outlier that is actually dampening the relationship. Without it, the relationship is stronger. 

mts.pool.log.o0u13k.lin.variables <- c("build_200m", "com_500m", "resid_750m", "ind_1000m", "open_50m", "mjrd_300m", "road_50m", "d_majrd", "bus_50m", "inter_50m",
                                 "traffic_50m", "tot_Nox_100m", "d_NPRI_PM", "d_railline", "d_airport", "d_port", "d_shore", "elevation")

mts.pool.log.o0u13k.nlin.variables <- data.frame(var = c("d_majrd", "d_NPRI_PM"), status = c("together", "together"))
#d_majrd and d_NPRI_PM not on it's own, use as x + x^2
  
log.mts.data.pool.stan <- mts.data.pool.stan
log.mts.data.pool.stan$logbc <- log(mts.data.pool.stan$bc_conc+1)

mts.pool.log.o0u13k.full.multi.lm <-lm(data = filter(log.mts.data.pool.stan, bc_conc < 10000 & bc_conc > 0), formula = logbc ~ build_200m + com_500m + resid_750m + ind_1000m + 
             open_50m + mjrd_300m + road_50m + d_majrd + bus_50m + inter_50m + traffic_50m + tot_Nox_100m + d_NPRI_PM + 
             d_railline + d_airport + d_port + d_shore + I(d_majrd^2) + I(d_NPRI_PM^2) + elevation)
summary(mts.pool.log.o0u13k.full.multi.lm)
#all in gives 0.446


summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 10000 & bc_conc > 0), formula = logbc ~ com_500m + ind_1000m + 
             d_majrd + inter_50m + tot_Nox_100m + d_railline + d_airport + d_shore + I(d_majrd^2)))
#this keeps the R2 pretty high still, at 0.4084
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ build_200m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & build_200m < 2), logbc ~ build_200m))
#goood. 

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ com_500m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & com_500m < 2), logbc ~ com_500m))
#goood. 

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ ind_1000m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & ind_1000m < 2), logbc ~ ind_1000m))
#goood. 


summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ open_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & open_50m < 2), logbc ~ open_50m))
#good


summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ mjrd_300m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & mjrd_300m < 2), logbc ~ mjrd_300m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ mjrd_100m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & mjrd_100m < 2), logbc ~ mjrd_100m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ mjrd_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & mjrd_50m < 2), logbc ~ mjrd_50m))

#with outliers removed, mjrd_300m keeps a higher R2 value. The other two keep a p < 0.05, but their R2 nearly halves

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ road_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & road_50m < 3), logbc ~ road_50m))
#nope. It is driven by those outleirs. Tried road_100m and it's the same. But there are 8 out there. It holds without hte 3sd outlier. Throw it in

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_majrd))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_majrd^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_majrd + I(d_majrd^2)))
#the x  + x^2 is the best

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ bus_100m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & bus_100m < 2), logbc ~ bus_100m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ bus_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & bus_50m < 2), logbc ~ bus_50m))
#_50m has the higher R2 with and wihout the outliers

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ inter_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & inter_50m < 2), logbc ~ inter_50m))
#goood

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ traffic_50m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & traffic_50m < 2), logbc ~ traffic_50m))
#goood. The extreme large value outlier is actually dampening the relationship

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ tot_Nox_100m))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0 & tot_Nox_100m < 2), logbc ~ tot_Nox_100m))
#good.

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_NPRI_Nox))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_NPRI_Nox^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_NPRI_Nox + I(d_NPRI_Nox^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0, d_NPRI_Nox < 3), logbc ~ d_NPRI_Nox))
#only lin, but driven by the 4 points above 2sds

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_NPRI_PM))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_NPRI_PM^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_NPRI_PM + I(d_NPRI_PM^2)))
#better with the x + x^2

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_airport))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_airport^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_airport + I(d_airport^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0, d_airport < 2), logbc ~ d_airport))
#x^2 not as high of R2. Not good together. Just keep linear

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_railline))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_railline^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_railline + I(d_railline^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0, d_railline < 2), logbc ~ d_railline))
#just lin

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_port))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_port^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_port + I(d_port^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0, d_port < 2), logbc ~ d_port))
#just lin

summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_shore))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ I(d_shore^2)))
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ d_shore + I(d_shore^2)))
#just lin



# Variable Selection Summary ######

#put all the full mutlis into a single data frame so I can look at them in markdown
all.multis.glance <- data.frame(Model = c("m.s.u4k.full.multi.lm", " m.s.log.full.multi.lm", " m.s.log.o0.full.multi.lm", "m.w.full.multi.lm", " m.a.u4k.full.multi.lm", " m.a.log.full.multi.lm", " m.a.log.u4k.full.multi.lm", " to.u10k.full.multi.lm", " to.log.o0.full.multi.lm", " to.log.o100u10k.full.multi.lm", " mts.pool.u10k.full.multi.lm", " mts.pool.log.o0u13k.full.multi.lm"), 
                         bind_rows(glance(m.s.u4k.full.multi.lm), glance(m.s.log.full.multi.lm), glance(m.s.log.o0.full.multi.lm), glance(m.w.full.multi.lm), glance(m.a.u4k.full.multi.lm), glance(m.a.log.full.multi.lm), glance(m.a.log.u4k.full.multi.lm), glance(to.u10k.full.multi.lm), glance(to.log.o0.full.multi.lm), glance(to.log.o100u10k.full.multi.lm), glance(mts.pool.u10k.full.multi.lm), glance(mts.pool.log.o0u13k.full.multi.lm)))
write.csv(all.multis.glance, "MTL TO markdown/all.multis.glance.csv")




#### MTL SUMMER
#change the names of variables to make it easier to split
sel.lin.var.m.s.u4k.bc.uni <- filter(m.s.u4k.bc.uni.reg, variable %in% m.s.u4k.lin.variables)
sel.lin.var.m.s.u4k.bc.uni$variable <- sel.lin.var.m.s.u4k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.m.s.u4k.bc.uni <- sel.lin.var.m.s.u4k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.s.u4k.bc.uni)
write.csv(sel.lin.var.m.s.u4k.bc.uni, "MTL TO markdown/sel.lin.var.m.s.u4k.bc.uni.csv")


sel.lin.var.m.s.log.bc.uni <- filter(m.s.log.bc.uni.reg, variable %in% m.s.log.lin.variables)
sel.lin.var.m.s.log.bc.uni$variable <- sel.lin.var.m.s.log.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.m.s.log.bc.uni <- sel.lin.var.m.s.log.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "Conf.Int", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.s.log.bc.uni)
write.csv(sel.lin.var.m.s.log.bc.uni, "MTL TO markdown/sel.lin.var.m.s.log.bc.uni.csv")

sel.lin.var.m.s.log.o0.bc.uni <- filter(m.s.log.o0.bc.uni.reg, variable %in% m.s.log.o0.lin.variables)
sel.lin.var.m.s.log.o0.bc.uni$variable <- sel.lin.var.m.s.log.o0.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.m.s.log.o0.bc.uni <- sel.lin.var.m.s.log.o0.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6)%>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.s.log.o0.bc.uni)
write.csv(sel.lin.var.m.s.log.o0.bc.uni, "MTL TO markdown/sel.lin.var.m.s.log.o0.bc.uni.csv")

### MTL Winter
sel.lin.var.m.w.bc.uni <- filter(m.w.bc.uni.reg, variable %in% m.w.lin.variables)
sel.lin.var.m.w.bc.uni$variable <- sel.lin.var.m.w.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)  %>%
  gsub("airport", "airport*", .)%>%
  gsub("NPRI.Nox", "NPRI.Nox*", .)
sel.lin.var.m.w.bc.uni <- sel.lin.var.m.w.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.w.bc.uni)
write.csv(sel.lin.var.m.w.bc.uni, "MTL TO markdown/sel.lin.var.m.w.bc.uni.csv")

### MTL ANNUAL
#not sure how to automate, so I just jamed it in there. 
m.a.u4k.nlin.variables
sel.lin.var.m.a.u4k.bc.uni <- filter(m.a.u4k.bc.uni.reg, variable %in% m.a.u4k.lin.variables)
sel.lin.var.m.a.u4k.bc.uni$variable <- sel.lin.var.m.a.u4k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .) %>%
  gsub("airport", "airport*", .)%>%
  gsub("shore", "shore*", .)
sel.lin.var.m.a.u4k.bc.uni <- sel.lin.var.m.a.u4k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.a.u4k.bc.uni)
write.csv(sel.lin.var.m.a.u4k.bc.uni, "MTL TO markdown/sel.lin.var.m.a.u4k.bc.uni.csv")


m.a.log.nlin.variables
sel.lin.var.m.a.log.bc.uni <- filter(m.a.log.bc.uni.reg, variable %in% m.a.log.lin.variables)
sel.lin.var.m.a.log.bc.uni$variable <- sel.lin.var.m.a.log.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.m.a.log.bc.uni <- sel.lin.var.m.a.log.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.a.log.bc.uni)
write.csv(sel.lin.var.m.a.log.bc.uni, "MTL TO markdown/sel.lin.var.m.a.log.bc.uni.csv")

m.a.log.u4k.nlin.variables
sel.lin.var.m.a.log.u4k.bc.uni <- filter(m.a.log.u4k.bc.uni.reg, variable %in% m.a.log.u4k.lin.variables)
sel.lin.var.m.a.log.u4k.bc.uni$variable <- sel.lin.var.m.a.log.u4k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .) %>%
  gsub("airport", "airport*", .)%>%
  gsub("shore", "shore*", .)
sel.lin.var.m.a.log.u4k.bc.uni <- sel.lin.var.m.a.log.u4k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.m.a.log.u4k.bc.uni)
write.csv(sel.lin.var.m.a.log.u4k.bc.uni, "MTL TO markdown/sel.lin.var.m.a.log.u4k.bc.uni.csv")


#####TO SUMMER
to.u10k.nlin.variables
sel.lin.var.to.u10k.bc.uni <- filter(to.u10k.bc.uni.reg, variable %in% to.u10k.lin.variables)
sel.lin.var.to.u10k.bc.uni$variable <- sel.lin.var.to.u10k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .) %>%
  gsub("NPRI.PM", "NPRI.PM**", .)
sel.lin.var.to.u10k.bc.uni <- sel.lin.var.to.u10k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.to.u10k.bc.uni)
write.csv(sel.lin.var.to.u10k.bc.uni, "MTL TO markdown/sel.lin.var.to.u10k.bc.uni.csv")

sel.lin.var.to.log.o0.bc.uni <- filter(to.log.o0.bc.uni.reg, variable %in% to.log.o0.lin.variables)
sel.lin.var.to.log.o0.bc.uni$variable <- sel.lin.var.to.log.o0.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.to.log.o0.bc.uni <- sel.lin.var.to.log.o0.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
  formattable(sel.lin.var.to.log.o0.bc.uni)
write.csv(sel.lin.var.to.log.o0.bc.uni, "MTL TO markdown/sel.lin.var.to.log.o0.bc.uni.csv")

to.log.o100u10k.nlin.variables
sel.lin.var.to.log.o100u10k.bc.uni <- filter(to.log.o100u10k.bc.uni.reg, variable %in% to.log.o100u10k.lin.variables)
sel.lin.var.to.log.o100u10k.bc.uni$variable <- sel.lin.var.to.log.o100u10k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.to.log.o100u10k.bc.uni <- sel.lin.var.to.log.o100u10k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.to.log.o100u10k.bc.uni)
write.csv(sel.lin.var.to.log.o100u10k.bc.uni, "MTL TO markdown/sel.lin.var.to.log.o100u10k.bc.uni.csv")

####MTL + TO SUMMER POOLED
mts.pool.u10k.lin.variables
sel.lin.var.mts.pool.u10k.bc.uni <- filter(mts.pool.u10k.bc.uni.reg, variable %in% mts.pool.u10k.lin.variables)
sel.lin.var.mts.pool.u10k.bc.uni$variable <- sel.lin.var.mts.pool.u10k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .)
sel.lin.var.mts.pool.u10k.bc.uni <- sel.lin.var.mts.pool.u10k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.mts.pool.u10k.bc.uni)
write.csv(sel.lin.var.mts.pool.u10k.bc.uni, "MTL TO markdown/sel.lin.var.mts.pool.u10k.bc.uni.csv")


mts.pool.log.o0u13k.nlin.variables
sel.lin.var.mts.pool.log.o0u13k.bc.uni <- filter(mts.pool.log.o0u13.bc.uni.reg, variable %in% mts.pool.log.o0u13k.lin.variables)
sel.lin.var.mts.pool.log.o0u13k.bc.uni$variable <- sel.lin.var.mts.pool.log.o0u13k.bc.uni$variable %>%
  gsub("^d_", "d.", .) %>%
  gsub("NPRI_Nox", "NPRI.Nox", .) %>%
  gsub("^tot_", "tot.", .) %>%
  gsub("^bus_stop", "bus.stop", .) %>%
  gsub("d.majrd", "d.majrd**", .) %>%
  gsub("NPRI.PM", "NPRI.PM**", .)
sel.lin.var.mts.pool.log.o0u13k.bc.uni <- sel.lin.var.mts.pool.log.o0u13k.bc.uni %>%
  arrange(variable) %>%
  separate(col = variable, into = c("IV", "Buffer Size"), sep = "_", remove = TRUE) %>%
  dplyr::select(-SE, -P.Value) %>%
  unite(col = "CI", `X2.5.`, `X97.5.`, sep = ", ") %>%
  mutate("95 percent Conf.Int" = paste0("(", CI, ")")) %>%
  dplyr::select(1,2,3,7,5,6) %>%
  rename(c("r.squared" = "R^2"))
formattable(sel.lin.var.mts.pool.log.o0u13k.bc.uni)
write.csv(sel.lin.var.mts.pool.log.o0u13k.bc.uni, "MTL TO markdown/sel.lin.var.mts.pool.o0u13k.bc.uni.csv")












# Selected Variable Residual Histograms #####


# MTL Summer
selected_resid_hist_m_s_u4k_bc <- filter(long.m.s.data.stan, bc_conc < 4000 & variable %in% m.s.u4k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 150) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Summer Residual Histograms for Uni Regs of Selected Variables (5 BC outliers > 4,000 removed)")

m.s.log.lin.variables

selected_resid_hist_m_s_log_bc <- filter(long.m.s.data.stan, variable %in% m.s.log.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Summer Residual Histograms for log Uni Regs of Selected Variables")


selected_resid_hist_m_s_log_o0_bc <- filter(long.m.s.data.stan, bc_conc > 0 & variable %in% m.s.log.o0.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram() +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Summer Residual Histograms for log Uni Regs of Selected Variables (6 BC = 0 removed)")
nrow(filter(m.s.data.stan, bc_conc ==0))

# MTL Winter
selected_resid_hist_m_w_bc <- filter(long.m.w.data.stan, variable %in% m.w.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 150) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Winter Residual Histograms for Linear Uni Regs of Selected Variables")

selected_nlin_resid_hist_m_w_bc <- filter(long.m.w.data.stan, variable %in% m.w.nlin.variables$var) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ I(value^2), data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 150) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Winter Residual Histograms for Non-Linear Uni Regs of Selected Variables")



#MTL Annual
selected_resid_hist_m_a_u4k_bc <- filter(long.m.a.data.stan, bc_conc < 4000 & variable %in% m.a.u4k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 150) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Annual Residual Histograms for Uni Regs of Selected Variables (3 BC > 4,000 removed)")
nrow(filter(m.a.data.stan, bc_conc > 4000))

selected_nlin_resid_hist_m_a_u4k_bc <- filter(long.m.a.data.stan, bc_conc < 4000 & variable %in% m.a.u4k.nlin.variables$var) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ I(value^2), data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 150) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Annual Residual Histograms for Non-Linear Uni Regs of Selected Variables (3 BC > 4,000 removed)")
nrow(filter(m.a.data.stan, bc_conc > 4000))

selected_resid_hist_m_a_log_bc <- filter(long.m.a.data.stan, variable %in% m.a.log.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.1) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Annual Residual Histograms for log Uni Regs of Selected Variables")

selected_resid_hist_m_a_log_u4k_bc <- filter(long.m.a.data.stan, bc_conc < 4000 & variable %in% m.a.log.u4k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.1) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Annual Residual Histograms for log Uni Regs of Selected Variables (3 BC > 4,000 removed)")

selected_nlin_resid_hist_m_a_log_u4k_bc <- filter(long.m.a.data.stan, bc_conc < 4000 & variable %in% m.a.log.u4k.nlin.variables$var) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc+1) ~ I(value^2), data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.1) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Montreal Annual Residual Histograms for Non-Linear log Uni Regs of Selected Variables (3 BC > 4,000 removed)")
nrow(filter(m.a.data.stan, bc_conc > 4000))

# Toronto Summer
selected_resid_hist_t_s_u10k_bc <- filter(long.t.s.data.stan, bc_conc < 10000 & variable %in% to.u10k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 350) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Toronto Summer Residual Histograms for Uni Regs of Selected Variables (1 BC > 10,000 removed)")

selected_nlin_resid_hist_t_s_u10k_bc <- filter(long.t.s.data.stan, bc_conc < 10000 & variable %in% to.u10k.nlin.variables$var) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value + I(value^2), data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 350) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Toronto Summer Residual Histograms for Non-Linear Uni Regs of Selected Variables (1 BC > 10,000 removed)")

selected_resid_hist_t_s_log_o0_bc <- filter(long.t.s.data.stan, bc_conc > 0 & variable %in% to.log.o0.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.15) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Toronto Summer Residual Histograms for log Uni Regs of Selected Variables (1 BC = 0 removed)")
nrow(filter(t.s.data.stan, bc_conc ==0))

selected_resid_hist_t_s_log_o100u10k_bc <- filter(long.t.s.data.stan, bc_conc > 100 & bc_conc < 10000 & variable %in% to.log.o100u10k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.15) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("Toronto Summer Residual Histograms for log Uni Regs of Selected Variables (2 BC < 100 and 1 BC > 10,000 removed)")



#MTL + TO Pooled Summer
selected_resid_hist_mts_pool_u10k_bc <- filter(long.mts.pool.data.stan, bc_conc < 10000 & variable %in% mts.pool.u10k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 350) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("MTL + TO Summer Pooled Residual Histograms for Uni Regs of Selected Variables (2 BC > 10,000 removed)")
nrow(filter(mts.data.pool.stan, bc_conc > 10000))

selected_resid_hist_mts_pool_log_o0u13k_bc <- filter(long.mts.pool.data.stan, bc_conc > 0 & bc_conc < 13000 & variable %in% mts.pool.log.o0u13k.lin.variables) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.15) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("MTL + TO Summer Pooled Residual Histograms for log Uni Regs of Selected Variables (7 BC = 0 and 1 BC > 13,000 removed)")
nrow(filter(mts.data.pool.stan, bc_conc == 0))
nrow(filter(mts.data.pool.stan, bc_conc > 13000))

selected_nlin_resid_hist_mts_pool_log_o0u13k_bc <- filter(long.mts.pool.data.stan, bc_conc > 0 & bc_conc < 13000 & variable %in% mts.pool.log.o0u13k.nlin.variables$var) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value + I(value^2), data = .)), resids = map(fit, residuals)) %>%
  unnest(resids) %>%
  ggplot(data = ., aes(x = resids)) + 
  geom_histogram(binwidth = 0.2) +  
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, colour = "red") +
  ggtitle("MTL + TO Summer Pooled Residual Histograms for Non-Linear log Uni Regs of Selected Variables (7 BC = 0 and 1 BC > 13,000 removed)")







############################ MTL Annual u4k Final Model Build (Rem Cor, Step, Ex Intx) ####
#these are the candidate variables
m.a.u4k.lin.variables
paste(m.a.u4k.lin.variables, collapse = ' + ')
m.a.u4k.nlin.variables

#now check to see if any are colinear. If any are above 0.7, then take the highest of the two and cut the other out
#note spearman was used in the UFP MTL paper
cor(dplyr::select(dplyr::filter(m.a.data.stan, bc_conc < 4000), build_1000m, mjrd_1000m, bus_stop_200m, traffic_1000m, 
                  tot_Nox_750m, d_NPRI_Nox, d_railline, d_airport, d_shore), method = "spearman")

#mjrd (0.101) and build (0.17), keep build, kick out mjrd
#traffic (0.08) and mjrd (0.101), mjrd already kicked out, keep traffic
#tot_Nox_750m (0.187) and traffic_1000m (0.08), tot_Nox is better and better to have, keep it, kick out traffic


#name the non-correlated variables something else
m.a.u4k.lin.var.ncor <- m.a.u4k.lin.variables[c(-2, -4)]
m.a.u4k.nlin.var.ncor <- m.a.u4k.nlin.variables
length(m.a.u4k.lin.var.ncor)
#using leaps(), it has a setting for exhaustive search and you can set the criteria to R2



#do an exhaustive search of all variable combinations, (simple effect, 2 way interactions and squared). MTL Annual only has 7 candidate variables, so no need for a supervised step down, my computer can handle it. 
m.a.u4k.regsubsets.out.7vars.ex <- regsubsets(bc_conc ~ (build_1000m + bus_stop_200m + tot_Nox_750m + d_NPRI_Nox + d_airport + 
                                                  d_railline + d_shore)^2 + I(build_1000m^2) + I(bus_stop_200m^2) + 
                                       I(tot_Nox_750m^2) + I(d_NPRI_Nox^2) + I(d_railline^2) + I(d_airport^2) + I(d_shore^2),
                                     data = dplyr::filter(m.a.data.stan, bc_conc < 4000),
                                     nbest = 1,       # 1 best model for each number of predictors
                                     nvmax = length(m.a.u4k.lin.var.ncor),    # the number of non-correlated candidate variables, note that the non-linear terms are in this list 
                                     force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                     method = "exhaustive")
#take a look at the outputs
sum.m.a.u4k.regsubsets.out.7vars.ex <- summary(m.a.u4k.regsubsets.out.7vars.ex)
#these show the values for the best models of each size (1 to 7)
sum.m.a.u4k.regsubsets.out.7vars.ex$rsq
sum.m.a.u4k.regsubsets.out.7vars.ex$bic
sum.m.a.u4k.regsubsets.out.7vars.ex$adjr2
which.max(sum.m.a.u4k.regsubsets.out.7vars.ex$adjr2)
sum.m.a.u4k.regsubsets.out.7vars.ex$which[7,]
#largest is the best (makes sense). Now just make a data frame that is easier to handle for listing the variables of the best model
m.a.u4k.7vars.ex <- as.data.frame(sum.m.a.u4k.regsubsets.out.7vars.ex$which[7,])
m.a.u4k.7vars.ex <- data.frame(Variables = rownames(m.a.u4k.7vars.ex), In = m.a.u4k.7vars.ex$`sum.m.a.u4k.regsubsets.out.7vars.ex$which[7, ]`)
m.a.u4k.final.model.vars <- dplyr::filter(m.a.u4k.7vars.ex, In == TRUE)$Variables
paste(m.a.u4k.final.model.vars[-1], collapse = ' + ')


#nifty plot we can do to see which variables get selected for each R2 outcome. It shows which variables get added for each size of model. 
plot(m.a.u4k.regsubsets.out.7vars.ex, scale = "adjr2", main = "Adjusted R^2")


#all in model without correlated variables. Not interactions. The squared terms are squared. 
m.a.u4k.simple.ncor.model.lm <- lm(data = dplyr::filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + bus_stop_200m +  
             tot_Nox_750m + d_NPRI_Nox + d_airport + d_railline + d_shore)
summary(m.a.u4k.simple.ncor.model.lm)

#final model after exhaustive search of all possible interactions
m.a.u4k.final.model.lm <- lm(data = dplyr::filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m + d_NPRI_Nox + d_airport + I(tot_Nox_750m^2) + 
             I(d_shore^2) + tot_Nox_750m:d_railline + d_railline:d_shore)
summary(m.a.u4k.final.model.lm)


#much better like this, R2 = 0.657
#and they are all p < 0.05
#Notice how it takes some of the linear and some of the nono-linear, whichever is best in the whole model. 
#Notice it takes d_railline only in interactions, not by itself.




#####MTL Annual u4k Assumptions and Cross Validation #######
#quick hist of residuals, qq plot, resids vs fitted
#simple multi
hist(m.a.u4k.simple.ncor.model.lm$residuals, main = "7 var Montreal Model")
qqnorm(residuals(m.a.u4k.simple.ncor.model.lm), ylab = "Residuals", main = "Q-Q Plot 7 var Montreal Model")
qqline(residuals(m.a.u4k.simple.ncor.model.lm))
plot(fitted(m.a.u4k.simple.ncor.model.lm), residuals(m.a.u4k.simple.ncor.model.lm), main = "7 var Montreal Model")
abline(h=0)
#all interactions
hist(m.a.u4k.final.model.lm$residuals, main = "7 var Montreal Model with Intx and ^2")
qqnorm(residuals(m.a.u4k.final.model.lm), ylab = "Residuals", main = "Q-Q Plot 7 var Montreal Model with Intx and ^2")
qqline(residuals(m.a.u4k.final.model.lm))
plot(fitted(m.a.u4k.final.model.lm), residuals(m.a.u4k.final.model.lm), main = "7 var Montreal Model with Intx and ^2")
abline(h=0)
#second is better than the first

#look at Cook's D
plot(cooks.distance(m.a.u4k.final.model.lm))
#max is about 0.2, that's goooood. People often use 1 as the threshold
#take a look at Cook's D for simpler/basic model
plot(cooks.distance(m.a.u4k.simple.ncor.model.lm))
#also good, nothing over 0.2. Montreal Annual is doing well. 


m.a.u4k.train.control <- trainControl(method = "LOOCV")

m.a.u4k.simple.ncor.model.caret <- train(bc_conc ~ build_1000m + mjrd_1000m + bus_stop_200m + traffic_1000m + 
                                           tot_Nox_750m + d_NPRI_Nox + d_airport + d_railline + d_shore,   # model to fit
                                   data = dplyr::filter(m.a.data.stan, bc_conc < 4000),                        
                                   trControl = m.a.u4k.train.control,              # folds
                                   method = "glm"                      # specifying regression model
                                   )
m.a.u4k.simple.ncor.model.caret
#R2 drops 0.19

m.a.u4k.final.model.caret <- train(bc_conc ~ tot_Nox_750m + d_NPRI_Nox + d_airport + I(tot_Nox_750m^2) + I(d_shore^2) + 
                               tot_Nox_750m:d_railline + d_railline:d_shore,   # model to fit
                     data = dplyr::filter(m.a.data.stan, bc_conc < 4000),                        
                     trControl = m.a.u4k.train.control,              # folds
                     method = "glm"                      # specifying regression model
                     )
m.a.u4k.final.model.caret
#R2 drops 0.13, suggesting the interactions might be slightly less overfit. 




######we could also use an algorithm to step to it for model selection. 
###this is dead for now. Not persuing it. 
#from something I found online on how to do step(), you need to tell it the simplest and most complicated models you want it to look at
#simplest is the intercept only
m.a.u4k.lower.lm <- lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ 1)
#the largest most complex is all and all interactions. Note that it also has all of them by themselves too.
m.a.u4k.upper.lm.2int <- lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ (build_1000m + bus_stop_200m + 
                                                                                       tot_Nox_750m + d_NPRI_Nox + I(d_airport^2) + d_railline + 
                                                                                       I(d_shore^2))^2)
#now do the step, the k = log(n) is to make it the BIC 
step(m.a.u4k.lower.lm, scope = list(lower = m.a.u4k.lower.lm, upper = m.a.u4k.upper.lm.2int), direction = "both", k = log(nrow(filter(m.a.data.stan, bc_conc < 4000))))
#cross check with the bic.blm function, THEY PICK THE SAME!!!
summary(bic.glm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + bus_stop_200m + tot_Nox_750m + d_NPRI_Nox + 
                  I(d_airport^2) + d_railline + I(d_shore^2), glm.family = gaussian))
#do plain old AIC
step(m.a.u4k.lower.lm, scope = list(lower = m.a.u4k.lower.lm, upper = m.a.u4k.upper.lm.2int), direction = "both", k = 2)
#this doesn't suggest interaction terms, but has more than BIC

#here's the BIC suggested model:
summary(lm(data = filter(m.a.data.stan, bc_conc < 4000), bc_conc ~ tot_Nox_750m + d_NPRI_Nox + I(d_shore^2)))









############ TO SUMMER u10k Final Model Build (Rem Cor, Step, Ex Intx) ####
to.u10k.lin.variables
paste(to.u10k.lin.variables, collapse = ' + ')
to.u10k.nlin.variables

#check for cor() > 0.7, remove those that do (the lowest of the 2 cor vars)
formattable(cor(dplyr::select(dplyr::filter(t.s.data.stan, bc_conc < 10000), build_200m, com_750m, resid_100m, ind_1000m, open_50m, 
                  mjrd_100m, road_50m, d_highway, d_majrd, bus_50m, inter_50m, traffic_100m, tot_traffic_100m, tot_Nox_100m,
                  Nox_50m, d_NPRI_PM, d_airport, rail_1000m
                  ), method = "spearman"))

#Nox is the highest R2, it has cor with: majrd, road, d_majrd, bus, traffic, tot_traffic ANNNNND kick out tot_Nox
#kick all of those out
#rerun, just for a more managable matrix:
formattable(cor(dplyr::select(dplyr::filter(t.s.data.stan, bc_conc < 10000), build_200m, com_750m, resid_100m, ind_1000m, open_50m, 
                            d_highway, inter_50m, Nox_50m, d_NPRI_PM, d_airport, rail_1000m
                              ), method = "spearman"))
#boom, now we're good

to.u10k.lin.variables
to.u10k.lin.var.ncor <- to.u10k.lin.variables[c(-6, -7, -9, -10, -12, -13, -14)]
to.u10k.nlin.var.ncor <- to.u10k.nlin.variables
length(to.u10k.lin.var.ncor)
paste(to.u10k.lin.var.ncor, collapse = ' + ')


#need to get it down to 8 variables or else I don't have enough computing power. 
#I did it supervised, taking out the highest p value and making sure the RMSE went down. It did for each. 
#all
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), 
           bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m + mjrd_100m + road_50m + d_highway + 
             d_majrd + bus_50m + inter_50m + traffic_100m + tot_traffic_100m + tot_Nox_100m + Nox_50m + d_NPRI_PM + d_airport +
             rail_1000m + I(d_NPRI_PM^2)))
#corr removed
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), 
           bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + inter_50m + Nox_50m + 
             d_NPRI_PM + I(d_NPRI_PM^2) + d_airport + rail_1000m))
#results of supervised step down. Each time I removed largest p value and made sure RMSE went down. It did and I stopped at this model:
summary(lm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), 
           bc_conc ~ com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + 
             d_NPRI_PM + d_airport))

#checked to see if exhaustive would get me to the same place. It does! 
summary(regsubsets(bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + inter_50m + Nox_50m + 
                     d_NPRI_PM + I(d_NPRI_PM^2) + d_airport + rail_1000m,
                   data = dplyr::filter(t.s.data.stan, bc_conc < 10000),
                   nbest = 1,       # 1 best model for each number of predictors
                   nvmax = 8,    # the number of non-correlated candidate variables 
                   force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                   method = "exhaustive",
                   really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                   ))
#now do an exhaustive search on all combinations of the remaining 8 variables
to.u10k.regsubsets.out.8vars.ex <- regsubsets(bc_conc ~ (com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + d_NPRI_PM + d_airport)^2 +
                     I(com_750m^2) + I(resid_100m^2) + I(ind_1000m^2) + I(d_highway^2) + I(inter_50m^2) + I(Nox_50m^2) + I(d_NPRI_PM^2) + I(d_airport^2),
                   data = dplyr::filter(t.s.data.stan, bc_conc < 10000),
                   nbest = 1,       # 1 best model for each number of predictors
                   nvmax = 8,    # the number of non-correlated candidate variables 
                   force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                   method = "exhaustive",
                   really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                   )
#take a look at the outputs
sum.to.u10k.regsubsets.out.8vars.ex <- summary(to.u10k.regsubsets.out.8vars.ex)
#see the values for each of the best models of their size
sum.to.u10k.regsubsets.out.8vars.ex$rsq
sum.to.u10k.regsubsets.out.8vars.ex$adjr2
sum.to.u10k.regsubsets.out.8vars.ex$bic
which.max(sum.to.u10k.regsubsets.out.8vars.ex$adjr2)
sum.to.u10k.regsubsets.out.8vars.ex$which[8,]
#put into data frame to make easier
to.u10k.8vars.ex <- as.data.frame(sum.to.u10k.regsubsets.out.8vars.ex$which[8,])
to.u10k.8vars.ex <- data.frame(Variables = rownames(to.u10k.8vars.ex), In = to.u10k.8vars.ex$`sum.to.u10k.regsubsets.out.8vars.ex$which[8, ]`)
t.s.u10k.final.model.vars <- dplyr::filter(to.u10k.8vars.ex, In == TRUE)$Variables
paste(t.s.u10k.final.model.vars[-1], collapse = ' + ')

#nifty plot we can do to see which variables get selected for each R2 outcome. It shows which variables get added for each size of model. 
plot(to.u10k.regsubsets.out.8vars.ex, scale = "adjr2", main = "Adjusted R^2")


#so the three models are:
#all candidates (after removing corr)
t.s.u10k.simple.all.ncor.model.lm <- lm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), 
           bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + inter_50m + Nox_50m + 
             d_NPRI_PM + d_airport + rail_1000m)
summary(t.s.u10k.simple.all.ncor.model.lm)

#down to 8 after backwars stepwise
t.s.u10k.simple.8vars.ncor.model.lm <- lm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), 
           bc_conc ~ com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + 
             d_NPRI_PM + d_airport)
summary(t.s.u10k.simple.8vars.ncor.model.lm)

#interactions and squared from ex.
t.s.u10k.final.model.lm <- lm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), 
           bc_conc ~ Nox_50m + com_750m:resid_100m + com_750m:ind_1000m + com_750m:d_airport + resid_100m:ind_1000m + 
             resid_100m:d_NPRI_PM + ind_1000m:d_NPRI_PM + Nox_50m:d_NPRI_PM)
summary(t.s.u10k.final.model.lm)


#also look at Cook's D
plot(cooks.distance(t.s.u10k.final.model.lm))
text(cooks.distance(t.s.u10k.final.model.lm), labels = as.character(filter(t.s.data.stan, bc_conc < 10000)[,1]), pos = 4)
#two points up around 0.6, so less beautifull than Montreal Annual, but still pretty good because under 1?
which.max(cooks.distance(t.s.u10k.final.model.lm))
#it's observations 23 and 26 that are th biggies. 
filter(t.s.data.stan, f.id == "TO_space_127")


summary(lm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000 & f.id != "TO_space_130" & f.id != "TO_space_130"), 
                              bc_conc ~ Nox_50m + com_750m:resid_100m + com_750m:ind_1000m + com_750m:d_airport + resid_100m:ind_1000m + 
                                resid_100m:d_NPRI_PM + ind_1000m:d_NPRI_PM + Nox_50m:d_NPRI_PM))
#remove both points and the biggest changes are: ind_1000m:d_NPRI_PM (371 to 310, 17%) and com_750m:resid_100m (-631 to -518, 18%),   
#remove 127 and the biggest are: resid_100m:ind_1000m (-525 to -651, 20%) and Nox_50m:d_NPRI_PM (488 to 543, 11%) and ind_1000m:d_NPRI_PM  (371 to 327, 12%)
#remove 130 and the biggest are:  com_750m:resid_100m (-631 to -486, 23%) and com_750m:ind_1000m  (-975 to -1111, 13%) and com_750m:d_airport (-450 to -377, 17%) (some others look like big changes but are less than 15%)
#so what? I think we're okay. 

#look basic model?
plot(cooks.distance(t.s.u10k.simple.all.ncor.model.lm))
text(cooks.distance(t.s.u10k.simple.all.ncor.model.lm), labels = as.character(filter(t.s.data.stan, bc_conc < 10000)[,1]), pos = 4)
#it's only TO_space_107 that is high, and it's less than 0.3
#I think this is stronger evidencve that I don't need to worry about it. I don't think we have any overly influencial observations 

#####TO Summer Cross Validation #######
#quick hist of residuals, qq plots, and resids vs fitted
#toronto simple multi models
hist(t.s.u10k.simple.all.ncor.model.lm$residuals, main = "11 var Toronto Model")
qqnorm(residuals(t.s.u10k.simple.all.ncor.model.lm), ylab = "Residuals", main = "Q-Q Plot 11 var Toronto Model")
qqline(residuals(t.s.u10k.simple.all.ncor.model.lm))
plot(fitted(t.s.u10k.simple.all.ncor.model.lm), residuals(t.s.u10k.simple.all.ncor.model.lm), main = "11 var Toronto Model")
abline(h=0)
hist(t.s.u10k.simple.8vars.ncor.model.lm$residuals, main = "8 var Toronto Model")
qqnorm(residuals(t.s.u10k.simple.8vars.ncor.model.lm), ylab = "Residuals", main = "Q-Q Plot 8 var Toronto Model")
qqline(residuals(t.s.u10k.simple.8vars.ncor.model.lm))
plot(fitted(t.s.u10k.simple.8vars.ncor.model.lm), residuals(t.s.u10k.simple.8vars.ncor.model.lm), main = "8 var Toronto Model")
abline(h=0)

#Toronto all interactions, final model
hist(t.s.u10k.final.model.lm$residuals, main = "8 var Toronto Model with Intx and ^2")
qqnorm(residuals(t.s.u10k.final.model.lm), ylab = "Residuals", main = "Q-Q Plot 8 var Toronto Model with Intx and ^2")
qqline(residuals(t.s.u10k.final.model.lm))
plot(fitted(t.s.u10k.final.model.lm), residuals(t.s.u10k.final.model.lm), main = "8 var Toronto Model with Intx and ^2")
abline(h=0)
#those four points, bad predictions. Other than that, the final model looks good. Better than the simple multis

t.s.u10k.train.control <- trainControl(method = "LOOCV")

t.s.u10k.simple.all.ncor.model.caret <- train(bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + inter_50m + Nox_50m + 
                                                d_NPRI_PM + d_airport + rail_1000m,   # model to fit
                                         data = dplyr::filter(t.s.data.stan, bc_conc < 10000),                       
                                         trControl = t.s.u10k.train.control,              # folds
                                         method = "glm"                      # specifying regression model
                                         )
t.s.u10k.simple.all.ncor.model.caret
#R2 drops 0.13


t.s.u10k.simple.8vars.ncor.model.caret <- train(bc_conc ~ com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + 
                                                  d_NPRI_PM + d_airport,   # model to fit
                                              data = dplyr::filter(t.s.data.stan, bc_conc < 10000),                       
                                              trControl = t.s.u10k.train.control,              # folds
                                              method = "glm"                      # specifying regression model
                                              )
t.s.u10k.simple.8vars.ncor.model.caret
#drops 0.08, suggests that the suppervised backwards steps helped reduce overfitting

t.s.u10k.final.model.caret <- train(bc_conc ~ Nox_50m + com_750m:resid_100m + com_750m:ind_1000m + com_750m:d_airport + resid_100m:ind_1000m + 
                                      resid_100m:d_NPRI_PM + ind_1000m:d_NPRI_PM + Nox_50m:d_NPRI_PM,   # model to fit
                                                data = dplyr::filter(t.s.data.stan, bc_conc < 10000),                       
                                                trControl = t.s.u10k.train.control,              # folds
                                                method = "glm"                      # specifying regression model
                                                )
t.s.u10k.final.model.caret
#drops 0.10




#tried a couple of other LOOCVs before I got caret working. Not sure if there is potential. There is that one nifty graph. Keep hidden for now.  
library(DAAG)
library(boot)
boot.cv <- glm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + 
       d_NPRI_PM + d_airport, family = gaussian)
cv.glm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), glmfit = boot.cv)
#I think the "delta" output is the MSE

cv.lm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + 
        d_NPRI_PM + d_airport)


#this one does a nice looking plot.....that I don't totally understand just yet. I think it's plotting the predicted vs observed for the 3 folds (ie: three subsets of data)
#changed it to 10 folds. It gives the MSE, so sqrt(ms) to get RMSE. 
CVlm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), form.lm = bc_conc ~ com_750m + resid_100m + ind_1000m + d_highway + inter_50m + Nox_50m + 
        d_NPRI_PM + d_airport, m = 10)

CVlm(data = dplyr::filter(t.s.data.stan, bc_conc < 10000), form.lm = bc_conc ~ Nox_50m + com_750m:resid_100m + com_750m:ind_1000m + com_750m:d_airport + resid_100m:ind_1000m + 
       resid_100m:d_NPRI_PM + ind_1000m:d_NPRI_PM + Nox_50m:d_NPRI_PM, m = 10)
?CVlm


##### a bunch more with leaps, but not useful for now. It was exploratory before deciding on a supervised backwards stepwise
#not looking at this any more

#too slow with all in, some a(with lowest uni R2 are removed). Check before running. Make sure really.big = F before runniung, then consider changing to T
to.u10k.regsubsets.out.9vars.ex <- regsubsets(bc_conc ~ (build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + 
                                                     inter_50m + Nox_50m + d_airport)^2 + 
                                          I(build_200m^2) + I(com_750m^2) + I(resid_100m^2) + I(ind_1000m^2) + I(open_50m^2) + 
                                          I(d_highway^2) + I(inter_50m^2) + I(Nox_50m^2) + I(d_airport^2),
                                     data = dplyr::filter(t.s.data.stan, bc_conc < 10000),
                                     nbest = 1,       # 1 best model for each number of predictors
                                     nvmax = (length(to.u10k.lin.var.ncor) - 2),    # the number of non-correlated candidate variables 
                                     force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                     method = "exhaustive",
                                     really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                                     )
#warns about linear dependencies, that warning goes away if I take out rail_1000m. Taking out rail still gives a slow warning. 
#Taking out rail_1000m + d_NPRI_PM removes the SLOW warning, but the squared terms aren't in there. It still takes about ~3 minutes.  
  #put the squared terms in there and ran it in spite of the slow warning. It took about ~30mins?
#Taking out rail_1000m + d_NPRI_PM + d_airport removes the SLOW warning.
sum.to.u10k.regsubsets.out.9vars.ex <- summary(to.u10k.regsubsets.out.9vars.ex)
sum.to.u10k.regsubsets.out.9vars.ex$rsq
sum.to.u10k.regsubsets.out.9vars.ex$adjr2
which.max(sum.to.u10k.regsubsets.out.9vars.ex$adjr2)
sum.to.u10k.regsubsets.out.9vars.ex$which[9,]
to.u10k.9vars.ex <- as.data.frame(sum.to.u10k.regsubsets.out.9vars.ex$which[9,])
to.u10k.9vars.ex <- data.frame(Variables = rownames(to.u10k.9vars.ex), In = to.u10k.9vars.ex$`sum.to.u10k.regsubsets.out.9vars.ex$which[9, ]`)
filter(to.u10k.9vars.ex, In == TRUE)

paste(filter(to.u10k.9vars.ex, In == TRUE)$Variables, collapse = ' + ')

#compare exhaustive to seqrep
to.u10k.regsubsets.out.9vars.seqrep <- regsubsets(bc_conc ~ (build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + 
                                                           inter_50m + Nox_50m + d_airport)^2 + 
                                                I(build_200m^2) + I(com_750m^2) + I(resid_100m^2) + I(ind_1000m^2) + I(open_50m^2) + 
                                                I(d_highway^2) + I(inter_50m^2) + I(Nox_50m^2) + I(d_airport^2),
                                              data = dplyr::filter(t.s.data.stan, bc_conc < 10000),
                                              nbest = 1,       # 1 best model for each number of predictors
                                              nvmax = (length(to.u10k.lin.var.ncor) - 2),    # the number of non-correlated candidate variables 
                                              force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                              method = "backward",
                                              really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
)
sum.to.u10k.regsubsets.out.9vars.seqrep <- summary(to.u10k.regsubsets.out.9vars.seqrep)
which.max(sum.to.u10k.regsubsets.out.9vars.seqrep$adjr2)
sum.to.u10k.regsubsets.out.9vars.seqrep$which[9,]
to.u10k.9vars.seqrep <- as.data.frame(sum.to.u10k.regsubsets.out.9vars.seqrep$which[9,])
to.u10k.9vars.seqrep <- data.frame(Variables = rownames(to.u10k.9vars.seqrep), In = to.u10k.9vars.seqrep$`sum.to.u10k.regsubsets.out.9vars.seqrep$which[9, ]`)
filter(to.u10k.9vars.seqrep, In == TRUE)
paste(filter(to.u10k.9vars.seqrep, In == TRUE)$Variables, collapse = ' + ')
#they are different. Also notice that there are many interactions that aren included as main effects. 
#his ain't good. 

to.u10k.regsubsets.out.11vars.seqrep <- regsubsets(bc_conc ~ (build_200m + com_750m + resid_100m + ind_1000m + open_50m + d_highway + 
                                                         inter_50m + Nox_50m + d_NPRI_PM + d_airport + rail_1000m)^2 + 
                                              I(build_200m^2) + I(com_750m^2) + I(resid_100m^2) + I(ind_1000m^2) + I(open_50m^2) + 
                                              I(d_highway^2) + I(inter_50m^2) + I(Nox_50m^2) + I(d_airport^2) + I(rail_1000m^2) + I(d_NPRI_PM^2),
                                        data = dplyr::filter(t.s.data.stan, bc_conc < 10000),
                                        nbest = 1,       # 1 best model for each number of predictors
                                        nvmax = (length(to.u10k.lin.var.ncor) - 0),    # the number of non-correlated candidate variables 
                                        force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                        method = "backward",
                                        really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                                        )
sum.to.u10k.regsubsets.out.11vars.seqrep <- summary(to.u10k.regsubsets.out.11vars.seqrep)
sum.to.u10k.regsubsets.out.11vars.seqrep$rsq
sum.to.u10k.regsubsets.out.11vars.seqrep$bic
sum.to.u10k.regsubsets.out.11vars.seqrep$adjr2
which.max(sum.to.u10k.regsubsets.out.11vars.seqrep$adjr2)
sum.to.u10k.regsubsets.out.11vars.seqrep$which[11,]
to.u10k.11vars.seqrep <- as.data.frame(sum.to.u10k.regsubsets.out.11vars.seqrep$which[11,])
to.u10k.11vars.seqrep <- data.frame(Variables = rownames(to.u10k.11vars.seqrep), In = to.u10k.11vars.seqrep$`sum.to.u10k.regsubsets.out.11vars.seqrep$which[11, ]`)
filter(to.u10k.11vars.seqrep, In == TRUE)
paste(filter(to.u10k.11vars.seqrep, In == TRUE)$Variables, collapse = ' + ')



#Mannually checking the ones that look bad, pull out rail_1000m and build_200m, gets the RMSE to go down for each one. 
summary(lm(data = dplyr::filter(mark.t.s.data.stan, bc_conc < 10000), bc_conc ~  com_750m + resid_100m + ind_1000m + 
              d_highway + inter_50m + Nox_50m + d_NPRI_PM + d_airport))

to.u10k.regsubsets.out.9vars.ma.ex <- regsubsets(bc_conc ~ (com_750m + resid_100m + ind_1000m + d_highway + inter_50m +
                                                                Nox_50m + d_NPRI_PM + d_airport)^2 + 
                                                     I(com_750m^2) + I(resid_100m^2) + I(ind_1000m^2) + 
                                                     I(d_highway^2) + I(inter_50m^2) + I(Nox_50m^2) + I(d_airport^2) + I(d_NPRI_PM^2),
                                                   data = dplyr::filter(t.s.data.stan, bc_conc < 10000),
                                                   nbest = 1,       # 1 best model for each number of predictors
                                                   nvmax = 9,    # the number of non-correlated candidate variables 
                                                   force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                                   method = "exhaustive",
                                                   really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                                                 )
sum.to.u10k.regsubsets.out.9vars.ma.ex <- summary(to.u10k.regsubsets.out.9vars.ma.ex)
sum.to.u10k.regsubsets.out.9vars.ma.ex$which[9,]

#######If we want to do algorithm. 

#simplest is the intercept only
t.s.u10k.lower.lm <- lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ 1)
#the largest most complex is all and all interactions
t.s.u10k.upper.lm.2int <- lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ (build_200m + com_750m + resid_100m + ind_1000m + open_50m + 
                                                                                         mjrd_100m + road_50m + d_highway + d_majrd + bus_50m + 
                                                                                         inter_50m + traffic_100m + tot_traffic_100m + tot_Nox_100m + 
                                                                                         Nox_50m + d_NPRI_PM + I(d_NPRI_PM^2) + d_airport + rail_1000m)^2)

#now do the step, the k = log(n) is to make it the BIC 
step(t.s.u10k.lower.lm, scope = list(lower = t.s.u10k.lower.lm, upper = t.s.u10k.upper.lm.2int), direction = "both", k = log(nrow(filter(t.s.data.stan, bc_conc < 10000))))
#cross check with the bic.blm function, THEY PICK THE SAME!!!
summary(bic.glm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ build_200m + com_750m + resid_100m + ind_1000m + open_50m + 
                  mjrd_100m + road_50m + d_highway + d_majrd + bus_50m + inter_50m + traffic_100m + tot_traffic_100m + 
                  Nox_50m + d_NPRI_PM + I(d_NPRI_PM^2) + d_airport + rail_1000m, glm.family = gaussian))
#BIC doesn't suggest interaction terms. 

#do plain old AIC
step(t.s.u10k.lower.lm, scope = list(lower = t.s.u10k.lower.lm, upper = t.s.u10k.upper.lm.2int), direction = "both", k = 2)
#this suggests several interaction terms. 
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ Nox_50m + ind_1000m + com_750m + resid_100m + d_majrd + open_50m + d_airport + road_50m + 
     ind_1000m:resid_100m + com_750m:resid_100m + ind_1000m:com_750m + ind_1000m:d_majrd))
#this is the AIC suggested model
#here's BIC sugggested:
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ Nox_50m + ind_1000m + com_750m))


######################## MTL Summer, the u4k is the best looking model so far. 
######Not looking any deeper into MTL Summer for now. It has a low R2 and Montreal Annual is probably more meaningful and interesting. And has a higher R2.


# to use step, it's a bit cleaner to separately define the lower and upper limits of model complexity
#simplest is the intercept only
m.s.u4k.lower.lm <- lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ 1)
#the largest most complex is all and all interactions
m.s.u4k.upper.lm.2int <- lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ (build_1000m + mjrd_750m + d_majrd + bus_50m + bus_stop_750m + 
                                                                                       traffic_750m + tot_traffic_750m + Nox_750m + tot_Nox_750m + 
                                                                                       d_NPRI_Nox + d_airport)^2)
#should look for cor(), but not looking at m.s for now. It's not very promissing and m.a is arguably a more informative data set. 
#now do the step, the k = log(n) is to make it the BIC 
step(m.s.u4k.lower.lm, scope = list(lower = m.s.u4k.lower.lm, upper = m.s.u4k.upper.lm.2int), direction = "both", k = log(nrow(filter(m.s.data.stan, bc_conc < 4000))))
#cross check with the bic.blm function, not sure why the above model is actaully the #2 model below. I tried increasing the steps but it didn't work. 
summary(bic.glm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ build_1000m + mjrd_750m + d_majrd + bus_50m + bus_stop_750m + traffic_750m + 
                  tot_traffic_750m + Nox_750m + tot_Nox_750m + d_NPRI_Nox + d_airport, glm.family = gaussian))
#do plain old AIC
step(m.s.u4k.lower.lm, scope = list(lower = m.s.u4k.lower.lm, upper = m.s.u4k.upper.lm.2int), direction = "both", k = 2)
#this suggests some interaction terms
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ mjrd_750m + bus_50m + bus_stop_750m + traffic_750m + tot_Nox_750m + tot_traffic_750m + 
             d_NPRI_Nox + mjrd_750m:d_NPRI_Nox + d_NPRI_Nox:tot_Nox_750m))

#here's the BIC suggested:
summary(lm(data = filter(m.s.data.stan, bc_conc < 4000), bc_conc ~ bus_50m + tot_Nox_750m + d_NPRI_Nox))


####################### MTL Winter
######Not looking any deeper into MTL Winter for now. It has a low R2 and Montreal Annual is probably more meaningful and interesting.

m.w.lin.variables
paste(m.w.lin.variables, collapse = ' + ')
m.w.nlin.variables
#simplest is the intercept only
m.w.lower.lm <- lm(data = filter(m.w.data.stan, !is.na(bc_conc)), bc_conc ~ 1)
#the largest most complex is all and all interactions
m.w.upper.lm.2int <- lm(data = filter(m.s.data.stan,!is.na(bc_conc)), bc_conc ~ (build_1000m + water_1000m + open_300m + ind_500m + bus_1000m + 
                                                                                   traffic_1000m + tot_Nox_750m + Nox_1000m + d_woodburn +
                                                                                   I(d_airport^2) + I(d_NPRI_Nox^2))^2)
#now do the step, the k = log(n) is to make it the BIC 
step(m.w.lower.lm, scope = list(lower = m.w.lower.lm, upper = m.w.upper.lm.2int), direction = "both", k = log(nrow(filter(m.w.data.stan, !is.na(bc_conc)))))
#We get warnings because woodburn has a bunch of NAs. 

#cross check with the bic.blm function, not sure why the above model is actaully the #3 model below or #2 when I remove d_woodburn from above. I tried increasing the steps but it didn't work. 
summary(bic.glm(data = filter(m.w.data.stan, !is.na(bc_conc)), bc_conc ~ build_1000m + water_1000m + open_300m + ind_500m + bus_1000m + traffic_1000m + tot_Nox_750m + 
                  Nox_1000m + I(d_airport^2) + I(d_NPRI_Nox^2), glm.family = gaussian))
#this only works if I take woodburn out
#do plain old AIC
step(m.w.lower.lm, scope = list(lower = m.w.lower.lm, upper = m.w.upper.lm.2int), direction = "both", k = 2)
#not even AIC suggests interaction terms. 

#Here's the BIC suggested model:
summary(lm(data = dplyr::filter(m.w.data.stan, !is.na(bc_conc)), bc_conc ~ build_1000m + open_300m + + I(d_airport^2)))


###########TO SUMMER log  100 < BC < 10,000
########Not looking at this for now. Happy with the residuals of the non-log transformed model. 
to.log.o100u10k.lin.variables
paste(to.log.o100u10k.lin.variables, collapse = ' + ')
to.log.o100u10k.nlin.variables

#simplest is the intercept only
t.s.log.o100u10k.lower.lm <- lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), logbc ~ 1)
#the largest most complex is all and all interactions
t.s.log.o100u10k.upper.lm.2int <- lm(data = filter(log.t.s.data.stan, bc_conc < 10000 & bc_conc > 100), 
                                     bc_conc ~ (build_200m + com_750m + resid_100m + ind_750m + open_50m + mjrd_100m + road_50m + d_highway + 
                                                  d_majrd + bus_50m + traffic_100m + tot_traffic_100m + Nox_50m + tot_Nox_100m + rail_1000m)^2)
#now do the step, the k = log(n) is to make it the BIC 
step(t.s.log.o100u10k.lower.lm, scope = list(lower = t.s.log.o100u10k.lower.lm, upper = t.s.log.o100u10k.upper.lm.2int), direction = "both", k = log(nrow(filter(log.t.s.data.stan, bc_conc > 100 & bc_conc < 10000))))
#cross check with the bic.blm function, the model from above is listed as teh #2 in bic.glm. Not sure why. 
summary(bic.glm(data = filter(log.t.s.data.stan, bc_conc > 100 & bc_conc < 10000), logbc ~ build_200m + com_750m + resid_100m + ind_750m + open_50m + 
                  mjrd_100m + road_50m + d_highway + d_majrd + bus_50m + traffic_100m + tot_traffic_100m + Nox_50m + 
                  rail_1000m, glm.family = gaussian))
#BIC doesn't suggest interaction terms. tot_Nox removed due to colinearity

#do plain old AIC
step(t.s.log.o100u10k.lower.lm, scope = list(lower = t.s.log.o100u10k.lower.lm, upper = t.s.log.o100u10k.upper.lm.2int), direction = "both", k = 2)
#this suggests an interaction terms. 
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ Nox_50m + ind_750m + com_750m + d_majrd + Nox_50m:d_majrd))
#this is the AIC suggested model. Gives an R2 of 0.6419, not baaaaad. 
#here's BIC suggested:
summary(lm(data = filter(t.s.data.stan, bc_conc < 10000), bc_conc ~ Nox_50m + ind_750m + mjrd_100m + road_50m))



################## MTL + TO SUMMER POOLED, Residuals are good enough for plain old linear u10k. 
#####Not looking at this any more for now.
mts.pool.u10k.lin.variables
paste(mts.pool.u10k.lin.variables, collapse = ' + ')
mts.pool.u10k.nlin.variables

#check for correlations
formattable(cor(dplyr::select(filter(mts.data.pool.stan, bc_conc < 10000), build_200m, com_750m, resid_750m, ind_500m, open_50m, mjrd_300m, road_50m, 
                              d_majrd, bus_50m, inter_50m, traffic_50m, tot_Nox_100m, d_NPRI_PM, d_airport, d_port, d_shore, elevation
                              ), method = "spearman")
            )

str(mts.data.pool.stan[,150:161])
#d_majrd (0.08) and majrd (0.14), keep majrd and kickk out d_majrd
#tot_Nox (0.1344) and traffic (0.0367), keep tot_Nox, kick out traffic
mts.pool.u10k.lin.var.ncor <- mts.pool.u10k.lin.variables[c(-8, -11)]
mts.pool.u10k.nlin.var.ncor <- mts.pool.u10k.nlin.variables
length(mts.pool.u10k.lin.var.ncor)


#need to get it down to 8 variables or else I don't have enough computing power. 
#I did it supervised, taking out the highest p value and making sure the RMSE went down. It did for each. 
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), 
           bc_conc ~ com_750m + resid_750m + road_50m + d_majrd + inter_50m + 
             tot_Nox_100m + d_airport + d_shore + elevation))
#checked to see if exhaustive would get me to the same place. It does! Note, force in city and make nvmax 9, so city + 8 variables
#thought? should I let city be an interaction term?
#redid this with elevation in. That messes it up a bit since there are quite a few NAs

summary(regsubsets(bc_conc ~ city + build_200m + com_750m + resid_750m + ind_500m + open_50m + mjrd_300m + road_50m + bus_50m + inter_50m + 
             tot_Nox_100m + d_NPRI_PM + d_airport + d_port + d_shore + elevation,
           data = dplyr::filter(mts.data.pool.stan, bc_conc < 10000),
           nbest = 1,       # 1 best model for each number of predictors
           nvmax = 9,    # the number of non-correlated candidate variables 
           force.in = 1, force.out = NULL,      #might consider forcing in a Nox. Force in City for pooled
           method = "exhaustive",
           really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
           ))
#if city is forced in, elevation doesn't make the cut. If city is taken out, then elevation makes it in. 


###Here's from when I was first trying the regsubsets. This hits a wall when using more than 9 variables, too computationally intensive. 
#it's too slow to run exhaustive of 
mts.pool.u10k.regsubsets.out.9vars.ex <- regsubsets(bc_conc ~ (ind_500m + mjrd_300m + road_50m + bus_50m + 
                                              inter_50m + tot_Nox_100m + d_airport + d_port + d_shore)^2 + 
                                                I(ind_500m^2) + I(mjrd_300m^2) + I(road_50m^2) + I(bus_50m^2) + 
                                                 I(inter_50m^2) + I(tot_Nox_100m^2) + I(d_airport^2) + I(d_port^2) + I(d_shore^2),
                                     data = dplyr::filter(mts.data.pool.stan, bc_conc < 10000),
                                     nbest = 1,       # 1 best model for each number of predictors
                                     nvmax = (length(mts.pool.u10k.lin.var.ncor) - 5),    # the number of non-correlated candidate variables 
                                     force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                     method = "exhaustive",
                                     really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                                     )
#it warns about being slow, try taking out some 1 by 1 to see. Take out all those with R2 below 0.05: build_200m, com_750m, d_NPRI_PM, open_50m, resid_750m, 
  #could run with those 5 removed but without the nonlinear terms. Reran without d_port and d_port^2 and there was no SLOW warning, 
  #ran with the SLOW warning for d_port and all the non-lin terms in. Took ~5 minutes
#still sort of wasting my time with this because it selects a bunch of interactions without their main effects, right? 

mts.pool.u10k.regsubsets.out.9vars.ex
sum.mts.pool.u10k.regsubsets.out.9vars.ex <- summary(mts.pool.u10k.regsubsets.out.9vars.ex)
sum.mts.pool.u10k.regsubsets.out.9vars.ex$rsq
sum.mts.pool.u10k.regsubsets.out.9vars.ex$bic
sum.mts.pool.u10k.regsubsets.out.9vars.ex$adjr2
which.max(sum.mts.pool.u10k.regsubsets.out.9vars.ex$adjr2)
sum.to.u10k.regsubsets.out.9vars.ex$which[9,]
mts.pool.u10k.9vars.ex <- as.data.frame(sum.to.u10k.regsubsets.out.9vars.ex$which[9,])
mts.pool.u10k.9vars.ex <- data.frame(Variables = rownames(mts.pool.u10k.9vars.ex), In = mts.pool.u10k.9vars.ex$`sum.to.u10k.regsubsets.out.9vars.ex$which[9, ]`)
filter(mts.pool.u10k.9vars.ex, In == TRUE)

plot(mts.pool.u10k.regsubsets.out.9vars.ex, scale = "adjr2", main = "Adjusted R^2")

#if we want algorithms n steps instead of exhaustive
?regsubsets
mts.pool.u10k.regsubsets.out.9vars.seqrep <- regsubsets(bc_conc ~ (ind_500m + mjrd_300m + road_50m + bus_50m + 
                                                                 inter_50m + tot_Nox_100m + d_airport + d_port + d_shore)^2 + 
                                                      I(ind_500m^2) + I(mjrd_300m^2) + I(road_50m^2) + I(bus_50m^2) + 
                                                      I(inter_50m^2) + I(tot_Nox_100m^2) + I(d_airport^2) + I(d_port^2) + I(d_shore^2),
                                                    data = dplyr::filter(mts.data.pool.stan, bc_conc < 10000),
                                                    nbest = 1,       # 1 best model for each number of predictors
                                                    nvmax = (length(mts.pool.u10k.lin.var.ncor) - 5),    # the number of non-correlated candidate variables 
                                                    force.in = NULL, force.out = NULL,      #might consider forcing in a Nox
                                                    method = "backward",
                                                    really.big = F         #this needs to be set to T for a large one such as this. Not sure how long it will take.
                                                    )
sum.mts.pool.u10k.regsubsets.out.9vars.seqrep <- summary(mts.pool.u10k.regsubsets.out.9vars.seqrep)
sum.mts.pool.u10k.regsubsets.out.9vars.seqrep$adjr2
which.max(sum.mts.pool.u10k.regsubsets.out.9vars.seqrep$adjr2)
sum.mts.pool.u10k.regsubsets.out.9vars.seqrep$which[9,]
mts.pool.u10k.9vars.ex <- as.data.frame(sum.to.u10k.regsubsets.out.9vars.ex$which[9,])
mts.pool.u10k.9vars.ex <- data.frame(Variables = rownames(mts.pool.u10k.9vars.ex), In = mts.pool.u10k.9vars.ex$`sum.to.u10k.regsubsets.out.9vars.ex$which[9, ]`)
filter(mts.pool.u10k.9vars.ex, In == TRUE)



#####If we want to use steps or algorithms. 

#simplest is the intercept only
mts.pool.u10k.lower.lm <- lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ 1)
#the largest most complex is all and all interactions
mts.pool.u10k.upper.lm.2int <- lm(data = filter(mts.data.pool.stan, bc_conc < 10000), 
                                        bc_conc ~ (build_200m + com_750m + resid_750m + ind_500m + open_50m + mjrd_300m + road_50m + bus_50m + 
                                                     inter_50m + tot_Nox_100m + d_NPRI_PM + d_airport + d_port + d_shore)^2)
#now do the step, the k = log(n) is to make it the BIC 
step(mts.pool.u10k.lower.lm, scope = list(lower = mts.pool.u10k.lower.lm, upper = mts.pool.u10k.upper.lm.2int), direction = "both", k = log(nrow(filter(mts.data.pool.stan, bc_conc < 10000))))
#cross check with the bic.blm function, they pick the same function. 
summary(bic.glm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ build_200m + com_750m + resid_750m + ind_500m + open_50m + 
                  mjrd_300m + road_50m + d_majrd + bus_50m + inter_50m + traffic_50m + tot_Nox_100m + d_NPRI_PM + d_airport + d_port + d_shore, 
                glm.family = gaussian))
#BIC doesn't suggest interaction terms. 

#do plain old AIC
step(mts.pool.u10k.lower.lm, scope = list(lower = mts.pool.u10k.lower.lm, upper = mts.pool.u10k.upper.lm.2int), direction = "both", k = 2)
#doesn't suggest any interaction terms, but does keep in more than BIC does.  
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ road_50m + mjrd_300m + d_airport + tot_Nox_100m + inter_50m + road_50m:d_airport + road_50m:inter_50m))
#the AIC suggests a model that gives R2 = 0.3271
summary(lm(data = filter(mts.data.pool.stan, bc_conc < 10000), bc_conc ~ city + road_50m + mjrd_300m + d_airport + tot_Nox_100m + inter_50m + road_50m:d_airport + road_50m:inter_50m))
#adding city does very little



################## MTL + TO SUMMER POOLED, the log 0 < BC < 13k is the best looking one. 
#####Not looking at this anymore for now. 

mts.pool.log.o0u13k.lin.variables
paste(mts.pool.log.o0u13k.lin.variables, collapse = ' + ')
mts.pool.log.o0u13k.nlin.variables



#check for correlations
formattable(cor(dplyr::select(filter(log.mts.data.pool.stan,  bc_conc < 13000 & bc_conc >0), build_200m, com_500m, resid_750m, ind_1000m, open_50m, 
                              mjrd_300m, road_50m, d_majrd, bus_50m, inter_50m, traffic_50m, tot_Nox_100m, d_NPRI_PM, d_railline, d_airport, 
                              d_port, d_shore
                              ))
            )
#nearly same data as non-logged, but there are different correlation scores, no longer have the d_majrd road cor we had before. But we do get d_port and d_shore
#d_port (0.03) and d_shore (0.08), kick out d_shore
#tot_Nox (0.13) and traffic (0.03), keep tot_Nox, kick out traffic
mts.pool.log.o0u13k.lin.var.ncor <- mts.pool.log.o0u13k.lin.variables[c(-11, -17)]
mts.pool.log.o0u13k.nlin.var.ncor <- mts.pool.log.o0u13k.nlin.variables
length(mts.pool.log.o0u13k.lin.var.ncor)





#####if we wwant to use steps or other algorithms instead of exhaustive

#simplest is the intercept only
mts.pool.log.o0u13k.lower.lm <- lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc >0), logbc ~ 1)
#the largest most complex is all and all interactions
mts.pool.log.o0u13k.upper.lm.2int <- lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc >0), 
                                    logbc ~ (build_200m + com_500m + resid_750m + ind_1000m + open_50m + mjrd_300m + road_50m + d_majrd + I(d_majrd^2) + 
                                               bus_50m + inter_50m + traffic_50m + tot_Nox_100m + d_NPRI_PM + I(d_NPRI_PM^2) + d_railline + d_airport + 
                                               d_port + d_shore)^2)
#now do the step, the k = log(n) is to make it the BIC 
step(mts.pool.log.o0u13k.lower.lm, scope = list(lower = mts.pool.log.o0u13k.lower.lm, upper = mts.pool.log.o0u13k.upper.lm.2int), direction = "both", k = log(nrow(filter(log.mts.data.pool.stan, bc_conc > 0 & bc_conc < 13000))))
#cross check with the bic.blm function, They don't pick the same. Maybe when the R2 is so low, they have trouble picking between equally bad models. 
summary(bic.glm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc > 0), logbc ~ build_200m + com_500m + resid_750m + ind_1000m + open_50m + mjrd_300m + 
                  road_50m + d_majrd + I(d_majrd^2) + bus_50m + inter_50m + traffic_50m + tot_Nox_100m + d_NPRI_PM + I(d_NPRI_PM^2) + d_railline + 
                  d_airport + d_port + d_shore, glm.family = gaussian))
#note tot_Nox_100m removed for colinearity
#BIC doesn't suggest interaction terms. 

#do plain old AIC
step(mts.pool.log.o0u13k.lower.lm, scope = list(lower = mts.pool.log.o0u13k.lower.lm, upper = mts.pool.log.o0u13k.upper.lm.2int), direction = "both", k = 2)
#doesn't suggest any interaction terms, but does keep in more than BIC does.  
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc >0), logbc ~ com_500m + ind_1000m + d_majrd + I(d_majrd^2) + 
              inter_50m + tot_Nox_100m + d_airport + d_railline + d_shore))
#the AIC suggests a model that gives R2 = 0.3752
#the BIC suggested is:
summary(lm(data = filter(log.mts.data.pool.stan, bc_conc < 13000 & bc_conc >0), logbc ~ ind_1000m + mjrd_300m + d_railline + d_shore))

#check for correlations
formattable(cor(dplyr::select(filter(log.mts.data.pool.stan,  bc_conc < 13000 & bc_conc >0), build_200m, com_500m, resid_750m, ind_1000m, open_50m, 
                              mjrd_300m, road_50m, d_majrd, bus_50m, inter_50m, traffic_50m, tot_Nox_100m, d_NPRI_PM, d_railline, d_airport, 
                              d_port, d_shore
                              ))
            )






### Final Model Tables ####
library(stargazer)
stargazer(m.a.u4k.simple.ncor.model.lm, m.a.u4k.final.model.lm, t.s.u10k.simple.all.ncor.model.lm, t.s.u10k.simple.8vars.ncor.model.lm, t.s.u10k.final.model.lm, ci = TRUE, ci.level = 0.95, type =  "text")
?stargazer

library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(m.a.u4k.final.model.lm)

#put the two MTL models in same table. I like having the non-interaction model because it's easier to interpret. It also shows how interactions improve the model and the LOOCV performance
#This is the same table format as the MTL UFP paper article
m.a.2.model.sum <- m.a.u4k.simple.ncor.model.lm %>%
  tidy() %>%
  transmute(.rownames = term, estimate = estimate) %>%
  left_join(tidy(confint(m.a.u4k.simple.ncor.model.lm, level = 0.95)), by = ".rownames") %>%
  transmute("Independent variables" = .rownames, "Effect estimate (95% CI)" = paste(round(estimate, 2), " (", round(X2.5.., 2), ", ", round(X97.5.., 2), ")", sep = "")) %>%
  bind_rows(., m.a.u4k.final.model.lm %>%
              tidy() %>%
              transmute(.rownames = term, estimate = estimate) %>%
              left_join(tidy(confint(m.a.u4k.final.model.lm, level = 0.95)), by = ".rownames") %>%
              transmute("Independent variables" = .rownames, "Effect estimate (95% CI)" = paste(round(estimate, 2), " (", round(X2.5.., 2), ", ", round(X97.5.., 2), ")", sep = ""))
            ) %>%
  transmute("Independent variables" = `Independent variables` %>% gsub("^NPRI_", "NPRI.", .) %>%
            gsub("tot_", "total ", .) %>%
            gsub("d_", "dist. to ", .) %>%
            gsub("^I", "", .) %>%
            gsub("2)", "2", .) %>%
            gsub("nox", "NOx", .) %>%
            gsub("Nox", "NOx", .), 
          "Effect estimate (95% CI)" = `Effect estimate (95% CI)`)

write.csv(m.a.2.model.sum, "Paper Tables/m.a.2.model.sum.csv")

#here's extra stuff about models to put into tables. Doesn't fit very nice into data.frame.
#no interactions
c(glance(m.a.u4k.simple.ncor.model.lm)[1:2], RMSE = sqrt(glance(m.a.u4k.simple.ncor.model.lm)[10]/nrow(filter(m.a.data.stan, bc_conc < 4000))), MaxCookD = max(cooks.distance(m.a.u4k.simple.ncor.model.lm)))
m.a.u4k.simple.ncor.model.caret
#final with interactions
c(glance(m.a.u4k.final.model.lm)[1:2], RMSE = sqrt(glance(m.a.u4k.final.model.lm)[10]/nrow(filter(m.a.data.stan, bc_conc < 4000))), MaxCookD = max(cooks.distance(m.a.u4k.final.model.lm)))
m.a.u4k.final.model.caret
#just to confirm the by hand is correct. 
RMSE(filter(m.a.data.stan, bc_conc < 4000)$bc_conc, predict(m.a.u4k.final.model.lm))
#it is. 


#for tononto. For now, this is only the two 8 variable models. I don't think I should bother putting in the 11 variable one. 
t.s.u10k.final.model.lm
t.s.u10k.simple.8vars.ncor.model.lm
t.s.2.model.sum <- t.s.u10k.simple.8vars.ncor.model.lm %>%
  tidy() %>%
  transmute(.rownames = term, estimate = estimate) %>%
  left_join(tidy(confint(t.s.u10k.simple.8vars.ncor.model.lm, level = 0.95)), by = ".rownames") %>%
  transmute("Independent variables" = .rownames, "Effect estimate (95% CI)" = paste(round(estimate, 2), " (", round(X2.5.., 2), ", ", round(X97.5.., 2), ")", sep = "")) %>%
  bind_rows(., t.s.u10k.final.model.lm %>%
              tidy() %>%
              transmute(.rownames = term, estimate = estimate) %>%
              left_join(tidy(confint(t.s.u10k.final.model.lm, level = 0.95)), by = ".rownames") %>%
              transmute("Independent variables" = .rownames, "Effect estimate (95% CI)" = paste(round(estimate, 2), " (", round(X2.5.., 2), ", ", round(X97.5.., 2), ")", sep = ""))
  ) %>%
  transmute("Independent variables" = `Independent variables` %>% gsub("^NPRI_", "NPRI.", .) %>%
              gsub("tot_", "total ", .) %>%
              gsub("^I", "", .) %>%
              gsub("2)", "2", .) %>%
              gsub("nox", "NOx", .) %>%
              gsub("Nox", "NOx", .), 
            "Effect estimate (95% CI)" = `Effect estimate (95% CI)`)


write.csv(t.s.2.model.sum, "Paper Tables/t.s.2.model.sum.csv")

#here's extra stuff about models to put into tables. Doesn't fit very nice into data.frame.
#no interactions
c(glance(t.s.u10k.simple.8vars.ncor.model.lm)[1:2], RMSE = sqrt(glance(t.s.u10k.simple.8vars.ncor.model.lm)[10]/nrow(filter(t.s.data.stan, bc_conc < 4000))), MaxCookD = max(cooks.distance(t.s.u10k.simple.8vars.ncor.model.lm)))
t.s.u10k.simple.8vars.ncor.model.caret
#final with interactions
c(glance(t.s.u10k.final.model.lm)[1:2], RMSE = sqrt(glance(t.s.u10k.final.model.lm)[10]/nrow(filter(t.s.data.stan, bc_conc < 4000))), MaxCookD = max(cooks.distance(t.s.u10k.final.model.lm)))
t.s.u10k.final.model.caret
#just to confirm the by hand is correct. 

#then for each add R2, Adj R2, LOOCV R2, RMSE, LOOCV RMSE?, BIC?, highest Cooks D, Moran's I
#mutate(RMSE = sqrt(deviance/sum(!is.na(m.a.data.stan$bc_conc))))




#####City Map Plots + Moran's I#####

##MTL Summer
#example code was written with lm predicted values added to the original data fram that included the lats and long. 
#I don't want to do that because of the way the uni regressions run (all at once on all the data in the data frame)
#create a new data from from the clean data, left join the lats and longs from the elevation code, and then smash in the predictions

map.m.a.data.stan <- dplyr::filter(m.a.data.stan, bc_conc < 4000 & !is.na(bc_conc))
colnames(map.m.a.data.stan)[1] <- "Filter_ID"    #needed to change column name to match for the full join. 
#lat and long are already in, no need to join. 
#map.m.a.data.stan <- dplyr::left_join(map.m.a.data.stan, dplyr::select(mtl.to.latlong.elevation, Filter_ID, latitude.x, longitude.x), by = "Filter_ID")

describe(m.a.data.stan$bc_conc)
describe(map.m.a.data.stan$bc_conc)
predict(m.a.u4k.final.model.lm)
map.m.a.data.stan[, c(1,2,(NCOL(map.m.a.data.stan)))]
#had to filter out the bc > 4k and is.na prior to putting in the data frame. I guess that's okay. Those are the only ones that I can make predictions for. I'll see later if this is an issue

map.m.a.data.stan$f.m.pred <- predict(m.a.u4k.final.model.lm)      #put final model predictions in there. Could have intermediary models in there too if I want
map.m.a.data.stan$Filter_ID <- as.factor(map.m.a.data.stan$Filter_ID)
str(map.m.a.data.stan)
head(map.m.a.data.stan)

# Make a map of the city using a shapefile downloaded from the Montreal open data portal
# This is code from Susannah
#mtl.tmp <- tempfile()
#download.file("http://donnees.ville.montreal.qc.ca/dataset/00bd85eb-23aa-4669-8f1b-ba9a000e3dd8/resource/62f7ce10-36ce-4bbd-b419-8f0a10d3b280/download/limadmin-shp.zip", destfile = mtl.tmp)
#unzip(mtl.tmp, exdir = ".")
#I think this is read in as sp format. Note that LIMADMIN is the name of the file in the project folder. It is the name of the file that was downloaded and unziped. 
#mtl.spdf <- readOGR(".", "LIMADMIN")
#below converts from sp to sf
#mtl.shp <- st_as_sf(mtl.spdf)
#can also just read directly as an sf object

#after watching some datacamp, I'll do it like this:
#oh, and save the map data to a subfolder to keep it neat
mtl.tmp.try <- tempfile()
download.file("http://donnees.ville.montreal.qc.ca/dataset/00bd85eb-23aa-4669-8f1b-ba9a000e3dd8/resource/62f7ce10-36ce-4bbd-b419-8f0a10d3b280/download/limadmin-shp.zip", destfile = mtl.tmp.try)
unzip(mtl.tmp.try, exdir = "Map Data/")
mtl.sf <- st_read("Map Data/LIMADMIN.shp")
#note that these are unprojected. If I want to do calculations, I should use a projected 

# In this case my outcome is bc_conc
m.a.bc.obs <- ggplot() + 
  geom_sf(data = mtl.sf) +
  geom_point(data = map.m.a.data.stan, aes(x = longitude.x, y = latitude.x, color = bc_conc, size=10)) +
  scale_color_gradient(low = "blue", high = "red", name = "Observed BC (ng/m3)") + 
  theme_minimal() +
  guides(color = guide_legend(order=1, rev=T),
         size = F)

# Plot of predicted ROS values
m.a.bc.pred <- ggplot() + 
  geom_sf(data = mtl.sf) +
  geom_point(data = map.m.a.data.stan, aes(x = longitude.x, y = latitude.x, color = f.m.pred, size=10)) +
  scale_color_gradient(low="blue", high="red", name = "Predicted BC (ng/m3)") + 
  theme_minimal() +
  guides(color = guide_legend(order=1, rev=T),
         size = F)
#I get an intermitent error about not finding an edge. It might be due to a font being disabled, but I can't get that solution to work.
#Another solution is just to force the graphics window to open first using quartz. Good enough for now.
#quartz makes it harder to save the file. I may have solved this by restoring standard fonts. 
quartz()
grid.arrange(m.a.bc.obs, m.a.bc.pred) # Arrange the 2 plots on the same field

# Map the difference between predicted and observed
map.m.a.data.stan$f.m.diffs <- map.m.a.data.stan$bc_conc - map.m.a.data.stan$f.m.pred
m.a.bc.diffs <- ggplot() + 
  geom_sf(data = mtl.sf) +
  geom_point(data = map.m.a.data.stan, aes(x = longitude.x, y = latitude.x, color = bc_conc, size = abs(f.m.diffs^2))) +
  scale_color_gradient(low="blue", high="red", name = "Obs BC") + 
  theme_minimal() +
  guides(color = guide_legend(order=1, rev=T),
         size = F)
m.a.bc.diffs
#make it so the color tells you BC concentration that was measured there and size of circle tells you how much the prediction was off. Look for a pattern




map.t.s.data.stan <- dplyr::filter(t.s.data.stan, bc_conc < 10000 & !is.na(bc_conc))
colnames(map.t.s.data.stan)[1] <- "Filter_ID"    #needed to change column name to match for the full join. 
#lat and long are already in, no need to join. 
#map.m.a.data.stan <- dplyr::left_join(map.m.a.data.stan, dplyr::select(mtl.to.latlong.elevation, Filter_ID, latitude.x, longitude.x), by = "Filter_ID")

describe(t.s.data.stan$bc_conc)
describe(map.t.s.data.stan$bc_conc)
predict(t.s.u10k.final.model.lm)
#had to filter out the bc > 10k and is.na prior to putting in the data frame. I guess that's okay. Those are the only ones that I can make predictions for. I'll see later if this is an issue

map.t.s.data.stan$f.m.pred <- predict(t.s.u10k.final.model.lm)      #put final model predictions in there. Could have intermediary models in there too if I want
map.t.s.data.stan$Filter_ID <- as.factor(map.t.s.data.stan$Filter_ID)
str(map.t.s.data.stan)
head(map.t.s.data.stan)

to.tmp.try <- tempfile()
download.file("http://opendata.toronto.ca/gcc/community_planning_boundary_wgs84.zip", destfile = to.tmp.try)
unzip(to.tmp.try, exdir = "Map Data/")
to.sf <- st_read("Map Data/COMMUNITY_PLANNING_BNDRY_WGS84.shp")


ggplot() + geom_point(data = map.t.s.data.stan, aes(x = longitude.x, y = latitude.x, color = bc_conc, size=10))
#they can plot separate, but when together the data lat goes to zero and data long goes a bit off....

st_crs(mtl.shp)
st_crs(to.shp)
extent(to.shp)
extent(mtl.shp)
#so the TO shape file is on some other coordinate system or smoe other scalling. 
str(map.t.s.data.stan)
t.s.bc.obs <- ggplot() + 
  geom_sf(data = to.sf) +
  geom_point(data = map.t.s.data.stan, aes(x = longitude.x, y = latitude.x, color = bc_conc, size=10)) +
  scale_color_gradient(low = "blue", high = "red", name = "Observed BC (ng/m3)") + 
  theme_minimal() +
  guides(color = guide_legend(order=1, rev=T),
         size = F)

# Plot of predicted ROS values
t.s.bc.pred <- ggplot() + 
  geom_sf(data = to.sf) +
  geom_point(data = map.t.s.data.stan, aes(x = longitude.x, y = latitude.x, color = f.m.pred, size=10)) +
  scale_color_gradient(low="blue", high="red", name = "Predicted BC (ng/m3)") + 
  theme_minimal() +
  guides(color = guide_legend(order=1, rev=T),
         size = F)

# Map the difference between predicted and observed
map.t.s.data.stan$f.m.diffs <- map.t.s.data.stan$bc_conc - map.t.s.data.stan$f.m.pred
t.s.bc.diffs <- ggplot() + 
  geom_sf(data = to.sf) +
  geom_point(data = map.t.s.data.stan, aes(x = longitude.x, y = latitude.x, color = bc_conc, size = abs(f.m.diffs^2))) +
  scale_color_gradient(low="blue", high="red", name = "Obs BC") + 
  theme_minimal() +
  guides(color = guide_legend(order=1, rev=T),
         size = F)
t.s.bc.diffs
#make it so the color tells you BC concentration that was measured there and size of circle tells you how much the prediction was off. Look for a pattern

library(spdep)
library(lctools)
library(MASS)
library(geosphere)
library(ape)

1/as.matrix(distm(cbind(map.t.s.data.stan$longitude.x, map.t.s.data.stan$latitude.x), fun=distVincentyEllipsoid))

#Moran's I
#Montreal

# https://cran.r-project.org/web/packages/ape/vignettes/MoranI.pdf
?dist
#ape was made for evolution. I probably shouldn't use it. It is for a different application/ 
#create a matrix of the inverse distances between each point. distm is from geosphere package.
m.a.dist.inv <- 1/as.matrix(distm(cbind(map.m.a.data.stan$longitude.x, map.m.a.data.stan$latitude.x), fun=distVincentyEllipsoid))
#inverse of 0 is inf, so get back to 0
diag(m.a.dist.inv) <- 0
#we are using inverse distance as the weight, which means that the smaller the distance, the greater the weight. 
Moran.I(map.m.a.data.stan$bc_conc, m.a.dist.inv)

#Toronto
#create a matrix of the inverse distances between each point
t.s.dist.inv <- 1/as.matrix(distm(cbind(map.t.s.data.stan$longitude.x, map.t.s.data.stan$latitude.x), fun=distVincentyEllipsoid))
#inverse of 0 is inf, so get back to 0
diag(t.s.dist.inv) <- 0
#there's an inf entry (two spots exactly the same lat long?)
which.max(t.s.dist.inv)
max(t.s.dist.inv)
#it's column 63, row 32 that is the Inf
colnames(t.s.dist.inv) <- 1:63
describe(t.s.dist.inv)
#replace anything over 200 with just 200. This solves the error and dapens the extreme values from dominating. 
t.s.dist.inv <- ifelse(t.s.dist.inv > 0.008, 0.008, t.s.dist.inv)
Moran.I(map.t.s.data.stan$bc_conc, t.s.dist.inv, alternative = "two.sided")
#soooo those are both cities done with the ape package. Probably not appropriate due to it being made for.....apes. 
#and Toronto and Montreal have different results, so either way, it'll be something to interpret. 



#From this post, http://r.789695.n4.nabble.com/troubles-performing-Moran-I-test-td878799.html
#this is a post saying don't use ape for geographical stuff, use spdep. It shows how to
#convert the weight matrix to a row standardised general weights object:
m.a.lw <- mat2listw(m.a.dist.inv)
m.a.lw$weights
#now this is how you pass on the inverse distance weights! W means that each row of weghts is standardized
m.a.lw.W <- nb2listw(m.a.lw$neighbours, m.a.lw$weights, style = "W")
moran.test(map.m.a.data.stan$bc_conc, m.a.lw.W, alternative = "two.sided")
#would ya look at that, I get the same answer as the ape function!
#that is just for looking at the overall spatial distribution, this is intercept only model (ie: teh mean). That is not looking at the model residuals to see if there are spatial paterns to the residuals
lm.morantest(m.a.u4k.final.model.lm, m.a.lw.W)
#so looks like the residuals are not spatially corrolated

t.s.lw <- mat2listw(t.s.dist.inv)
t.s.lw$weights
#now this is how you pass on the inverse distance weights!
t.s.lw.W <- nb2listw(t.s.lw$neighbours, t.s.lw$weights, style = "W")
moran.test(map.t.s.data.stan$bc_conc, t.s.lw.W, alternative = "two.sided")
#the same as well. Can feel pretty confident that is that. 
#that is just for looking at the overall spatial distribution, this is intercept only model (ie: teh mean). That is not looking at the model residuals to see if there are spatial paterns to the residuals
lm.morantest(t.s.u10k.final.model.lm, t.s.lw.W)
#so looks like the residuals are not spatially corrolated



#here is trying some other methods. Not sure iif there are any nuggets worth keeping in there....
m.a.coords <- cbind(map.m.a.data.stan$longitude.x, map.m.a.data.stan$latitude.x)
#the second entry is the nearest neighbour. This is a somewhat arbitrary number. It says how many neighbours to look at
#I think this is less ideal than distance, but not bad. The null is that there are no clusters, there is no spatial correlation, distribution is random. 
moransI(m.a.coords, 6, map.m.a.data.stan$bc_conc)
moransI("help")

#d1 is min distance, d2 is max distance, they don't have defaults so I put a large number for d2. longlat = T means it is expecting long lats in degrees and is calculating in km
dnearneigh(m.a.coords, d1 = 0, d2 = 1000, longlat = TRUE)

moran.test(map.m.a.data.stan$bc_conc, 
          nb2listw(dnearneigh(m.a.coords, d1 = 0, d2 = 1000, longlat = TRUE), style = "W"))

moran.plot(map.m.a.data.stan$bc_conc, 
           nb2listw(dnearneigh(m.a.coords, d1 = 0, d2 = 1000, longlat = TRUE)))

m.a.nearest.6 <- dnearneigh(m.a.coords, d1 = 0, d2 = 1000, longlat = TRUE)
m.a.nearest.6.nb <- knn2nb(m.a.nearest.6)
plot(m.a.nearest.6.nb, m.a.coords)
?nb2listw
m.a.spatial.weights.6 <- nb2listw(m.a.nearest.6)
lm.morantest(m.a.u4k.final.model.lm, m.a.spatial.weights.6)
#doesn't look like there are spatial dependencies with the 6 nearest neighbours. Keep in mind that I have not weighted the distances, and that montreal has a bottleneck

t.s.coords <- cbind(map.t.s.data.stan$longitude.x, map.t.s.data.stan$latitude.x)
t.s.nearest.6 <- knearneigh(t.s.coords, k = 1, RANN = F)
t.s.nearest.6.nb <- knn2nb(t.s.nearest.6)
plot(t.s.nearest.6.nb, t.s.coords)
t.s.spatial.weights.6 <- nb2listw(t.s.nearest.6.nb)
lm.morantest(t.s.u10k.final.model.lm, t.s.spatial.weights.6)
#doesn't look like there are spatial dependencies with the 6 nearest neighbours. Keep in mind that I have not weighted the distances.






library(sp)
library(spdep)
example(columbus) 
coords <- st_as_sf(columbus, coords = c("longitude.x", "latitude.x"), crs = 4326)
dlist <- nbdists(col.gal.nb, coords) 
dlist <- lapply(dlist, function(x) 1/x) 
Wa <- nb2listw(col.gal.nb, glist = dlist, style = "W") 
Wb <- nb2listw(col.gal.nb, glist = dlist, style = "B") 
summary(sapply(Wa$weights, sum)) 
summary(sapply(Wb$weights, sum)) 










#notes from datacamp:
  #can convert a dataframe with coordinates to an sf object. The coords are the column names, the crs is the coord system. df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  #crs = 4326 is the unprojected, lat and long in degrees, usually you want a projected version for calculations (eg: buffers). Use st_transfrorm() to change the crs
  #st_buffer() creates buffers of a defined distance around sf data points
  #can plot(map.sf) then plot(st_geometry(bufferobj)) and some other stuff to get the map and add the geometries to it

#just a bit of example messing around code
plot(to.sf)
to.data.stan.sf <- st_as_sf(map.t.s.data.stan, coords = c("longitude.x", "latitude.x"), crs = 4326)
plot(st_geometry(to.data.stan.sf))
to.data.stan.sf <- st_transform(to.data.stan.sf, crs = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
to.data.stan.sf.buffer <- st_buffer(to.data.stan.sf, dist = 10000)
plot(st_geometry(to.data.stan.sf.buffer))


###Obs vs Pred Graphs ######
#montreal
ggplot(data = filter(map.m.a.data.stan, bc_conc < 4000), aes(x = f.m.pred, y = bc_conc)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f',
                                      stat(r.squared))), parse = TRUE)
  geom_abline(slope = 0.527, intercept = 0) +
  geom_abline(slope = 0.657, intercept = 0) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 3000) + ylim(0, 3000)
  
nrow(map.m.a.data.stan)
max(map.m.a.data.stan$bc_conc)
nrow(map.t.s.data.stan)



#toronto
ggplot(data = filter(map.t.s.data.stan, bc_conc < 10000), aes(x = f.m.pred, y = bc_conc)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f',
                                      stat(r.squared))), parse = TRUE)
  geom_abline(slope = 0.527, intercept = 0) +
  geom_abline(slope = 0.657, intercept = 0) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 3000) + ylim(0, 3000)
#notice the four points that are 4 of the 5 lowest observed, notice how they have predicted values that are too high. What's going on with those points?
map.t.s.data.stan %>%
  arrange(f.m.diffs) %>%
  filter(f.m.diffs < -1000) %>%
  dplyr::select(Filter_ID, bc_conc, f.m.pred, f.m.diffs, Nox_50m, com_750m, resid_100m, ind_1000m, d_airport, d_NPRI_PM, latitude.x, longitude.x) %>%
  formattable()
#nothing jumps out at me from those spots. They are all pretty regular looking. Maybe they weren't as close to the intersections as most of the others?

# Predictor Counts ######

# old code for a different rmd file. 

#export the tables to the project folder so they can be called up by the RMD file. I'm exporting the raw files as csv, that'll be a bit more code in markdown, but porbablly better for control 

write.csv(m.s.uni.var.sel, "m.s.uni.var.sel.csv")
write.csv(m.w.uni.var.sel, "m.w.uni.var.sel.csv")
write.csv(m.a.uni.var.sel, "m.a.uni.var.sel.csv")
write.csv(to.uni.var.sel, "to.uni.var.sel.csv")
write.csv(mts.pool.uni.var.sel, "mts.pool.uni.var.sel.csv")

#now just try to count how many time p < 0.05 showed up for each variable
#get a total for number of p < 0.05 per varaible in m.s. Also made a string......didn't need it
str(m.s.uni.var.sel)
m.s.var.count <- m.s.uni.var.sel %>%
  dplyr::select(2:6) %>%
  mutate(freq = 4-rowSums(is.na(m.s.uni.var.sel))) %>%
  dplyr::select(1,6) %>%
  mutate(data = rep("m.s", nrow(m.s.uni.var.sel)))
m.s.var.string <- as.character(rep(m.s.var.count$Predictor, m.s.var.count$freq))
#get a total for number of p < 0.05 per varaible in m.w
m.w.var.count <- m.w.uni.var.sel %>%
  dplyr::select(2:4) %>%
  mutate(freq = 2-rowSums(is.na(m.w.uni.var.sel))) %>%
  dplyr::select(1,4) %>%
  mutate(data = rep("m.w", nrow(m.w.uni.var.sel)))
m.w.var.string <- as.character(rep(m.w.var.count$Predictor, m.w.var.count$freq))
sum(m.w.var.count$freq)
length(m.w.var.string)
#get a total for number of p < 0.05 per varaible in m.a
m.a.var.count <- m.a.uni.var.sel %>%
  dplyr::select(2:6) %>%
  mutate(freq = 4-rowSums(is.na(m.a.uni.var.sel))) %>%
  dplyr::select(1,6) %>%
  mutate(data = rep("m.a", nrow(m.a.uni.var.sel)))
m.a.var.string <- as.character(rep(m.a.var.count$Predictor, m.a.var.count$freq))
#bind them together, group the variable, sum them, then sort to see the most important
mtl.var.freq <- bind_rows(m.s.var.count, m.w.var.count, m.a.var.count) %>%
  group_by(Predictor) %>% 
  summarise(tot = sum(freq)) %>%
  arrange(desc(tot))


str(mts.pool.uni.var.sel)
mts.pool.uni.var.sel
#get a total for number of p < 0.05 per varaible in to
to.var.count <- to.uni.var.sel %>%
  dplyr::select(2:6) %>%
  mutate(freq = 4-rowSums(is.na(to.uni.var.sel))) %>%
  dplyr::select(1,6) %>%
  mutate(data = rep("to", nrow(to.uni.var.sel)))
to.var.string <- as.character(rep(to.var.count$Predictor, to.var.count$freq))
sum(to.var.count$freq)
length(to.var.string)
#get a total for number of p < 0.05 per varaible in mts pooled
mts.pool.var.count <- mts.pool.uni.var.sel %>%
  dplyr::select(2:6) %>%
  mutate(freq = 4-rowSums(is.na(mts.pool.uni.var.sel))) %>%
  dplyr::select(1,6) %>%
  mutate(data = rep("mts", nrow(mts.pool.uni.var.sel)))
mts.pool.var.string <- as.character(rep(mts.pool.var.count$Predictor, mts.pool.var.count$freq))

#bind allllll of them together, group the variable, sum them, then sort to see the most important
all.var.freq <- bind_rows(m.s.var.count, m.w.var.count, m.a.var.count,to.var.count,mts.pool.var.count) %>%
  group_by(Predictor) %>% 
  summarise(tot = sum(freq)) %>%
  arrange(desc(tot))
glimpse(all.var.freq)
formattable(all.var.freq)

#bind allllll of them together, group the variable, sum them, then sort to see the most important
all.var.freq <- bind_rows(m.s.var.count, m.w.var.count, m.a.var.count,to.var.count,mts.pool.var.count) %>%
  group_by(Predictor) %>% 
  summarise(tot = sum(freq)) %>%
  arrange(desc(tot))
glimpse(all.var.freq)
formattable(all.var.freq)

barplot(mtl.var.freq$tot)
barplot(all.var.freq$tot)


all.var.freq$prop <- all.var.freq$tot/18
mtl.var.freq$prop <- mtl.var.freq$tot/10
write.csv(all.var.freq, "all_var_freq.csv")
write.csv(mtl.var.freq, "mtl_var_freq.csv")







#tried to do this as a data frame, but some are of differing lenghts (recall some varaibles were removed for some of the unis)
uni.vars <- cbind(MTL_S_BC = ifelse(m.s.bc.uni.reg$P.Value < 0.05, as.character(m.s.bc.uni.reg$variable), NA), 
           MTL_S_UVPM = ifelse(m.s.uvpm.uni.reg$P.Value < 0.05, as.character(m.s.uvpm.uni.reg$variable), NA),
           MTL_W_BC = ifelse(m.w.bc.uni.reg$P.Value < 0.05, as.character(m.w.bc.uni.reg$variable), NA), 
           MTL_W_UVPM = ifelse(m.w.uvpm.uni.reg$P.Value < 0.05, as.character(m.w.uvpm.uni.reg$variable), NA),
           MTL_A_BC = ifelse(m.a.bc.uni.reg$P.Value < 0.05, as.character(m.a.bc.uni.reg$variable), NA), 
           MTL_A_UVPM = ifelse(m.a.uvpm.uni.reg$P.Value < 0.05, as.character(m.a.uvpm.uni.reg$variable), NA),
           TO_S_BC = ifelse(to.bc.uni.reg$P.Value < 0.05, as.character(to.bc.uni.reg$variable), NA), 
           TO_S_UVPM = ifelse(to.uvpm.uni.reg$P.Value < 0.05, as.character(to.uvpm.uni.reg$variable), NA),
           MTS_P_BC = ifelse(mts.pool.bc.uni.reg$P.Value < 0.05, as.character(mts.pool.bc.uni.reg$variable), NA), 
           MTS_P_UVPM = ifelse(mts.pool.uvpm.uni.reg$P.Value < 0.05, as.character(mts.pool.uvpm.uni.reg$variable), NA)
           )

formattable(as.data.frame(uni.vars))



# Working Notes ########

#26 July notes:
#are there two TO sites with the exact same lat and long? When I make the inverse distance matrix, I get 1 cell = Inf, I think it's cell 63x32
#Montreal has a pretty janky spread. Look into it. See what coords are not on the map. Ask if that's legit. 
# I think I have some sort of Moran's I., though it is not wrt to the regression and residuals. Montreal has spatial correlation (ie: downtown) and Toronto doesn't
#Clean up the paper, what I have already. The Intro is solid, but the methods still have old language in it (talkinga bout 5 models)


#5 July notes:
#leaps() works for MTL Annual, not so much for TO S and MTL+TO Pooled
#sent an email with notes an updates
#to do before meeting:
  #organize the dropbox folders and make a txt file guide
  #finish the RMD and put in dropbox
  #create a google docs task list (or Microsoft onedive doc)
  #review heat notes for potential projects
#low priority to do:
  #figure out why caret i not working
  #look into rFSA package (algo)
  #look into dredge() from MuMln package (ehaustive with more options)
  #look into RStata


#bringing in woodburn buffer
#we see later that woodburn_500m is the only wb buffer that shows up anywhere and it only shows up when all MTL summer data is around. 
#It's the 12k data point that has the max woodburn_500m value
#Note that it doesn't have a max value for any of the other woodburns, but has high _750 and _1000


#28 June:
#write intro
#look at selected variables in rmd and see if the beta directsion are logical


###HOw to update if new observations/variables get added"
#do the uni regressions in order to get a .reg data frame that has the p values, CIs and R2 for each of the 150+ uni regressions
#that .reg data frame is used to select the p < 0.05 variables in several different locations
#do the XY Fit plots, this helps to see if the p < 0.05 is actually legit (no outliers driving the relationship or non-linearity)
#then go to variable selection section, that's the judgement zone where you look at the XY fit plots and then list the variables you will select
#update as appropriate in the variable selection summary section (may need to update code if there is a change in non-linearity)
#update the selected variables residual histograms. 





#13 June
#made a bit of progress on making a pretty table to show the uni results
#did first pass variable selection for MTL S. When combined, the R^2 is pretty low. Could got through the process for the others.
#made residual histogram plots for all the uni regressions (the 5 for BC and with minimal outliers cut out)
  #MTL residuals look pretty normal
#TO and pooled residuals look a little more skewed. 
#next steps:
  #do log models and look at residuals
  #organize residual plots and maybe make an html
  #build the 2 other non-log MTL models
  #maybe build TO or log models.
  #make prettier plots
  #look into wood burning coordinates


#2 June direction from Scott:
#focus on MTL S, TO S, Pooled S, MTL Winter, MTL Annual
#find variables that are important predictors (for all?). Make sure one point isn't driving everything. Want good variation
#make tables of important looking variables (include R2, p val, outlier?)
#make a paper outline that I think makes sense and we can work to fill it in

#31 May Log transforming.
#there are bc = 0 values, should I do log(bc +1)? +100? + mean?
#log transforma all of them? (ie: MTL, TO, Pooled)

##29 May. There are some questions:
#rows MTL_space_114,115 (bc value looks regular) and 131 (big bc value and NA latlong) are in the montreal outcomes (bc) but not the montreal determinants (ArcGIS variables in Variable Data Montreal)
#this looks like 114 and 115 are legit missing, maybe 131
#they are different, there is only 83 winter observations. Also, some of the montreal winter observations have Toronto labels. And the montreal labels say "Winter" instead of "space". There are no such entries in the "Variable Data Montreal"
#this looks like I might need a separate "Variable Data Montreal" file. Or some file that converts the sensor IDs or the the site IDs
#how do I average this if they are different sites?
#MTL_wood_58, 60, TO_space_02, 108, 37, 41, 51, 55, 81, and 85 (all NA bc values) are in the outcomes but not in the determinants. No biggie. 
#these don't look like they are actually missing, they look like non-existant observations


##MTL summer has 1 extreme outlier that looks like and error (ran for only 1 day, small volume) and 4 outliers that might be legit data
  #taking the extreme out gives 5 pvals, different too
  #taking the 5 outliers out gives 41 pvals. 
##TO summer has 1 extreme outlier
  #taking the 1 extreme outlier out gives 70 pvals (as opposed to keeping it in which gives 61)

#Pooling
  #pool mtl summer with to? Or MTL annual with TO?
  #ANSWERED: pool MTLsummer and TO

#regression list:
#mtl summer YES -check
#to summer YES - check
#combined summer YES - check (both summer; plot todo)
#mtl winter -  YES - check
#mtl average - YES - check 

#mtl summer log YES - check....don't worry about log for now
#to summer log YES  
#combined summer log YES
#mtl winter log Qs
#mtl average log Qs

#to winter NOT A THING, no data
#to average, NOT A THING due to no to winter data
#to winter log NOT A THING, no data
#to average, log NOT A THING due to no to winter data


#DATA NOTES
#open_750m appears twice, I think the second one (..25) is actually open_500m
