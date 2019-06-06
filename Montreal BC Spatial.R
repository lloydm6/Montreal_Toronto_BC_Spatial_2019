#setup####

library(readxl)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(table1)
library(stargazer)
library(xlsx)
library(formattable)
library(purrr)
library(reshape)
library(broom)
library(tidyverse)
library(BMA)
library(ggpmisc)
library(miscTools)
library(ImportExport)
library(rJava)
install.packages('rJava',,'http://www.rforge.net/', type = "source")
install.packages("helloJavaWorld")
library(helloJavaWorld)
options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/jre")


setwd("/Users/macbook/Documents/McGill School/Practicum/Montreal_Toronto_BC_Spatial_2019")

#data read in####

########### Read in all the data
#columns AB and AC in the spread sheet are the outcomes of interest. AD also indicates if the data is good or not (eliminate the = 0)
m.t.s.outcomes.raw <- read.csv("Montreal and Toronto SPATIAL STUDY FILTER DATA and Chain of Custody 29-05-2019 SR.csv")
#different columns in this one, I check it lower down in this code. 
m.w.outcomes.raw <- read_xlsx("Montreal winter spatial study filter database 13 May (1).xlsx")
str(m.w.outcomes.raw)
str(m.t.s.outcomes.raw)

m.s.determinants <- read_xlsx("Variable Data Montreal.xlsx", sheet = 2)
t.s.determinants <- read_xlsx("Variable Data Toronto.xlsx", sheet = 2)

#this is to link the site id of winter to summer to the determinants (which are listed by summer site)
m.w.id.key <- read_xlsx("Winter and summer sites.xlsx")

#this is an explanation of each of the determinants
det.legend <- read_xlsx("Variable Data Montreal.xlsx", sheet = 1)


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

str(t.determinants)
#100 observations of 152 variabales

str(m.t.s.outcomes.raw)
#275 observations o 30 variables

#create a data frame with just montreal determinants and outcomes. First figure out which filters are used 
str(m.t.s.outcomes.raw$filter)

str(m.s.determinants$Filter_ID)

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
m.s.data.all <- data.frame(f.id = m.s.determinants$Filter_ID, bc_conc = as.numeric(m.s.outcomes[ , 28]), uvpm_conc = as.numeric(as.character(m.s.outcomes[ , 29])), good_data = as.factor(m.s.outcomes[ , 30]), m.s.determinants[ , -1])

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

#and for toronto. Recall that it is mtl winter that is different, so 26 and 27 are still the correct columns. 
head(t.s.outcomes)
str(t.s.outcomes)
as.numeric(t.s.outcomes[ , 28])
as.numeric(as.character(t.s.outcomes[ , 29]))

#i'll keep them separate for the unpooled unis. I'm sure I could fancy code this, but not tonight cowboy. Will mash togther for the pooled. 
t.s.data.all <- data.frame(f.id = t.s.determinants$Filter_ID, bc_conc = as.numeric(t.s.outcomes[ , 28]), uvpm_conc = as.numeric(as.character(t.s.outcomes[ , 29])), good_data = as.factor(t.s.outcomes[ , 30]), t.s.determinants[ , -1])

#just checking to make sure they are the correct length
length(t.s.determinants$Filter_ID)
length(as.numeric(t.s.outcomes[ , 26]))
length(as.numeric(as.character(t.s.outcomes[ , 27])))
nrow(t.s.determinants[ , -1])
head(t.s.data.all[,1:4 ])
str(t.s.data.all)

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
length(m.s.outcomes$SITE_ID)
length(m.w.outcomes$summer.id)

m.w.sum.site.sumfilt.id <- m.s.outcomes[m.s.outcomes$SITE_ID %in% m.w.outcomes$summer.id, ]
m.w.sum.site.sumfilt.id <- data.frame(id.summer = m.w.sum.site.sumfilt.id$SITE_ID, filter = m.w.sum.site.sumfilt.id$filter)

m.w.sum.site.sumfilt.id <- m.w.sum.site.sumfilt.id[order(m.w.sum.site.sumfilt.id$id.summer),]
m.w.outcomes <- m.w.outcomes[order(m.w.outcomes$summer.id),]
m.w.outcomes[c(-NA, -"u47", -"u32"),]

m.w.outcomes.w.id.convert <- data.frame(m.w.outcomes[! m.w.outcomes$summer.id %in% c(NA, "u47", "u32"),], m.w.sum.site.sumfilt.id)
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

m.a.data <- select(m.a.data, -2)
head(m.a.data)

m.a.data <- mutate(m.a.data, a.bc_conc = (w.bc_conc + s.bc_conc)/2, d.bc = round((w.bc_conc - s.bc_conc)/w.bc_conc*100, 0), a.uvpm_conc = (w.uvpm_conc + s.uvpm_conc)/2, d.uvpm = round((w.uvpm_conc - s.uvpm_conc)/w.uvpm_conc*100, 0))
# I added d. columns that are the % change in concentration between summer and winter. There are some really big values. 
#can take away those when making the "long" data frame for regressions. 

#slap the determinants in there
#make sure they are all the same
all.equal(m.a.s.data[ , 5:ncol(m.a.s.data)], m.a.w.data[ , 10:ncol(m.a.w.data)])

m.a.data <- data.frame(m.a.data, m.a.s.data[ , 5:ncol(m.a.s.data)])
str(m.a.data)




#Outlier Inspection #####
which.max(t.s.data$bc_conc)
t.s.data[75, ]
formattable(t.s.data[75,])

formattable(subset(m.s.data, bc_conc > 4000))
describe(subset(m.s.data, bc_conc > 4000))


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

hist(m.s.data$uvpm_conc, breaks = 100)
hist(m.s.data$bc_conc, breaks = 100, xlim = c(0, 4000))
#One out at 22k (MTL_space_23). Looks normallish under 3000. 
describe(m.s.data$uvpm_conc)
describe(m.s.data$bc_conc)


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













#TO Histograms ####
#take a quick gander at some of the distributions
hist(t.s.data$bc_conc, breaks = 100)
hist(t.s.data$bc_conc, breaks = 100, xlim = c(0, 7000))
#One out at 40k (XXX). Looks normallish under 8000, bit of a right tail

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




# Standardizing ####
###MTL Summer
#time to standardize it
#this is how to standardize just part of the data frame
colnames(m.s.data)[c(-1,-2,-3,-4)]
summary(scale(m.s.data[ , c(-1,-2,-3,-4)]))
m.s.data.stan <- data.frame(m.s.data[ , 1:4], scale(m.s.data[ , c(-1,-2,-3,-4)]))
summary(m.s.data.stan)
#can see in the summary that all the means are zero
apply(m.s.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!


##MTL Winter
colnames(m.w.data)[c(-1:-9)]
summary(scale(m.s.data[ , c(-1:-9)]))
m.w.data.stan <- data.frame(m.w.data[ , 1:9], scale(m.w.data[ , c(-1:-9)]))
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
head(m.w.data.stan[,1:10])
m.w.data.stan <- select(m.w.data.stan, 1, 9, 2:3, 10:ncol(m.w.data.stan))


####MTL Annual
colnames(m.a.data)[c(1:10)]
colnames(m.a.data)[c(-1:-10)]
summary(scale(m.a.data[ , c(-1:-10)]))
m.a.data.stan <- data.frame(m.a.data[ , 1:10], scale(m.a.data[ , c(-1:-10)]))
summary(m.w.data.stan)

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
head(m.a.data.stan[,1:10])
m.a.data.stan <- select(m.a.data.stan, 1:2, 7, 9, 11:ncol(m.a.data.stan))




###TO
#time to standardize it
#this is how to standardize just part of the data frame
colnames(t.s.data)[c(-1,-2,-3,-4)]
summary(scale(t.s.data[ , c(-1,-2,-3,-4)]))
t.s.data.stan <- data.frame(t.s.data[ , 1:4], scale(t.s.data[ , c(-1,-2,-3,-4)]))
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

str(t.s.data.pool)  

mts.data.pool <- as.data.frame(rbind(m.s.data.pool, t.s.data.pool))
head(mts.data.pool[,1:4])
mts.data.pool$city <- c(rep("MTL", nrow(m.s.data.pool)), rep("TO", nrow(t.s.data.pool)))
describe(mts.data.pool$city)

#standardize them
colnames(mts.data.pool)[c(-1,-2,-3,-4, -ncol(mts.data.pool))]
summary(scale(mts.data.pool[ , c(-1,-2,-3,-4, -ncol(mts.data.pool))]))
mts.data.pool.stan <- data.frame(mts.data.pool[ , 1:4], city =mts.data.pool$city, scale(mts.data.pool[ , c(-1,-2,-3,-4,-ncol(mts.data.pool))]))
summary(mts.data.pool.stan)
head(mts.data.pool.stan[ ,1:5])
tail(mts.data.pool.stan[ ,1:5])
#TOs are coded TO and MTLs are coded MTL

#can see in the summary that all the means are zero
apply(mts.data.pool.stan, 2, sd)







# MTL Summer Uni Regressions #####

#####MTL UNI Regressions
str(m.s.data.stan)
long.m.s.data.stan <- melt(m.s.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc", "good_data"))
str(long.m.s.data.stan)

#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#check to make sure nothing is too squirrelly
summary(long.m.s.data.stan)
nrow(long.m.s.data.stan)/nrow(m.s.data.stan)
str(m.s.determinants)
#determinants had 154 columns, filter_id and 153 others. Now the data frame is 153 times longer, I think we're good to go
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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.bc.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.bc.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

options(scipen=999)
#put em together 
m.s.bc.uni.reg <- data.frame(m.s.bc.uni.beta.p[ , c(1,2)], lapply(m.s.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.bc.uni.r2[ ,2], 5), m.s.bc.uni.beta.p[ , c(3,4)])
m.s.bc.uni.reg$Beta <- as.numeric(m.s.bc.uni.reg$Beta)
str(m.s.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
m.s.uvpm.uni.beta.p <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.s.uvpm.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.s.uvpm.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 

m.s.uvpm.uni.reg <- data.frame(m.s.uvpm.uni.beta.p[ , c(1,2)], lapply(m.s.uvpm.uni.cis[ , c(2,3)], as.numeric), m.s.uvpm.uni.r2[ ,2], m.s.uvpm.uni.beta.p[ , c(3,4)])
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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.u4k.bc.uni.cis <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.u4k.bc.uni.r2 <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
m.s.u4k.bc.uni.reg <- data.frame(m.s.u4k.bc.uni.beta.p[ , c(1,2)], lapply(m.s.u4k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.u4k.bc.uni.r2[ ,2], 5), m.s.u4k.bc.uni.beta.p[ , c(3,4)])
m.s.u4k.bc.uni.reg$Beta <- as.numeric(m.s.u4k.bc.uni.reg$Beta)

nrow(subset(m.s.u4k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.bc.uni.reg, P.Value <= 0.05))
#8 with all data and 42 with the 4 over 5k outliers cut out

formattable(m.s.u4k.bc.uni.reg)
formattable(subset(m.s.u4k.bc.uni.reg, P.Value <= 0.05))
#could do this all again for uvpm, but I'll wait until I hear back from Susannah re outliers


#repeat for uvpm
m.s.u4k.uvpm.uni.beta.p <- subset(long.m.s.data.stan, uvpm_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.u4k.uvpm.uni.cis <- subset(long.m.s.data.stan, uvpm_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.u4k.uvpm.uni.r2 <- subset(long.m.s.data.stan, uvpm_conc < m.s.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
m.s.u4k.uvpm.uni.reg <- data.frame(m.s.u4k.uvpm.uni.beta.p[ , c(1,2)], lapply(m.s.u4k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(m.s.u4k.uvpm.uni.r2[ ,2], 5), m.s.u4k.uvpm.uni.beta.p[ , c(3,4)])
m.s.u4k.uvpm.uni.reg$Beta <- as.numeric(m.s.u4k.uvpm.uni.reg$Beta)

nrow(subset(m.s.u4k.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.uvpm.uni.reg, P.Value <= 0.05))
#3 with all data and 33 with the 4 over 4k outliers cut out

formattable(m.s.u4k.uvpm.uni.reg)
formattable(subset(m.s.u4k.uvpm.uni.reg, P.Value <= 0.05))
#could do this all again for uvpm, but I'll wait until I hear back from Susannah re outliers


write.csv(m.s.u4k.bc.uni.reg, file = "MTL_S_u4k_BC_Uni_Regressions.csv")
write.csv(m.s.u4k.uvpm.uni.reg, file = "MTL_S_u4k_UVPM_Uni_Regressions.csv")





# MTL Winter Uni Regressions #####

#####MTL Winter UNI Regressions
str(m.w.data.stan)
colnames(m.w.data.stan)[1:2] <- c("f.id.winter", "f.id")
long.m.w.data.stan <- melt(m.w.data.stan, id.vars = c("f.id", "f.id.winter", "bc_conc", "uvpm_conc"))
str(long.m.w.data.stan)

#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#check to make sure nothing is too squirrelly
summary(long.m.w.data.stan)
ncol(m.w.data.stan)
nrow(long.m.w.data.stan)/nrow(m.w.data.stan)
#data.stan had 154 columns, 2 filter ids, 2 outcomes, and 150 determinants. Now the data frame is 150 times longer, I think we're good to go
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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.w.bc.uni.cis <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.w.bc.uni.r2 <- long.m.w.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)


#put em together 
m.w.bc.uni.reg <- data.frame(m.w.bc.uni.beta.p[ , c(1,2)], lapply(m.w.bc.uni.cis[ , c(2,3)], as.numeric), round(m.w.bc.uni.r2[ ,2], 5), m.w.bc.uni.beta.p[ , c(3,4)])
m.w.bc.uni.reg$Beta <- as.numeric(m.w.bc.uni.reg$Beta)
str(m.w.bc.uni.reg)


#do simple regressions on uvpm_conc for each of the determinants
m.w.uvpm.uni.beta.p <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.w.uvpm.uni.cis <- long.m.w.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.w.uvpm.uni.r2 <- long.m.w.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 

m.w.uvpm.uni.reg <- data.frame(m.w.uvpm.uni.beta.p[ , c(1,2)], lapply(m.w.uvpm.uni.cis[ , c(2,3)], as.numeric), m.w.uvpm.uni.r2[ ,2], m.w.uvpm.uni.beta.p[ , c(3,4)])
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


formattable(subset(m.w.bc.uni.reg, P.Value <= 0.05))
formattable(subset(m.w.uvpm.uni.reg, P.Value <= 0.05))




#MTL Winter Uni Reg -Outliers #####

hist(m.w.data.stan$bc_conc)
hist(m.w.data.stan$uvpm_conc)
#nothing too squirelly. Don't even bother with this. 












# MTL Annual Uni Regressions ######
str(m.a.data.stan)
colnames(m.a.data.stan)[3:4] <- c("bc_conc", "uvpm_conc")
long.m.a.data.stan <- melt(m.a.data.stan, id.vars = c("f.id.summer", "f.id.winter", "bc_conc", "uvpm_conc"))
str(long.m.w.data.stan)

#check to make sure nothing is too squirrelly
summary(long.m.w.data.stan)
ncol(m.w.data.stan)
nrow(long.m.w.data.stan)/nrow(m.w.data.stan)
#data.stan had 154 columns, 2 filter ids, 2 outcomes, and 150 determinants. Now the data frame is 150 times longer, I think we're good to go
slice(long.m.w.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.m.w.data.stan, 200:210)

#do simple regressions on bc_conc for each of the determinants
m.a.bc.uni.beta.p <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.bc.uni.cis <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.bc.uni.r2 <- long.m.a.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)
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
m.a.bc.uni.reg <- data.frame(m.a.bc.uni.beta.p[ , c(1,2)], lapply(m.a.bc.uni.cis[ , c(2,3)], as.numeric), round(m.a.bc.uni.r2[ ,2], 5), m.a.bc.uni.beta.p[ , c(3,4)])
m.a.bc.uni.reg$Beta <- as.numeric(m.a.bc.uni.reg$Beta)
str(m.a.bc.uni.reg)



#do simple regressions on uvpm_conc for each of the determinants
m.a.uvpm.uni.beta.p <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.a.uvpm.uni.cis <- long.m.a.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.a.uvpm.uni.r2 <- long.m.a.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 


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

m.a.uvpm.uni.reg <- data.frame(m.a.uvpm.uni.beta.p[ , c(1,2)], lapply(m.a.uvpm.uni.cis[ , c(2,3)], as.numeric), m.a.uvpm.uni.r2[ ,2], m.a.uvpm.uni.beta.p[ , c(3,4)])
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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.u4k.bc.uni.cis <- subset(long.m.a.data.stan, bc_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.u4k.bc.uni.r2 <- subset(long.m.a.data.stan, bc_conc < m.a.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

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
m.a.u4k.bc.uni.reg <- data.frame(m.a.u4k.bc.uni.beta.p[ , c(1,2)], lapply(m.a.u4k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.a.u4k.bc.uni.r2[ ,2], 5), m.a.u4k.bc.uni.beta.p[ , c(3,4)])
m.a.u4k.bc.uni.reg$Beta <- as.numeric(m.a.u4k.bc.uni.reg$Beta)
m.a.u4k.bc.uni.reg$P.Value <- as.numeric(m.a.u4k.bc.uni.reg$P.Value)

str(m.a.u4k.bc.uni.reg)

nrow(subset(m.a.u4k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.bc.uni.reg, P.Value <= 0.05))
#6 with all data and 25 with the 3 over 4k outliers cut out

formattable(m.a.u4k.bc.uni.reg)
formattable(subset(m.a.u4k.bc.uni.reg, P.Value <= 0.05))
#could do this all again for uvpm, but I'll wait until I hear back from Susannah re outliers




#repeat for uvpm
m.a.u4k.uvpm.uni.beta.p <- subset(long.m.a.data.stan, uvpm_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.a.u4k.uvpm.uni.cis <- subset(long.m.a.data.stan, uvpm_conc < m.a.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.a.u4k.uvpm.uni.r2 <- subset(long.m.a.data.stan, uvpm_conc < m.a.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

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
m.a.u4k.uvpm.uni.reg <- data.frame(m.a.u4k.uvpm.uni.beta.p[ , c(1,2)], lapply(m.a.u4k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(m.a.u4k.uvpm.uni.r2[ ,2], 5), m.a.u4k.uvpm.uni.beta.p[ , c(3,4)])
m.a.u4k.uvpm.uni.reg$Beta <- as.numeric(m.a.u4k.uvpm.uni.reg$Beta)

nrow(subset(m.a.u4k.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(m.a.uvpm.uni.reg, P.Value <= 0.05))
#5 with all data and 18 with the 3 over 4k outliers cut out

formattable(m.a.u4k.uvpm.uni.reg)
formattable(subset(m.a.u4k.uvpm.uni.reg, P.Value <= 0.05))


write.csv(m.a.u4k.bc.uni.reg, file = "MTL_A_u4k_BC_Uni_Regressions.csv")
write.csv(m.a.u4k.uvpm.uni.reg, file = "MTL_A_u4k_UVPM_Uni_Regressions.csv")














#MTL Uni log Regression ########
#####MTL UNI Regressions Log on outcome
#Don't focus on this too much right now

#there are some 0 values for bc that go to -Inf when logged. 
log(long.mts.data.pool.stan$bc_conc)
describe(long.mts.data.pool.stan$bc_conc)
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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.log.bc.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.log.bc.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)


#put em together 
m.s.log.bc.uni.reg <- data.frame(m.s.log.bc.uni.beta.p[ , c(1,2)], lapply(m.s.log.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.log.bc.uni.r2[ ,2], 5), m.s.log.bc.uni.beta.p[ , c(3,4)])
m.s.log.bc.uni.reg$Beta <- as.numeric(m.s.log.bc.uni.reg$Beta)
str(m.s.log.bc.uni.reg)

#do simple regressions on uvpm_conc for each of the determinants
m.s.log.uvpm.uni.beta.p <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(log(uvpm_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
m.s.log.uvpm.uni.cis <- long.m.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(uvpm_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
m.s.log.uvpm.uni.r2 <- long.m.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(uvpm_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 

m.s.log.uvpm.uni.reg <- data.frame(m.s.log.uvpm.uni.beta.p[ , c(1,2)], lapply(m.s.log.uvpm.uni.cis[ , c(2,3)], as.numeric), m.s.log.uvpm.uni.r2[ ,2], m.s.log.uvpm.uni.beta.p[ , c(3,4)])
m.s.log.uvpm.uni.reg$Beta <- as.numeric(m.s.log.uvpm.uni.reg$Beta)

write.csv(m.s.log.bc.uni.reg, file = "MTL_S_log_BC_Uni_Regressions.csv")
write.csv(m.s.log.uvpm.uni.reg, file = "MTL_S_log_UVPM_Uni_Regressions.csv")

formattable(m.s.log.bc.uni.reg)
formattable(m.s.log.uvpm.uni.reg)

nrow(subset(m.s.log.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.bc.uni.reg, P.Value <= 0.05))
#9 with the log transform, 8 without it

#MTL Uni log Reg -Outliers #####

#do log unis without all 4 outliers
#do simple regressions on bc_conc for each of the determinants

m.s.outlier.level <- 5000
m.s.log.u5k.bc.uni.beta.p <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(log(bc_conc + 1) ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.log.u5k.bc.uni.cis <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(log(bc_conc + 1) ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.log.u5k.bc.uni.r2 <- subset(long.m.s.data.stan, bc_conc < m.s.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(log(bc_conc + 1) ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
m.s.log.u5k.bc.uni.reg <- data.frame(m.s.log.u5k.bc.uni.beta.p[ , c(1,2)], lapply(m.s.log.u5k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.log.u5k.bc.uni.r2[ ,2], 5), m.s.log.u5k.bc.uni.beta.p[ , c(3,4)])
m.s.log.u5k.bc.uni.reg$Beta <- as.numeric(m.s.log.u5k.bc.uni.reg$Beta)

nrow(subset(m.s.log.u5k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.log.bc.uni.reg, P.Value <= 0.05))
#9 with all data and 5 with the 4 over 5k outliers cut out

formattable(subset(m.s.log.u5k.bc.uni.reg, P.Value <= 0.05))
#could do this all again for uvpm, but I'll wait until I hear back from Susannah re outliers



#TO Uni Regressions #####

#####TO UNI Regressions
#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#this first step makes the wide data long to set up the code further down
str(t.s.data.stan)
long.t.s.data.stan <- melt(t.s.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc", "good_data"))
str(long.t.s.data.stan)

#check to make sure nothing is too squirrelly
summary(long.t.s.data.stan)
nrow(long.t.s.data.stan)/nrow(t.s.data.stan)
ncol(t.s.data.stan)
str(t.s.data.stan)
#we had 78 rows for 149 columns, all but 4 got staked underneath. We should now have a df that is 145 times longer. CHECK!

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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.bc.uni.cis <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.bc.uni.r2 <- long.t.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)


#put em together 
to.bc.uni.reg <- data.frame(to.bc.uni.beta.p[ , c(1,2)], lapply(to.bc.uni.cis[ , c(2,3)], as.numeric), round(to.bc.uni.r2[ ,2], 5), to.bc.uni.beta.p[ , c(3,4)])
to.bc.uni.reg$Beta <- as.numeric(to.bc.uni.reg$Beta)
str(to.bc.uni.reg)



#do simple regressions on uvpm_conc for each of the determinants
to.uvpm.uni.beta.p <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
to.uvpm.uni.cis <- long.t.s.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
to.uvpm.uni.r2 <- long.t.s.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 

to.uvpm.uni.reg <- data.frame(to.uvpm.uni.beta.p[ , c(1,2)], lapply(to.uvpm.uni.cis[ , c(2,3)], as.numeric), to.uvpm.uni.r2[ ,2], to.uvpm.uni.beta.p[ , c(3,4)])
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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.u10k.bc.uni.cis <- subset(long.t.s.data.stan, bc_conc < t.outlier.level)  %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.u10k.bc.uni.r2 <- subset(long.t.s.data.stan, bc_conc < t.outlier.level)  %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)


#put em together 
to.u10k.bc.uni.reg <- data.frame(to.u10k.bc.uni.beta.p[ , c(1,2)], lapply(to.u10k.bc.uni.cis[ , c(2,3)], as.numeric), round(to.u10k.bc.uni.r2[ ,2], 5), to.u10k.bc.uni.beta.p[ , c(3,4)])
to.u10k.bc.uni.reg$Beta <- as.numeric(to.u10k.bc.uni.reg$Beta)
str(to.u10k.bc.uni.reg)

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
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.u10k.uvpm.uni.cis <- subset(long.t.s.data.stan, uvpm_conc < t.outlier.level)  %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.u10k.uvpm.uni.r2 <- subset(long.t.s.data.stan, uvpm_conc < t.outlier.level)  %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)


#put em together 
to.u10k.uvpm.uni.reg <- data.frame(to.u10k.uvpm.uni.beta.p[ , c(1,2)], lapply(to.u10k.uvpm.uni.cis[ , c(2,3)], as.numeric), round(to.u10k.uvpm.uni.r2[ ,2], 5), to.u10k.uvpm.uni.beta.p[ , c(3,4)])
to.u10k.uvpm.uni.reg$Beta <- as.numeric(to.u10k.uvpm.uni.reg$Beta)
str(to.u10k.uvpm.uni.reg)

formattable(to.u10k.uvpm.uni.reg)
nrow(subset(to.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(to.u10k.uvpm.uni.reg, P.Value <= 0.05))
#removing the outlier takes it from 54 pvals, ot 61 pvals

write.csv(to.u10k.bc.uni.reg, file = "TO_u10k_BC_Uni_Regressions.csv")
write.csv(to.u10k.uvpm.uni.reg, file = "TO_u10k_UVPM_Uni_Regressions.csv")















#MTL+TO Summer Pool Uni Reg #####


#now run all the uni regressions. Not sure if the pooled will be an MTL annual average pooled with TO or MTL summer pooled with TO. THis is MTL summer with TO
str(mts.data.pool.stan)
long.mts.data.pool.stan <- melt(mts.data.pool.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc", "good_data", "city"))
str(long.mts.data.pool.stan)

#check to make sure nothing is too squirrelly
summary(long.mts.data.pool.stan)
nrow(long.mts.data.pool.stan)/nrow(mts.data.pool.stan)

###Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution

#do simple regressions on bc_conc for each of the determinants
mts.pool.bc.uni.beta.p <- long.mts.data.pool.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.bc.uni.cis <- long.mts.data.pool.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.bc.uni.r2 <- long.mts.data.pool.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
mts.pool.bc.uni.reg <- data.frame(mts.pool.bc.uni.beta.p[ , c(1,2)], lapply(mts.pool.bc.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.bc.uni.r2[ ,2], 5), mts.pool.bc.uni.beta.p[ , c(3,4)])
mts.pool.bc.uni.reg$Beta <- as.numeric(mts.pool.bc.uni.reg$Beta)
str(mts.pool.bc.uni.reg)
formattable(mts.pool.bc.uni.reg)

#repat for uvpm
#do simple regressions on uvpm_conc for each of the determinants
mts.pool.uvpm.uni.beta.p <- long.mts.data.pool.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
mts.pool.uvpm.uni.cis <- long.mts.data.pool.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
mts.pool.uvpm.uni.r2 <- long.mts.data.pool.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
mts.pool.uvpm.uni.reg <- data.frame(mts.pool.uvpm.uni.beta.p[ , c(1,2)], lapply(mts.pool.uvpm.uni.cis[ , c(2,3)], as.numeric), round(mts.pool.uvpm.uni.r2[ ,2], 5), mts.pool.uvpm.uni.beta.p[ , c(3,4)])
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
u10k.mts.pool.bc.uni.beta.p <- subset(long.mts.data.pool.stan, bc_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
u10k.mts.pool.bc.uni.cis <- subset(long.mts.data.pool.stan, bc_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
u10k.mts.pool.bc.uni.r2 <- subset(long.mts.data.pool.stan, bc_conc < mts.pool.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
u10k.mts.pool.bc.uni.reg <- data.frame(u10k.mts.pool.bc.uni.beta.p[ , c(1,2)], lapply(u10k.mts.pool.bc.uni.cis[ , c(2,3)], as.numeric), round(u10k.mts.pool.bc.uni.r2[ ,2], 5), u10k.mts.pool.bc.uni.beta.p[ , c(3,4)])
u10k.mts.pool.bc.uni.reg$Beta <- as.numeric(u10k.mts.pool.bc.uni.reg$Beta)
str(u10k.mts.pool.bc.uni.reg)
formattable(u10k.mts.pool.bc.uni.reg)

nrow(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
nrow(subset(u10k.mts.pool.bc.uni.reg, P.Value <= 0.05))
#there are 52 with the outlier and 42 without the outlier

formattable(subset(mts.pool.bc.uni.reg, P.Value <= 0.05))
formattable(subset(u10k.mts.pool.bc.uni.reg, P.Value <= 0.05))



#repeat for uvpm
u10k.mts.pool.uvpm.uni.beta.p <- subset(long.mts.data.pool.stan, uvpm_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
u10k.mts.pool.uvpm.uni.cis <- subset(long.mts.data.pool.stan, uvpm_conc < mts.pool.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
u10k.mts.pool.uvpm.uni.r2 <- subset(long.mts.data.pool.stan, uvpm_conc < mts.pool.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
u10k.mts.pool.uvpm.uni.reg <- data.frame(u10k.mts.pool.uvpm.uni.beta.p[ , c(1,2)], lapply(u10k.mts.pool.uvpm.uni.cis[ , c(2,3)], as.numeric), round(u10k.mts.pool.uvpm.uni.r2[ ,2], 5), u10k.mts.pool.uvpm.uni.beta.p[ , c(3,4)])
u10k.mts.pool.uvpm.uni.reg$Beta <- as.numeric(u10k.mts.pool.uvpm.uni.reg$Beta)
str(u10k.mts.pool.uvpm.uni.reg)
formattable(u10k.mts.pool.uvpm.uni.reg)

nrow(subset(mts.pool.uvpm.uni.reg, P.Value <= 0.05))
nrow(subset(u10k.mts.pool.uvpm.uni.reg, P.Value <= 0.05))
#there are 51 with the outlier and 31 without the outlier

formattable(subset(mts.pool.uvpm.uni.reg, P.Value <= 0.05))
formattable(subset(u10k.mts.pool.uvpm.uni.reg, P.Value <= 0.05))


write.csv(u10k.mts.pool.bc.uni.reg, file = "M_T_Pool_u10k_BC_Uni_Regressions.csv")
write.csv(u10k.mts.pool.uvpm.uni.reg, file = "M_T_Pool_u10k_UVPM_Uni_Regressions.csv")





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

#That one 40k point is driving a lot of stuff. Saved as 4k x 4k and then look at the image (file name MTL.s.bc.v.var.plot.loess.u5k.png)
nrow(subset(long.m.s.data.stan, bc_conc > 5000))
m.s.bc.xy.fit.plot.u4k <- ggplot(data = subset(long.m.s.data.stan, bc_conc < 4000), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal Summer Variables vs BC (4 outliers over 5,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTL.s.uvpm.v.var.plot.loess.u5k.png
m.s.uvpm.xy.fit.plot.u4k <- ggplot(data = subset(long.m.s.data.stan, uvpm_conc < 4000), aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal Summer Variables vs UVPM (4 outliers over 5,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#They all look pretty linear. Some have sparse data and outliers. 
#mjrd_200m is really weird. All the other mjrds have a slope, but that one is flat. 

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


#in case we want them on separate sheets. There were 121 rows for each variable when I wrote this, but now there are fewer. Check before running
#number of rws times 16 to make it a 4 x 4 grid of plots. Coulda probably made a loop for this, but I didn't. Need to learn loops. 
#####MTL S separate sheets
nnn <- nrow(m.s.data.stan)
ggplot(data = subset(long.m.s.data.stan[1:(nnn*16), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16+1):(nnn*16*2), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*2+1):(nnn*16*3), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*3+1):(nnn*16*4), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*4+1):(nnn*16*5), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*5+1):(nnn*16*6), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*6+1):(nnn*16*7), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*7+1):(nnn*16*8), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*8+1):(nnn*16*9), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="loess") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(nnn*16*9+1):nrow(long.m.s.data.stan), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="loess") +
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


#in case we want them on separate sheets. This was oroginally with a longer data set (100 rows), but now it is shorter, so the 1:1600 etc need to be changed
ggplot(data = subset(long.t.s.data.stan[1:1600, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[1601:3200, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[3201:4800, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[4801:6400, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[6401:8000, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[8001:9600, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[9601:11200, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[11201:12800, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[12801:14400, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="loess") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.s.data.stan[14401:14800, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="loess") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                  stat(r.squared), stat(p.value))), parse = TRUE)


# Pooled XY Fit Plots #######

########MTL plots, start with just scatter plots of each variable vs bc. Add fit lines and p values and R2
str(long.mts.data.pool.stan)
describe(long.mts.data.pool.stan$bc_conc)
#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name MTS.pool.bc.v.var.plot.loess.alldata.png)
mts.pool.bc.xy.fit.plot.alldata <- ggplot(data = long.mts.data.pool.stan, aes(x = value, y = bc_conc)) +
  ggtitle("Pooled Summer Variables vs BC") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTS.pool.uvpm.v.var.plot.loess.alldata.png
mts.pool.uvpm.xy.fit.plot.alldata <- ggplot(data = long.mts.data.pool.stan, aes(x = value, y = uvpm_conc)) +
  ggtitle("Pooled Summer Variables vs UVPM") +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


#That one 40k point might be driving a lot of stuff. Saved as 4k x 4k and then look at the image (file name MTS.pool.bc.v.var.plot.loess.u10k.png)
nrow(subset(long.mts.data.pool.stan, bc_conc > 10000))
mts.pool.bc.xy.fit.plot.u10k <- ggplot(data = subset(long.mts.data.pool.stan, bc_conc < 10000), aes(x = value, y = bc_conc)) +
  ggtitle("Montreal+Toronto Summer Pooled Variables vs BC (4 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#repeat for uvpm, saved as MTS.pool.uvpm.v.var.plot.loess.u10k.png
mts.pool.uvpm.xy.fit.plot.u10k <- ggplot(data = subset(long.mts.data.pool.stan, uvpm_conc < 10000), aes(x = value, y = uvpm_conc)) +
  ggtitle("Montreal+Toronto Summer Pooled Variables vs UVPM (4 outliers over 10,000 removed)") +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="loess") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)


#looks at the hists for NPRI_PM, NPRI_Nox, tot_traffic




# Predictor Selection ######
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
                             M_T_BC_u10k = ifelse(u10k.mts.pool.bc.uni.reg$P.Value < 0.05, round(u10k.mts.pool.bc.uni.reg$r.squared, 2), NA), 
                             M_T_UVPM = ifelse(mts.pool.uvpm.uni.reg$P.Value < 0.05, round(mts.pool.uvpm.uni.reg$r.squared, 2), NA),
                             M_T_UVPM_u10k = ifelse(u10k.mts.pool.uvpm.uni.reg$P.Value < 0.05, round(u10k.mts.pool.uvpm.uni.reg$r.squared, 2), NA),
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

#export the tables to the project folder so they can be called up by the RMD file. I'm exporting the raw files as csv, that'll be a bit more code in markdown, but porbablly better for control 






str(m.s.uni.var.sel)
str(filter(m.s.uni.var.sel, rowSums(is.na(m.s.uni.var.sel)) != 4))

formattable(m.w.uni.var.sel)
formattable(m.a.uni.var.sel)
formattable(to.uni.var.sel)
formattable(mts.pool.uni.var.sel)

filter(m.s.uni.var.sel, !is.na(m.s.uni.var.sel[ , 1]) && !is.na(m.s.uni.var.sel[ , 2]) && !is.na(m.s.uni.var.sel[ , 3]) && !is.na(m.s.uni.var.sel[ , 4]))


filter(m.s.uni.var.sel, sum(m.s.uni.var.sel[ ,1:4]) > 0)
str(m.s.uni.var.sel)
filter_at

outliers.3sd(t.s.data.stan$water_750m)

apply(t.s.data.stan[ , 5:ncol(t.s.data.stan)], 2, outliers.3sd)

length(which(abs(t.s.data.stan$water_750m) > 3))

m.s.determinants %>% ncol
t.s.determinants %>% ncol()

setdiff(colnames(m.s.determinants), colnames(t.s.determinants))
setdiff(colnames(t.s.determinants), colnames(m.s.determinants))

colnames(m.s.determinants[ ,1:5])
colnames(t.s.determinants[ ,1:5])

m.s.bc.uni.reg %>% nrow
m.s.uvpm.uni.reg

m.w.bc.uni.reg %>% nrow
m.w.uvpm.uni.reg 

m.a.bc.uni.reg %>% nrow
m.a.uvpm.uni.reg

to.bc.uni.reg %>% nrow
to.uvpm.uni.reg

mts.pool.bc.uni.reg %>% nrow
mts.pool.uvpm.uni.reg

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

# BIC ######

###trying bic.glm
str(m.s.data.stan)

bic.glm(data = m.s.data.stan, formula(bc_conc ~ . - bc_conc - uvpm_conc), glm.family = gaussian())
nrow(m.s.data.stan[ , 4:156])
length(m.s.data.stan$d_port)

bic.glm.formula(data = m.s.data.stan, f = m.s.data.stan$bc_conc ~ colnames(m.s.data.stan[ , 4:15]), famliy = gaussian())
bic.glm.formula(data = m.s.data.stan, f = m.s.data.stan$bc_conc ~ m.s.data.stan$d_port + m.s.data.stan$rail_200m + m.s.data.stan$rail_100m + m.s.data.stan$rail_50m, glm.family = gaussian())
traceback()
describe(m.s.data.stan$d_port)
describe(m.s.data.stan$bc_conc)

str(m.s.data.stan)

# Working Notes ########

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
