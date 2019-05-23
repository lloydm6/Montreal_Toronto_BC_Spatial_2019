library(readxl)
library(ggplot2)
library(Hmisc)
library(dplyr)

setwd("/Users/macbook/Documents/McGill School/Practicum/Montreal_Toronto_BC_Spatial_2019")

#columns Z and AA in the spread sheet are the outcomes of interest
m.t.outcomes <- read.csv("Montreal and Toronto Spatial Study Summer Marshall Lloyd 13-05-2019 (1).csv")

m.determinants <- read_xlsx("Variable Data Montreal.xlsx", sheet = 2)
t.determinants <- read_xlsx("Variable Data Toronto.xlsx", sheet = 2)

#this is an explanation of each of the determinants
det.legend <- read_xlsx("Variable Data Montreal.xlsx", sheet = 1)

str(m.determinants)
#121 obs of 154 variables

str(t.determinants)
#100 observations of 152 variabales

str(m.t.outcomes)
#275 observations o 30 variables

#create a data frame with just montreal determinants and outcomes. First figure out which filters are used 
str(m.t.outcomes$filter)

str(m.determinants$Filter_ID)

m.determinants$Filter_ID <- as.factor(m.determinants$Filter_ID)

#I saw that m.dets only had MTL_space labelled filter, so keep only those ones for m.outcomes
m.outcomes <- filter(m.t.outcomes, grepl('MTL_space', filter))
nrow(m.outcomes)
length(m.outcomes$filter)
length(m.determinants$Filter_ID)

#there are still some MTL_space labelled filters that weren't used, need to figre out which ones and remmove them
m.outcomes <- filter(m.outcomes, !(filter %in% setdiff(m.outcomes$filter, m.determinants$Filter_ID)))
nrow(m.outcomes)
length(m.outcomes$filter)
length(m.determinants$Filter_ID)
#looking good! for numbers, but there are more leves of fators for outcomes. I'll try to reset it.

str(m.outcomes$filter)
str(as.factor(as.character(m.outcomes$filter)))
m.outcomes$filter <- as.factor(as.character(m.outcomes$filter))

m.outcomes$filter == m.determinants$Filter_ID
#they are all the same and in the same order, I can mush them together. 

m.data <- data.frame(f.id = m.determinants$Filter_ID, bc_conc = m.outcomes[ , 26], uvpm_conc = m.outcomes[ , 27], m.determinants[ , -1])
m.data
str(m.data)
#note that uvpm_conc is a factor because it has some words in there. I'll sort that out later
#note: majrd and mjrd are same categories
#note: no bus_stop_50m category
#Note: NPRI_PM25_1000m vs NPRI_PM_750m
#note:  Missing NPRI_Nox_100m and 50m
#note: missing pop_300m, 200m, 100m, and 50m

#standardizing the determinants/predictor variables.
m.data.stan <- m.data

#take a quick gander at some of the distributions
hist(m.data$bc_conc, breaks = 100)
hist(m.data$bc_conc, breaks = 100, xlim = c(0, 3000))
#some really big values, even some out at 30k. Looks normallish under 3000

hist(m.data$build_1000m)
hist(m.data$build_750m)
hist(m.data$build_500m)
hist(m.data$build_300m)
hist(m.data$build_200m)
hist(m.data$build_100m)
hist(m.data$build_50m)
#looks like linear decline for 1000m, then looks more and more like -exp()

hist(m.data$com_1000m)
hist(m.data$com_1000m, breaks = 100, xlim = c(10000, 500000), ylim = c(0, 10))
hist(m.data$com_500m)
hist(m.data$com_50m)
#a lot of small values

hist(m.data$gov_1000m)
hist(m.data$gov_500m)
hist(m.data$gov_50m)
# a lot of small values...mkay, they are all looking pretty similar. Assume that no comment means a lot of small values

hist(m.data$open_1000m)
hist(m.data$open_500m)
hist(m.data$open_50m)

hist(m.data$resid_1000m)
hist(m.data$resid_750m)
hist(m.data$resid_500m)
hist(m.data$resid_300m)
hist(m.data$resid_200m)
hist(m.data$resid_100m)
hist(m.data$resid_50m)
#more high values. The 1000m looks normalish. The 50 m is mostly large values, but has a nice chunk of very small

hist(m.data$ind_1000m)
hist(m.data$ind_500m)
hist(m.data$ind_50m)

hist(m.data$water_1000m)
hist(m.data$water_500m)
hist(m.data$water_50m)

hist(m.data$highway_1000m)
hist(m.data$highway_750m)
hist(m.data$highway_500m)
hist(m.data$highway_50m)

hist(m.data$majrd_1000m)
hist(m.data$mjrd_750m)
hist(m.data$mjrd_500m)
hist(m.data$mjrd_300m)
hist(m.data$mjrd_50m)
#more evenly distributed. 

hist(m.data$road_1000m)
hist(m.data$road_750m)
hist(m.data$road_500m)
hist(m.data$road_300m)
hist(m.data$road_200m)
hist(m.data$road_100m)
hist(m.data$road_50m)
#more highr numbers, closer to looking normal. Makes sense, there are roads in cities....

hist(m.data$d_highway)

hist(m.data$d_majrd)

hist(m.data$bus_1000m)
hist(m.data$bus_750m)
hist(m.data$bus_500m)
hist(m.data$bus_300m)
hist(m.data$bus_200m)
hist(m.data$bus_100m)
hist(m.data$bus_50m)
#the peak is at larger values for 1000m and slowly moves down until it is at zero/low numbers for 300m

hist(m.data$bus_stop_1000m)
hist(m.data$bus_stop_750m)
hist(m.data$bus_stop_500m)
hist(m.data$bus_stop_300m)
hist(m.data$bus_stop_200m)
hist(m.data$bus_stop_100m)
#more normal looking until 300m, though that is still a straight decline. 200m and 100m are mostly low values. 
#note there is no bus_stop_50m

hist(m.data$inter_1000m)
hist(m.data$inter_750m)
hist(m.data$inter_500m)
hist(m.data$inter_300m)
hist(m.data$inter_200m)
hist(m.data$inter_100m)
hist(m.data$inter_50m)
#pretty normal looking until 200m. Then typical shape. Note that for 50 m, there are only 3 values (0,1,2), which makes sense

hist(m.data$traffic_1000m)
hist(m.data$traffic_500m)
hist(m.data$traffic_50m)

hist(m.data$tot_traffic_1000m)
hist(m.data$tot_traffic_750m)
hist(m.data$tot_traffic_50m)

hist(m.data$Nox_1000m)
hist(m.data$Nox_750m)
hist(m.data$Nox_500m)
hist(m.data$Nox_50m)

hist(m.data$tot_Nox_1000m)
hist(m.data$tot_Nox_500m)
hist(m.data$tot_Nox_50m)

hist(m.data$NPRI_PM25_1000m)
hist(m.data$NPRI_PM_750m)
hist(m.data$NPRI_PM_500m)
hist(m.data$NPRI_PM_300m)
hist(m.data$NPRI_PM_200m)
hist(m.data$NPRI_PM_100m)
hist(m.data$NPRI_PM_50m)
#this is number of chimneys. It's an integer. Most are zero, a couple of 1s. Only 1000m has anything over 2 (and not much)

hist(m.data$NPRI_Nox_1000m)
hist(m.data$NPRI_Nox_750m)
hist(m.data$NPRI_Nox_500m)
hist(m.data$NPRI_Nox_200m)
#this is number of chimneys, integer. Most are zero. Fewer than PM chimneys. Missing NPRI_Nox_100m and 50m

hist(m.data$d_NPRI_Nox)
#normalish is a long right tail

hist(m.data$d_NPRI_PM)
#normalish is a long right tail

hist(m.data$d_airport)
#normal, bit of a tail

hist(m.data$d_railline)

hist(m.data$d_port)
#normal

hist(m.data$d_shore)

hist(m.data$pop_1000m)
hist(m.data$pop_750m)
hist(m.data$pop_500m)
#missing pop_300m, 200m, 100m, and 50m

hist(m.data$rail_1000m)
hist(m.data$rail_500m)
hist(m.data$rail_50m)
