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

setwd("/Users/macbook/Documents/McGill School/Practicum/Montreal_Toronto_BC_Spatial_2019")

#data read n clean ####

########### Read in all the data
#columns Z and AA in the spread sheet are the outcomes of interest
m.t.s.outcomes.raw <- read.csv("Montreal and Toronto Spatial Study Summer Marshall Lloyd 13-05-2019 (1).csv")
#different columns in this one, I check it lower down in this code. 
m.w.outcomes.raw <- read_xlsx("Montreal winter spatial study filter database 13 May (1).xlsx")
str(m.w.outcomes.raw)
str(m.t.s.outcomes.raw)

m.s.determinants <- read_xlsx("Variable Data Montreal.xlsx", sheet = 2)
t.s.determinants <- read_xlsx("Variable Data Toronto.xlsx", sheet = 2)

#this is an explanation of each of the determinants
det.legend <- read_xlsx("Variable Data Montreal.xlsx", sheet = 1)


#######inspection
#compare headings fromt the two outcomes, this is FYI for incorporating the winter data into the summer data. Note: cbind repeats entries of shorter vector
cbind(colnames(m.t.s.outcomes.raw), colnames(m.w.outcomes.raw))
#for m.t.s.outcomes, I care about columns 1, 26, and 27 
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
nrow(t.determinants)
#100
setdiff(as.character(subset(m.t.s.outcomes.raw, city == "Toronto")$filter), as.character(t.determinants$Filter_ID))
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

as.numeric(m.s.outcomes[ , 27])
as.numeric(as.character(m.s.outcomes[ , 27]))
m.s.data <- data.frame(f.id = m.s.determinants$Filter_ID, bc_conc = as.numeric(m.s.outcomes[ , 26]), uvpm_conc = as.numeric(as.character(m.s.outcomes[ , 27])), m.s.determinants[ , -1])

#just checking to make sure they are the correct length
length(m.s.determinants$Filter_ID)
length(as.numeric(m.s.outcomes[ , 26]))
length(as.numeric(as.character(m.s.outcomes[ , 27])))
nrow(m.s.determinants[ , -1])
head(m.s.data)
str(m.s.data)

#and for toronto. Recall that it is mtl winter that is different, so 26 and 27 are still the correct columns. 
head(t.s.outcomes)
str(t.s.outcomes)
as.numeric(t.s.outcomes[ , 27])
as.numeric(as.character(t.s.outcomes[ , 27]))
#i'll keep them separate for the unpooled unis. I'm sure I could fancy code this, but not tonight cowboy. Will mash togther for the pooled. 
t.data <- data.frame(f.id = t.s.determinants$Filter_ID, bc_conc = as.numeric(t.s.outcomes[ , 26]), uvpm_conc = as.numeric(as.character(t.s.outcomes[ , 27])), t.s.determinants[ , -1])

#just checking to make sure they are the correct length
length(t.s.determinants$Filter_ID)
length(as.numeric(t.s.outcomes[ , 26]))
length(as.numeric(as.character(t.s.outcomes[ , 27])))
nrow(t.s.determinants[ , -1])
head(t.data)
str(t.data)

# MTL Histograms ####

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
#One out at 30k (MTL_space_23). Looks normallish under 3000. 
#another little clump out at ~8000, note that TO has many values under 10k. That's not an unreasonable amount of bc
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





#TO Histograms ####
#take a quick gander at some of the distributions
hist(t.data$bc_conc, breaks = 100)
hist(t.data$bc_conc, breaks = 100, xlim = c(0, 8000))
#One out at 40k (XXX). Looks normallish under 8000, but of a right tail

hist(t.data$uvpm_conc, breaks = 100)
hist(t.data$uvpm_conc, breaks = 100, xlim = c(0, 8000))
#One out at 35k (). Looks normallish under 7000, bit of a right tail
describe(t.data$uvpm_conc)
describe(t.data$bc_conc)


hist(t.data$build_1000m)
hist(t.data$build_750m)
hist(t.data$build_500m)
hist(t.data$build_300m)
hist(t.data$build_200m)
hist(t.data$build_100m)
hist(t.data$build_50m)
#looks like linear decline for 1000m, then looks more and more like -exp()

hist(t.data$com_1000m)
hist(t.data$com_1000m, breaks = 100, xlim = c(10000, 500000), ylim = c(0, 10))
hist(t.data$com_500m)
hist(t.data$com_50m)
#a lot of small values

hist(t.data$gov_1000m)
hist(t.data$gov_500m)
hist(t.data$gov_50m)
# a lot of small values...mkay, they are all looking pretty similar. Assume that no comment means a lot of small values

hist(t.data$open_1000m)
hist(t.data$open_500m)
hist(t.data$open_50m)

hist(t.data$resid_1000m)
hist(t.data$resid_750m)
hist(t.data$resid_500m)
hist(t.data$resid_300m)
hist(t.data$resid_200m)
hist(t.data$resid_100m)
hist(t.data$resid_50m)
#more high values. The 1000m looks normalish. The 50 m is mostly large values, but has a lil chunk of very small

hist(t.data$ind_1000m)
hist(t.data$ind_500m)
hist(t.data$ind_50m)

hist(t.data$water_1000m)
hist(t.data$water_500m)
hist(t.data$water_50m)

hist(t.data$highway_1000m)
hist(t.data$highway_750m)
hist(t.data$highway_500m)
hist(t.data$highway_50m)

hist(t.data$majrd_1000m)
hist(t.data$mjrd_750m)
hist(t.data$mjrd_500m)
hist(t.data$mjrd_300m)
hist(t.data$mjrd_50m)
#more evenly distributed. Still a sharp decline

hist(t.data$road_1000m)
hist(t.data$road_750m)
hist(t.data$road_500m)
hist(t.data$road_300m)
hist(t.data$road_200m)
hist(t.data$road_100m)
hist(t.data$road_50m)
#closer to looking normal. Makes sense, there are roads in cities.....The MTL of this had more large numbers (I think)

hist(t.data$d_highway)

hist(t.data$d_majrd)

hist(t.data$bus_1000m)
hist(t.data$bus_750m)
hist(t.data$bus_500m)
hist(t.data$bus_300m)
hist(t.data$bus_200m)
hist(t.data$bus_100m)
hist(t.data$bus_50m)
#the peak is in the middle for 1000m (was at larger values for MTL) and slowly moves down (MTL was nearly all at zero/low numbers for 300m and below)

hist(t.data$bus_stop_1000m)
hist(t.data$bus_stop_750m)
hist(t.data$bus_stop_500m)
hist(t.data$bus_stop_300m)
hist(t.data$bus_stop_200m)
hist(t.data$bus_stop_100m)
#mostly small values, not like MTL at all
#MTL was: more normal looking until 300m, though that is still a straight decline. 200m and 100m are mostly low values. 
#note there is no bus_stop_50m

hist(t.data$inter_1000m)
hist(t.data$inter_750m)
hist(t.data$inter_500m)
hist(t.data$inter_300m)
hist(t.data$inter_200m)
hist(t.data$inter_100m)
hist(t.data$inter_50m)
#pretty normal looking until 200m. Then typical shape. 
#MTL had: Note that for 50 m, there are only 3 values (0,1,2), which makes sense

hist(t.data$traffic_1000m)
hist(t.data$traffic_500m)
hist(t.data$traffic_50m)

hist(t.data$tot_traffic_1000m)
hist(t.data$tot_traffic_750m)
hist(t.data$tot_traffic_50m)

hist(t.data$Nox_1000m)
hist(t.data$Nox_750m)
hist(t.data$Nox_500m)
hist(t.data$Nox_50m)

hist(t.data$tot_Nox_1000m)
hist(t.data$tot_Nox_500m)
hist(t.data$tot_Nox_50m)

hist(t.data$NPRI_PM25_1000m)
hist(t.data$NPRI_PM_750m)
hist(t.data$NPRI_PM_500m)
hist(t.data$NPRI_PM_300m)
hist(t.data$NPRI_PM_200m)
hist(t.data$NPRI_PM_100m)
hist(t.data$NPRI_PM_50m)
#this is number of chimneys. It's an integer. Most are zero, a couple of 1s. Only 1000m has anything over 2 (and not much)

hist(t.data$NPRI_Nox_1000m)
hist(t.data$NPRI_Nox_750m)
hist(t.data$NPRI_Nox_500m)
hist(t.data$NPRI_Nox_300m)
hist(t.data$NPRI_Nox_200m)
hist(t.data$NPRI_Nox_100m)
#this is number of chimneys, integer. Most are zero. Fewer than PM chimneys. Missing NPRI_Nox_50m. 300, 200, and 100 are completely zero.


hist(t.data$d_NPRI_Nox)
#Chunky
#MTL:normalish is a long right tail

hist(t.data$d_NPRI_PM)
#normalish is a long right tail

hist(t.data$d_airport)
#normal, bit of a tail

hist(t.data$d_railline)

hist(t.data$d_port)
#chunky normal, MtL might be more normal looking

hist(t.data$d_shore)

hist(t.data$pop_1000m)
hist(t.data$pop_750m)
hist(t.data$pop_500m)
#missing pop_300m, 200m, 100m, and 50m

hist(t.data$rail_1000m)
hist(t.data$rail_500m)
hist(t.data$rail_50m)

###end TO histograms


# Standardizing ####
###MTL
#time to standardize it
#this is how to standardize just part of the data frame
colnames(m.s.data)[c(-1,-2,-3)]
summary(scale(m.s.data[ , c(-1,-2,-3)]))
m.s.data.stan <- data.frame(m.s.data[ , 1:3], scale(m.s.data[ , c(-1,-2,-3)]))
summary(m.s.data.stan)
#can see in the summary that all the means are zero
apply(m.s.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!

#take a look at that one observation that has huge values for bc and uvpm
formattable(subset(m.s.data.stan, f.id == "MTL_space_23"))



str(m.s.data.stan)
long.m.s.data.stan <- melt(m.s.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc"))
str(long.m.s.data.stan)
#had this tibble conversion in there but I don't know why. It makes things bad if I do it.....#out for now
#m.s.data.stan <- as_tibble(m.s.data.stan)


###TO
#time to standardize it
#this is how to standardize just part of the data frame
colnames(t.data)[c(-1,-2,-3)]
summary(scale(t.data[ , c(-1,-2,-3)]))
t.data.stan <- data.frame(t.data[ , 1:3], scale(t.data[ , c(-1,-2,-3)]))
summary(t.data.stan)
#can see in the summary that all the means are zero
apply(t.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!

#take a look at that one MTL observation that has huge values for bc and uvpm
formattable(subset(m.s.data.stan, f.id == "MTL_space_23"))
#same for TO
which.max(t.data$bc_conc)
t.data[97, 2]
formattable(t.data.stan[97, ])
formattable(t.data[97, ])
#The TO max has some NaNs for NPRI_Nox_
describe(t.data$NPRI_Nox_300m)
describe(t.data$NPRI_Nox_200m)
describe(t.data$NPRI_Nox_100m)
summary(t.data)
#NPRI_NOx_300m, 200m, and 100m are entirely zero, need to do something about it to make sure it doesn't scale them. Actually, don't need em at all! Take em out!
t.data.stan <- subset(t.data.stan, select = -c(NPRI_Nox_300m, NPRI_Nox_200m, NPRI_Nox_100m))
summary(t.data.stan)
#looks like TO data is all good now. Note that I only modified the standardized data frame. If I do stuff with the non-stan, may consider removing those variables there too

# MTL Uni Regressions #####

#####MTL UNI Regressions
str(m.s.data.stan)
long.m.s.data.stan <- melt(m.s.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc"))
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

write.csv(m.s.bc.uni.reg, file = "BC_Uni_Regressions.csv")
write.csv(m.s.uvpm.uni.reg, file = "UVPM_Uni_Regressions.csv")
write.csv(m.s.data, file = "montreal_data.csv")
write.csv(m.s.data.stan, file = "montreal_standardized_data.csv")

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

#MTL Uni Reg -Outliers #####

#based on the graphs below, we see that there is between 1 to 5 outliers that really drive the fits. I'll take a quick look at them to see what's going on. 
#They may be bad data points depending on the volumes used or time run. Or maybe they are points that we don't want in our model (eg: right on a train, we want to describe city living, not train living) 
describe(subset(m.s.outcomes, BC_ng_m3 > 5000))
formattable(subset(m.s.outcomes, Monitor_type == "Harvard" & BC_ng_m3 > 5000))
formattable(subset(m.s.outcomes, Monitor_type == "UPAS" & BC_ng_m3 > 5000))

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
str(subset(m.s.outcomes, BC_ng_m3 > 5000)$filter)
subset(m.s.outcomes, BC_ng_m3 > 5000 & BC_ng_m3 < 10000)$filter
subset(long.m.data.stan, bc_conc < 5000)

#do unis without all 5 outliers
#do simple regressions on bc_conc for each of the determinants

m.s.outlier.level <- 5000
m.s.u5k.bc.uni.beta.p <- subset(long.m.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
m.s.u5k.bc.uni.cis <- subset(long.m.data.stan, bc_conc < m.s.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
m.s.u5k.bc.uni.r2 <- subset(long.m.data.stan, bc_conc < m.s.outlier.level) %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

#put em together 
m.s.u5k.bc.uni.reg <- data.frame(m.s.u5k.bc.uni.beta.p[ , c(1,2)], lapply(m.s.u5k.bc.uni.cis[ , c(2,3)], as.numeric), round(m.s.u5k.bc.uni.r2[ ,2], 5), m.s.u5k.bc.uni.beta.p[ , c(3,4)])
m.s.u5k.bc.uni.reg$Beta <- as.numeric(m.s.u5k.bc.uni.reg$Beta)

nrow(subset(m.s.u5k.bc.uni.reg, P.Value <= 0.05))
nrow(subset(m.s.bc.uni.reg, P.Value <= 0.05))
#4 with all data, 5 with 1 30k outlier cut out, and 44 with 5 over 5k outliers cut out

formattable(m.s.u5k.bc.uni.reg)
formattable(subset(m.s.u5k.bc.uni.reg, P.Value <= 0.05))
#could do this all again for uvpm, but I'll wait until I hear back from Susannah re outliers





#TO Uni Regressions #####

#####TO UNI Regressions
#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#this first step makes the wide data long to set up the code further down
str(t.data.stan)
long.t.data.stan <- melt(t.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc"))
str(long.t.data.stan)

#check to make sure nothing is too squirrelly
summary(long.t.data.stan)
nrow(long.t.data.stan)/nrow(t.data.stan)
str(t.data.stan)
#we had 100 rows for 151 columns, all but 3 got staked underneath. We should now have a df that is 148 times longer. CHECK!

slice(long.t.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.t.data.stan, 200:210)

####Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution
#https://stackoverflow.com/questions/51567914/hundreds-of-linear-regressions-that-run-by-group-in-r

#do simple regressions on bc_conc for each of the determinants
to.bc.uni.beta.p <- long.t.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
to.bc.uni.cis <- long.t.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
to.bc.uni.r2 <- long.t.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

options(scipen=999)
#put em together 
to.bc.uni.reg <- data.frame(to.bc.uni.beta.p[ , c(1,2)], lapply(to.bc.uni.cis[ , c(2,3)], as.numeric), round(to.bc.uni.r2[ ,2], 5), to.bc.uni.beta.p[ , c(3,4)])
to.bc.uni.reg$Beta <- as.numeric(to.bc.uni.reg$Beta)
str(to.bc.uni.reg)



#do simple regressions on uvpm_conc for each of the determinants
to.uvpm.uni.beta.p <- long.t.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
to.uvpm.uni.cis <- long.t.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
to.uvpm.uni.r2 <- long.t.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 

to.uvpm.uni.reg <- data.frame(to.uvpm.uni.beta.p[ , c(1,2)], lapply(to.uvpm.uni.cis[ , c(2,3)], as.numeric), to.uvpm.uni.r2[ ,2], to.uvpm.uni.beta.p[ , c(3,4)])
to.uvpm.uni.reg$Beta <- as.numeric(to.uvpm.uni.reg$Beta)

write.csv(to.bc.uni.reg, file = "TO_BC_Uni_Regressions.csv")
write.csv(to.uvpm.uni.reg, file = "TO_UVPM_Uni_Regressions.csv")
write.csv(t.data, file = "toronto_data.csv")
write.csv(t.data.stan, file = "toronto_standardized_data.csv")

formattable(to.bc.uni.reg)
formattable(to.uvpm.uni.reg)

nrow(subset(to.bc.uni.reg, P.Value <= 0.05))
#aawwwwww yeah!

#note: no bus_stop_50m category; it doesn't look like bus_stop was getting there (bus_stop_100m p = 0.742)
#note:  Missing NPRI_Nox_100m and 50m; it doesn't look like the NPRI_Nox that are there are heading towards significance (NPRI_Nox_200m p = 0.760, though the 200m and 300m are the same. Is that a result of data source?)
#note: missing pop_300m, 200m, 100m, and 50m; it doesn't look like pop_ is heading to sig (pop_500m p = 0.669)

#I saw two of the NPRI_Nox had same values in the automated regression. Wanted to make sure the automation worked correctly
summary(lm(data = t.data.stan, bc_conc ~ NPRI_Nox_300m))
summary(lm(data = t.data.stan, bc_conc ~ NPRI_Nox_200m))
#all the values are the same
all_equal(t.data.stan$NPRI_Nox_300m, t.data.stan$NPRI_Nox_200m)
all_equal(t.data$NPRI_Nox_300m, t.data$NPRI_Nox_200m)

#check one of each to make sure it worked
summary(lm(data = t.data.stan, formula = bc_conc ~ rail_200m))
summary(lm(data = t.data.stan, formula = uvpm_conc ~ pop_500m))
#tried to figure out how to get more info out of the regressions. Not sure if I need more, but curious. The code turns the regression into a tibble like this:
tidy(lm(data = t.data.stan, formula = uvpm_conc ~ pop_500m))
tidy(confint(lm(data = t.data.stan, formula = uvpm_conc ~ pop_500m)))

#TO Uni Reg - Outliers ######

#The toronto regressions seem to run semi okay even with this outlier, but we can take a look to see what happens when we take it out. 
describe(subset(t.s.outcomes, BC_ng_m3 > 10000))
#it's TO_space_94, it's a UPAS, it ran for 10 days
t.outlier.level<- 10000
u10k.to.bc.uni.beta.p <- subset(long.t.data.stan, bc_conc < t.outlier.level) %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
u10k.to.bc.uni.cis <- subset(long.t.data.stan, bc_conc < t.outlier.level)  %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
u10k.to.bc.uni.r2 <- subset(long.t.data.stan, bc_conc < t.outlier.level)  %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)


#put em together 
u10k.to.bc.uni.reg <- data.frame(u10k.to.bc.uni.beta.p[ , c(1,2)], lapply(u10k.to.bc.uni.cis[ , c(2,3)], as.numeric), round(u10k.to.bc.uni.r2[ ,2], 5), u10k.to.bc.uni.beta.p[ , c(3,4)])
u10k.to.bc.uni.reg$Beta <- as.numeric(u10k.to.bc.uni.reg$Beta)
str(u10k.to.bc.uni.reg)

formattable(u10k.to.bc.uni.reg)
nrow(subset(to.bc.uni.reg, P.Value <= 0.05))
nrow(subset(u10k.to.bc.uni.reg, P.Value <= 0.05))
#removing the outlier takes it from 61 pvals, ot 70 pvals

#MTL XY Fit Plots######

########MTL plots, start with just scatter plots of each variable vs bc. Add fit lines and p values and R2
str(long.m.s.data.stan)
#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name t.bc.xy.plot.alldata.png)
m.bc.xy.fit.plot.alldata <- ggplot(data = long.m.s.data.stan, aes(x = value, y = bc_conc)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#That one 40k point is driving a lot of stuff. Saved as 4k x 4k and then look at the image (file name t.bc.xy.plot.u40k.png)
nrow(subset(long.m.s.data.stan, bc_conc < 10000))
m.bc.xy.fit.plot.u10k <- ggplot(data = subset(long.m.s.data.stan, bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

#They all look pretty linear. Some have sparse data and outliers. 
#mjrd_200m is really weird. All the other mjrds have a slope, but that one is flat. 

#in case we want them on separate sheets. There are 121 rows for each variable
nrow(m.s.data.stan)
ggplot(data = subset(long.m.s.data.stan[1:(121*16), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16+1):(121*16*2), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*2+1):(121*16*3), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*3+1):(121*16*4), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*4+1):(121*16*5), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*5+1):(121*16*6), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*6+1):(121*16*7), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*7+1):(121*16*8), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*8+1):(121*16*9), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="lm") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.m.s.data.stan[(121*16*9+1):nrow(long.m.s.data.stan), ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="lm") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)





# TO XY Fit Plots #####

########TO plots, start with just scatter plots of each variable vs bc. Add fit lines and p values and R2
str(long.t.data.stan)
#super slow, but good. Only slow when I display the ggplot, entering as below is fast. Saved as 4k x 4k and then look at the image (file name t.bc.xy.plot.alldata.png)
t.bc.xy.fit.plot.alldata <- ggplot(data = long.t.data.stan, aes(x = value, y = bc_conc)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#that one 30k point is near zero for all determinants except rail. That's what's driving everything. 

#Remove 1 outlier. Saved as 4k x 4k and then look at the image (file name t.bc.xy.plot.u10k.png)
nrow(subset(long.t.data.stan, bc_conc < 10000))
t.bc.xy.fit.plot.u10k <- ggplot(data = subset(long.t.data.stan, bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)
#a little easier to see the spread of the data witht he 1 outlier removed. As noted in an earlier section, removal of the outlier gives 9 more pvals under 0.05

#in case we want them on separate sheets
ggplot(data = subset(long.t.data.stan[1:1600, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[1601:3200, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[3201:4800, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[4801:6400, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[6401:8000, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[8001:9600, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[9601:11200, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[11201:12800, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method="lm") + 
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[12801:14400, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="lm") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                                      stat(r.squared), stat(p.value))), parse = TRUE)

ggplot(data = subset(long.t.data.stan[14401:14800, ], bc_conc < 10000), aes(x = value, y = bc_conc)) +
  geom_point() + 
  facet_wrap(~ variable, scales = "free") +
  geom_smooth(method="lm") +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2f',
                  stat(r.squared), stat(p.value))), parse = TRUE)


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

#next work
#ask the questions
#keep going on the Toronto data. Should be able to do that

#regression list:
#mtl summer YES
#to summer YES
#combined summer YES
#mtl summer log YES
#to summer log YES  
#combined summer log YES
#mtl winter log Qs
#mtl average log Qs
#mtl winter Qs
#mtl average Qs
#to winter NOT A THING, no data
#to average, NOT A THING due to no to winter data
#to winter log NOT A THING, no data
#to average, log NOT A THING due to no to winter data



