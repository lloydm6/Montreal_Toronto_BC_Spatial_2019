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
as.numeric(m.outcomes[ , 27])
as.numeric(as.character(m.outcomes[ , 27]))
m.data <- data.frame(f.id = m.determinants$Filter_ID, bc_conc = m.outcomes[ , 26], uvpm_conc = as.numeric(as.character(m.outcomes[ , 27])), m.determinants[ , -1])
head(m.data)
str(m.data)
#note that uvpm_conc was a factor and I changed to numeric. Blank and "too dark" entries are now NA.
#note: majrd and mjrd are same categories
#note: no bus_stop_50m category
#Note: NPRI_PM25_1000m vs NPRI_PM_750m
#note:  Missing NPRI_Nox_100m and 50m
#note: missing pop_300m, 200m, 100m, and 50m
#note: MTL_space_23 has a huge BC and UVPM values
#what other info from the outcomes should I keep in the data frame?

formattable(subset(m.data.stan, f.id == "MTL_space_23"))

#take a quick gander at some of the distributions
hist(m.data$bc_conc, breaks = 100)
hist(m.data$bc_conc, breaks = 100, xlim = c(0, 3000))
#One out at 30k (MTL_space_23). Looks normallish under 3000. 

hist(m.data$uvpm_conc, breaks = 100)
hist(m.data$bc_conc, breaks = 100, xlim = c(0, 3000))
#One out at 22k (MTL_space_23). Looks normallish under 3000. 
describe(m.data$uvpm_conc)
describe(m.data$bc_conc)


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

colnames(m.data)[c(-1,-2,-3)]

#time to standardize it
#this is how to standardize just part of the data frame
summary(scale(m.data[ , c(-1,-2,-3)]))
m.data.stan <- data.frame(m.data[ , 1:3], scale(m.data[ , c(-1,-2,-3)]))
summary(m.data.stan)
#can see in the summary that all the means are zero
apply(m.data.stan, 2, sd)
#can see all the sds are 1, nnnnnnnoice!




str(m.data.stan)
long.m.data.stan <- melt(m.data.stan, id.vars = c("f.id", "bc_conc", "uvpm_conc"))
str(long.m.data.stan)
#had this tibble conversion in there but I don't know why. It makes things bad if I do it.....#out for now
#m.data.stan <- as_tibble(m.data.stan)

#trying out this: https://datascienceplus.com/how-to-do-regression-analysis-for-multiple-independent-or-dependent-variables/
#check to make sure nothing is too squirrelly
summary(long.m.data.stan)
nrow(long.m.data.stan)/nrow(m.data.stan)
str(m.determinants)
#determinants had 154 columns, filter_id and 153 others. Now the data frame is 153 times longer, I think we're good to go
slice(long.m.data.stan, 1:10)
#slice is basially head, but you can pick where to look
slice(long.m.data.stan, 200:210)

####Univariate regressions. Found various code that will run all the univariate at once, but each one gives diferent outputs. I'm just going to frankenstein them together instead of finding an elegant solution
#https://stackoverflow.com/questions/51567914/hundreds-of-linear-regressions-that-run-by-group-in-r

#do simple regressions on bc_conc for each of the determinants
bc.uni.beta.p <- long.m.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(bc_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#to get CIs
bc.uni.cis <- long.m.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(bc_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#get the R2
bc.uni.r2 <- long.m.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(bc_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared)

options(scipen=999)
#put em together 
bc.uni.reg <- data.frame(bc.uni.beta.p[ , c(1,2)], lapply(bc.uni.cis[ , c(2,3)], as.numeric), round(bc.uni.r2[ ,2], 5), bc.uni.beta.p[ , c(3,4)])
bc.uni.reg$Beta <- as.numeric(bc.uni.reg$Beta)
str(bc.uni.reg)



#do simple regressions on uvpm_conc for each of the determinants
uvpm.uni.beta.p <- long.m.data.stan %>%
  group_by(variable) %>%
  do(tidy(lm(uvpm_conc ~ value, .))) %>%
  filter(term == "value") %>%
  mutate(Beta = as.character(round(estimate, 2)), "P Value" = round(p.value, 3), SE = round(std.error, 1)) %>% 
  select(Beta, SE, "P Value") %>% 
  as.data.frame()
#uvpm CIs
uvpm.uni.cis <- long.m.data.stan %>%
  group_by(variable) %>%
  do(tidy(confint(lm(uvpm_conc ~ value, .)))) %>%
  filter(.rownames == "value") %>%
  mutate("2.5%" = as.character(round(X2.5.., 2)), "97.5%" = as.character(round(X97.5.., 2))) %>% 
  select("2.5%", "97.5%") %>% 
  as.data.frame()
#uvpm R2
uvpm.uni.r2 <- long.m.data.stan %>% 
  nest(-variable) %>% 
  mutate(fit = map(data, ~ lm(uvpm_conc ~ value, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(variable, r.squared) 

uvpm.uni.reg <- data.frame(uvpm.uni.beta.p[ , c(1,2)], lapply(uvpm.uni.cis[ , c(2,3)], as.numeric), uvpm.uni.r2[ ,2], uvpm.uni.beta.p[ , c(3,4)])
uvpm.uni.reg$Beta <- as.numeric(uvpm.uni.reg$Beta)

write.csv(bc.uni.reg, file = "BC_Uni_Regressions.csv")
write.csv(uvpm.uni.reg, file = "UVPM_Uni_Regressions.csv")

formattable(bc.uni.reg)
formattable(uvpm.uni.reg)

#check one of each to make sure it worked
summary(lm(data = m.data.stan, formula = bc_conc ~ rail_200m))
summary(lm(data = m.data.stan, formula = uvpm_conc ~ pop_500m))
#tried to figure out how to get more info out of the regressions. Not sure if I need more, but curious. The code turns the regression into a tibble like this:
tidy(lm(data = m.data.stan, formula = uvpm_conc ~ pop_500m))
tidy(confint(lm(data = m.data.stan, formula = uvpm_conc ~ pop_500m)))

str(m.data.stan)

bic.glm.formula(data = m.data.stan, f = m.data.stan$bc_conc ~ . - bc_conc - uvpm_conc, family = "Gaussian")

