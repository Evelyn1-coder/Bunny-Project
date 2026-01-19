library(readxl)
library(ggplot2)
library(lubridate)
library(stringr)
library(dplyr)

bnuy <- read_excel("~/Downloads/pone.0300633.s001.xlsx")
bnuy$datein = as.Date(bnuy$`Source Date`)
bnuy$update = as.Date(bnuy$`Status Date`)
col_names <- names(bnuy)


bnuy$timediff = difftime(bnuy$datein, bnuy$update, units = "days")
bnuy$adopted = if_else (bnuy$Status == "Adopted"|bnuy$Status == "Adopted Altered"|bnuy$Status =="Adopted Unaltered", 1, 0)

bnuy$timediff = bnuy$timediff*bnuy$adopted
bnuy$timediff[bnuy$timediff == 0] = NA
bnuy$timediff = bnuy$timediff*(-1) #Makes values positive, since I used a time difference subtracting the closer date from the further one.

bnuy$numtimediff = as.numeric(bnuy$timediff, units = "days")

nrow(bnuy)
# SOME SUMMARY STATISTICS 
sd(bnuy$numtimediff, na.rm= T) 
# The standard deviation of adopted rabbits' time in system is 85 days. There's a LOT of variation there. 
mean(bnuy$numtimediff, na.rm = T)
# The mean of adopted rabbits' time in system is 79.78 days. This is over two months. :(  
median(bnuy$numtimediff, na.rm = T)
# The median of adopted rabbits' time-in-system is 53 days. 
reseen = bnuy[duplicated(bnuy$`Animal ID`), ]
# This checks for duplicate rabbits. However, the way we coded numtimediff (adoption days)
# makes it so duplicate rabbits only count in those statistics if they had two different stays at the SPCA.
# Hence, we did not think it necessary to remove these records. 
strays <- reseen %>% 
  filter(Source == "Stray")
strays
# Of 133 rabbits seen again, four were found as strays. 

#Most/Least Common Breeds:
valuecounts = table(bnuy$`Primary Breed`)
valuecounts


#We do some basic data engineering for a linear model of adoption time:
bnuy$`Age Years` <- as.numeric(bnuy$`Age Years`)
bnuy$`Age Months`<- as.numeric(bnuy$`Age Months`)

bnuy$age = 12*bnuy$`Age Years` + bnuy$`Age Months`


bnuy = bnuy %>% mutate( agegroup = case_when( age < 12 ~ "baby", age >= 12 & age <= 36 ~ "adult", age > 36 ~ "old", age >72 ~ 'veryold', TRUE ~ NA_character_ ) )
#Rabbit age group definitions taken from a vet friend based on general pet rabbits. While these may vary by 
#breed and treatment, and age isn't the be-all end-all of a rabbit's adoption, it's a consideration for sure.
#Most importantly, bunnies lose a lot of their "baby" features by age 1 and while they remain babies metaphorically
#many people think they're a lot more cute under 1. Those people are wrong, by the way. They're always very cute. (ignore the fallacy there. they are always the most cute)

bnuy$`Surrender Reason`[is.na(bnuy$`Surrender Reason`)] <- "Not Surrendered"
# In case surrender reason can be significant. Some rabbits who are surrendered have already been neutered/spayed
# or have vet history or other beneficial circumstances that contribute to faster adoption. 
# A common one is them being more personable since they're used to human interaction, or litter trained!

df <- subset(bnuy, select = -c(`Age Years`, `Age Months`, DOB, `Animal ID`, `Animal Type`, `Source`, `Status Date`, update, datein))
df <- df[df$adopted != 0, ]


# We now have a nicer subset of bunny data to try and build a regression or tool to help us understand the arcane 
# science of loaf homing.
test = lm(numtimediff ~., data=df)
summary(test)

df$timediff = NULL
df$`Outcome Eval Cat` = NULL
df$adopted = NULL
df$`Source Date`= NULL
# Dropping columns that don't matter. 
# Timediff dropped because it is the same as numtimediff but as datetime.
# Outcome Eval Cat dropped because it (and adopted) are largely the same for all adopted rabbits and uninterpretable.
# Adopted status dropped because 
test = lm(numtimediff ~.-`Primary Color`, data=df)
summary(test)

df <- df %>%
  mutate(
    aggro = case_when(
      str_detect(`Surrender Reason`, "Behaviour") ~ "aggressive",
      str_detect(`Surrender Reason`, "Chewing") ~ "chewer",
      str_detect(`Surrender Reason`, "Feral")              ~ "feral",
      str_detect(`Surrender Reason`, "Allergies")          ~ "allergic",
      TRUE                                                 ~ "notrabbitfault"
    )
  )
df$`Surrender Reason` = NULL
# This is a bit of a simplification of surrender reasons, but it was necessary to coalesce the variety of surrender
# reasons into something more important. Biting, chewing, and other aggressive behaviour, feral history,
# and allergies are some of the most important reasons people consider when adopting.
# These reasons are relevant BUT exact reasons are difficult to discern and too numerous to yield significant
# results especially with only 1400 entries. 

test = lm(numtimediff ~.-`Primary Color`, data=df)
summary(test)
#Every time, colour proves to be nearly irrelevant. Hence it is always removed. 

df_filtered <- df %>% filter(numtimediff <= 200) #Going to drop any observations where rabbits stay over 200 days.
test = lm(numtimediff ~.-`Primary Color`, data=df_filtered)
summary(test)
#While this test isn't particularly perfect, it does tell us which bunny breeds are significant so we can omit the others
#in Random Forest, since that one is limited to 53 categoricals.
df <- df %>%
  mutate(
    breed = case_when(
      str_detect(`Primary Breed`, "American Fuzzy Lop") ~ "American Fuzzy Lop",
      str_detect(`Primary Breed`, "Angora") ~ "Angora",
      str_detect(`Primary Breed`, "Beveren") ~ "Beveren",
      str_detect(`Primary Breed`, "English Spot") ~ "EngSpot",
      str_detect(`Primary Breed`, "Flemish Giant") ~ "LargeLoaf",
      str_detect(`Primary Breed`, "Harlequin") ~ "Harleq",
      str_detect(`Primary Breed`, "Himalayan") ~ "Himalayan",
      str_detect(`Primary Breed`, "Holland Lop") ~ "Holland Lop",
      str_detect(`Primary Breed`, "Lionhead") ~ "Lionhead",
      str_detect(`Primary Breed`, "Lop Eared") ~ "Lop",
      str_detect(`Primary Breed`, "Mini Lop") ~ "Mini Lop",
      str_detect(`Primary Breed`, "Mini Rex") ~ "Mini Rex",
      str_detect(`Primary Breed`, "Silver Fox") ~ "Silver Fox",
      str_detect(`Primary Breed`, "Silver Marten") ~ "Silver Mart",
      str_detect(`Primary Breed`, "Netherland Dwarf") ~ "Netherland",
      TRUE                                                 ~ "other breed"
    )
  )
df$`Primary Breed` = NULL
df$`Primary Color` = NULL
df$age = NULL




#Random Forest Analysis

df <- df %>%
  mutate(
    long = case_when(
      numtimediff < 60 ~ "Under 2 Months",
      numtimediff >= 60 & numtimediff <= 120 ~ "2-4 Months",
      numtimediff > 120 & numtimediff <= 180 ~"4-6 Months",
      numtimediff > 180 ~ "Over Half Year"
    )
  )

library(randomForest)
library(caTools)
set.seed(61)
split <- sample.split(df$numtimediff, SplitRatio = 0.8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

df1 = na.omit(df)

nrow(df1)

df1$long = as.factor(df1$long)W

df1 <- df1 %>%
  rename(intake = `Intake Region`)
AdoptionFactors = randomForest(long~. -numtimediff -aggro,data = df1, ntree = 500, random_state = 0)

print(AdoptionFactors)

BunnyAdoptionFactors = varImpPlot(AdoptionFactors)
# This is very messy data, but we get somewhere with this. We see that intake area is the most important factor, followed by breed. 
# Data made available under CC BY 4.0. 
# Data Source: U., Ashley Sum Yin; Hou, Cheng Yu; Protopopova, Alexandra (2024). Rabbit length of stay data.. PLOS ONE. Dataset. https://doi.org/10.1371/journal.pone.0300633.s002