library(R.utils)
library(dplyr)
library(sqldf)

dataDirectory <-'data'
dataFile <- file.path(dataDirectory,'repdata_data_StormData.csv')
compressedDataFile <- file.path(dataDirectory,'repdata_data_StormData.csv.bz2')

storms <- read.csv(dataFile,header = TRUE)
storms <- storms %>% select_(
	"EVTYPE", "FATALITIES",
	"INJURIES", "PROPDMG",
	"PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
str(storms)

#To count the total number of causalities, we added the number of fatalities and injuries. 
#As this sum was used as indicator for the damage with respect to population health.
storms <- storms %>% 
	mutate(CAUSALITIES = FATALITIES+INJURIES) %>% 
	select(-c(FATALITIES,INJURIES))

#To evaluate the cost inflicted by each event, as an indicator for the economic consequences, 
#we used the variables containing the Property Damage and Crop Damage, 
#while applying the exponents given by Property Damage Exponent and Crop Damage Exponent, respectively. 

#The multipliers were: Hundred (H), Thousand (K), Million (M) and Billion (B)

storms$PROPDMGEXP <- toupper(storms$PROPDMGEXP)
storms$CROPDMGEXP <- toupper(storms$CROPDMGEXP)

storms$PROPDMGEXP <- gsub("^$| |\\?|\\+|-","1",storms$PROPDMGEXP)
storms$CROPDMGEXP <- gsub("^$| |\\?|\\+|-","1",storms$CROPDMGEXP)

storms$PROPDMGEXP <- gsub("^H$","2",storms$PROPDMGEXP)
storms$CROPDMGEXP <- gsub("^H$","2",storms$CROPDMGEXP)

storms$PROPDMGEXP <- gsub("^K$","3",storms$PROPDMGEXP)
storms$CROPDMGEXP <- gsub("^K$","3",storms$CROPDMGEXP)

storms$PROPDMGEXP <- gsub("^M$","6",storms$PROPDMGEXP)
storms$CROPDMGEXP <- gsub("^M$","6",storms$CROPDMGEXP)

storms$PROPDMGEXP <- gsub("^B$","9",storms$PROPDMGEXP)
storms$CROPDMGEXP <- gsub("^B$","9",storms$CROPDMGEXP)

storms <- storms %>% 
	mutate(PROPDMGEXP = 10^as.numeric(PROPDMGEXP), CROPDMGEXP=10^as.numeric(CROPDMGEXP)) %>%
	mutate(COSTS = PROPDMG * PROPDMGEXP + CROPDMG*CROPDMGEXP) %>%
	select(-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

#Filter for significant data, as most of the variables are null
storms <- filter(storms, COSTS!=0 & CAUSALITIES!=0)

#Data is extreamly maked by typos and wording problemes
storms$EVTYPE <- toupper(storms$EVTYPE)

#Data is extremely marked by typos and wording problems, possibly semantically erroneous.
#This is an attempt to filter some inconsistencies. 
storms$EVTYPE[grepl("HURRICANE|SUMMARY|TROPICAL", storms$EVTYPE)] <- "HURRICANE"
storms$EVTYPE[grepl("FIRE", storms$EVTYPE)] <-"WILD FIRE"
storms$EVTYPE[grepl("ICE|ICY|FREEZ|SNOW|WINT", storms$EVTYPE)] <-"WINTER STORM"
storms$EVTYPE[grepl("CHILL|COLD", storms$EVTYPE)] <-"COLD"
storms$EVTYPE[grepl("WIND", storms$EVTYPE)] <-"WIND"
storms$EVTYPE[grepl("FLOOD|URBAN", storms$EVTYPE)] <-"FLOOD"
storms$EVTYPE[grepl("HEAT", storms$EVTYPE)] <-"HEAT"
storms$EVTYPE[grepl("DRY", storms$EVTYPE)] <-"DROUGHT"
storms$EVTYPE[grepl("HAIL", storms$EVTYPE)] <-"HAIL"
storms$EVTYPE[grepl("TORN|WATERSPROUT|FUNNEL|SPOUT|MICRO", storms$EVTYPE)] <- "TORNADO"
storms$EVTYPE[grepl("RAIN", storms$EVTYPE)] <-"RAIN"

storms$EVTYPE <- gsub("TSTM", "THUNDERSTORM", storms$EVTYPE)
storms$EVTYPE[grepl("THUNDERTORM", storms$EVTYPE)] <- "THUNDERSTORM"
storms$EVTYPE[grepl("THUNDERSTORM",storms$EVTYPE)] <- "THUNDERSTORM"

storms$EVTYPE[grepl("TROPICAL STORM", storms$EVTYPE)] <- "TROPICAL STORM"

storms$EVTYPE <- gsub("\\W+", " ", storms$EVTYPE)
storms$EVTYPE <- gsub("(S|ING)\\b", "", storms$EVTYPE)
storms$EVTYPE <- gsub("\\b[a-zA-Z]*\\d+\\b", "", storms$EVTYPE)
storms$EVTYPE <- gsub("^\\s+|\\s+$", "", storms$EVTYPE)

storms <- sqldf("SELECT EVTYPE, AVG(CAUSALITIES) AS CAUSALITIES, AVG(COSTS) AS COSTS FROM storms GROUP BY EVTYPE")