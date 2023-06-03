library(dplyr)
library(stringr)
library(shiny)
library(countrycode)


activity_df <- read.csv("GDADS2_Data.csv")
CSR_df <- read.csv("B Corp Impact Data.csv")


#Data wrangling(The reason why I chose OUTTYPE2 as the outcome results is because the reliability is higher than OUTTYPE1.)
clean_act_df <- select(activity_df, VIOL, GOVTYPE, BYEAR, TARGCOUN, OUTTYPE2)
clean_CSR_df <- select(CSR_df, country, assessment_year, current_status, overall_score)


#Remove the reliability and rows that contains na.
clean_act_df <- clean_act_df[-1,]
clean_act_df <- filter(clean_act_df, !is.na(clean_act_df$VIOL)&TARGCOUN!="99")
clean_act_df$Countryname <- countrycode(clean_act_df$TARGCOUN, "iso3c", "country.name")
for(i in 1:425){
  if(clean_act_df$TARGCOUN[i]=="JAP"){
    clean_act_df$Countryname[i] <- "Japan"
  }else if(clean_act_df$TARGCOUN[i]=="PHI"){
    clean_act_df$Countryname[i] <- "Philippines"
  }else if(clean_act_df$TARGCOUN[i]=="UK"){
    clean_act_df$Countryname[i] <- "United Kingdom"
  }
}

#CSR companies are still certified since 2010(The online activism dataset is from 2010-2012).
clean_CSR_df <- filter(clean_CSR_df, assessment_year >= 2010 & current_status=="certified")


#The Goal achieved percent of different types of Government.
democracy_df <- filter(clean_act_df, clean_act_df$GOVTYPE=="Democracy")
demo_achieved_percent <- round(sum(str_count(democracy_df$OUTTYPE2, "1"))/nrow(democracy_df), 3)*100

emerging_df <- filter(clean_act_df, clean_act_df$GOVTYPE=="Emerging Democracy")
emer_achieved_percent <- round(sum(str_count(emerging_df$OUTTYPE2, "1"))/nrow(emerging_df), 3)*100

author_df <- filter(clean_act_df, clean_act_df$GOVTYPE=="Authoritarian")
author_achieved_percent <- round(sum(str_count(author_df$OUTTYPE2, "1"))/nrow(author_df), 3)*100

trans_df <- filter(clean_act_df, clean_act_df$GOVTYPE=="Transition")
trans_achieved_percent <- round(sum(str_count(trans_df$OUTTYPE2, "1"))/nrow(trans_df), 3)*100

gov_df <- data.frame(Governmenttype = c("Democracy", "Emerging Democracy", "Authoritarian", "Transition"), 
                     AchievedPercent = c(demo_achieved_percent, emer_achieved_percent, author_achieved_percent, trans_achieved_percent))

#The Goal achieved percent by occurring violence.
viol_df <- filter(clean_act_df, clean_act_df$VIOL=="1")
viol_achieved_percent <- round(sum(str_count(viol_df$OUTTYPE2, "1"))/nrow(viol_df), 3)*100

#The goal achieved percent in different countries.
country_act_grouped <- group_by(clean_act_df, Countryname)
act_country_df <- summarize(country_act_grouped, total_act = n(), achieved_act = sum(OUTTYPE2=="1"), achieved_percent = round(achieved_act/total_act, 3)*100)

#Numbers of CSR companies in different countries.
country_CSR_grouped <- group_by(clean_CSR_df, country)
CSR_country_df <- summarize(country_CSR_grouped, total_CSR = n(), mean_CSR_score = mean(overall_score))

#Merge two datasets with same countries.
merge_df <- merge(act_country_df, CSR_country_df, by.x = "Countryname", by.y = "country")