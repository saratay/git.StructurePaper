# all discrepancy analyses in one script

library(dplyr)
library(readxl)
library(stringr)
library(lm.beta)
library(pwr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# read in and format data  -----------------------------------------------------------


setwd("~/Sheets & Scripts/Discrepancy figures")


input <- read.csv("REDCapData_4.8.21.csv")
df <- input[, colSums(is.na(input)) != nrow(input)]
df <- df[,c(1,3:12)]
df <- df %>% 
  group_by(study_id) %>% 
  fill(colnames(df), .direction = "downup") %>% 
  distinct()

df$part_sex <- as.factor(df$part_sex)
levels(df$part_sex) <- c("Female", "Male")
df$recruit_type <- as.factor(df$recruit_type) #convert recruit_type to a meaningful factor
levels(df$recruit_type) <- c("ASD proband","family member")

index <- c(0,1,2,3,4,5,8,9,10,11)
values <- c("Parent","Parent","Sibling","Sibling","Child","Child","Spouse","Friend","Other", "Therapist")
df$srs2a_ir_relation <- values[match(df$srs2a_ir_relation, index)]
df$briefa_inf_rel <- values[match(df$briefa_inf_rel, index)]


setwd("~/Sheets & Scripts/Discrepancy figures")
dx <- read.csv("ASPE_8.18.20_psycSummary.csv") #read in diagnosis information
age <- read.csv("Age_5.26.21.csv")
  
#merge dem and self / informant dataframes
mergeAll <- merge(df, dx, by = "study_id")
mergeAll <- merge(mergeAll, age, by = "study_id")

mergeAll$SRSdiscrep <- mergeAll$srs2a_ir_raw - mergeAll$srs2a_sr_raw
mergeAll$BRIEFdiscrep <- mergeAll$briefa_inf_gec_raw - mergeAll$briefa_sr_gec_raw


mergeAll <- filter (mergeAll, recruit_type == "ASD proband" | 
                      (recruit_type == "family member" & Affected == "No"))


# Filter out participants without SRS discrepancy OR BRIEF discrep --------

mergeAll_compl <- filter (mergeAll, !is.na(SRSdiscrep) | !is.na(BRIEFdiscrep) )
proband_compl <- filter(mergeAll_compl, recruit_type =="ASD proband")
fam_compl <- filter(mergeAll_compl, recruit_type =="family member")


# Tables for informant types ----------------------------------------------


table(mergeAll_compl$srs2a_ir_relation)
table(mergeAll_compl$briefa_inf_rel)


table(proband_compl$srs2a_ir_relation)
table(proband_compl$briefa_inf_rel)


table(fam_compl$srs2a_ir_relation)
table(fam_compl$briefa_inf_rel)


table(mergeAll_compl$srs2a_ir_inf1_sex)
table(mergeAll_compl$briefa_inf_sex)


table(proband_compl$srs2a_ir_inf1_sex)
table(proband_compl$briefa_inf_sex)


table(fam_compl$srs2a_ir_inf1_sex)
table(fam_compl$briefa_inf_sex)
