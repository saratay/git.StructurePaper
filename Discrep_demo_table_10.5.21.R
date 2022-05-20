
library(readxl)
library(gtsummary)
library(dplyr)
library(tidyverse)

# read in and format data  -----------------------------------------------------------

setwd("~/Sheets & Scripts/Discrepancy figures")

# manually deleted empty columns, read in demographic and self-report data
# manually added informant ids and sex to the informant spreadsheet
infData <- read.csv("inf_8.18.20.csv")
selfData <- read.csv("self_8.18.20.csv")
selfData <- selfData[,c("study_id","srs2a_sr_raw",
                        "briefa_sr_gec_raw")]
demData <- read.csv("Edu_7.16.21.csv")

setwd("~/Sheets & Scripts/Discrepancy figures")
dx <- read.csv("ASPE_8.18.20_psycSummary.csv") #read in diagnosis information

#merge dem and self / informant dataframes
mergeAll <- merge(demData, selfData, by = "study_id")
mergeAll <- merge(mergeAll, infData, by = "study_id")
mergeAll <- merge(mergeAll, dx, by = "study_id")
mergeAll$SRSdiscrep <- mergeAll$srs2a_ir_raw - mergeAll$srs2a_sr_raw
mergeAll$BRIEFdiscrep <- mergeAll$briefa_inf_gec_raw - mergeAll$briefa_sr_gec_raw

mergeAll$recruit_type <- as.factor(mergeAll$recruit_type) #convert recruit_type to a meaningful factor
levels(mergeAll$recruit_type) <- c("ASD proband","family member")
mergeAll$part_sex <- as.factor(mergeAll$part_sex) #convert sex to a meaningful factor
levels(mergeAll$part_sex) <- c("Female", "Male")
mergeAll$part_gender_id <- as.factor(mergeAll$part_gender_id) #convert sex to a meaningful factor
levels(mergeAll$part_gender_id) <- c("Cis-female","Cis-male",
                                     "Trans-male","Trans-female",
                                     "Other","Non-binary")

# rename levels -----------------------------------------------------------
mergeAll <- mergeAll %>% mutate(part_edu =
                                  case_when(part_edu <= 11 ~ "less than high school", 
                                            part_edu == 12 ~ "high school graduate",
                                            part_edu <= 15 ~ "some college",
                                            part_edu <= 17 ~ "undergraduate degree",
                                            part_edu <= 19 ~ "master's degree",
                                            part_edu == 20 ~ "doctoral degree")
)


# Read in patch data for revisions ----------------------------------------

patch <- read.csv("Patch_discrep_data_10.5.21.csv")

patch <- patch[,-which(colnames(patch) %in% c("redcap_event_name"))]
            
patch <- patch[, colSums(is.na(patch)) != nrow(patch)]

patch <- patch %>% 
  group_by(study_id) %>% 
  fill(colnames(patch), .direction = "downup") %>% 
  distinct()

# merge patch with old data and then filter -------------------------------

mergeAll <- merge(mergeAll, patch, by = "study_id")

#subset probands and unafffected family members and select demographic variables of interest
prob <- mergeAll %>%
  filter (recruit_type == "ASD proband" &
            (!is.na(SRSdiscrep) | !is.na(BRIEFdiscrep)))
fam <- mergeAll %>%
  filter (recruit_type == "family member" &
            (!is.na(SRSdiscrep) | !is.na(BRIEFdiscrep)) &
            Affected == "No")


# make spreadsheet with ids with complete data (2/24/22 addition) ---------

write.csv(prob$study_id,"prob_IDlist.csv")
write.csv(fam$study_id,"fam_IDlist.csv")


# Get additional values needed for revisions manuscript -------------------

## age range for probands and family members
min (prob$part_age_screen)
max (prob$part_age_screen)

min (fam$part_age_screen)
max (fam$part_age_screen)

## sd and range for SRS and BRIEF for probands and family members

values <- function(variable){
  b <- sd(na.omit(variable))
  c <- min(na.omit(variable))
  d <- max(na.omit(variable))
  return(c(b,c,d))
}

values (prob$srs2a_sr_raw)
values (prob$srs2a_ir_raw)
values (prob$briefa_sr_gec_raw)
values (prob$briefa_inf_gec_raw)

values (fam$srs2a_sr_raw)
values (fam$srs2a_ir_raw)
values (fam$briefa_sr_gec_raw)
values (fam$briefa_inf_gec_raw)

## mean and sd and range for SRS T scores and SCQ


mean(na.omit(prob$srs2a_sr_total_t))
sd(na.omit(prob$srs2a_sr_total_t))
mean(na.omit(prob$srs2a_ir_total_t))
sd(na.omit(prob$srs2a_ir_total_t))
mean(na.omit(prob$scq_score))
sd(na.omit(prob$scq_score))

mean(na.omit(fam$srs2a_sr_total_t))
sd(na.omit(fam$srs2a_sr_total_t))
mean(na.omit(fam$srs2a_ir_total_t))
sd(na.omit(fam$srs2a_ir_total_t))
mean(na.omit(fam$scq_score))
sd(na.omit(fam$scq_score))
