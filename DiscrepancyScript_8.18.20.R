# all discrepancy analyses in one script

library(ggplot2)
library(ggpubr)
library(readxl)
library(stringr)
library(lm.beta)
library(pwr)

# read in and format data  -----------------------------------------------------------


setwd("~/Actual Documents/UPenn Third Year/Scripts and Sheets/StructurePaper")

# manually deleted empty columns, read in demographic and self-report data
# manually added informant ids and sex to the informant spreadsheet
infData <- read.csv("inf_8.18.20.csv")
infData <- infData[,c("study_id","srs2a_ir_raw","briefa_inf_gec_raw")]
selfData <- read.csv("self_8.18.20.csv")
selfData <- selfData[,c("study_id","srs2a_sr_raw","briefa_sr_gec_raw")]
demData <- read.csv("dem_8.18.20.csv")
demData$part_sex <- as.factor(demData$part_sex)
levels(demData$part_sex) <- c("Female", "Male")

setwd("~/Actual Documents/UPenn Third Year/Scripts and Sheets/Progeny")
dx <- read.csv("ASPE_8.18.20_psycSummary.csv") #read in diagnosis information

#merge dem and self / informant dataframes
mergeAll <- merge(demData, selfData, by = "study_id")
mergeAll <- merge(mergeAll, infData, by = "study_id")
mergeAll <- merge(mergeAll, dx, by = "study_id")
mergeAll$SRSdiscrep <- mergeAll$srs2a_ir_raw - mergeAll$srs2a_sr_raw
mergeAll$BRIEFdiscrep <- mergeAll$briefa_inf_gec_raw - mergeAll$briefa_sr_gec_raw
mergeAll$SRSdiscrep.abs <- abs(mergeAll$srs2a_ir_raw - mergeAll$srs2a_sr_raw)
mergeAll$BRIEFdiscrep.abs <- abs(mergeAll$briefa_inf_gec_raw - mergeAll$briefa_sr_gec_raw)


#subset probands and unafffected family members
prob <- subset(mergeAll, mergeAll$recruit_type == 1)
fam <- subset(mergeAll, mergeAll$recruit_type == 2)


# Discrepancy sex effects -------------------------------------------------
#test normality and variance of data

#test for normality of both data sets
prob.f <- subset(prob, prob$part_sex == "Female")
prob.m <- subset(prob, prob$part_sex == "Male")
shapiro.test(prob.f$SRSdiscrep)
shapiro.test(prob.f$BRIEFdiscrep)
shapiro.test(prob.m$SRSdiscrep)
shapiro.test(prob.m$BRIEFdiscrep)
unaff.fam.f <- subset(unaff.fam, unaff.fam$part_sex == "Female")
unaff.fam.m <- subset(unaff.fam, unaff.fam$part_sex == "Male")
shapiro.test(unaff.fam.f$SRSdiscrep)
shapiro.test(unaff.fam.f$BRIEFdiscrep)
shapiro.test(unaff.fam.m$SRSdiscrep)
shapiro.test(unaff.fam.m$BRIEFdiscrep)

#note: if samples large enough (over 25-30), t-test is preferred to Wilcoxon


#check to see if the variances are significantly different
#null-hypothesis of the F test is that the two group variances are equal
var.test(prob.f$SRSdiscrep, prob.m$SRSdiscrep)
var.test(prob.f$BRIEFdiscrep, prob.m$BRIEFdiscrep)

var.test(unaff.fam.f$SRSdiscrep, unaff.fam.m$SRSdiscrep)
var.test(unaff.fam.f$BRIEFdiscrep, unaff.fam.m$BRIEFdiscrep)

#two sample t-tests to test difference between groups
t.test(SRSdiscrep ~ part_sex, data = prob, var.equal = TRUE)
t.test(BRIEFdiscrep ~ part_sex, data = prob, var.equal = TRUE)

t.test(SRSdiscrep ~ part_sex, data = unaff.fam, var.equal = FALSE)
t.test(BRIEFdiscrep ~ part_sex, data = unaff.fam, var.equal = TRUE)

#one sample t-tests to test distance from zero for discrepancy for each group
t.test(prob.f$SRSdiscrep, mu = 0, alternative = "two.sided")
t.test(prob.m$SRSdiscrep, mu = 0, alternative = "two.sided")
t.test(prob.f$BRIEFdiscrep, mu = 0, alternative = "two.sided")
t.test(prob.m$BRIEFdiscrep, mu = 0, alternative = "two.sided")

t.test(unaff.fam.f$SRSdiscrep, mu = 0, alternative = "two.sided")
t.test(unaff.fam.m$SRSdiscrep, mu = 0, alternative = "two.sided")
t.test(unaff.fam.f$BRIEFdiscrep, mu = 0, alternative = "two.sided")
t.test(unaff.fam.m$BRIEFdiscrep, mu = 0, alternative = "two.sided")

# Discrepancy hierarchical regression -------------------------------------
#change ASD column to get rid of No Response as an option
for (i in 1:nrow(psycData)){
  if (psycData[i,2] == "No Response"){
    psycData[i,2] <- "No"
  }
}

yesASD <- list()
counter = 1
for (i in 1:nrow(psycData)){
  if (psycData[i,2] == "Yes"){
    yesASD <- c(yesASD, as.character(psycData[i,1]))
    counter <- counter +1
  }
}

#merge demographic, self report, and informant information
mergeDF <- merge(demData, selfData, by = "study_id")
mergeDF <- merge(mergeDF, infData, by = "study_id")

index <- c(0,1,2,3,4,5,8,9,10,11)
values <- c("Mother","Father","Brother","Sister","Son","Daughter","Spouse","Friend","Other", "Therapist")
mergeDF$srs2a_ir_relation <- values[match(mergeDF$srs2a_ir_relation, index)]
mergeDF$briefa_inf_rel <- values[match(mergeDF$briefa_inf_rel, index)]

#add in psyc information for participants
mergeDF$ASD <- "No"
for (i in 1:nrow(mergeDF)){
  if (str_sub(mergeDF[i,1],-2) == "_1" & str_sub(mergeDF[i,1],1,3) == "AS0"){
    mergeDF[i,"ASD"] <- "Yes"
  }
  if (mergeDF[i,1] %in% yesASD)
    mergeDF[i,"ASD"] <- "Yes"
}

#add in ASD information of informants
mergeDF$SRSinf_ASD <- "No"
mergeDF$BRIEFinf_ASD <- "No"

i = 1

while (i < (nrow(mergeDF)+1)){
  if (str_sub(mergeDF[i,"SRSinf_study_id"],-2) == "_1" & str_sub(mergeDF[i,1],1,3) == "AS0"){
    mergeDF[i,"SRSinf_ASD"] <- "Yes"
  }
  if (mergeDF[i,"SRSinf_study_id"] %in% yesASD){
    mergeDF[i,"SRSinf_ASD"] <- "Yes"}
  else {
    print ("No SRS informant ASD")
  }
  if (str_sub(mergeDF[i,"BRIEFinf_study_id"],-2) == "_1" & str_sub(mergeDF[i,1],1,3) == "AS0"){
    mergeDF[i,"BRIEFinf_ASD"] <- "Yes"
  }
  if (mergeDF[i,"BRIEFinf_study_id"] %in% yesASD){
    mergeDF[i,"BRIEFinf_ASD"] <- "Yes"}
  else {
    print ("No BRIEF informant ASD")
  }
  i = i+1
}

#add in SRS information of informants
mergeDF$SRSinf_SRS <- NA
mergeDF$BRIEFinf_SRS <- NA

i = 1
temp <- NA

while (i < (nrow(mergeDF)+1)){
  if (mergeDF[i,"SRSinf_study_id"] %in% mergeDF$study_id){
    temp <- match(mergeDF[i,"SRSinf_study_id"],mergeDF$study_id)
    mergeDF[i,"SRSinf_SRS"] <- mergeDF[temp,"srs2a_sr_raw"]}
  else {
    print ("No SRS informant SRS")
  }
  if (mergeDF[i,"BRIEFinf_study_id"] %in% mergeDF$study_id){
    temp <- match(mergeDF[i,"BRIEFinf_study_id"],mergeDF$study_id)
    mergeDF[i,"BRIEFinf_SRS"] <- mergeDF[temp,"briefa_sr_gec_raw"]}
  else {
    print ("No BRIEF informant SRS")
  }
  i = i+1
}



#split mergeDF into an SRS and a BRIEF dataframe
SRSdf <- mergeDF[,c("study_id","part_age_screen","part_sex",          
                    "srs2a_sr_raw","srs2a_ir_raw",      
                    "srs2a_ir_relation","SRSinf_study_id",
                    "SRSinf_sex","ASD",
                    "SRSinf_ASD","SRSinf_SRS")]
BRIEFdf <- mergeDF[,c("study_id","part_age_screen","part_sex",          
                      "briefa_sr_gec_raw","briefa_inf_gec_raw",
                      "briefa_inf_rel","BRIEFinf_study_id", 
                      "BRIEFinf_sex","ASD", 
                      "BRIEFinf_ASD","BRIEFinf_SRS")]

#calculate discprepancy score
SRSdf$SRSdelta <- SRSdf$srs2a_ir_raw - SRSdf$srs2a_sr_raw
BRIEFdf$BRIEFdelta <- BRIEFdf$briefa_inf_gec_raw - BRIEFdf$briefa_sr_gec_raw


SRSdf <- na.omit(SRSdf[,c("ASD","SRSdelta", "part_age_screen", "part_sex", "srs2a_ir_relation", "SRSinf_sex", "SRSinf_SRS")])

# hierarchical regression to test the effects of different variables on the difference between self-report and informant

# Build SRS models
m0 <- lm(SRSdelta ~ 1, data=SRSdf)  # to obtain Total SS
m1 <- lm(SRSdelta ~ part_age_screen + part_sex, data=SRSdf)  # Model 1, demographics of the participant
m2 <- lm(SRSdelta ~ part_age_screen + part_sex + ASD, data = SRSdf) # Model 2, add ASD status of participant
m3 <- lm(SRSdelta ~ part_age_screen + part_sex + ASD + srs2a_ir_relation, data=SRSdf)  # Model 3, add informant type
m4 <- lm(SRSdelta ~ part_age_screen + part_sex + ASD + srs2a_ir_relation + SRSinf_SRS, data=SRSdf) #Model 4, add informant SRS score
m5 <- lm(SRSdelta ~ part_age_screen + part_sex + ASD + srs2a_ir_relation + SRSinf_SRS + SRSinf_sex, data=SRSdf)  # Model 5, add informant sex

#
anova(m0) # obtain Total SS
anova(m1,m2,m3,m4,m5) # model comparison

# used to fill out table
# lm.beta provides standard coefficients for the table
# summary provides F and adjusted R-squared for the bottom of the table
summary(m1)
lm.beta(m1)
summary(m2)
lm.beta(m2)
summary(m3)
lm.beta(m3)
summary(m4)
lm.beta(m4)
summary(m5)
lm.beta(m5)


BRIEFdf <- na.omit(BRIEFdf[,c("ASD","BRIEFdiscrep", "part_age_screen", "part_sex", "briefa_inf_rel", "BRIEFinf_sex", "BRIEFinf_SRS")])

# hierarchical regression to test the effects of different variables on the difference between self-report and informant

# Build BRIEF models
m0 <- lm(BRIEFdelta ~ 1, data=BRIEFdf)  # to obtain Total SS
m1 <- lm(BRIEFdelta ~ part_age_screen + part_sex, data=BRIEFdf)  # Model 1, demographics of the participant
m2 <- lm(BRIEFdelta ~ part_age_screen + part_sex + ASD, data = BRIEFdf) # Model 2, add ASD status of participant
m3 <- lm(BRIEFdelta ~ part_age_screen + part_sex + ASD + briefa_inf_rel, data=BRIEFdf)  # Model 3, add informant type
m4 <- lm(BRIEFdelta ~ part_age_screen + part_sex + ASD + briefa_inf_rel + BRIEFinf_SRS, data=BRIEFdf)  # Model 4, add informant SRS score
m5 <- lm(BRIEFdelta ~ part_age_screen + part_sex + ASD + briefa_inf_rel + BRIEFinf_SRS + BRIEFinf_sex, data=BRIEFdf)  # Model 5, add informant sex


anova(m0) # obtain Total SS
anova(m1,m2,m3,m4,m5) # model comparison

# used to fill out table
# lm.beta provides standard coefficients for the table
# summary provides F and adjusted R-squared for the bottom of the table
summary(m1)
lm.beta(m1)
summary(m2)
lm.beta(m2)
summary(m3)
lm.beta(m3)
summary(m4)
lm.beta(m4)
summary(m5)
lm.beta(m5)
