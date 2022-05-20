
library(dplyr)
library(stringr)
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

dx <- read.csv("ASPE_8.18.20_psycSummary.csv") #read in diagnosis information
mergeAll <- merge(df, dx, by = "study_id")

# Clean data --------------------------------------------------------------

mergeAll <- mergeAll %>%
  #keep only relevant columns
  select("recruit_type","srs2a_ir_raw","srs2a_sr_raw", "part_sex", 
         "srs2a_ir_inf1_sex","briefa_inf_gec_raw","briefa_sr_gec_raw",
         "briefa_inf_sex","Affected")%>%
  #remove rows with NA values
  filter(complete.cases(.))%>%
  #turn key variables into factors
  mutate_at(vars(part_sex, recruit_type,srs2a_ir_inf1_sex,briefa_inf_sex), factor)%>%
  #filter out all participants besides phenotype first probands and unaffected family
  filter (recruit_type == "1" | (recruit_type == "2" & Affected == "No")) %>%
  #create discrepancy score columns for SRS and BRIEF
  rowwise() %>%
  mutate(SRSdiscrep = srs2a_ir_raw - srs2a_sr_raw) %>%
  mutate(BRIEFdiscrep = briefa_inf_gec_raw - briefa_sr_gec_raw)


#change factor levels from numbers to words for easier labeling in graph
#error with doing this using dplyr above
levels(mergeAll$part_sex) <- c("Female","Male")
levels(mergeAll$recruit_type) <- c("ASD proband","family member")
levels(mergeAll$srs2a_ir_inf1_sex) <- c("Female","Male")
levels(mergeAll$briefa_inf_sex) <- c("Female","Male")
  


#split mergeDF into an SRS and a BRIEF dataframe
SRSdf <- mergeAll %>%
  select("recruit_type","SRSdiscrep", "part_sex", 
         "srs2a_ir_inf1_sex")

BRIEFdf <- mergeAll %>%
  select("recruit_type","BRIEFdiscrep","part_sex",          
                              "briefa_inf_sex")


# group comparisons for participant sex and informant sex -----------------

probSRS <- subset (SRSdf, recruit_type == "ASD proband")
probBRIEF <- subset (BRIEFdf, recruit_type == "ASD proband")

famSRS <- subset (SRSdf, recruit_type == "family member")
famBRIEF <- subset (BRIEFdf, recruit_type == "family member")


#check to see if the variances are significantly different
#null-hypothesis of the F test is that the two group variances are equal
var.test(SRSdiscrep ~ srs2a_ir_inf1_sex, probSRS)
var.test(BRIEFdiscrep ~ briefa_inf_sex, probBRIEF)

var.test(SRSdiscrep ~ srs2a_ir_inf1_sex, famSRS)
var.test(BRIEFdiscrep ~ briefa_inf_sex, famBRIEF)

#two sample t-tests to test difference between groups
t.test(SRSdiscrep ~ srs2a_ir_inf1_sex, data = probSRS, var.equal = TRUE)
maleinf <- subset(SRSdf, srs2a_ir_inf1_sex == "Male")
femaleinf <- subset(SRSdf, srs2a_ir_inf1_sex == "Female")
sd(maleinf$SRSdiscrep)
sd(femaleinf$SRSdiscrep)

t.test(BRIEFdiscrep ~ briefa_inf_sex, data = probBRIEF, var.equal = TRUE)
t.test(SRSdiscrep ~ srs2a_ir_inf1_sex, data = famSRS, var.equal = TRUE)
t.test(BRIEFdiscrep ~ briefa_inf_sex, data = famBRIEF, var.equal = TRUE)


# plot discrepancy: probands --------------------------------------------------------

IS.p1 <- ggplot(probSRS, aes(x=srs2a_ir_inf1_sex, y=SRSdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color=srs2a_ir_inf1_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  geom_signif(comparisons=list(c("Female", "Male")), annotations="*",
              y_position = 70, tip_length = 0, extend_line = -0.1) +
  ylab("SRS Total Discrepancy") +
  xlab("Informant Sex") 

IS.p2 <- ggplot(probBRIEF, aes(briefa_inf_sex, BRIEFdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color=briefa_inf_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  ylab("BRIEF Discrepancy") +
  xlab("Informant Sex") 

S.p1 <- ggplot(probSRS, aes(x=part_sex, y=SRSdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color=part_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  geom_signif(comparisons=list(c("Female", "Male")), annotations="*",
              y_position = 70, tip_length = 0, extend_line = -0.1) +
  ylab("SRS Total Discrepancy") +
  xlab("Participant Sex") 

S.p2 <- ggplot(probBRIEF, aes(part_sex, BRIEFdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color= part_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  ylab("BRIEF Discrepancy") +
  xlab("Participant Sex") 


# plot discrepancy: family --------------------------------------------------------

IS.p3 <- ggplot(famSRS, aes(x=srs2a_ir_inf1_sex, y=SRSdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color=srs2a_ir_inf1_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  ylab("SRS Total Discrepancy") +
  xlab("Informant Sex") 

IS.p4 <- ggplot(famBRIEF, aes(briefa_inf_sex, BRIEFdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color=briefa_inf_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  ylab("BRIEF Discrepancy") +
  xlab("Informant Sex") 

S.p3 <- ggplot(famSRS, aes(x=part_sex, y=SRSdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color=part_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  ylab("SRS Total Discrepancy") +
  xlab("Participant Sex") 

S.p4 <- ggplot(famBRIEF, aes(part_sex, BRIEFdiscrep)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_bw(base_size = 7)+ 
  theme(legend.position = "none") +
  geom_point (aes(color= part_sex), 
              position = position_jitter(width= 0.35), alpha = 0.6,
              size = 1) +
  ylab("BRIEF Discrepancy") +
  xlab("Participant Sex") 



# Figures -----------------------------------------------------------------

png("Figure2.png", width = 6, height = 4, units = 'in', res = 300)

ggarrange(S.p1,S.p2,IS.p1,IS.p2,
          nrow = 2,
          ncol = 2, 
          labels = c("A","B","C","D"))
dev.off()

png("SuppFigure2.png", width = 6, height = 4, units = 'in', res = 300)

ggarrange(S.p3,S.p4,IS.p3,IS.p4,
          nrow = 2,
          ncol = 2, 
          labels = c("A","B","C","D"))
dev.off()