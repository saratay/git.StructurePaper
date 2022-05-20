library(readxl)
library(Hmisc)
library(stats)

library(ggplot2)
library(ggpubr)

# Read in and format data -------------------------------------------------

setwd("~/Sheets & Scripts/Discrepancy figures")
dx <- read.csv("ASPE_8.18.20_psycSummary.csv") #read in diagnosis information

setwd("~/Sheets & Scripts/Discrepancy figures")
dem <- read.csv("dem_8.18.20.csv") #read in demographic information
self <- read.csv("self_8.18.20.csv") #read in self-report scores
inf <- read.csv("inf_8.18.20.csv") #read in informant-report scores and informant information
scq <- read.csv("scq_8.18.20.csv")

dem$recruit_type <- as.factor(dem$recruit_type) #convert recruit_type to a meaningful factor
levels(dem$recruit_type) <- c("ASD proband","family member")
dem$part_sex <- as.factor(dem$part_sex) #convert sex to a meaningful factor
levels(dem$part_sex) <- c("Female", "Male")

#merge dem and dx with self and inf
mergeSelf <- merge(self,dem,by = "study_id")
mergeSelf <- merge(mergeSelf,dx, by = "study_id")
mergeInf <- merge(inf,dem,by = "study_id")
mergeInf <- merge(mergeInf,dx,by = "study_id")

#subset probands and family
mergeSelf.p <- subset (mergeSelf, mergeSelf$recruit_type == "ASD proband")
mergeInf.p <- subset (mergeInf, mergeInf$recruit_type == "ASD proband")
mergeSelf.f <- subset (mergeSelf, mergeSelf$recruit_type == "family member")
mergeInf.f <- subset (mergeInf, mergeInf$recruit_type == "family member")

#subset affected status
unaff.f.self <- subset(mergeSelf.f, mergeSelf.f$Affected == "No")
unaff.f.inf <- subset(mergeInf.f, mergeInf.f$Affected == "No")


# Merge df for method comparison ----------------------------------------------
#probands
merge.p.na <- na.omit(merge (mergeSelf.p[,c(1:5,11)],
                             mergeInf.p[,c(1:6)], 
                             by = "study_id"))


#family members without any psyc - unaffected
merge.f.na <- na.omit(merge (unaff.f.self[,c(1:5,11)],
                             unaff.f.inf[,c(1:6)], 
                             by = "study_id"))

# prep for Bland-Altman plot ----------------------------------------------

## calculate discrepancy score for each participant
merge.p.na$srs_diff <- merge.p.na$srs2a_ir_raw - merge.p.na$srs2a_sr_raw
merge.p.na$brief_diff <- merge.p.na$briefa_inf_gec_raw - merge.p.na$briefa_sr_gec_raw
merge.f.na$srs_diff <- merge.f.na$srs2a_ir_raw - merge.f.na$srs2a_sr_raw
merge.f.na$brief_diff <- merge.f.na$briefa_inf_gec_raw - merge.f.na$briefa_sr_gec_raw


## calculate average score for each participant across SR and IR value for each questionnaire
merge.p.na$srs_avg <- rowMeans(merge.p.na[,c("srs2a_sr_raw",
                                             "srs2a_ir_raw")])
merge.p.na$brief_avg <- rowMeans(merge.p.na[,c("briefa_sr_gec_raw",
                                               "briefa_inf_gec_raw")])
merge.f.na$srs_avg <- rowMeans(merge.f.na[,c("srs2a_sr_raw",
                                             "srs2a_ir_raw")])
merge.f.na$brief_avg <- rowMeans(merge.f.na[,c("briefa_sr_gec_raw",
                                               "briefa_inf_gec_raw")])

## calculate average difference for group / questionnaire
mean_diff_p_srs <- mean(merge.p.na$srs_diff)
mean_diff_p_brief <- mean(merge.p.na$brief_diff)
mean_diff_f_srs <- mean(merge.f.na$srs_diff)
mean_diff_f_brief <- mean(merge.f.na$brief_diff)

## calculate lower CI for average difference
mean_lower_p_srs <- mean_diff_p_srs - 1.96*sd(merge.p.na$srs_diff)
mean_lower_p_brief <- mean_diff_p_brief - 1.96*sd(merge.p.na$brief_diff)
mean_lower_f_srs <- mean_diff_f_srs - 1.96*sd(merge.f.na$srs_diff)
mean_lower_f_brief <- mean_diff_f_brief - 1.96*sd(merge.f.na$brief_diff)

## calculate upper CI for average difference

mean_upper_p_srs <- mean_diff_p_srs + 1.96*sd(merge.p.na$srs_diff)
mean_upper_p_brief <- mean_diff_p_brief + 1.96*sd(merge.p.na$brief_diff)
mean_upper_f_srs <- mean_diff_f_srs + 1.96*sd(merge.f.na$srs_diff)
mean_upper_f_brief <- mean_diff_f_brief + 1.96*sd(merge.f.na$brief_diff)


# function for bland altman plot ------------------------------------------

blandaltmanplot <- function(df, avg, diff, 
                            mean_diff,
                            lower,upper){
  ggplot(data = df, aes(x = avg, y = diff)) +
    geom_point(size=1) +
    geom_hline(yintercept = mean_diff) +
    geom_hline(yintercept = lower, color = "red", linetype="dashed") +
    geom_hline(yintercept = upper, color = "red", linetype="dashed") +
    ylab("Difference Between Raters") +
    xlab("Average Score")+
    theme_bw(base_size = 7)
}



# prep for correlation plots ----------------------------------------------

SRStotal.p <- cor.test(merge.p.na$srs2a_sr_raw, merge.p.na$srs2a_ir_raw,
                       method = "spearman")
BRIEF.p <- cor.test(merge.p.na$briefa_sr_gec_raw, merge.p.na$briefa_inf_gec_raw,
                    method = "spearman")

SRStotal.f <- cor.test(merge.f.na$srs2a_sr_raw, merge.f.na$srs2a_ir_raw,
                       method = "spearman")
BRIEF.f <- cor.test(merge.f.na$briefa_sr_gec_raw, merge.f.na$briefa_inf_gec_raw,
                    method = "spearman")



# make all plot objects ---------------------------------------------------

BA.p1 <- blandaltmanplot(merge.p.na, merge.p.na$srs_avg, 
                      merge.p.na$srs_diff, 
                      mean_diff_p_srs,
                      mean_lower_p_srs,mean_upper_p_srs) +
  theme(plot.margin = unit(c(1,0.1,0,0.1), "lines"))+
  xlim(0,150)+
  ylim(-125,100)+
  ggtitle("SRS-2A")

BA.p2 <- blandaltmanplot(merge.p.na, merge.p.na$brief_avg, 
                      merge.p.na$brief_diff, 
                      mean_diff_p_brief,
                      mean_lower_p_brief,mean_upper_p_brief)+
  theme(plot.margin = unit(c(1,0.1,0,0.1), "lines"))+
  xlim(50,190)+
  ylim(-100,80)+
  ggtitle("BRIEF-A")

BA.p3 <- blandaltmanplot(merge.f.na, merge.f.na$srs_avg, 
                      merge.f.na$srs_diff, 
                      mean_diff_f_srs,
                      mean_lower_f_srs,mean_upper_f_srs)+
  theme(plot.margin = unit(c(1,0.1,0,0.1), "lines"))+
  xlim(0,150)+
  ylim(-125,100)+
  ggtitle("SRS-2A")

BA.p4 <- blandaltmanplot(merge.f.na, merge.f.na$brief_avg, 
                      merge.f.na$brief_diff, 
                      mean_diff_f_brief,
                      mean_lower_f_brief,mean_upper_f_brief)+
  theme(plot.margin = unit(c(1,0.1,0,0.1), "lines"))+
  xlim(50,190)+
  ylim(-100,80)+
  ggtitle("BRIEF-A")

C.p1 <- ggplot(merge.p.na, aes(srs2a_sr_raw, srs2a_ir_raw)) +
  geom_point(size = 1) + 
  geom_text (x=120, y=155, 
             color = "blue",
             label= paste("r =",format(round(SRStotal.p$estimate[1], 2), nsmall = 2),
                          ", p =", format(round(SRStotal.p$p.value, 2), nsmall = 2), 
                          sep = " "),
             size = 2.5) +
  xlab ("SRS Total Self-Report") +
  ylab ("SRS Total Informant-Report") +
  geom_smooth(method=lm) +
  theme_bw(base_size = 7)+
  ggtitle("SRS-2A")

C.p2 <- ggplot(merge.p.na, aes (briefa_sr_gec_raw, briefa_inf_gec_raw))+
  geom_point(size = 1)+ 
  geom_text (x=165, y=190, 
             color = "blue",
             label= paste("r =",format(round(BRIEF.p$estimate[1], 2), nsmall = 2),
                          ", p =", format(round(BRIEF.p$p.value, 2), nsmall = 2), 
                          sep = " "),
             size = 2.5) +
  xlab ("BRIEF Self-Report") +
  ylab ("BRIEF Informant-Report") +
  geom_smooth(method=lm) +
  theme_bw(base_size = 7)+
  ggtitle("BRIEF-A")

C.p3 <- ggplot(merge.f.na, aes(srs2a_sr_raw, srs2a_ir_raw)) +
  geom_point(size = 1)+ 
  geom_text (x=67, y=150, 
             color = "blue",
             label= paste("r =",format(round(SRStotal.f$estimate[1], 2), nsmall = 2),
                          ", p =", format(round(SRStotal.f$p.value, 2), nsmall = 2), 
                          sep = " "),
             size = 2.5) +
  xlab ("SRS Total Self-Report") +
  ylab ("SRS Total Informant-Report") +
  geom_smooth(method=lm) +
  theme_bw(base_size = 7)+
  ggtitle("SRS-2A")

C.p4 <- ggplot(merge.f.na, aes (briefa_sr_gec_raw, briefa_inf_gec_raw))+
  geom_point(size = 1) + 
  geom_text (x=145, y=185, 
             color = "blue",
             label= paste("r =",format(round(BRIEF.f$estimate[1], 2), nsmall = 2),
                          ", p =", format(round(BRIEF.f$p.value, 2), nsmall = 2), 
                          sep = " "),
             size = 2.5) +
  xlab ("BRIEF Self-Report") +
  ylab ("BRIEF Informant-Report") +
  geom_smooth(method=lm) +
  theme_bw(base_size = 7)+
  ggtitle("BRIEF-A")


# make Figures ------------------------------------------------------------

png("Figure1.png", width = 6, height = 4, units = 'in', res = 300)
ggarrange(BA.p1,BA.p2,C.p1,C.p2,
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2)
dev.off()

png("SuppFigure1.png", width = 6, height = 4, units = 'in', res = 300)
ggarrange(BA.p3,BA.p4,C.p3,C.p4,
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2)
dev.off()

