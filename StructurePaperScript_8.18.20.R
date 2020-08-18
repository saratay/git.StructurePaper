# complete all analyses for ASD structure paper in one script

library(readxl)
library(Hmisc)
library(stats)
library(corrplot)

# Read in and format data -------------------------------------------------

setwd("~/Actual Documents/UPenn Third Year/Scripts and Sheets/StructurePaper") #set working directory
cnp.remote <- read.csv("CNPremote_8.18.20.csv") #read in remote CNP data
cnp.proc <- read.csv("CNPproc_8.18.20.csv") #read in proctored CNP data

setwd("~/Actual Documents/UPenn Third Year/Scripts and Sheets/Progeny") #set working directory
dx <- read.csv("ASPE_8.18.20_psycSummary.csv") #read in diagnosis information

setwd("~/Actual Documents/UPenn Third Year/Scripts and Sheets/StructurePaper") #set working directory
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


# Calculate demographics --------------------------------------------------

dem.p <- subset (dem, dem$recruit_type == "ASD proband") #subset probands with demographic information
dem.f <- subset (dem, dem$recruit_type == "family member") #subset family members with demographic information
dem.f <- merge(dem.f, dx, by = "study_id") # merge family dem information with dx information
dem.f.unaff <- subset(dem.f, dem.f$Affected == "No") # subset unaffected family

cat ("Mean age probands", as.character(mean(na.omit(dem.p$part_age_screen))))
cat ("SD age probands", as.character(sd(na.omit(dem.p$part_age_screen))))
cat ("Mean age unaff fam", as.character(mean(na.omit(dem.f.unaff$part_age_screen))))
cat ("SD age unaff fam", as.character(sd(na.omit(dem.f.unaff$part_age_screen))))

cat ("% female probands",as.character(table(dem.p$part_sex)[1]/
                                        (table(dem.p$part_sex)[1]+table(dem.p$part_sex)[2])))
cat ("% female unaff fam",as.character(table(dem.f.unaff$part_sex)[1]/
                                         (table(dem.f.unaff$part_sex)[1]+table(dem.f.unaff$part_sex)[2])))

cat ("Mean SR SRS probands", as.character(mean(na.omit(mergeSelf.p$srs2a_sr_raw))))
cat ("SD SR SRS probands", as.character(sd(na.omit(mergeSelf.p$srs2a_sr_raw))))
cat ("Mean IR SRS probands", as.character(mean(na.omit(mergeInf.p$srs2a_ir_raw))))
cat ("SD IR SRS probands", as.character(sd(na.omit(mergeInf.p$srs2a_ir_raw))))

cat ("Mean SR SRS unaff fam", as.character(mean(na.omit(unaff.f.self$srs2a_sr_raw))))
cat ("SD SR SRS unaff fam", as.character(sd(na.omit(unaff.f.self$srs2a_sr_raw))))
cat ("Mean IR SRS unaff fam", as.character(mean(na.omit(unaff.f.inf$srs2a_ir_raw))))
cat ("SD IR SRS unaff fam", as.character(sd(na.omit(unaff.f.inf$srs2a_ir_raw))))

scq.p <- merge(dem.p,scq, by = "study_id") # merge proband dem with SCQ scores
scq.f.unaff <- merge(dem.f.unaff, scq, by = "study_id") #merge unaff family dem with SCQ scores

cat ("Mean SCQ probands", as.character(mean(na.omit(scq.p$scq_score))))
cat ("SD SCQ probands", as.character(sd(na.omit(scq.p$scq_score))))
cat ("Mean SCQ unaff fam", as.character(mean(na.omit(scq.f.unaff$scq_score))))
cat ("SD SCQ unaff fam", as.character(sd(na.omit(scq.f.unaff$scq_score))))

# Self-report correlations ------------------------------------------------

self.p.na <- na.omit(mergeSelf.p[,c(2:6,9:11,13)]) #probands

cormat.self.p = as.matrix(as.data.frame(lapply(self.p.na, as.numeric))) #prepare data for correlation
colnames(cormat.self.p) <- c("SRS \n Total", "SRS \n SocMot",
                             "SRS \n SocCog", "SRS \n RRB",
                             "LSAS", "BAPQ \n Total",
                             "BAPQ \n Aloof", "BRIEF","Age")

stat.self.p <- rcorr(cormat.self.p, type = "spearman") #calculate correlations
p.self.p  <- formatC(stat.self.p$P, format = "e", digits = 2) #store p-values

self.f.na <- na.omit(unaff.f.self[,c(2:6,9:11,13)]) #family members without any psyc

cormat.self.f = as.matrix(as.data.frame(lapply(self.f.na, as.numeric))) #prepare data for correlation
colnames(cormat.self.f) <- c("SRS \n Total", "SRS \n SocMot",
                             "SRS \n SocCog", "SRS \n RRB",
                             "LSAS", "BAPQ \n Total",
                             "BAPQ \n Aloof", "BRIEF","Age")

stat.self.f <- rcorr(cormat.self.f, type = "spearman") #calculate correlations
p.self.f  <- formatC(stat.self.f$P, format = "e", digits = 2) # store p-values


# Informant-report correlations -------------------------------------------

inf.p.na <- na.omit(mergeInf.p[,c(2:8)]) # probands

cormat.inf.p = as.matrix(as.data.frame(lapply(inf.p.na, as.numeric))) #prepare data for correlation
colnames(cormat.inf.p ) <- c("SRS Total", "SRS \n SocMot",
                             "SRS \n SocCog", "SRS RRB",
                             "BRIEF", "ABC \n Stereotypy","Age")


stat.inf.p <- rcorr(cormat.inf.p, type = "spearman") #complete correlations
p.inf.p  <- formatC(stat.inf.p$P, format = "e", digits = 2) #store p-values

inf.f.na <- na.omit(unaff.f.inf[,c(2:8)]) # family members without any psyc

cormat.inf.f = as.matrix(as.data.frame(lapply(inf.f.na, as.numeric))) #prepare data for correlation
colnames(cormat.inf.f ) <- c("SRS Total", "SRS \n SocMot",
                             "SRS \n SocCog", "SRS RRB",
                             "BRIEF", "ABC \n Stereotypy","Age")

stat.inf.f <- rcorr(cormat.inf.f, type = "spearman") #complete correlations
p.inf.f  <- formatC(stat.inf.f$P, format = "e", digits = 2) #store p-values

# Method effect correlations ----------------------------------------------
#probands
merge.p.na <- na.omit(merge (mergeSelf.p[,c(1:5,11)],
                             mergeInf.p[,c(1:6)], 
                             by = "study_id"))
merge.p.na <- merge.p.na[,c(2:11)]
cormat.p.comp = as.matrix(as.data.frame(lapply(merge.p.na, as.numeric)))

colnames(cormat.p.comp) <- c("SRS SR \n Total","SRS SR \n SocMot",
                             "SRS SR \n SocCog","SRS SR \n RRB",
                             "BRIEF \n SR", "SRS IR \n Total",
                             "SRS IR \n SocMot","SRS IR \n SocCog",
                             "SRS IR \n RRB", "BRIEF \n IR")


stat.comp.p <- rcorr(cormat.p.comp, type = "spearman")
p.comp.p <- formatC(stat.comp.p$P, format = "e", digits = 2)


#family members without any psyc - unaffected
merge.f.na <- na.omit(merge (unaff.f.self[,c(1:5,11)],
                             unaff.f.inf[,c(1:6)], 
                             by = "study_id"))
merge.f.na <- merge.f.na[,c(2:11)]
cormat.f.comp = as.matrix(as.data.frame(lapply(merge.f.na, as.numeric)))

colnames(cormat.f.comp) <- c("SRS SR \n Total","SRS SR \n SocMot",
                             "SRS SR \n SocCog","SRS SR \n RRB",
                             "BRIEF \n SR", "SRS IR \n Total",
                             "SRS IR \n SocMot","SRS IR \n SocCog",
                             "SRS IR \n RRB", "BRIEF \n IR")

stat.comp.f <- rcorr(cormat.f.comp, type = "spearman")
p.comp.f <- formatC(stat.comp.f$P, format = "e", digits = 2)

# Create correlation figures ----------------------------------------------------------

#plot self- and informant-report for probands

M.self.p = cor(cormat.self.p, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))
corrplot(M.self.p, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=60, #Text label color and rotation
         # Combine with significance
         p.mat = stat.self.p$P, sig.level = 0.04, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

M.inf.p = cor(cormat.inf.p, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))
corrplot(M.inf.p, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=60, #Text label color and rotation
         # Combine with significance
         p.mat = stat.inf.p$P, sig.level = 0.038, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

#plot self- and informant-report for unaffected family members

M.self.f = cor(cormat.self.f, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))
corrplot(M.self.f, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=60, #Text label color and rotation
         # Combine with significance
         p.mat = stat.self.f$P, sig.level = 0.03889, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

M.inf.f = cor(cormat.inf.f, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))
corrplot(M.inf.f, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=60, #Text label color and rotation
         # Combine with significance
         p.mat = stat.inf.f$P, sig.level = 0.038, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)


#examine method effects

M.comp.p = cor(cormat.p.comp, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))
corrplot(M.comp.p, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=60, #Text label color and rotation
         p.mat = stat.comp.p$P, sig.level = 0.026, insig = "blank", # Combine with significance
         diag=FALSE)

M.comp.f = cor(cormat.f.comp, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))
corrplot(M.comp.f, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black",
         tl.col="black", tl.srt=60, 
         p.mat = stat.comp.f$P, sig.level = 0.037, insig = "blank", 
         diag=FALSE)

# format CNP data ---------------------------------------------------------

#reformat id so that family id and subject id are combined in study-id column
CNPproc$study_id <- paste(CNPproc$test_sessions.famid, CNPproc$test_sessions.subid, sep = "")

#change name of first column to study_id
colnames(CNPremote)[colnames(CNPremote)=="test_sessions_v.localid"] <- "study_id"

#select relevant columns
CNPremote <- CNPremote[,c("study_id",
                          "K_ER40.ER40_CR","K_ER40.ER40_CRT",
                          "MEDF36.MEDF36_A","MEDF36.MEDF36_T",
                          "ADT36.ADT36_A","ADT36.ADT36_T",
                          "PMAT24A.PMAT24_A_CR", "PMAT24A.PMAT24_A_RTCR",
                          "PCET.PCETRTCR","PCET.PCET_ACC2")]
CNPproc <- CNPproc[,c("study_id",
                      "K_ER40.ER40_CR","K_ER40.ER40_CRT",
                      "MEDF36.MEDF36_A","MEDF36.MEDF36_T",
                      "ADT36.ADT36_A","ADT36.ADT36_T",
                      "PMAT24A.PMAT24_A_CR", "PMAT24A.PMAT24_A_RTCR",
                      "PCET.PCETRTCR","PCET.PCET_ACC2")]
#merge data to be graphed
merge.remote <- merge(dem, CNPremote, by = "study_id")
merge.proc <- merge(dem, CNPproc, by = "study_id")

merge.remote$Method <- "Remote"
merge.proc$Method <- "Proctored"
merge.cnp <- rbind(merge.proc,merge.remote)

#create proband dataframe
cnp.p <- subset (merge.cnp, merge.cnp$recruit_type == "ASD proband")
#create family dataframe
cnp.f <- subset(merge.cnp, merge.cnp$recruit_type == "family member")
temp <- merge(cnp.f,dx, by = "study_id")
cnp.f.unaff <- subset(temp, temp$Affected == "No")

# CNP correlations --------------------------------------------------------

cnp.p.na <- na.omit (cnp.p)

cormat.p = as.matrix(as.data.frame(lapply(cnp.p.na, as.numeric)))
stat.p <- rcorr(cormat.p, type = "spearman")
p.p <- formatC(stat.p$P, format = "e", digits = 2)

cnp.f.na <- na.omit (cnp.f.unaff)

cormat.f = as.matrix(as.data.frame(lapply(cnp.f.na, as.numeric)))
stat.f <- rcorr(cormat.f, type = "spearman")
p.f <- formatC(stat.f$P, format = "e", digits = 2)

# CNP regressions ---------------------------------------------------------
#PROBANDS
#ER40 accuracy with SRS SR total and SRS SR Soc Cog
lm1 <- lm (K_ER40.ER40_CR ~ part_age_screen +  
             srs2a_sr_raw , data = merge.all.p)
summary(lm1)
lm.beta(lm1)

#ER40 accuracy with SRS IR total and SRS IR Soc Cog
lm2 <- lm (K_ER40.ER40_CR ~ part_age_screen +  
             srs2a_ir_raw , data = merge.all.p)
summary(lm2)
lm.beta(lm2)

#ER40 RT with SRS SR total and SRS SR Soc Cog
lm3 <- lm (K_ER40.ER40_CRT ~ part_age_screen + 
             srs2a_sr_raw , data = merge.all.p)
summary(lm3)
lm.beta(lm3)

#ER40 RT with SRS IR total and SRS IR Soc Cog
lm4 <- lm (K_ER40.ER40_CRT ~ part_age_screen +  
             srs2a_ir_raw , data = merge.all.p)
summary(lm4)
lm.beta(lm4)

#PCET accuracy with SRS SR total and BRIEF SR total
lm1 <- lm (PCET.PCET_ACC2 ~ part_age_screen + briefa_sr_gec_raw , data = merge.all.p)
summary(lm1)
lm.beta(lm1)

#PCET accuracy with SRS IR total and BRIEF IR total
lm2 <- lm (PCET.PCET_ACC2 ~ part_age_screen + 
             briefa_inf_gec_raw , data = merge.all.p)
summary(lm2)
lm.beta(lm2)

#PCET RT with SRS SR total and BRIEF SR total
lm3 <- lm (PCET.PCETRTCR ~ part_age_screen + 
             briefa_sr_gec_raw , data = merge.all.p)
summary(lm3)
lm.beta(lm3)

#PCET RT with SRS IR total and BRIEF IR total
lm4 <- lm (PCET.PCETRTCR ~ part_age_screen + 
             briefa_inf_gec_raw, data = merge.all.p)
summary(lm4)
lm.beta(lm4)

#PMAT accuracy with SRS SR total and BRIEF SR total
lm1 <- lm (PMAT24A.PMAT24_A_CR ~ part_age_screen + 
             briefa_sr_gec_raw, data = merge.all.p)
summary(lm1)
lm.beta(lm1)

#PMAT accuracy with SRS IR total and BRIEF IR total
lm2 <- lm (PMAT24A.PMAT24_A_CR ~ part_age_screen + 
             briefa_inf_gec_raw, data = merge.all.p)
summary(lm2)
lm.beta(lm2)

#PMAT RT with SRS SR total and BRIEF SR total
lm3 <- lm (PMAT24A.PMAT24_A_RTCR ~ part_age_screen + 
             briefa_sr_gec_raw, data = merge.all.p)
summary(lm3)
lm.beta(lm3)

#PMAT RT with SRS IR total and BRIEF IR total
lm4 <- lm (PMAT24A.PMAT24_A_RTCR ~ part_age_screen + 
             briefa_inf_gec_raw, data = merge.all.p)
summary(lm4)
lm.beta(lm4)

#UNAFFECTED FAMILY MEMBERS
#ER40 accuracy with SRS SR total and SRS SR Soc Cog
lm1 <- lm (K_ER40.ER40_CR ~ part_age_screen +  
             srs2a_sr_raw , data = unaff.f)
summary(lm1)
lm.beta(lm1)

#ER40 accuracy with SRS IR total and SRS IR Soc Cog
lm2 <- lm (K_ER40.ER40_CR ~ part_age_screen +  
             srs2a_ir_raw , data = unaff.f)
summary(lm2)
lm.beta(lm2)

#ER40 RT with SRS SR total and SRS SR Soc Cog
lm3 <- lm (K_ER40.ER40_CRT ~ part_age_screen + 
             srs2a_sr_raw , data = merge.all.p)
summary(lm3)
lm.beta(lm3)

#ER40 RT with SRS IR total and SRS IR Soc Cog
lm4 <- lm (K_ER40.ER40_CRT ~ part_age_screen +  
             srs2a_ir_raw , data = unaff.f)
summary(lm4)
lm.beta(lm4)


#PCET accuracy with SRS SR total and BRIEF SR total
lm1 <- lm (PCET.PCET_ACC2 ~ part_age_screen + 
             briefa_sr_gec_raw , data = unaff.f)
summary(lm1)
lm.beta(lm1)

#PCET accuracy with SRS IR total and BRIEF IR total
lm2 <- lm (PCET.PCET_ACC2 ~ part_age_screen + 
             briefa_inf_gec_raw , data = unaff.f)
summary(lm2)
lm.beta(lm2)

#PCET RT with SRS SR total and BRIEF SR total
lm3 <- lm (PCET.PCETRTCR ~ part_age_screen + 
             briefa_sr_gec_raw , data = unaff.f)
summary(lm3)
lm.beta(lm3)

#PCET RT with SRS IR total and BRIEF IR total
lm4 <- lm (PCET.PCETRTCR ~ part_age_screen + 
             briefa_inf_gec_raw, data = unaff.f)
summary(lm4)
lm.beta(lm4)

#PMAT accuracy with SRS SR total and BRIEF SR total
lm1 <- lm (PMAT24A.PMAT24_A_CR ~ part_age_screen + 
             briefa_sr_gec_raw, data = unaff.f)
summary(lm1)
lm.beta(lm1)

#PMAT accuracy with SRS IR total and BRIEF IR total
lm2 <- lm (PMAT24A.PMAT24_A_CR ~ part_age_screen + 
             briefa_inf_gec_raw, data = unaff.f)
summary(lm2)
lm.beta(lm2)

#PMAT RT with SRS SR total and BRIEF SR total
lm3 <- lm (PMAT24A.PMAT24_A_RTCR ~ part_age_screen + 
             briefa_sr_gec_raw, data = unaff.f)
summary(lm3)
lm.beta(lm3)

#PMAT RT with SRS IR total and BRIEF IR total
lm4 <- lm (PMAT24A.PMAT24_A_RTCR ~ part_age_screen + 
             briefa_inf_gec_raw, data = unaff.f)
summary(lm4)
lm.beta(lm4)

# Quantitative score regressions ------------------------------------------





