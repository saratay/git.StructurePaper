# correlation analyses for discrepancy paper
# SRS Total and BRIEF GEC only

library(irr)

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



# ICC Agreement Single Rater---------------------------------------------------------------------

irr::icc(merge.p.na[,c("srs2a_sr_raw","srs2a_ir_raw")],
    model = "oneway",
    type = "agreement",
    unit = "single")

irr::icc(merge.f.na[,c("srs2a_sr_raw","srs2a_ir_raw")],
    model = "oneway",
    type = "agreement",
    unit = "single")

irr::icc(merge.p.na[,c("briefa_sr_gec_raw","briefa_inf_gec_raw")],
    model = "oneway",
    type = "agreement",
    unit = "single")

irr::icc(merge.f.na[,c("briefa_sr_gec_raw","briefa_inf_gec_raw")],
    model = "oneway",
    type = "agreement",
    unit = "single")
