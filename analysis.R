########################################################################################################
# Learner Autonomy Meta-Analysis: Main Analyses
########################################################################################################


########################################################################################################
# Initial Set-up
########################################################################################################
# Clear Variables
rm(list=ls(all=TRUE))

# Load packages
test<-require(googledrive)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googledrive")
  require(googledrive)
}
test<-require(googlesheets4)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googlesheets4")
  require(googlesheets4)
}
test<-require(plyr)   #rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
test<-require(metafor)   #rma()
if (test == FALSE) {
  install.packages("metafor")
  require(metafor)
}
test<-require(clubSandwich)   #coef_test()
if (test == FALSE) {
  install.packages("clubSandwich")
  require(clubSandwich)
}
test<-require(ggplot2)   #ggplot()
if (test == FALSE) {
  install.packages("ggplot2")
  require(ggplot2)
}
test<-require(dplyr)   #bind_rows()
if (test == FALSE) {
  install.packages("dplyr")
  require(dplyr)
}
test<-require(flextable)   
if (test == FALSE) {
  install.packages("flextable")
  require(flextable)
}
test<-require(officer)   
if (test == FALSE) {
  install.packages("officer")
  require(officer)
}
test<-require(weightr)   
if (test == FALSE) {
  install.packages("weightr")
  require(weightr)
}
rm(test)

# Define functions

########################################################################################################
# Load data
# ########################################################################################################
# # set up to load from Google
drive_auth(email = "xxxx@gmail.com")

id <- drive_find(pattern = "autonomy coding ESSA", type = "spreadsheet")$id[1]

# load findings and studies
gs4_auth(email = "xxxx@gmail.com")


# load findings and studies
df <- read_sheet(id, sheet = "Sheet1", col_types = "c", skip = 1)

rm(id)

########################################################################################################
# Clean data
########################################################################################################
# drop irrelevant rows/columns
df <- subset(df, is.na(Drop) == TRUE)

# reformat columns as necessary
num <- c("TreatmentCluster", "ControlCluster","TreatmentN","ControlN","SampleSize", "TreatPreMean","TreatPreSD",
         "ControlPreMean","ControlPreSD","tPreTreatControl","tPrePostTreat","TreatPostMean","TreatPostSD",
         "ControlPostMean","ControlPostSD","tPostTreatControl","tPrePostControl","EffectSize")
df[num] <- lapply(df[num], as.numeric)
rm(num)

df[df == "ClusterQuasi-Experiment"] <- "Cluster Quasi-Experiment"
df[df == "ClusterRandomizedControlTrial"] <- "Cluster Randomized Control Trial"
df[df == "Researcher-designed test"] <- "Researcher-designed Test"
df[df == "Standardized test"] <- "Standardized Test"

########################################################################################################
# Prep data
########################################################################################################
################################################################
# Create unique identifiers (ES, study, program)
################################################################
df$ESId <- as.numeric(rownames(df))
df$StudyID <- as.numeric(as.factor(paste(df$Authors, df$Year, sep = ", ")))
table(df$StudyID)


################################################################
# Create dummies
################################################################

df$LargeSample <-0
df$LargeSample[which(df$`SampleSize`>=80)] <- 1
table(df$`SampleSize`, df$LargeSample, useNA = "ifany")

df$DelayedTest <- 0
df$DelayedTest[!is.na(df$DelayTest)] <- 1
table(df$`DelayTest`, df$DelayedTest, useNA = "ifany")

df$ClassroomSettingOnly <- 0
df$ClassroomSettingOnly[which(df$`Setting`=="Classroom")] <- 1
table(df$`Setting`, df$ClassroomSettingOnly, useNA = "ifany")

df$Lessthan12weeks <- 0
df$Lessthan12weeks[which(df$`Less12 weeks`=="1")] <- 1
table(df$`Less12 weeks`, df$Lessthan12weeks, useNA = "ifany")

df$OverTwoSemester <- 0
df$OverTwoSemester[which(df$`Duration`=="1 school year")] <- 1
df$OverTwoSemester[which(df$`Duration`=="1 academic year")] <- 1
df$OverTwoSemester[which(df$`Duration`=="2 school years")] <- 1
df$OverTwoSemester[which(df$`Duration`=="2 semesters")] <- 1
df$OverTwoSemester[which(df$`Duration`=="3 semesters")] <- 1
df$OverTwoSemester[which(df$`Duration`=="10 months")] <- 1
df$OverTwoSemester[which(df$`Duration`=="9 months")] <- 1
table(df$`Duration`, df$OverTwoSemester, useNA = "ifany")

df$CognitiveTraining <-0
df$CognitiveTraining[which(df$`TargetSkills`=="Cognitive")] <- 1
df$CognitiveTraining[which(df$`TargetSkills`=="Cognitive + Meta-cognitive")] <- 1
df$CognitiveTraining[which(df$`TargetSkills`=="Cognitive + Socio-emotional")] <- 1
df$CognitiveTraining[which(df$`TargetSkills`=="Cognitive + Meta-cognitive + Socio-emotional")] <- 1
table(df$`TargetSkills`, df$CognitiveTraining, useNA = "ifany")

df$MetacognitiveTraining <-0
df$MetacognitiveTraining[which(df$`TargetSkills`=="Meta-cognitive")] <- 1
df$MetacognitiveTraining[which(df$`TargetSkills`=="Cognitive + Meta-cognitive")] <- 1
df$MetacognitiveTraining[which(df$`TargetSkills`=="Meta-cognitive + Socio-emotional")] <- 1
df$MetacognitiveTraining[which(df$`TargetSkills`=="Cognitive + Meta-cognitive + Socio-emotional")] <- 1
table(df$`TargetSkills`, df$MetacognitiveTraining, useNA = "ifany")

df[df == "Socio-emotional"] <- "Social-emotional"
df$SocioaffectiveTraining <-0
df$SocioaffectiveTraining[which(df$`TargetSkills`=="Social-emotional")] <- 1
df$SocioaffectiveTraining[which(df$`TargetSkills`=="Cognitive + Socio-emotional")] <- 1
df$SocioaffectiveTraining[which(df$`TargetSkills`=="Meta-cognitive + Socio-emotional")] <- 1
table(df$`TargetSkills`, df$SocioaffectiveTraining, useNA = "ifany")

df$TrainMultiple <- 0
df$TrainMultiple[which(df$`TargetSkills`=="Cognitive + Meta-cognitive")] <- 1
df$TrainMultiple[which(df$`TargetSkills`=="Cognitive + Socio-emotional")] <- 1
df$TrainMultiple[which(df$`TargetSkills`=="Meta-cognitive + Socio-emotional")] <- 1
df$TrainMultiple[which(df$`TargetSkills`=="Cognitive + Meta-cognitive + Socio-emotional")] <- 1
table(df$`TargetSkills`, df$TrainMultiple, useNA = "ifany")

df$CooperativeLearning <- 0
df$CooperativeLearning[which(df$`Peer`=="Yes")] <- 1
table(df$`Peer`, df$CooperativeLearning, useNA = "ifany")

df$LearningDiaries <- 0
df$LearningDiaries[which(df$`Logs`=="Yes")] <- 1
table(df$`Logs`, df$LearningDiaries, useNA = "ifany")

df[df == "Overall proficiency"] <- "Overallproficiency"
df$OverallProficiency <- 0
df$OverallProficiency[which(df$`LanguageSkills`=="Overallproficiency")] <- 1
table(df$`LanguageSkills`, df$OverallProficiency, useNA = "ifany")

# make outcomes exclusive categories
df$AcademicOutcome <- 0
df$AcademicOutcome[which(df$`Academic`=="1")] <- 1
table(df$`Academic`, df$AcademicOutcome, useNA = "ifany")

df$CognitiveOutcome <- 0
df$CognitiveOutcome[which(df$`Cognitive`=="1" & is.na(df$Metacognitive) & is.na(df$Motivational) & is.na(df$Social))] <- 1
table(df$`Cognitive`, df$CognitiveOutcome, useNA = "ifany")

df$MetacognitiveOutcome <- 0
df$MetacognitiveOutcome[which(df$`Metacognitive`=="1" & is.na(df$Cognitive) & is.na(df$Motivational) & is.na(df$Social))] <- 1
table(df$`Metacognitive`, df$MetacognitiveOutcome, useNA = "ifany")

df$MotivationalOutcome <- 0
df$MotivationalOutcome[which(df$`Motivational`=="1" & is.na(df$Metacognitive) & is.na(df$Cognitive) & is.na(df$Social))] <- 1
table(df$`Motivational`, df$MotivationalOutcome, useNA = "ifany")

df$SocialOutcome <- 0
df$SocialOutcome[which(df$`Social`=="1" & is.na(df$Metacognitive) & is.na(df$Motivational) & is.na(df$Cognitive))] <- 1
table(df$`Social`, df$SocialOutcome, useNA = "ifany")

df$MultipleOutcome <- 0
df$MultipleOutcome[which(df$`Cognitive`=="1" & df$`Metacognitive`=="1")] <- 1
df$MultipleOutcome[which(df$`Metacognitive`=="1" & df$`Motivational`=="1")] <- 1
df$MultipleOutcome[which(df$`Metacognitive`=="1" & df$`Social`=="1" & df$`Motivational`=="1")] <- 1
df$MultipleOutcome[which(df$`Cognitive`=="1" & df$`Metacognitive`=="1" & df$`Social`=="1" & df$`Motivational`=="1")] <- 1

# check confounding
# list of variables
variable_list <- c("AcademicOutcome", "CognitiveOutcome", "MetacognitiveOutcome", "MotivationalOutcome", "SocialOutcome", "MultipleOutcome")

# Sample data frame (replace this with your actual data)
data <- df

# Calculate pairwise correlations
cor_matrix <- cor(data[, variable_list])

# Set a correlation threshold for considering variables as potentially confounding
correlation_threshold <- 0.7

# Find pairs of variables with correlation above the threshold
confounding_pairs <- data.frame()
for (i in 1:(length(variable_list) - 1)) {
  for (j in (i + 1):length(variable_list)) {
    if (abs(cor_matrix[i, j]) > correlation_threshold) {
      confounding_pairs <- rbind(confounding_pairs, c(variable_list[i], variable_list[j]))
    }
  }
}

if (nrow(confounding_pairs) == 0) {
  cat("No potential confounding variables found.\n")
} else {
  cat("Potential confounding variable pairs:\n")
  print(confounding_pairs)
}



################################################################
# Create study level
################################################################


################################################################
# Create centered variables
################################################################
vars <- c("LargeSample", "ClassroomSettingOnly", "Lessthan12weeks", "OverTwoSemester", 
          "CognitiveTraining", "MetacognitiveTraining", "SocioaffectiveTraining", "TrainMultiple", "CooperativeLearning", 
       "LearningDiaries", "AcademicOutcome", "CognitiveOutcome", "MetacognitiveOutcome", 
       "MotivationalOutcome", "SocialOutcome", "MultipleOutcome", "DelayedTest")  

centered <- paste(vars, ".c", sep = "")
df[centered] <- as.data.frame(lapply(df[vars], function(x) x-mean(x)))
rm(vars, centered)


################################################################
# Calculate effect sizes
################################################################
df <- escalc(measure = "SMD", m1i = TreatPreMean, m2i = ControlPreMean, 
             sd1i = TreatPreSD, sd2i = ControlPreSD, n1i = TreatmentN, 
             n2i = ControlN, data = df, append = TRUE, var.names=c("ES_pre", "var_pre"))

df <- escalc(measure = "SMD", m1i = TreatPostMean, m2i = ControlPostMean, 
             sd1i = TreatPostSD, sd2i = ControlPostSD, n1i = TreatmentN, 
             n2i = ControlN, data = df, append = TRUE, var.names=c("ES_post", "var_post"))

df$ES <- df$ES_post - df$ES_pre


################################################################
# Calculate variance
################################################################
#calculate standard errors
df$se<-sqrt(((df$TreatmentN+df$ControlN)/(df$TreatmentN*df$ControlN))+((df$ES*df$ES)/(2*(df$TreatmentN+df$ControlN))))

#calculate variance
df$var<-df$se*df$se


########################################################################################################
# Analyses
########################################################################################################
#################################################################################
# Exploratory Analyses
#################################################################################

#################################################################################
# Descriptive Statistics
#################################################################################
library(tableone)


# identify variables for descriptive tables (study-level and outcome-level)
vars_study <- c("Language", "PubType", "Studydesign", "LargeSample",
                "Lessthan12weeks", "OverTwoSemester", "ClassroomSettingOnly", "CooperativeLearning", "LearningDiaries",  
                "CognitiveTraining", "MetacognitiveTraining", "SocioaffectiveTraining", "TrainMultiple")

vars_outcome <- c("AssessmentTool", 
                  "DelayedTest", "AcademicOutcome", "OverallProficiency", 
                  "CognitiveOutcome", "MetacognitiveOutcome", "MotivationalOutcome", "SocialOutcome", "MultipleOutcome")



# To make this work, you will need a df that is at the study-level for study-level 
# variables (such as research design) you may have already created this (see above, with study-level ESs), but if you didn't, here is an easy way:
# 1) make df with *only* the study-level variables of interest and studyIDs in it
# study <- unique(df[c("Authors", "Year", "StudyID")])

study_level <- unique(df[c("Authors", "Year", "StudyID", "Logs")])

#study_level <- unique(df[c("Authors", "Year", "StudyID")])
study_level <- df[c("Authors", "StudyID", vars_study)]
# 2) remove duplicated rows
study_level <- unique(study_level)
# 3) make sure it is the correct number of rows (should be same number of studies you have)
length(study_level$StudyID)==length(unique(df$StudyID))
# don't skip step 3 - depending on your data structure, some moderators can be
# study-level in one review, but outcome-level in another

# create the table "chunks"
table_study_df <- as.data.frame(print(CreateTableOne(vars = vars_study, data = study_level, factorVars = vars_study, includeNA = TRUE), showAllLevels = TRUE))
table_outcome_df <- as.data.frame(print(CreateTableOne(vars = vars_outcome, data = df, includeNA = TRUE, factorVars = vars_outcome), showAllLevels = TRUE))

rm(vars_study, vars_outcome)

#################################################################################
# Null Model
#################################################################################
# estimate a covariance matrix
V_list <- impute_covariance_matrix(vi = df$var,  #known correlation vector
                                   cluster = df$StudyID, #study ID
                                   r = 0.80) #assumed correlation 

MVnull <- rma.mv(yi=ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 random = ~1 | StudyID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=df, #define data
                 method="REML") #estimate variances using REML
MVnull

#t-tests of each covariate #
MVnull.coef <- coef_test(MVnull,#estimation model above
                         cluster=df$StudyID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best) #robust standard error
MVnull.coef

#################################################################################
# Metaregression
#################################################################################

terms <- c("LargeSample", "ClassroomSettingOnly", 
           "Lessthan12weeks", "OverTwoSemester", "CognitiveTraining", "MetacognitiveTraining", 
           "SocioaffectiveTraining", "CooperativeLearning", "LearningDiaries", 
           "AcademicOutcome", "CognitiveOutcome", "MetacognitiveOutcome", "MotivationalOutcome",
           "SocialOutcome", "TrainMultiple", "MultipleOutcome") 


# use centered versions
terms.c <- paste(terms, ".c", sep = "")


formula <- reformulate(termlabels = c(terms.c))

MVfull <- rma.mv(yi=ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | StudyID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=df, #define data
                 method="REML") #estimate variances using REML
MVfull

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=df$StudyID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVfull.coef

# Forest plot
forest(MVfull)

#################################################################################
# Calculating Marginal Means
#################################################################################
# re-run model for each moderator to get marginal means for each #

# set up table to store results
means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))

### moderators for means need to be factors...so if true binary, just recode the dummy, if multi-level, need to replace with a multi-level factor variable instead of multiple dummies.

# # make factor moderators for means

df$LargeSample.f <- as.factor(ifelse(df$SampleSize>=80, "Large Sample", "Small Sample"))

df$ClassroomSettingOnly.f <- as.factor(ifelse(df$Setting=="Classroom", "Classroom Only", "Classroom & Other Settings"))

df$Lessthan12weeks.f <- as.factor(ifelse(df$Lessthan12weeks==1, "Less than 12 weeks", "12 weeks or longer"))

df$OverTwoSemester.f <- as.factor(ifelse(df$OverTwoSemester==1, "Two semesters or longer", "Less than two semesters"))

df$TrainingContent.f <- NA
df$TrainingContent.f[which(df$CognitiveTraining==1)] <- "Cognitive training"
df$TrainingContent.f[which(df$MetacognitiveTraining==1)] <- "Metacognitive training"
df$TrainingContent.f[which(df$SocioaffectiveTraining==1)] <- "Socioaffective training"
df$TrainingContent.f[which(df$TrainMultiple==1)] <- "Multiple skills"

df$CooperativeLearning.f <- as.factor(ifelse(df$CooperativeLearning==1, "Cooperative learning", "No cooperative learning"))

df$LearningDiaries.f <- as.factor(ifelse(df$LearningDiaries==1, "Learning diaries", "No learning diaries"))

df$OutcomeType.f <- factor(1* df$AcademicOutcome + 2 * df$CognitiveOutcome + 3 * df$MetacognitiveOutcome + 4 * df$MotivationalOutcome + 5 * df$SocialOutcome + 6 * df$MultipleOutcome, 
                         labels = c("Academic outcome", "Cognitive outcome", "Metacognitive outcome", "Motivational outcome", "Social outcome", "Multiple strategies outcome"))


mods <- c("LargeSample.f", "ClassroomSettingOnly.f", "Lessthan12weeks.f",
          "OverTwoSemester.f", "TrainingContent.f",
         "CooperativeLearning.f", "LearningDiaries.f", 
         "OutcomeType.f")

for(i in 1:length(mods)){
  # i <- 8
  formula <- reformulate(termlabels = c(mods[i], terms.c, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=ES, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=df, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=df$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means

#publication bias , selection modeling
MVfull_y <- MVfull$ES
MVfull_v <- MVfull$var
weightfunct(effect = df$ES, v = df$var)
weightfunct(df$ES, df$var, steps = c(.05, 1))

#################################################################################
# Percent Variance Explained
#################################################################################
##% explained by model
# Variation of first (smaller) model
tot_var_null <- sum(MVnull$sigma2) #total variation in effect sizes (omega2 + tau2)

# Variation of second (larger) model
tot_var_covs <- sum(MVfull$sigma2) #total variation in effect sizes after covariates

# difference between the two
perc_var_explained <- 100*(1-tot_var_covs/tot_var_null)
print(perc_var_explained) #R2 for model 

#################################################################################
# Heterogeneity
#################################################################################
# 95% prediction intervals
print(PI_upper <- round(MVfull$b[1] + (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2])), 2))
print(PI_lower <- round(MVfull$b[1] - (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2])), 2))


#################################################################################
# Descriptives Table
table_study_df$Category <- row.names(table_study_df)
rownames(table_study_df) <- c()
table_study_df <- table_study_df[c("Category", "level", "Overall")]
table_study_df$Category[which(substr(table_study_df$Category, 1, 1)=="X")] <- NA
table_study_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_study_df$Category)
table_study_df$Category[which(table_study_df$Category=="n")] <- "Total Studies"
table_study_df$Category[which(table_study_df$Category=="PubType")] <- "Publication Type"
table_study_df$level[which(table_study_df$level=="1")] <- "Yes"
table_study_df$level[which(table_study_df$level=="0")] <- "No"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_study_df$Category)) {
  if(is.na(table_study_df$Category[i])) {
    table_study_df$Category[i] <- table_study_df$Category[i-1]
  }
}

table_outcome_df$Category <- row.names(table_outcome_df)
rownames(table_outcome_df) <- c()
table_outcome_df <- table_outcome_df[c("Category", "level", "Overall")]
table_outcome_df$Category[which(substr(table_outcome_df$Category, 1, 1)=="X")] <- NA
table_outcome_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_outcome_df$Category)
table_outcome_df$Category[which(table_outcome_df$Category=="n")] <- "Total Effect Sizes"
table_outcome_df$level[which(table_outcome_df$level=="1")] <- "Yes"
table_outcome_df$level[which(table_outcome_df$level=="0")] <- "No"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_outcome_df$Category)) {
  if(is.na(table_outcome_df$Category[i])) {
    table_outcome_df$Category[i] <- table_outcome_df$Category[i-1]
  }
}

# MetaRegression Table

# Marginal Means Table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

#########################
#########################
#########################
# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(df[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(df[c("StudyID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# Descriptives Table
myreport <- body_add_par(x = myreport, value = "Table: Descriptive Statistics", style = "Normal")
descriptives_study <- flextable(head(table_study_df, n=nrow(table_study_df)))
descriptives_study <- add_header_lines(descriptives_study, values = c("Study Level"), top = FALSE)
descriptives_study <- theme_vanilla(descriptives_study)
descriptives_study <- merge_v(descriptives_study, j = c("Category"))
descriptives_study <- set_table_properties(descriptives_study, width = 1, layout = "autofit")
myreport <- body_add_flextable(x = myreport, descriptives_study, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = " ", style = "Normal")

descriptives_outcome <- flextable(head(table_outcome_df, n=nrow(table_outcome_df)))
descriptives_outcome <- delete_part(descriptives_outcome, part = "header")
descriptives_outcome <- add_header_lines(descriptives_outcome, values = c("Outcome Level"))
descriptives_outcome <- theme_vanilla(descriptives_outcome)
descriptives_outcome <- merge_v(descriptives_outcome, j = c("Category"))
descriptives_outcome <- set_table_properties(descriptives_outcome, width = 1, layout = "autofit")
myreport <- body_add_flextable(x = myreport, descriptives_outcome, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# MetaRegression Table
model_null <- flextable(head(MVnull.coef, n=nrow(MVnull.coef)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)
#myreport <- body_add_par(x = myreport, value = "", style = "Normal")

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full <- colformat_double(model_full,  j = colkeys, digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression"))
model_full <- theme_vanilla(model_full)
model_full <- set_table_properties(model_full, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_full, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Heterogeneity Assessment
# 95% PI
myreport <- body_add_par(x = myreport, value = paste("95% PI: ", PI_lower, " to ", PI_upper, sep = ""), style = "Normal")
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Percent variance explained
myreport <- body_add_par(x = myreport, value = paste("Percent Variance Explained: ", round(perc_var_explained, 2), "%", sep = ""), style = "Normal")
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Write to word doc
file = paste("TableResults.docx", sep = "")
print(myreport, file)