setwd("/Users/Downloads/summer term/data/0 all data")
##################################################
########mortality rate summary by pathogen########
##################################################
BAIV <- read.csv(file = "B_aiv_farm_result.csv", stringsAsFactors = TRUE)
attach(BAIV)
BAIV$ttmortality_rate <- BAIV$mortality_rate*10000
View(BAIV)

install.packages("psych")
library(psych) 
by(BAIV$ttmortality_rate,BAIV$m_pos,describe)
by(BAIV$ttmortality_rate,BAIV$h9_pos,describe)
by(BAIV$ttmortality_rate,BAIV$h5_pos,describe)

#####camoy in siteB
BCM <- read.csv(file = "B_camoy_farm_result.csv", stringsAsFactors = TRUE)
names(BCM)
attach(BCM)
BCM$ttmortality_rate <- BCM$mortality_rate*10000
View(BCM)

by(BCM$ttmortality_rate,BCM$ccoli_pos,describe)
by(BCM$ttmortality_rate,BCM$cjejuni_pos,describe)

####ecoli in siteB
BECL <- read.csv(file = "B_ecoli_farm_result.csv", stringsAsFactors = TRUE)
names(BECL)
attach(BECL)
BECL$ttmortality_rate <- BECL$mortality_rate*10000
View(BECL)

by(BECL$ttmortality_rate,BECL$ecoli_pos,describe)


####nts in siteB
BNTS <- read.csv(file = "B_nts_farm_result.csv", stringsAsFactors = TRUE)
names(BNTS)
attach(BNTS)
BNTS$ttmortality_rate <- BNTS$mortality_rate*10000
View(BNTS)
by(BNTS$ttmortality_rate,BNTS$nts_pos,describe)


####campy in siteB
GLB <- read.csv(file = "G_lab_campy_farm_result.csv", stringsAsFactors = TRUE)
names(GLB)
attach(GLB)
GLB$ttmortality_rate <- GLB$mortality_rate*10000
View(GLB)

by(GLB$ttmortality_rate,GLB$pcr_ccoli,describe)
by(GLB$ttmortality_rate,GLB$pcr_cjejun,describe)

####ecoli in siteG
GLBE <- read.csv(file = "G_lab_ecoli_farm_result.csv", stringsAsFactors = TRUE)
names(GLBE)
attach(GLBE)
GLBE$ttmortality_rate <- GLBE$mortality_rate*10000
View(GLBE)

by(GLBE$ttmortality_rate,GLBE$pcr,describe)
####nts in siteG
GNTS <- read.csv(file = "G_nts_farm_result.csv", stringsAsFactors = TRUE)
names(GNTS)
attach(GNTS)
GNTS$ttmortality_rate <- GNTS$mortality_rate*10000
View(GNTS)

by(GNTS$ttmortality_rate,GNTS$pcr,describe)
####campyin siteT
TLBC <- read.csv(file = "T_lab_campy_farm_result.csv", stringsAsFactors = TRUE)
names(TLBC)
attach(TLBC)
TLBC$ttmortality_rate <- TLBC$mortality_rate*10000
View(TLBC)

by(TLBC$ttmortality_rate,TLBC$pcr,describe)

####ecoli in siteT
TLBE <- read.csv(file = "T_lab_ecoli_farm_result.csv", stringsAsFactors = TRUE)
names(TLBE)
attach(TLBE)
TLBE$ttmortality_rate <- TLBE$mortality_rate*10000
View(TLBE)

by(TLBE$ttmortality_rate,TLBE$pcr,describe)

####nts in siteT
TLBN <- read.csv(file = "T_lab_nts_farm_result.csv", stringsAsFactors = TRUE)
names(TLBN)
attach(TLBN)
TLBN$ttmortality_rate <- TLBN$mortality_rate*10000
View(TLBN)

by(TLBN$ttmortality_rate,TLBN$pcr,describe)

####
VAL <- read.csv(file = "V_all.csv", stringsAsFactors = TRUE)
names(VAL)
attach(VAL)
VAL$ttmortality_rate <- VAL$mortality_rate*10000
View(VAL)


by(VAL$ttmortality_rate,VAL$ct_m,describe)
by(VAL$ttmortality_rate,VAL$Ct_m,describe)
by(VAL$ttmortality_rate,VAL$cm_pcr,describe)
by(VAL$ttmortality_rate,VAL$nts_pcr,describe)
by(VAL$ttmortality_rate,VAL$eco_pcr,describe)

##########################################################
##########all country outcome summary with pathogen#######
##########################################################

NTSAL <- read.csv(file = "nts_all.csv", stringsAsFactors = TRUE)
names(NTSAL)
attach(NTSAL)
NTSAL$ttmortality_rate <- NTSAL$mortality_rate*10000

by(NTSAL$ttmortality_rate,NTSAL$pcr,describe)

####
ECOAL <- read.csv(file = "ecoli_all.csv", stringsAsFactors = TRUE)
names(ECOAL)
attach(ECOAL)
ECOAL$ttmortality_rate <- ECOAL$mortality_rate*10000

by(ECOAL$ttmortality_rate,ECOAL$pcr,describe)
###
CMPAL <- read.csv(file = "campy_all.csv", stringsAsFactors = TRUE)
names(CMPAL)
attach(CMPAL)
CMPAL$ttmortality_rate <- CMPAL$mortality_rate*10000

by(CMPAL$ttmortality_rate,CMPAL$pcr,describe)

###
AIVAL <- read.csv(file = "aiv_all.csv", stringsAsFactors = TRUE)
names(AIVAL)
attach(AIVAL)
AIVAL$ttmortality_rate <- AIVAL$mortality_rate*1000

by(AIVAL$ttmortality_rate,AIVAL$Ct_m,describe)

##########################################################################
######################## quantile of basic data###########################
##########################################################################

BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
quantile(BSAL$num_dead,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$real_sick,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$tmorbidity_rate,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$morbidity_risk,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$tmortality_rate,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$mortality_risk,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$animals,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$currentage,c(0.25,0.5,0.75,1),na.rm=TRUE)
quantile(BSAL$age,c(0.25,0.5,0.75,1),na.rm=TRUE)


describe(BSAL$morbidity_risk)
describe(BSAL$mortality_risk)
describe(BSAL$currentage)
describe(BSAL$age)

BSAL$hmorbidity_risk <- BSAL$morbidity_risk * 100
BSAL$hmortality_risk <- BSAL$mortality_risk * 100
describe(BSAL$hmorbidity_risk)
describe(BSAL$hmortality_risk)

describe(BSAL$tmorbidity_rate)
describe(BSAL$tmortality_rate)

#########################################################################################################################
##########screening explanatory variables of 4 regions###################################################################
####################Clear all variables with NA values and all variables with the same value in the original file########
#########################################################################################################################
##############siteB
dataB <- read.csv("SA_farm_questionnaire B.csv")

#Find variables with all missing values
missing_varsB <- names(dataB)[apply(dataB, 2, function(x) all(is.na(x)))]

#Find variables with the same value for all
same_value_varsB <- names(dataB)[sapply(dataB, function(x) length(unique(x))) == 1]
#Eliminate variables with all missing values and all values that are the same
filtered_dataB <- dataB[, !(names(dataB) %in% c(missing_varsB, same_value_varsB)), drop = FALSE]

write.csv(filtered_dataB, "filtered_dataB.csv", row.names = FALSE)

##############siteG
dataG <- read.csv("SA_farm_questionnaire G.csv")
missing_varsG <- names(dataG)[apply(dataG, 2, function(x) all(is.na(x)))]

same_value_varsG <- names(dataG)[sapply(dataG, function(x) length(unique(x))) == 1]
filtered_dataG <- dataG[, !(names(dataG) %in% c(missing_varsG, same_value_varsG)), drop = FALSE]
write.csv(filtered_dataG, "filtered_dataG.csv", row.names = FALSE)

##############siteV
dataV <- read.csv("SA_farm_questionnaire V.csv")

missing_varsV <- names(dataV)[apply(dataV, 2, function(x) all(is.na(x)))]
same_value_varsV <- names(dataV)[sapply(dataV, function(x) length(unique(x))) == 1]
filtered_dataV <- dataV[, !(names(dataV) %in% c(missing_varsV, same_value_varsV)), drop = FALSE]
write.csv(filtered_dataV, "filtered_dataV.csv", row.names = FALSE)

#############################################################################
####################Extract the same valid variables across countries########
#############################################################################
file_paths <- c("filtered_dataB.csv", "filtered_dataG.csv", "filtered_dataV.csv", "T all variables.csv") 

#Create an empty variable list
common_variables <- c()

#Iterate through each file
for (file_path in file_paths) {
  data <- read.csv(file_path)
  
  #Extract variables from the current file
  file_variables <- colnames(data)
  
  #If it is the first file, add all variables to the variable list
  if (length(common_variables) == 0) {
    common_variables <- file_variables
  } else {
    #If not the first file, keep variables that co-occur with the previous file
    common_variables <- intersect(common_variables, file_variables)
  }
}
print(common_variables)

#############################################################################
####################### description of total sampled chickens################
#############################################################################
BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
quantile(BSAL$n,c(0.25,0.5,0.75,1),na.rm=TRUE)
describe(BSAL$n)

min_value <- 0

#The boundaries of the histogram
bin_width <- 2500
max_value <- 27000

#Create a histogram, breaks is the width of each column, freq and probability are the statistics + expression of the y-axis
hist(BSAL$n, breaks = max_value/bin_width, freq = TRUE, probability = FALSE, main = "Total number of sampled chickens",
     xlab = "number", ylab = "Frequency", xlim = c(0, 29000), ylim = c(0, 130))

##

#################### frequency table of startage############################
table(BSAL$startage,BSAL$region)

############################ boxplot of startage############################
BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
library(ggplot2)

#Create a new dataset, screen out observations with tmortality_rate less than 10 and startage less than 2, and ignore NA values
BSALSGmt <- BSAL[!is.na(BSAL$tmortality_rate) & !is.na(BSAL$startage) &
                   BSAL$tmortality_rate < 10 & BSAL$startage < 2, ]
#Create a new dataset, screen out observations with tmortality_rate less than 20 and startage less than 2, and ignore NA values
BSALSGmb <- BSAL[!is.na(BSAL$tmorbidity_rate) & !is.na(BSAL$startage) &
                   BSAL$tmorbidity_rate < 10 & BSAL$startage < 2, ]
#set color of legend
my_colors <- c("0" = "blue", "1" = "red")


##Box plot of the mortality rate grouped by start age
ggplot(BSALSGmt, aes(x = factor(BSALSGmt$startage), y = BSALSGmt$tmortality_rate, fill =factor(BSALSGmt$startage))) +
  geom_boxplot() +
  facet_wrap(~ BSALSGmt$region, ncol = 4) +
  labs(x = "Start Age", y = "Mortality Rate(/1000days)", fill = "Start Age", title="Box plot of the mortality rate grouped by start age and regions") +
  theme(plot.title=element_text(hjust=0.5, vjust=0.5,angle=360) ) + 
  scale_fill_manual(values = my_colors)



##Box plot of the morbidity rate grouped by start age
ggplot(BSALSGmb, aes(x = factor(BSALSGmb$startage), y = BSALSGmb$tmorbidity_rate, fill =factor(BSALSGmb$startage))) +
  geom_boxplot() +
  facet_wrap(~ BSALSGmb$region, ncol = 4) +
  labs(x = "Start Age", y = "Morbidity Rate(/1000days)", fill = "Start Age", title="Box plot of the morbidity rate grouped by start age and regions") +
  theme(plot.title=element_text(hjust=0.5, vjust=0.5,angle=360) ) + 
  scale_fill_manual(values = my_colors)


##########################################################################
################### describe a change of age by outcomes##################
##########################################################################
BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)


##########log(mortality rate)~age

#BSAL1: Select rows in the tmortality_rate column of table BSAL that do not contain NA and 0, because they will become inf after log
BSAL1 <- BSAL[!is.na(BSAL$tmortality_rate) & BSAL$tmortality_rate != 0, ]
y1 <-log(BSAL1$tmortality_rate)

cor(BSAL1$age,y1) #pearson
cor.test(BSAL1$age,y1) 

#plot chart
plot(BSAL1$age, y1, xlab="age(days)", ylab="log(mortality rate)(/1000days)", main="plot between age and log(mortality rate)")
FIT1 <- lm(y1~BSAL1$age)
summary(FIT1)
abline(FIT1)#Add trend line
library(car)
outlierTest(FIT1) #Verify the presence of outlier in the lm() fit. NoStudentized residuals with Bonferonni p < 0.05


#Normality test-SW test
#The closer the calculated W value is to 1, the closer it is to normality
shapiro.test(BSAL1$age) 
shapiro.test(y1) 

#perform Kolmogorov-Smirnov test：P >0.05, cannot reject the normality assumption
ks.test(BSAL1$age, "pnorm")
ks.test(y1, "pnorm")

#################log(mortality rate)~log(age)
y1 <-log(BSAL1$tmortality_rate)
x1 <-log(BSAL1$age)
cor(x1,y1)
cor.test(x1,y1) 

plot(x1, y1, xlab="log(age)(days)", ylab="log(mortality rate)(/1000days)", main="plot between log(age) and log(mortality rate)")
FIT2 <- lm(y1~x1)
summary(FIT2)
abline(FIT2)
ks.test(x1, "pnorm")

cor(x1,y1) 
cor.test(x1,y1) 



##########morbidity rate~age
plot(BSAL$currentage, BSAL$tmorbidity_rate, xlab="current age(day)", ylab="morbidity rate(/1000days)", main="plot between current age and morbidity rate")

##########log(morbidity rate)~age

#BSAL2:Select rows in the tmorbidity_rate column of the BSAL table that do not contain NA and 0, because they will change to inf after log
BSAL2 <- BSAL[!is.na(BSAL$tmorbidity_rate) & BSAL$tmorbidity_rate != 0, ]
y2 <-log(BSAL2$tmorbidity_rate)

cor(BSAL2$age,y2) 
cor.test(BSAL2$age,y2) 

plot(BSAL2$age, y2, xlab="age(days)", ylab="log(morbidity rate)(/1000days)", main="plot between age and log(morbidity rate)")
FIT2 <- lm(y2~BSAL2$age)
summary(FIT2)
abline(FIT2)

outlierTest(FIT2) 

#perform Kolmogorov-Smirnov test：P >0.05, cannot reject the normality assumption
ks.test(BSAL2$age, "pnorm")
ks.test(y2, "pnorm")

##########log(mortality risk)~age

#BSAL1: Select rows in the mortality_risk column of table BSAL that do not contain NA and 0, because they will become inf after log
BSAL3 <- BSAL[!is.na(BSAL$mortality_risk) & BSAL$mortality_risk != 0, ]
y3 <-log(BSAL3$mortality_risk)

cor(BSAL3$age,y3)
cor.test(BSAL3$age,y3) 

plot(BSAL3$age, y3, xlab="age(days)", ylab="log(mortality rate)(/1000days)", main="plot between age and log(mortality risk)")
FIT3 <- lm(y3~BSAL3$age)
summary(FIT3)
abline(FIT3)
library(car)
outlierTest(FIT3)

shapiro.test(BSAL3$age)
shapiro.test(y3) 

#perform Kolmogorov-Smirnov test：P >0.05, cannot reject the normality assumption
ks.test(BSAL1$age, "pnorm")
ks.test(y3, "pnorm")


##########log(morbidity risk)~age

BSAL3 <- BSAL[!is.na(BSAL$morbidity_risk) & BSAL$morbidity_risk != 0, ]
y3 <-log(BSAL3$morbidity_risk)

cor(BSAL3$age,y3) 
cor.test(BSAL3$age,y3) 

plot(BSAL3$age, y3, xlab="age(days)", ylab="log(morbidity rate)(/1000days)", main="plot between age and log(morbidity risk)")
FIT3 <- lm(y3~BSAL3$age)
summary(FIT3)
abline(FIT3)
library(car)
outlierTest(FIT3)

shapiro.test(BSAL3$age)
shapiro.test(y3) 

#perform Kolmogorov-Smirnov test：P >0.05, cannot reject the normality assumption
ks.test(BSAL1$age, "pnorm")
ks.test(y3, "pnorm")


##########mortality risk~current age
BSAL$hmortality_risk <- BSAL$mortality_risk*100
plot(BSAL$currentage, BSAL$hmortality_risk, xlab="current age(day)", ylab="mortality risk(%))", main="plot between current age and mortality risk")


##########morbidity risk~current age
BSAL$hmorbidity_risk <- BSAL$morbidity_risk*100
plot(BSAL$currentage, BSAL$hmorbidity_risk, xlab="current age(day)", ylab="morbidity risk(%)", main="plot between current age and morbidity risk")


##########lethality~current age
plot(BSAL$currentage, BSAL$lethality, xlab="curren age(day)", ylab="lethality(%)", main="plot between current age and lethality")



#####################################################################
#######describe current age&age&dead&real sick after dropping########
#######.     outlier observations of start age.              ########
#####################################################################
BSAL_drop_outliers_startage <- BSAL[!is.na(BSAL$age) & BSAL$startage < 2, ]

describe(BSAL_drop_outliers_startage$currentage)
quantile(BSAL_drop_outliers_startage$currentage)

describe(BSAL_drop_outliers_startage$age)
quantile(BSAL_drop_outliers_startage$age)

##histogram of startage & age
min_value <- 0  #Initial value

bin_width <- 10
max_value <- 200

hist(BSAL_drop_outliers_startage$currentage, breaks = max_value/bin_width, freq = TRUE, probability = FALSE, main = "Histogram of current age",
     xlab = "Current Age(day)", ylab = "Frequency", xlim = c(0, 200), ylim = c(0, 130))

hist(BSAL_drop_outliers_startage$age, breaks = max_value/bin_width, freq = TRUE, probability = FALSE, main = "Histogram of age",
     xlab = "Age(days)", ylab = "Frequency", xlim = c(0, 200), ylim = c(0, 130))

##histogram of dead
bin_width_dead <- 100
max_value_dead <- 2500
hist(BSAL_drop_outliers_startage$num_dead, breaks = max_value_dead/bin_width_dead, freq = TRUE, probability = FALSE, main = "Histogram of dead number",
     xlab = "Number of Dead", ylab = "Frequency", xlim = c(0, 2500), ylim = c(0, 150))

##histogram of real sick
bin_width_rsick <- 100
max_value_rsick <- 5000
hist(BSAL_drop_outliers_startage$real_sick, breaks = max_value_rsick/bin_width_rsick, freq = TRUE, probability = FALSE, main = "Histogram of real sick number",
     xlab = "Number of Real Sick", ylab = "Frequency", xlim = c(0, 5000), ylim = c(0, 80))


##########################################################################
#######general describe mortality rate, risk & morbidity rate, risk#######
#######.    after dropping outlier observations of start age.      #######
##########################################################################
describe(BSAL_drop_outliers_startage$tmortality_rate)
quantile(BSAL_drop_outliers_startage$tmortality_rate, na.rm = TRUE)

describe(BSAL_drop_outliers_startage$tmorbidity_rate)
quantile(BSAL_drop_outliers_startage$tmorbidity_rate, na.rm = TRUE)

describe(BSAL_drop_outliers_startage$hmortality_risk)
quantile(BSAL_drop_outliers_startage$hmortality_risk, na.rm = TRUE)

describe(BSAL_drop_outliers_startage$hmorbidity_risk)
quantile(BSAL_drop_outliers_startage$hmorbidity_risk, na.rm = TRUE)


##histogram of mortality rate
bin_width_mtrt <- 5
max_value_mtrt <- 100
hist(BSAL_drop_outliers_startage$tmortality_rate, breaks = max_value_mtrt/bin_width_mtrt, freq = TRUE, probability = FALSE, main = "Histogram of mortality rate",
     xlab = "Mortality Rate (/1000days)", ylab = "Frequency", xlim = c(0, 60), ylim = c(0, 250))

##histogram of morbidity rate
bin_width_mbrt <- 5
max_value_mbrt <- 100
hist(BSAL_drop_outliers_startage$tmorbidity_rate, breaks = max_value_mbrt/bin_width_mbrt, freq = TRUE, probability = FALSE, main = "Histogram of morbidity rate",
     xlab = "Morbidity Rate (/1000days)", ylab = "Frequency", xlim = c(0, 60), ylim = c(0, 250))


##histogram of mortality risk
BSAL_drop_outliers_startage$hmortality_risk <- BSAL_drop_outliers_startage$mortality_risk*100
bin_width_mtrk <- 5
max_value_mtrk <- 100
hist(BSAL_drop_outliers_startage$hmortality_risk, breaks = max_value_mtrk/bin_width_mtrk, freq = TRUE, probability = FALSE, main = "Histogram of mortality risk",
     xlab = "Mortality Risk (%)", ylab = "Frequency", xlim = c(0, 100), ylim = c(0, 200))

##histogram of morbidity risk
BSAL_drop_outliers_startage$hmorbidity_risk <- BSAL_drop_outliers_startage$morbidity_risk*100
bin_width_mbrk <- 5
max_value_mbrk <- 100
hist(BSAL_drop_outliers_startage$hmorbidity_risk, breaks = max_value_mbrk/bin_width_mbrk, freq = TRUE, probability = FALSE, main = "Histogram of morbidity risk",
     xlab = "Morbidity Risk (%)", ylab = "Frequency", xlim = c(0, 100), ylim = c(0, 200))


##########################################################################
##############cross table using poisson count ############################
#######clinical signs (Respiratory,Gastro-intestinal) & pathogen (nts)####
##########################################################################
ALNTS_RG <- read.csv(file = "all_nts_RG.csv", stringsAsFactors = TRUE)

# table() function to create frequency table
#addmargins() function to add sum row and column
table_nts_R <- table(ALNTS_RG$signs_R, ALNTS_RG$pcr, ALNTS_RG$region)
addmargins(table_nts_R)

table_nts_G <- table(ALNTS_RG$signs_G, ALNTS_RG$pcr, ALNTS_RG$region)
addmargins(table_nts_G)

##########################################################################
##############Histogram of age by region in one graph#####################
##########################################################################
BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)

BSAL_drop_outliers_startage <- BSAL[!is.na(BSAL$age) & BSAL$startage < 2, ]
library(ggplot2)

##histogram of age overall
min_value <- 0

bin_width <- 10
max_value <- 200

p <- hist(BSAL_drop_outliers_startage$currentage, breaks = max_value/bin_width, freq = TRUE, probability = FALSE, main = "Histogram of current age",
          xlab = "Current Age(day)", ylab = "Frequency", xlim = c(0, 200), ylim = c(0, 130))


##histogram of age by regions
library(tidyverse)
library(hrbrthemes)

# plot1 not good
p <- BSAL_drop_outliers_startage  %>%
  ggplot( aes(x=BSAL_drop_outliers_startage$currentage)) +
  geom_histogram( binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=1) +
  ggtitle("Histogram of current age by region") +
  coord_cartesian(ylim = c(0, 120)) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p

#plot2 good but Cannot display all counts of each region
p2 <- ggplot(data = BSAL_drop_outliers_startage, aes(x = currentage, fill = factor(region))) +
  geom_histogram(binwidth = 10, position = "identity") +
  ggtitle("Histogram of current age by region") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15)) +
  coord_cartesian(ylim = c(0, 100))
p2

#plot for G seperately: seems that the proportion of region G is strange in the p2, so we plot for G seperately to check.
pG <- ggplot(data = subset(BSAL_drop_outliers_startage, region == "G"), aes(x = currentage)) +
  geom_histogram(binwidth = 10, fill = "grey", color = "#e9ecef", alpha = 1) +
  ggtitle("Histogram of current age in region G") +
  theme_ipsum() +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 100)) + 
  theme(plot.title = element_text(size = 15))
pG

#The occlusion of G does appear, the following is the modification
#p3:Side-by-side bar chart, but not clear enough
p3 <- ggplot(data = BSAL_drop_outliers_startage, aes(x = currentage, fill = factor(region))) +
  geom_histogram(binwidth = 10, position = "dodge", color = "black") +
  ggtitle("Histogram of current age by region") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15)) +
  coord_cartesian(ylim = c(0, 100))
p3

#Stacked bar chart of each currentage
p4 <- ggplot(data = BSAL_drop_outliers_startage, aes(x = currentage, fill = factor(region))) +
  geom_bar(position = "stack", color = "black") +
  ggtitle("Histogram of current age by region") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15)) +
  coord_cartesian(ylim = c(0, 40))
p4

BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
BSAL_drop_outliers_startage <- BSAL[!is.na(BSAL$age) & BSAL$startage < 2, ]

library(ggplot2)

library(tidyverse)
library(hrbrthemes)
library(dplyr)

BSAL_drop_outliers_startage_processed <- BSAL_drop_outliers_startage %>%
  mutate(age_group = cut(age, breaks = seq(0, max(180), by = 20), include.lowest = TRUE)) %>%
  group_by(age_group, region) %>%
  summarize(count = sum(n()))

#histogram of time period(by region)
p4_1 <- ggplot() +
  geom_bar(data = BSAL_drop_outliers_startage_processed,
           aes(x = age_group, y = count, fill = factor(region)),
           stat = "identity",
           width = 0.5,
           position = "stack") +
  theme_classic() +
  scale_fill_manual(values = c( "#b7d6d8","#fccc8e","#bbbb9a","#94715b"), name = "Site") +
  scale_color_manual(values = c( "#b7d6d8","#fccc8e","#bbbb9a","#94715b") )+
  theme(
    panel.grid=element_blank(),
    panel.spacing.x = unit(2, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 10, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  )+
  labs(x = "Time Period (days)", y = "Count") +
  coord_cartesian(ylim = c(0, 150))+
  scale_y_continuous(expand=c(0,0))
p4_1

ggsave(filename = "histogram of time period bysite.png", plot = p4_1, width = 14, height = 6, dpi = 1300)

##dead number-Stacked bar chart
BSAL_drop_outliers_startage_processed <- BSAL_drop_outliers_startage %>%
  mutate(dead_group = cut(num_dead, breaks = seq(0, max(2300), by = 100), include.lowest = TRUE)) %>%
  group_by(dead_group, region) %>%
  summarize(count = sum(n()))
p5_1 <- ggplot() +
  geom_bar(data = BSAL_drop_outliers_startage_processed,
           aes(x = dead_group, y = count, fill = factor(region)),
           stat = "identity",
           width = 0.5,
           position = "stack") +
  theme_classic() +
  scale_fill_manual(values = c( "#b7d6d8","#fccc8e","#bbbb9a","#94715b"), name = "Site") +
  scale_color_manual(values = c( "#b7d6d8","#fccc8e","#bbbb9a","#94715b") )+
  theme(
    panel.grid=element_blank(),
    panel.spacing.x = unit(2, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 10, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  )+
  labs(x = "Time Period (days)", y = "Count") +
  coord_cartesian(ylim = c(0, 150))+
  scale_y_continuous(expand=c(0,0))
p5_1

ggsave(filename = "histogram of dead by site.png", plot = p4_1, width = 14, height = 6, dpi = 1300)



#################################################poster---outcome description histogram containing 4quantiles
BSAL_drop_outliers_startage_processed1 <- BSAL_drop_outliers_startage %>%
  mutate(quantile_group = cut(tmortality_rate, breaks = seq(0,0.41,0.85,1.58,47.04))) %>%
  group_by(quantile_group) %>%
  summarize(count = sum(n()))

BSAL_drop_outliers_startage_processed2 <- BSAL_drop_outliers_startage %>%
  mutate(age_group = cut(age, breaks = seq(0, max(180), by = 20), include.lowest = TRUE)) %>%
  group_by(age_group, region) %>%
  summarize(count = sum(n()))
#histogram of time period(by region)
p4_2 <- ggplot() +
  geom_bar(data = BSAL_drop_outliers_startage_processed,
           aes(x = age_group, y = count, fill = factor(region)),
           stat = "identity",
           width = 0.5,
           position = "stack") +
  theme_classic() +
  scale_fill_manual(values = c( "#e4f6e0","#b2e0ab","#3da75a","#00441b"), name = "Site") +
  scale_color_manual(values = c( "#e4f6e0","#b2e0ab","#3da75a","#00441b") )+
  theme(
    panel.grid=element_blank(),
    panel.spacing.x = unit(2, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 10, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  )+
  labs(x = "Time Period (days)", y = "Count") +
  coord_cartesian(ylim = c(0, 150))+
  scale_y_continuous(expand=c(0,0))
p4_2

ggsave(filename = "histogram of time period bysite2.png", plot = p4_2, width = 14, height = 6, dpi = 1300)

#################################################

ggplot(data = BSAL_drop_outliers_startage_processed, aes(x = age_group, y = count, fill = factor(region))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  #ggtitle("Histogram of time period by region") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15, hjust = 0.5), 
        axis.text.x = element_text(size = 8.5, hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5, vjust = 0.3),
        axis.title.y = element_text(size = 12, hjust = 0.5)) +
  
  
  #histogram of current age(ouverall)
  p4_2 <- ggplot(data = BSAL_drop_outliers_startage_processed, aes(x = currentage_group, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "transparent") +
  ggtitle("Histogram of current age") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 8.5, hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5, vjust = 0.3),
        axis.title.y = element_text(size = 12, hjust = 0.5)) +
  labs(x = "Currentage (day)", y = "Count") +
  coord_cartesian(ylim = c(0, 120))
p4_2


#####################################################not important..
#p4_3 add label "count number" of each part of histogram: to check if every region's count is right
library(dplyr)

BSAL_drop_outliers_startage_processed <- BSAL_drop_outliers_startage %>%
  mutate(currentage_group = cut(currentage, breaks = seq(0, max(currentage), by = 10), include.lowest = TRUE)) %>%
  group_by(currentage_group, region) %>%
  summarize(count = sum(n()))

p4_3 <- ggplot(data = BSAL_drop_outliers_startage_processed, aes(x = currentage_group, y = count, fill = factor(region))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  ggtitle("Histogram of current age by region") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15)) +
  coord_cartesian(ylim = c(0, 120))
p4_3




#####################################################################################
#####################################################################################
######univariable regression (mortality rate & morbidity rate)#######################
#####################################################################################
#####################################################################################
####  PS:                                                             ###############
####      These code's idea is not applicable to this project,        ###############
####  and their outputs are not presented to Anne. But I kept         ###############
####  them for documentingthe basic linear regression code.           ###############
####      The codes used to display the regression results of 17 variables ##########
####  in word are in the "univariable regression2" section below.      ##############
#####################################################################################
#####################################################################################
UR <- read.csv(file = "univariable_regression_processed.csv", stringsAsFactors = TRUE)
UR$tmortality_rate <- UR$mortality_rate*1000
UR$tmorbidity_rate <- UR$morbidity_rate*1000

#####poster----p-value in boxplot "pathogen occurence vs. mortality rate"

###############num
####mortality rate
FIT11 <- lm(UR$tmortality_rate~UR$num)
summary(FIT11)
####morbidity rate
FIT12 <- lm(UR$tmorbidity_rate~UR$num)
summary(FIT12)

###############shed
####mortality rate
FIT21 <- lm(UR$tmortality_rate~UR$shed)
summary(FIT21)
####morbidity rate
FIT22 <- lm(UR$tmorbidity_rate~UR$shed)
summary(FIT22)

###############populated
####mortality rate
FIT31 <- lm(UR$tmortality_rate~UR$populated)
summary(FIT31)
####morbidity rate
FIT32 <- lm(UR$tmorbidity_rate~UR$populated)
summary(FIT32)

###############num_relatives
####mortality rate
FIT41 <- lm(UR$tmortality_rate~UR$num_relatives)
summary(FIT41)
####morbidity rate
FIT42 <- lm(UR$tmorbidity_rate~UR$num_relatives)
summary(FIT42)

###############education
#Identify variable as categorical variable
UR$education <- as.factor(UR$education)
####mortality rate
summary(lm(UR$tmortality_rate~UR$education))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$education))

###############income
UR$income <- as.factor(UR$income)
####mortality rate
summary(lm(UR$tmortality_rate~UR$income))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$income))

###############years
####mortality rate
summary(lm(UR$tmortality_rate~UR$years))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$years))

###############farm_dist
####mortality rate
summary(lm(UR$tmortality_rate~UR$farm_dist))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$farm_dist))

###############water_dist
#Recognize variable as numeric variable
UR$water_dist <- as.numeric(UR$water_dist)
####mortality rate
summary(lm(UR$tmortality_rate~UR$water_dist))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$water_dist))

###############practice
UR$practice <- as.factor(UR$practice)
####mortality rate
summary(lm(UR$tmortality_rate~UR$practice))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$practice))

###############disvisitors
####mortality rate
summary(lm(UR$tmortality_rate~UR$disvisitors))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$disvisitors))

###############traders
UR$traders <- as.factor(UR$traders)
####mortality rate
summary(lm(UR$tmortality_rate~UR$traders))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$traders))

###############disvehicles
UR$disvehicles <- as.factor(UR$disvehicles)
####mortality rate
summary(lm(UR$tmortality_rate~UR$disvehicles))
####morbidity rate
summary(lm(UR$tmorbidity_rate~UR$disvehicles))

####################################################################
####################boxplot of startage (region B)##################
####################################################################
BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
library(ggplot2)

BSALSGmt <- BSAL[!is.na(BSAL$tmortality_rate) & !is.na(BSAL$startage) &
                   BSAL$startage < 2, ]
BSALSGmb <- BSAL[!is.na(BSAL$tmorbidity_rate) & !is.na(BSAL$startage) &
                   BSAL$startage < 2, ]

#set color of legend
my_colors <- c("0" = "blue", "1" = "red")

##Box plot of the mortality rate grouped by start age (B)
ggplot(subset(BSALSGmt, region == "B"), aes(x = factor(startage), y = tmortality_rate, fill = factor(startage))) +
  geom_boxplot() +
  labs(x = "Start Age", y = "Mortality Rate (/1000 days)", fill = "Start Age", 
       title = "Box plot of the mortality rate grouped by start age in region B") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, angle = 360)) +
  scale_fill_manual(values = my_colors)

##Box plot of the morbidity rate grouped by start age (B)
ggplot(subset(BSALSGmb, region == "B"), aes(x = factor(startage), y = tmortality_rate, fill = factor(startage))) +
  geom_boxplot() +
  labs(x = "Start Age", y = "Mortality Rate (/1000 days)", fill = "Start Age", 
       title = "Box plot of the morbidity rate grouped by start age in region B") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, angle = 360)) +
  scale_fill_manual(values = my_colors)


##zoom in-Box plot of the mortality rate grouped by start age (B)
ggplot(subset(BSALSGmt, region == "B"), aes(x = factor(startage), y = tmortality_rate, fill = factor(startage))) +
  geom_boxplot() +
  ylim(0,10) +
  labs(x = "Start Age", y = "Mortality Rate (/1000 days)", fill = "Start Age", 
       title = "Box plot of the mortality rate grouped by start age in region B") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, angle = 360)) +
  scale_fill_manual(values = my_colors)

##zoom in-Box plot of the morbidity rate grouped by start age (B)
ggplot(subset(BSALSGmb, region == "B"), aes(x = factor(startage), y = tmortality_rate, fill = factor(startage))) +
  geom_boxplot() +
  ylim(0,10) +
  labs(x = "Start Age", y = "Mortality Rate (/1000 days)", fill = "Start Age", 
       title = "Box plot of the morbidity rate grouped by start age in region B") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, angle = 360)) +
  scale_fill_manual(values = my_colors)

###################################################################################
###################################################################################
#####plot between mortality rate & other 3 outcomes~current age (by region)########
###################################################################################
###################################################################################
BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
BSAL$hmortality_risk <- BSAL$mortality_risk*100
BSAL$hmorbidity_risk <- BSAL$morbidity_risk*100
BSAL$time_period <- BSAL$currentage-BSAL$startage
#####mortality rate
#split data
split_data <- split(BSAL, BSAL$region)
#Create blank graph
par(mfrow = c(2, 2))

#Draw graphics in a loop
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$currentage, subset$tmorbidity_rate,
       xlab = "current age (day)", ylab = "morbidity rate (/1000 days)",
       main = paste("Plot between current age and morbidity rate -", region))
}


#####Mortality risk 
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$time_period, subset$hmortality_risk,
       xlab = "Time Period (days)", ylab = "Mortality Risk (%)",
       main = paste("Plot between time period and mortality risk -", region))
}

#in same scale
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$currentage, subset$hmortality_risk,
       xlab = "current age (day)", ylab = "mortality risk (%)",
       main = paste("Plot between current age and mortality risk -", region),
       xlim = c(20,180), ylim = c(0,60))
}


##########morbidity risk~time period
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$time_period, subset$hmorbidity_risk,
       xlab = "Time Period (days)", ylab = "Morbidity Risk (%)",
       main = paste("Plot between time period and morbidity risk -", region))
}
##########morbidity rate~time period
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$time_period, subset$tmorbidity_rate,
       xlab = "Time Period (days)", ylab = "Morbidity Rate (/1000days)",
       main = paste("Plot between time period and morbidity rate -", region))
}
#in same scale
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$currentage, subset$hmorbidity_risk,
       xlab = "current age (day)", ylab = "morbidity risk (%)",
       main = paste("Plot between current age and morbidity risk -", region),
       xlim = c(0,100), ylim = c(20,160))
}


##########lethality~current age
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$currentage, subset$lethality,
       xlab = "current age (day)", ylab = "lethality (%)",
       main = paste("Plot between current age and lethality -", region))
}

#in same scale
for (i in 1:length(split_data)) {
  subset <- split_data[[i]]
  region <- unique(subset$region)
  
  plot(subset$currentage, subset$lethality,
       xlab = "current age (day)", ylab = "lethality (%)",
       main = paste("Plot between current age and lethality -", region),
       xlim = c(20,180), ylim = c(0,100))
}


#############################################################################
#############################################################################
#######################univariable regression2###############################
##remove outliers (farms whose startage isn't 0 or 1); y = log(rate) (/1000days)##
############################## 17 variables  ################################
#############################################################################
UR <- read.csv(file = "univariable_regression_processed.csv", stringsAsFactors = TRUE)

##########################variables part1####################################
####numertric####################
####categorical-uncombination####
UR$pay2 <- as.factor(UR$pay2)
UR$education <- as.factor(UR$education)
UR$income <- as.factor(UR$income)

UR$num <- as.numeric(UR$num)
UR$years <- as.numeric(UR$years)

uncombinationVB <- c("num", "shed", "populated", "num_relatives", "education", "income", "years")

########log mortality rate
UR1 <- UR[!is.na(UR$mortality_rate) & UR$mortality_rate != 0, ]
UR1$log_mortality_rate <- log(UR1$mortality_rate)

#####overall
#print in console
for (variable in uncombinationVB) {
  formula <- paste("log_mortality_rate ~", variable)
  model <- lm(formula, data = UR1)
  print(summary(model))
}


#####by site
#print in console
unique_regions1 <- c("B", "T", "G", "V")
subset_data_B <- subset(UR1, region == "B")
subset_data_T <- subset(UR1, region == "T")
subset_data_G <- subset(UR1, region == "G")
subset_data_V <- subset(UR1, region == "V")

for (variable in uncombinationVB) {
  for (region in unique_regions1) {
    #get(): Use the get() function to get the corresponding data frame content according to the string name
    #paste0(): To concatenate multiple strings without delimiters, which is convenient for generating dynamic variable names or other string concatenation requirements.
    data_name <- get(paste0("subset_data_", region)) 
    formula <- as.formula(paste("log_mortality_rate ~", variable))
    model <- lm(formula, data = data_name)
    print(region)
    print(summary(model))
  }
}


#########log morbidity rate
UR2 <- UR[!is.na(UR$morbidity_rate) & UR$morbidity_rate != 0, ]
UR2$log_morbidity_rate <- log(UR2$morbidity_rate)

#####overall
#print in console
for (variable in uncombinationVB) {
  formula <- paste("log_morbidity_rate ~", variable)
  model <- lm(formula, data = UR2)
  print(summary(model))
}

#####by site
#print in console
subset_data_B <- subset(UR2, region == "B")
subset_data_T <- subset(UR2, region == "T")
subset_data_G <- subset(UR2, region == "G")
subset_data_V <- subset(UR2, region == "V")

for (variable in uncombinationVB) {
  for (region in unique_regions1) {
    #get(): Use the get() function to get the corresponding data frame content according to the string name
    #paste0(): To concatenate multiple strings without delimiters, which is convenient for generating dynamic variable names or other string concatenation requirements.
    data_name <- get(paste0("subset_data_", region)) 
    formula <- as.formula(paste("log_morbidity_rate ~", variable))
    model <- lm(formula, data = data_name)
    print(region)
    print(summary(model))
  }
}

##########################variables part2####################################
####categorical-combination####
combinationVB <- c("presentation_1 + presentation_2 + presentation_3 + presentation_99", 
                   "adders_1 + adders_2 + adders_3 + adders_99", 
                   "additives_0 + additives_1 + additives_2 + additives_3 + additives_4 + additives_5 + additives_6 + additives_7 + additives_99", 
                   "water_additives_0 + water_additives_1 + water_additives_2 + water_additives_3 + water_additives_4 + water_additives_5 + water_additives_6 + water_additives_7 + water_additives_99", 
                   "planning_1 + planning_2 + planning_3 + planning_99 + planning_0", 
                   "workers_1 + workers_2 + workers_3 + workers_4 + workers_5 + workers_99", 
                   "pay2_1 + pay2_2 + pay2_3 + pay2_99", 
                   "sources_1 + sources_2 + sources_3 + sources_4 + sources_5 + sources_6 + sources_7 + sources_8 + sources_9 + sources_10 + sources_11 + sources_12", 
                   "capital_1 + capital_2 + capital_3 + capital_4 + capital_5 + capital_99")

#########log mortality rate
#####overall
#print in console
for (variable in combinationVB) {
  formula <- paste("log_mortality_rate ~", variable)
  model <- lm(formula, data = UR1)
  print(summary(model))
}

#####by site
#print in console
subset_data_B <- subset(UR1, region == "B")
subset_data_T <- subset(UR1, region == "T")
subset_data_G <- subset(UR1, region == "G")
subset_data_V <- subset(UR1, region == "V")

for (variable in combinationVB) {
  for (region in unique_regions1) {
    #get(): Use the get() function to get the corresponding data frame content according to the string name
    #paste0(): To concatenate multiple strings without delimiters, which is convenient for generating dynamic variable names or other string concatenation requirements.
    data_name <- get(paste0("subset_data_", region)) 
    formula <- as.formula(paste("log_mortality_rate ~", variable))
    model <- lm(formula, data = data_name)
    print(region)
    print(summary(model))
  }
}

#########log morbidity rate
#####overall
#print in console
for (variable in combinationVB) {
  formula <- paste("log_morbidity_rate ~", variable)
  model <- lm(formula, data = UR2)
  print(summary(model))
}

#####by site
#print in console
subset_data_B <- subset(UR2, region == "B")
subset_data_T <- subset(UR2, region == "T")
subset_data_G <- subset(UR2, region == "G")
subset_data_V <- subset(UR2, region == "V")

for (variable in combinationVB) {
  for (region in unique_regions1) {
    #get(): Use the get() function to get the corresponding data frame content according to the string name
    #paste0(): To concatenate multiple strings without delimiters, which is convenient for generating dynamic variable names or other string concatenation requirements.
    data_name <- get(paste0("subset_data_", region)) 
    formula <- as.formula(paste("log_morbidity_rate ~", variable))
    model <- lm(formula, data = data_name)
    print(region)
    print(summary(model))
  }
}



#store -> .docx
library(xtable)
library(flextable)
library(officer)
############################################
#define function to read overall p-value.  #
overall_p <- function(my_model) {          #
  f <- summary(my_model)$fstatistic        #
  p <- pf(f[1],f[2],f[3],lower.tail=F)     #
  attributes(p) <- NULL                    #
  return(p)                                #
}                                          #
#code to extract overall p-value of model: #
#overall_p(model)                          #
############################################

#set the number of decimal in regression output table---"table_model <- as_flextable(xtable(model))" : Keep 3 decimal places
set_flextable_defaults(digits = 3)
#doc0 = read_docx()
#for (variable in uncombinationVB) {
#formula <- paste("log_mortality_rate ~", variable)
#model <- lm(formula, data = UR1)
#model_summary <- summary(model)

#f_statistic <- model_summary$fstatistic[1]
#p_value <- overall_p(model)

#table_model <- as_flextable(xtable(model))
#table_model <- set_caption(table_model, 
#paste("log(mortality rate) ~ ", variable, 
# "F-statistic:", round(f_statistic, 4), 
# "P-value:", round(p_value, digits = 6)))

#doc0 <- body_add_flextable(doc0, table_model)
#}
#print(doc0, target = "regression_log_mortality_rate_overall.docx")

#############################################################################
#############################################################################
############summary of currentage, death, real sick..by region###############
##########remove outliers (farms whose startage is not 0 or 1)  #############
#############################################################################
#############################################################################
library(psych)

BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
BSAL_drop_outliers_startage <- BSAL[!is.na(BSAL$age) & BSAL$startage < 2, ]
BSAL_drop_outliers_startage$hmorbidity_risk <- BSAL_drop_outliers_startage$morbidity_risk * 100
BSAL_drop_outliers_startage$hmortality_risk <- BSAL_drop_outliers_startage$mortality_risk * 100
#####current age
##describe variables overall
describe(BSAL_drop_outliers_startage$currentage)
quantile(BSAL_drop_outliers_startage$currentage, na.rm=TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$currentage, list(region = BSAL_drop_outliers_startage$region))
aggregate(currentage ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site

#####number of death
##describe variables overall
describe(BSAL_drop_outliers_startage$num_dead)
quantile(BSAL_drop_outliers_startage$num_dead, na.rm = TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$num_dead, list(region = BSAL_drop_outliers_startage$region))
aggregate(num_dead ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site

#####number of real sick
##describe variables overall
describe(BSAL_drop_outliers_startage$real_sick)
quantile(BSAL_drop_outliers_startage$real_sick, na.rm = TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$real_sick, list(region = BSAL_drop_outliers_startage$region))
aggregate(real_sick ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site


#####morbidity rate
##describe variables overall
describe(BSAL_drop_outliers_startage$tmorbidity_rate)
quantile(BSAL_drop_outliers_startage$tmorbidity_rate, na.rm = TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$tmorbidity_rate, list(region = BSAL_drop_outliers_startage$region))
aggregate(tmorbidity_rate ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site

#####morbidity risk
##describe variables overall
describe(BSAL_drop_outliers_startage$hmorbidity_risk)
quantile(BSAL_drop_outliers_startage$hmorbidity_risk, na.rm = TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$hmorbidity_risk, list(region = BSAL_drop_outliers_startage$region))
aggregate(hmorbidity_risk ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site

#####mortality rate
##describe variables overall
describe(BSAL_drop_outliers_startage$tmortality_rate)
quantile(BSAL_drop_outliers_startage$tmortality_rate, na.rm = TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$tmortality_rate, list(region = BSAL_drop_outliers_startage$region))
aggregate(tmortality_rate ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site

#####mortality risk
##describe variables overall
describe(BSAL_drop_outliers_startage$hmortality_risk)
quantile(BSAL_drop_outliers_startage$hmortality_risk, na.rm = TRUE)
##describe variables by site
describeBy(BSAL_drop_outliers_startage$hmortality_risk, list(region = BSAL_drop_outliers_startage$region))
aggregate(hmortality_risk ~ region, data = BSAL_drop_outliers_startage, FUN = quantile, probs = c(0.25, 0.5, 0.75))#aggregate(): calculate quantiles of variables by site

write.csv(BSAL_drop_outliers_startage,file="basic_data_drop_outlier_startage.csv",quote=F,row.names = F)

#############################################
######boxplot of clinical signs & pathogens##
#############################################
#############################################
SvP_his <- read.csv(file = "clinical_signs_vs_pathogen_his.csv", stringsAsFactors = TRUE)
attach(regionq)
library(ggplot2)
library(tidyr)
library(tidyverse)
# mortality rate
regionq2 <- regionq[regionq$mortality_rate <1,]
regionq_long <- regionq2 %>% ##管道连接标志，把这一步生成的变量直接用于下一步操作
  pivot_longer(cols = starts_with("p"), names_to = "pathogen", values_to = "result") %>%
  na.omit() 
regionq_long2 <- mutate(regionq_long, new_mortality_rate=mortality_rate*1000)
regionq_long3 <- regionq_long2[regionq_long2$new_mortality_rate < 10, ]
regionq_long3$pathogen <- ifelse(regionq_long3$pathogen == "p_campy", "campy",
                                 ifelse(regionq_long3$pathogen == "p_ecoli", "ecoli",
                                        ifelse(regionq_long3$pathogen == "p_campy_ccoli", "coli",
                                               ifelse(regionq_long3$pathogen == "p_campy_cjejuni", "jejuni",
                                                      ifelse(regionq_long3$pathogen == "p_aiv_h9", "aiv_h9",
                                                             ifelse(regionq_long3$pathogen == "p_aiv_h5", "aiv_h5",
                                                                    ifelse(regionq_long3$pathogen == "p_nts", "nts", "")))))))
##ifelse(): ifelse(结果为TRUE/FALSE或1/0的条件，"TRUE时显示的标签", "FALSE时显示的标签")
boxplot <- ggplot(regionq_long3, aes(x = pathogen, y = new_mortality_rate, fill = result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  facet_wrap(~ region, ncol = 4)

print(boxplot)
ggsave(filename = "desktop/boxplot.png",plot= boxplot, width = 16, height = 4.48, dpi = 600)

#morbidity rate
regionq2 <- regionq[regionq$morbidity_rate <1,]

regionq_long <- regionq2 %>%
  pivot_longer(cols = starts_with("p"), names_to = "pathogen", values_to = "result") %>%
  na.omit() 

regionq_long2 <- mutate(regionq_long, new_morbidity_rate=morbidity_rate*1000)
regionq_long3 <- regionq_long2[regionq_long2$new_morbidity_rate < 10, ]

regionq_long3$pathogen <- ifelse(regionq_long3$pathogen == "p_campy", "campy",
                                 ifelse(regionq_long3$pathogen == "p_ecoli", "ecoli",
                                        ifelse(regionq_long3$pathogen == "p_campy_ccoli", "coli",
                                               ifelse(regionq_long3$pathogen == "p_campy_cjejuni", "jejuni",
                                                      ifelse(regionq_long3$pathogen == "p_aiv_h9", "aiv_h9",
                                                             ifelse(regionq_long3$pathogen == "p_aiv_h5", "aiv_h5",
                                                                    ifelse(regionq_long3$pathogen == "p_nts", "nts", "")))))))

boxplot <- ggplot(regionq_long3, aes(x = pathogen, y = new_morbidity_rate, fill = result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  facet_wrap(~ region, ncol = 4)

print(boxplot)
ggsave(filename = "desktop/boxplot2.png",plot= boxplot, width = 16, height = 4.48, dpi = 600)

attach(regiont)
library(ggplot2)
library(tidyverse)

regiont2 <- regiont[regiont$T_mortality_rate <1,]

regiont_long <- regiont2 %>%
  pivot_longer(cols = c(starts_with("campy"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regiont_long2 <- mutate(regiont_long, new_T_mortality_rate=T_mortality_rate*1000)

regiont_boxplot <- ggplot(regiont_long2, aes(x = Pathogen, y = new_T_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))

attach(regionb)
regionb2 <- regionb[regionb$B_mortality_rate <1,]

regionb_long <- regionb2 %>%
  pivot_longer(cols = c(starts_with("m"), starts_with("h9"), starts_with("h5"), starts_with("ccoli"), starts_with("cjejuni"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regionb_long2 <- mutate(regionb_long, new_B_mortality_rate=B_mortality_rate*1000)
regionb_long3 <- regionb_long2[regionb_long2$new_B_mortality_rate < 10, ]
regionb_boxplot <- ggplot(regionb_long3, aes(x = Pathogen, y = new_B_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))

attach(regiong)
regiong2 <- regiong[regiong$G_mortality_rate <1,]

regiong_long <- regiong2 %>%
  pivot_longer(cols = c(starts_with("ccoli"), starts_with("cjejuni"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regiong_long2 <- mutate(regiong_long, new_G_mortality_rate=G_mortality_rate*1000)
regiong_long3 <- regiong_long2[regiong_long2$new_G_mortality_rate <10,]  
regiong_boxplot <- ggplot(regiong_long3, aes(x = Pathogen, y = new_G_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))

attach(regionv)
regionv2 <- regionv[regionv$V_mortality_rate <1,]

regionv_long <- regionv2 %>%
  pivot_longer(cols = c(starts_with("m"), starts_with("h9"), starts_with("h5"), starts_with("ccoli"), starts_with("cjejuni"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regionv_long2 <- mutate(regionv_long, new_V_mortality_rate=V_mortality_rate*1000)
regionv_long3 <- regionv_long2[regionv_long2$new_V_mortality_rate < 10, ]
regionv_boxplot <- ggplot(regionv_long2, aes(x = Pathogen, y = new_V_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))

attach(regiont)
library(ggplot2)
library(tidyverse)

regiont2 <- regiont[regiont$T_morbidity_rate <1,]

regiont_long <- regiont2 %>%
  pivot_longer(cols = c(starts_with("campy"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regiont_long2 <- mutate(regiont_long, new_T_morbidity_rate=T_morbidity_rate*1000)
regiont_long3 <- regiont_long2[regiont_long2$new_T_morbidity_rate < 10, ]
regiont_boxplot <- ggplot(regiont_long3, aes(x = Pathogen, y = new_T_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
print(regiont_boxplot)

attach(regionb)
regionb2 <- regionb[regionb$B_morbidity_rate <1,]

regionb_long <- regionb2 %>%
  pivot_longer(cols = c(starts_with("m"), starts_with("h9"), starts_with("h5"), starts_with("ccoli"), starts_with("cjejuni"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regionb_long2 <- mutate(regionb_long, new_B_morbidity_rate=B_morbidity_rate*1000)
regionb_long3 <- regionb_long2[regionb_long2$new_B_morbidity_rate < 10, ]
regionb_boxplot <- ggplot(regionb_long3, aes(x = Pathogen, y = new_B_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
print(regionb_boxplot)

attach(regiong)
regiong2 <- regiong[regiong$G_morbidity_rate <1,]

regiong_long <- regiong2 %>%
  pivot_longer(cols = c(starts_with("ccoli"), starts_with("cjejuni"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regiong_long2 <- mutate(regiong_long, new_G_morbidity_rate=G_morbidity_rate*1000)
regiong_long3 <- regiong_long2[regiong_long2$new_G_morbidity_rate <10,]  
regiong_boxplot <- ggplot(regiong_long3, aes(x = Pathogen, y = new_G_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
print(regiong_boxplot)

attach(regionv)
regionv2 <- regionv[regionv$V_morbidity_rate <1,]

regionv_long <- regionv2 %>%
  pivot_longer(cols = c(starts_with("m"), starts_with("h9"), starts_with("h5"), starts_with("ccoli"), starts_with("cjejuni"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regionv_long2 <- mutate(regionv_long, new_V_morbidity_rate=V_morbidity_rate*1000)
regionv_long3 <- regionv_long2[regionv_long2$new_V_morbidity_rate < 10, ]
regionv_boxplot <- ggplot(regionv_long2, aes(x = Pathogen, y = new_V_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
print(regionv_boxplot)

ggsave(filename = "desktop/boxplot_t.png",plot= regionv_boxplot, width = 3.65, height = 4.48, dpi = 400)
ggsave(filename = "desktop/boxplot_b.png",plot= regionv_boxplot, width = 5, height = 4.48, dpi = 400)
ggsave(filename = "desktop/boxplot_g.png",plot= regionv_boxplot, width = 3.65, height = 4.48, dpi = 400)
ggsave(filename = "desktop/boxplot_v.png",plot= regionv_boxplot, width = 5, height = 4.48, dpi = 400)

attach(regiono)
library(ggplot2)
library(tidyverse)

regiono2 <- regiono[regiono$mortality_rate <1,]
regiono_long <- regiono2 %>%
  pivot_longer(cols = c(starts_with("campy"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regiono_long2 <- mutate(regiono_long, new_mortality_rate=mortality_rate*1000)
regiono_long3 <- regiono_long2[regiono_long2$new_mortality_rate <10, ]
regiono_long3 <- na.omit(regiono_long3)
regiono_boxplot <- ggplot(regiono_long3, aes(x = Pathogen, y = new_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
regiono_boxplot
ggsave(filename = "desktop/boxplot.png", plot = regiono_boxplot, width = 3.65, height = 4.48, dpi = 400)

attach(regionc)
library(ggplot2)
library(tidyverse)

regionc2 <- regionc[regionc$mortality_rate <1,]
regionc_long <- regionc2 %>%
  pivot_longer(cols = c(starts_with("ccoli"), starts_with("cjejuni")), names_to = "Pathogen", values_to = "Result")

regionc_long2 <- mutate(regionc_long, new_mortality_rate=mortality_rate*1000)
#optional!
regionc_long3 <- regionc_long2[regionc_long2$new_mortality_rate <10, ]
regionc_long3 <- na.omit(regionc_long3)

regionc_boxplot <- ggplot(regionc_long3, aes(x = Pathogen, y = new_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen campy", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
regionc_boxplot
ggsave(filename = "desktop/boxplot c.png", plot = regionc_boxplot, width = 3.65, height = 4.48, dpi = 400)

attach(regiona)
library(ggplot2)
library(tidyverse)

regiona2 <- regiona[regiona$a_mortality_rate <1,]

regiona_long <- regiona2 %>%
  pivot_longer(cols = c(starts_with("m"), starts_with("h9"), starts_with("h5")), names_to = "Pathogen", values_to = "Result")

regiona_long2 <- mutate(regiona_long, new_mortality_rate=a_mortality_rate*1000)
regiona_long3 <- regiona_long2[regiona_long2$new_mortality_rate <10, ]
regiona_long3 <- na.omit(regiona_long3)
regiona_boxplot <- ggplot(regiona_long3, aes(x = Pathogen, y = new_mortality_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen aiv", y = "Mortality Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
regiona_boxplot
ggsave(filename = "desktop/boxplot_a.png", plot = regiona_boxplot, width = 3.65, height = 4.48, dpi = 400)

attach(regiono)
library(ggplot2)
library(tidyverse)

regiono2 <- regiono[regiono$morbidity_rate <1,]
regiono_long <- regiono2 %>%
  pivot_longer(cols = c(starts_with("campy"), starts_with("ecoli"), starts_with("nts")), names_to = "Pathogen", values_to = "Result")

regiono_long2 <- mutate(regiono_long, new_morbidity_rate=morbidity_rate*1000)
regiono_long3 <- regiono_long2[regiono_long2$new_morbidity_rate <10, ]
regiono_long3 <- na.omit(regiono_long3)
regiono_boxplot <- ggplot(regiono_long3, aes(x = Pathogen, y = new_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
print(regiono_boxplot)
ggsave(filename = "desktop/boxplot.png", plot = regiono_boxplot, width = 3.65, height = 4.48, dpi = 400)

attach(regionc)
library(ggplot2)
library(tidyverse)

regionc2 <- regionc[regionc$morbidity_rate <1,]
regionc_long <- regionc2 %>%
  pivot_longer(cols = c(starts_with("ccoli"), starts_with("cjejuni")), names_to = "Pathogen", values_to = "Result")

regionc_long2 <- mutate(regionc_long, new_morbidity_rate=morbidity_rate*1000)
#optional!
regionc_long3 <- regionc_long2[regionc_long2$new_morbidity_rate <10, ]
regionc_long3 <- na.omit(regionc_long3)

regionc_boxplot <- ggplot(regionc_long3, aes(x = Pathogen, y = new_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen campy", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
regionc_boxplot
ggsave(filename = "desktop/boxplot c.png", plot = regionc_boxplot, width = 3.65, height = 4.48, dpi = 400)

attach(regiona)
library(ggplot2)
library(tidyverse)

regiona2 <- regiona[regiona$a_morbidity_rate <1,]

regiona_long <- regiona2 %>%
  pivot_longer(cols = c(starts_with("m"), starts_with("h9"), starts_with("h5")), names_to = "Pathogen", values_to = "Result")

regiona_long2 <- mutate(regiona_long, new_morbidity_rate=a_morbidity_rate*1000)
regiona_long3 <- regiona_long2[regiona_long2$new_morbidity_rate <10, ]
regiona_long3 <- na.omit(regiona_long3)
regiona_boxplot <- ggplot(regiona_long3, aes(x = Pathogen, y = new_morbidity_rate, fill = Result)) +
  geom_boxplot() +
  labs(x = "Pathogen aiv", y = "Morbidity Rate") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "salmon")) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white"))
regiona_boxplot
ggsave(filename = "desktop/boxplot_a.png", plot = regiona_boxplot, width = 3.65, height = 4.48, dpi = 400)

###############################################################################################################################
###########poster##############################################################################################################
######stacked percentage histogram of clinical signs(R & G) vs. pathogens(nts,camp,aiv_h5,aiv_h9,aiv_m) by relevant% of farms##
###############################################################################################################################
###############################################################################################################################
SvP_his <- read.csv(file = "clinical_signs_vs_pathogen_his.csv", stringsAsFactors = TRUE)

table_camp_R <- table(SvP_his$camp, SvP_his$sign_R)
addmargins(table_camp_R)

table_camp_G <- table(SvP_his$camp, SvP_his$sign_G)
addmargins(table_camp_G)

print(table_nts_R <- table(SvP_his$nts, SvP_his$sign_R))
print(table_nts_G <- table(SvP_his$nts, SvP_his$sign_G))

print(table_camp_R <- table(SvP_his$camp, SvP_his$sign_R))
print(table_camp_G <- table(SvP_his$camp, SvP_his$sign_G))

print(table_aivh5_R <- table(SvP_his$aiv_h5, SvP_his$sign_R))
print(table_aivh5_G <- table(SvP_his$aiv_h5, SvP_his$sign_G))

print(table_aivh9_R <- table(SvP_his$aiv_h9, SvP_his$sign_R))
print(table_aivh9_G <- table(SvP_his$aiv_h9, SvP_his$sign_G))

print(table_aivm_R <- table(SvP_his$aiv_m, SvP_his$sign_R))
print(table_aivm_G <- table(SvP_his$aiv_m, SvP_his$sign_G))

###
library(ggplot2)
SvP_final <- read.csv(file = "his_result_SvP_no_aivm.csv", stringsAsFactors = TRUE)
SvP_final$percentage <- 100*(SvP_final$count/SvP_final$total)
# 物种组成百分比堆叠图
perct1 <- ggplot() +
  geom_bar(data = SvP_final,
           aes(x = pathogen, y = percentage, fill = reorder(result, -percentage)),
           stat = "identity",
           width = 0.5,
           position = "fill") +# ggplot2会自行计算相对丰度，无需提前计算
  facet_wrap(~ sign, ncol = 4) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("#96D8E3","#BED0A6","#6C9585","#288994"), name = "Result") +
  scale_color_manual(values = c("#96D8E3","#BED0A6","#6C9585","#288994") )+
  theme(
    panel.grid=element_blank(),
    panel.spacing.x = unit(0, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 12, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  )+
  labs(x = "Pathogen",y= "Relative Percentage of Result")
perct1

ggsave(filename = "Pathogen occurrence and clinical signs0.5.png", plot = perct1, width = 14, height = 6, dpi = 1800)



###############################################################################
#####poster----p-value in boxplot "pathogen occurence vs. mortality rate"######
###############################################################################
###############B
P_BoxplotB0 <- read.csv(file = "B divided in chicken tpyes.csv", stringsAsFactors = TRUE)
P_BoxplotB <- P_BoxplotB0[!is.na(P_BoxplotB0$B_mortality_rate) & P_BoxplotB0$B_mortality_rate != 0, ]

P_BoxplotB$log_tmortality_rate <- log(P_BoxplotB$B_mortality_rate*1000)

P_BoxplotB$log_tmortality_rate <- as.numeric(P_BoxplotB$log_tmortality_rate)
P_BoxplotB$m <- as.factor(P_BoxplotB$m)
P_BoxplotB$h5 <- as.factor(P_BoxplotB$h5)
P_BoxplotB$h9 <- as.factor(P_BoxplotB$h9)

pathogenAIV <- c("m", "h5", "h9")

#log tmortality rate
for (variable in pathogenAIV) {
  formula <- paste("log_tmortality_rate ~", variable)
  model <- lm(formula, data = P_BoxplotB)
  print(summary(model))
}

###############V
P_BoxplotV0 <- read.csv(file = "regionv.csv", stringsAsFactors = TRUE)
P_BoxplotV <- P_BoxplotV0[!is.na(P_BoxplotV0$V_mortality_rate) & P_BoxplotV0$V_mortality_rate != 0, ]

P_BoxplotV$log_tmortality_rate <- log(P_BoxplotV$V_mortality_rate*1000)
P_BoxplotV$log_tmortality_rate <- as.numeric(P_BoxplotV$log_tmortality_rate)
P_BoxplotV$m <- as.factor(P_BoxplotV$m)
P_BoxplotV$h5 <- as.factor(P_BoxplotV$h5)
P_BoxplotV$h9 <- as.factor(P_BoxplotV$h9)

pathogenAIV2 <- c("m", "h9")

#####current age
#log tmortality rate
for (variable in pathogenAIV2) {
  formula <- paste("log_tmortality_rate ~", variable)
  model <- lm(formula, data = P_BoxplotV)
  print(summary(model))
}


###############overall mortality rate boxplot
library(ggplot2)

BSAL <- read.csv(file = "basic_data_all_known_uncertain.csv", stringsAsFactors = TRUE)
BSAL_drop_outliers_startage <- BSAL[!is.na(BSAL$age) & BSAL$startage < 2, ]
BSAL_drop_outliers_startage$tmortality_rate <- BSAL_drop_outliers_startage$mortality_rate * 1000
BSAL_drop_outliers_startage$tmorbidity_rate <- BSAL_drop_outliers_startage$morbidity_rate * 1000

ggplot(BSAL_drop_outliers_startage, aes(y = tmortality_rate, na.rm = FALSE)) +
  geom_boxplot() +
  ylim(0,2)+
  ylab("mortality rate(/1000 bird.days)")


#############################################################
#########histogram of mean/sd mortality rate & morbidity rate
#############################################################
library(plyr)
library(ggplot2)

bar_mortalityrate <- read.csv(file = "Error_bar_histogram__mortality_rate.csv", stringsAsFactors = TRUE)

#define functions that calculate mean and standard deviation
dat_sd <- function(data, varname, groupnames){
  sum_func <- function(x, col){
    c(Mean = mean(x[[col]], na.rm=TRUE),
      SD = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=sum_func,varname)
  return(data_sum)
}

#Calculate standard deviation
df <- dat_sd(bar_mortalityrate, varname="tmortality_rate", groupnames="region")

library(ggplot2)
library(dplyr)

#Modify region to ordered factor
df$region <- factor(df$region, levels = c("B", "G", "T", "V", "Overall"))

pbar <- ggplot(df, aes(x = region, y = Mean, fill = region)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.2,
                position = position_dodge(0.9)) +
  #geom_text(aes(label = paste("Mean:", round(Mean, 2), "\nSD:", round(SD, 2))),position = position_dodge(width = 0.9),vjust = -0.5, size = 4, fontface = "bold") +
  theme_classic() +
  scale_fill_manual(values = c("#b2ccc9", "#ecbdb7", "#aeb8d1", "#e8bdd1", "gray")) +
  theme(
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 12, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  ) +
  labs(x = "Region", y = "Mortality rate (/1000 bird.days)") +
  ylim(0, 22) +
  guides(fill = FALSE)  #Hide legend

pbar

ggsave(filename = "Mortality rate mean&sd.png", plot = pbar, width = 12, height = 6, dpi = 1300)

####morbidity rate
bar_morbidityrate <- read.csv(file = "Error_bar_histogram__morbidity_rate.csv", stringsAsFactors = TRUE)

df2 <- dat_sd(bar_morbidityrate, varname="tmorbidity_rate", groupnames="region")
df2$region <- factor(df2$region, levels = c("B", "G", "T", "V", "Overall"))

pbar2 <- ggplot(df2, aes(x = region, y = Mean, fill = region)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.2,
                position = position_dodge(0.9)) +
  #geom_text(aes(label = paste("Mean:", round(Mean, 2), "\nSD:", round(SD, 2))),position = position_dodge(width = 0.9),vjust = -0.5, size = 4, fontface = "bold") +
  theme_classic() +
  scale_fill_manual(values = c("#b2ccc9", "#ecbdb7", "#aeb8d1", "#e8bdd1", "gray")) +
  theme(
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 12, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  ) +
  labs(x = "Region", y = "Morbidity rate (/1000 bird.days)") +
  ylim(0, 18) +
  guides(fill = FALSE)  #Hide legend

pbar2

ggsave(filename = "Morbidity rate mean&sd.png", plot = pbar2, width = 14, height = 6, dpi = 1300)

####################################################################
###################boxplot of 4 sites and overall###################
####################################################################
library(ggplot2)
##mortality rate
bar_mortalityrate <- read.csv(file = "Error_bar_histogram__mortality_rate.csv", stringsAsFactors = TRUE)
bar_mortalityrate$region <- factor(bar_mortalityrate$region, levels = c("B", "G", "T", "V", "Overall"))

pbar1 <- ggplot(bar_mortalityrate, aes(x = region, y = tmortality_rate, fill = region)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar",width=0.2)+
  #geom_text(aes(label = paste("Mean:", round(Mean, 2), "\nSD:", round(SD, 2))),position = position_dodge(width = 0.9),vjust = -0.5, size = 4, fontface = "bold") +
  theme_classic() +
  scale_fill_manual(values = c("#b2ccc9", "#ecbdb7", "#aeb8d1", "#e8bdd1", "gray")) +
  theme(
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 12, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  ) +
  labs(x = "Region", y = "Mortality rate (/1000 bird.days)") +
  ylim(0, 8) +
  guides(fill = FALSE)  #Hide legend
pbar1

ggsave(filename = "Mortality rate boxplot.png", plot = pbar1, width = 14, height = 6, dpi = 1300)

##morbidity rate
bar_morbidityrate <- read.csv(file = "Error_bar_histogram__morbidity_rate.csv", stringsAsFactors = TRUE)
bar_morbidityrate$region <- factor(bar_morbidityrate$region, levels = c("B", "G", "T", "V", "Overall"))

pbar2 <- ggplot(bar_morbidityrate, aes(x = region, y = tmorbidity_rate, fill = region)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar",width=0.2)+
  #geom_text(aes(label = paste("Mean:", round(Mean, 2), "\nSD:", round(SD, 2))),position = position_dodge(width = 0.9),vjust = -0.5, size = 4, fontface = "bold") +
  theme_classic() +
  scale_fill_manual(values = c("#b2ccc9", "#ecbdb7", "#aeb8d1", "#e8bdd1", "gray")) +
  theme(
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, units = "cm"),
    strip.background = element_rect(
      color = "white", fill = "white",
      linetype = "solid", linewidth = 1),
    strip.placement = "outside",
    axis.line.y.left = element_line(color = "black", linewidth = 0.7),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.7),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(face = "bold",
                             size = 12, color = "black"),
    axis.title = element_text(face = "bold",
                              size = 14, colour = "black"),
    legend.title = element_text(face = "bold",
                                size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black"),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
  ) +
  labs(x = "Region", y = "Morbidity rate (/1000 bird.days)") +
  ylim(0, 18) +
  guides(fill = FALSE)  #Hide legend

pbar2

ggsave(filename = "Morbidity rate mean&sd.png", plot = pbar2, width = 14, height = 6, dpi = 1300)
