library(plotly)
library(readxl)
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages('tidyverse')
install.packages("ggplot2")
library(ggplot2)
library(mgcv)
library(mgcViz)
install.packages("readxl")
library(forcats)
library(tidypaleo)
install.packages("tidypaleo")
library(ggpubr)
install.packages("ggpubr")
install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(grid) 
install.packages("devtools")
library(devtools)
library(tidyverse)
install.packages("broom")
library(broom)
install.packages("rstatix")
library(rstatix)
install.packages("qqplotr")
library(qqplotr)

install.packages("devtools")
library(devtools)
library(tidyverse)
install.packages("broom")
library(broom)
install.packages("rstatix")
library(rstatix)
install.packages("qqplotr")
library(qqplotr)
library(moments)
install.packages("moments")

library(multcomp)
install.packages("multcomp")

install.packages("multcompletters")
library(multcompletters)

install.packages("multcompView")
library(multcompView)

NPDR <- with(Cardinal_points_TABLE$NPDR)

############Shapiro Wilk by RESIDUALS - same as by group (not same as doing by whole raw dataset) ###########################################

############ first need to compute the ANOVA #######################
#### then check qq plot, histogram for normal distribution################
######### Then do a SHAPIRO WILK on the residuals ##############################

Max_NP_PARsat.anova <- aov(Max_NP_PARsat ~ factor(Species), data = Cardinal_points_TABLE)
hist(Max_NP_PARsat.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(Max_NP_PARsat.anova$residuals)
summary(Max_NP_PARsat.anova)


Max_DR_PARsat.anova <- aov(Max_DR_PARsat ~ factor(Species), data = Cardinal_points_TABLE)
hist(Max_DR_PARsat.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(Max_DR_PARsat.anova$residuals)
summary(Max_DR_PARsat.anova)


LCP.anova <- aov(LCP~ factor(Species), data = Cardinal_points_TABLE)
hist(LCP.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(LCP.anova$residuals) 
summary(LCP.anova)


LSP.anova <- aov(LSP ~ factor(Species), data = Cardinal_points_TABLE)
hist(LSP.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(LSP.anova$residuals)
summary(LSP.anova)

WC_Min.anova <- aov(WC_Min ~ factor(Species), data = Cardinal_points_TABLE)
hist(WC_Min.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Min.anova$residuals)
summary(WC_Min.anova)


WC_Max.anova <- aov(WC_Max ~ factor(Species), data = Cardinal_points_TABLE)
hist(WC_Max.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Max.anova$residuals)
summary(WC_Max.anova)


WC_Optimal_Range.anova <- aov(WC_Optimal_Range ~ factor(Species), data = Cardinal_points_TABLE)
hist(WC_Optimal_Range.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Optimal_Range.anova$residuals)
summary(WC_Optimal_Range.anova)



Chl.anova <- aov(Chl ~ factor(Species), data = Cardinal_points_TABLE)
hist(Chl.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(Chl.anova$residuals)
summary(Chl.anova)


NPDR.anova <- aov(NPDR ~ factor(Species), data = Cardinal_points_TABLE)
hist(NPDR.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(NPDR.anova$residuals)
summary(NPDR.anova)


#####################################################
#################### not plotting these ones in paper ##################################################


DR_WC_optimal.anova <- aov(DR_WC_optimal ~ factor(Species), data = Cardinal_points_TABLE)
hist(DR_WC_optimal.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(DR_WC_optimal.anova$residuals)
summary(DR_WC_optimal.anova)
TukeyHSD(DR_WC_optimal.anova)

NPDR.anova <- aov(NPDR_log ~ factor(Species), data = Cardinal_points_TABLE)
hist(NPDR.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(NPDR.anova$residuals)
summary(NPDR.anova)
TukeyHSD(NPDR.anova)
######################################################################################



#########################################################################################
################Shapiro Wilk on RESIDUALS ##############################################

shapiro.test(Max_NP_PARsat.anova$residuals)## = not normal 0.01828

shapiro.test(Max_DR_PARsat.anova$residuals) ## = normal 0.7124

shapiro.test(LCP.anova$residuals) ## = not normal 0.02523

shapiro.test(LSP.anova$residuals) ## = normal 0.1506

shapiro.test(WC_Min.anova$residuals) ## = not normal 0.0000149

shapiro.test(WC_Max.anova$residuals) ## = not normal 0.000769

shapiro.test(Chl.anova$residuals) ## = normal 0.7687

shapiro.test(WC_Optimal_Range.anova$residuals) ## = not normal 0.0003075

shapiro.test(NPDR.anova$residuals) ## = normal 0.8




################ not plotting these ones in paper ################################################################
shapiro.test(NP_WC_optimal.anova$residuals) ## normal = 0.1158

shapiro.test(DR_WC_optimal.anova$residuals) ## not normal 0.5849


shapiro.test(NPDR.anova$residuals)

#################################################################################

#### if not normal distribution after S_W test, then need to transform data ###################################


############################ TRANSFORM ABNORMAL DATA #########################################
## TRANSFORMATIONS HAVE BEEN MADE TO ABOVE CODE ###############################

Max_NP_PARsat_log10 <-(log10(Cardinal_points_TABLE$Max_NP_PARsat))

LCP_log10 <- (log10(Cardinal_points_TABLE$LCP))

WC_Min_log10 <- (log10(Cardinal_points_TABLE$WC_Min))

WC_Max_log10 <- (log10(Cardinal_points_TABLE$WC_Max))

WC_Optimal_range_log10 <- (log10(Cardinal_points_TABLE$WC_Optimal_Range))

################################################################



#################################################################################################

################ redo ANOVA for logged data, so I can get RESIDUALS for new Shapiro-Wilk tests (these are not the final ANOVAS) ######################
Max_NP_PARsat.anova.log10 <- aov(Max_NP_PARsat_log10 ~ factor(Species), data = Cardinal_points_TABLE)

LCP.anova.log10 <- aov(LCP_log10~ factor(Species), data = Cardinal_points_TABLE)

WC_Min.anova.log10 <- aov(WC_Min_log10 ~ factor(Species), data = Cardinal_points_TABLE)

WC_Max.anova.log10 <- aov(WC_Max_log10 ~ factor(Species), data = Cardinal_points_TABLE)

WC_optimal.range.anova.log10 <- aov(WC_Optimal_range_log10 ~ factor(Species), data = Cardinal_points_TABLE)



########### S-W on logged data!!!!! ####################

shapiro.test(Max_NP_PARsat.anova.log10$residuals)## now all these are above 0.05 

shapiro.test(LCP.anova.log10$residuals) ## = 

shapiro.test(WC_Min.anova.log10$residuals) ## 

shapiro.test(WC_Max.anova.log10$residuals) 

shapiro.test(WC_optimal.range.anova.log10$residuals) 

##################################################################


########## ANOVAs ####################################
############################ ANOVAS #############
##########ANOVAS #########################################

Max_NP_PARsat.anova <- aov(Max_NP_PARsat_log10  ~ factor(Species), data = Cardinal_points_TABLE)
summary(Max_NP_PARsat.anova.log10)


Max_DR_PARsat.anova <- aov(Max_DR_PARsat ~ factor(Species), data = Cardinal_points_TABLE)
summary(Max_DR_PARsat.anova)


LCP.anova <- aov(LCP_log10~ factor(Species), data = Cardinal_points_TABLE)
summary(LCP.anova)


LSP.anova <- aov(LSP ~ factor(Species), data = Cardinal_points_TABLE)
summary(LSP.anova)

WC_Min.anova <- aov(WC_Min_log10 ~ factor(Species), data = Cardinal_points_TABLE)
summary(WC_Min.anova)


WC_Max.anova <- aov(WC_Max_log10 ~ factor(Species), data = Cardinal_points_TABLE)
summary(WC_Max.anova)

WC_optimal.range.anova <- aov(WC_Optimal_range_log10 ~ factor(Species), data = Cardinal_points_TABLE)
summary(WC_optimal.range.anova)


Chl.anova <- aov(Chl ~ factor(Species), data = Cardinal_points_TABLE)
summary(Chl.anova)

NPDR.anova <- aov(NPDR ~ factor(Species), data = Cardinal_points_TABLE)
summary(NPDR.anova)

################### then TUKEY POST HOC TEST - to see where differences lie #########################

tukeyNP.par.sat <- TukeyHSD(Max_NP_PARsat.anova)


tukeyDR.par.sat <- TukeyHSD(Max_DR_PARsat.anova)


tukeyLCP <-TukeyHSD(LCP.anova)


tukeyLSP <- TukeyHSD(LSP.anova)

tukey.WC.min <- TukeyHSD(WC_Min.anova.log10)

tukey.WC.max <- TukeyHSD(WC_Max.anova)


tukey.WC.optimal.range <- TukeyHSD(WC_optimal.range.anova)


tukey.chl <- TukeyHSD(Chl.anova)

tukey.NPDR <- TukeyHSD(NPDR.anova)


########### Generate CLD letter of significance - Wiout RV  ########################################

cld.NP.par.sat <- multcompLetters4(Max_NP_PARsat.anova ,tukeyNP.par.sat)
print(cld.NP.par.sat)

cldDR.par.sat <- multcompLetters4(Max_DR_PARsat.anova,tukeyDR.par.sat)
print(cldDR.par.sat)

cldLCP <- multcompLetters4(LCP.anova,tukeyLCP)
print(cldLCP)

cldLSP <- multcompLetters4(LSP.anova,tukeyLSP)
print(cldLSP)

cld.WC.min <- multcompLetters4(WC_Min.anova,tukey.WC.min)
print(cld.WC.min)

cld.WC.max <- multcompLetters4(WC_Max.anova,tukey.WC.max)
print(cld.WC.max)

cld.Chl <- multcompLetters4(Chl.anova,tukey.chl)
print(cld.Chl)

cld.WC.optimal.range <- multcompLetters4(WC_optimal.range.anova,tukey.WC.optimal.range)
print(cld.WC.optimal.range)

cld.NPDR<- multcompLetters4(NPDR.anova,tukey.NPDR)
print(cld.NPDR)
































############# new set of data without Lp ###########################

########## Normality testing, without Lobaria pulmonaria and Ricasolia virens #########################
#######################################################################################################################################################################
#######################################################################################################################################################################
######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################

Max_NP_PARsat.anova.noLp <- aov(Max_NP_PARsat ~ factor(Species), data = Cardinal_points_TABLE)
hist(Max_NP_PARsat.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(Max_NP_PARsat.anova.NoLp$residuals)
summary(Max_NP_PARsat.anova.noLp)


Max_DR_PARsat.anova.noLp <- aov(Max_DR_PARsat ~ factor(Species), data = Cardinal_points_TABLE)
hist(Max_DR_PARsat.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(Max_DR_PARsat.anova.noLp$residuals)
summary(Max_DR_PARsat.anova.noLp)


LCP.anova.noLp <- aov(LCP~ factor(Species), data = Cardinal_points_TABLE)
hist(LCP.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(LCP.anova$residuals) 
summary(LCP.anova.noLp)


LSP.anova.noLp <- aov(LSP ~ factor(Species), data = Cardinal_points_TABLE)
hist(LSP.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(LSP.anova.noLp$residuals)
summary(LSP.anova.noLp)

WC_Min.anova.noLp <- aov(WC_Min ~ factor(Species), data = Cardinal_points_TABLE)
hist(WC_Min.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Min.anova.noLp$residuals)
summary(WC_Min.anova.noLp)


WC_Max.anova.noLp <- aov(WC_Max ~ factor(Species), data = Cardinal_points_TABLE)
hist(WC_Max.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Max.anova.noLp$residuals)
summary(WC_Max.anova.noLp)


Chl.anova.noLp <- aov(Chl ~ factor(Species), data = Cardinal_points_TABLE)
hist(Chl.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(Chl.anova.noLp$residuals)
summary(Chl.anova.noLp)


WC_Optimal_Range.anova.noLp <- aov(WC_Optimal_Range ~ factor(Species), data = Cardinal_points_TABLE)
hist(WC_Optimal_Range.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Optimal_Range.anova.noLp$residuals)
summary(WC_Optimal_Range.anova.noLp)


#########################################################################################
################Shapiro Wilk on RESIDUALS ##############################################


shapiro.test(Max_NP_PARsat.anova.noLp$residuals)## = not normal 0.007078

shapiro.test(Max_DR_PARsat.anova.noLp$residuals) ## = normal 0.7124

shapiro.test(LCP.anova.noLp$residuals) ## = not normal 0.02523

shapiro.test(LSP.anova.noLp$residuals) ## = normal 0.1506

shapiro.test(WC_Min.anova.noLp$residuals) ## = not normal 0.0000149

shapiro.test(WC_Max.anova.noLp$residuals) ## = not normal 0.000769

shapiro.test(Chl.anova.noLp$residuals) ## = normal 0.7687

shapiro.test(WC_Optimal_Range.anova.noLp$residuals) ## = not normal 0.00257


## TRANSFORMATIONS HAVE BEEN MADE TO ABOVE CODE ###############################

Max_NP_PARsat_log10_no_Lp <-(log10(Cardinal_points_TABLE$Max_NP_PARsat))

LCP_log10_no_Lp <- (log10(Cardinal_points_TABLE$LCP))

WC_Min_log10_no_Lp <- (log10(Cardinal_points_TABLE$WC_Min))

WC_Max_log10_no_Lp <- (log10(Cardinal_points_TABLE$WC_Max))

WC_optimal_range_log10_no_Lp <- (log10(Cardinal_points_TABLE$WC_Optimal_Range))



################ redo ANOVAs with logged data, so I can get RESIDUALS for new Shapiro-Wilk tests (these are not the final ANOVAS) ######################
Max_NP_PARsat.anova.log10.noLp <- aov(Max_NP_PARsat_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

LCP.anova.log10.noLp <- aov(LCP_log10_no_Lp~ factor(Species), data = Cardinal_points_TABLE)

WC_Min.anova.log10.noLp <- aov(WC_Min_log10_no_Lp~ factor(Species), data = Cardinal_points_TABLE)

WC_Max.anova.log10.noLp <- aov(WC_Max_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

WC_optimal.range.anova.log10.noLp <- aov(WC_optimal_range_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)



################################################################
########### redo S-W on logged data!!!!! ####################

shapiro.test(Max_NP_PARsat.anova.log10.noLp$residuals)## now all these are above 0.05 

shapiro.test(LCP.anova.log10.noLp$residuals) ## = 

shapiro.test(WC_Min.anova.log10.noLp$residuals) ## 

shapiro.test(WC_Max.anova.log10.noLp$residuals) #

shapiro.test(WC_optimal.range.anova.log10.noLp$residuals) #


#########################################################################################


########## ANOVAs REDONE####################################
############################ ANOVAS #############
##########ANOVAS #########################################

Max_NP_PARsat.anova.noLp <- aov(Max_NP_PARsat_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

summary(Max_NP_PARsat.anova.noLp)


Max_DR_PARsat.anova.noLp <- aov(Max_DR_PARsat ~ factor(Species), data = Cardinal_points_TABLE)

summary(Max_DR_PARsat.anova.noLp)


LCP.anova.noLp <- aov(LCP_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

summary(LCP.anova.noLp)


LSP.anova.noLp <- aov(LSP ~ factor(Species), data = Cardinal_points_TABLE)

summary(LSP.anova.noLp)

WC_Min.anova.noLp <- aov(WC_Min_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

summary(WC_Min.anova.noLp)

WC_Max.anova.noLp <- aov(WC_Max_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

summary(WC_Max.anova.noLp)


Chl.anova.noLp <- aov(Chl ~ factor(Species), data = Cardinal_points_TABLE)

summary(Chl.anova.noLp)


optimal.range.anova.noLp <- aov(WC_optimal_range_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

summary(optimal.range.anova.noLp )


################### then TUKEY POST HOC TEST - to see where differences lie #########################

tukeyNP.parsat.noLp <- TukeyHSD(Max_NP_PARsat.anova.noLp)


tukeyDR.parsat.noLp <- TukeyHSD(Max_DR_PARsat.anova.noLp)


tukeyLCP.noLp <-TukeyHSD(LCP.anova.noLp)


tukeyLSP.noLp <- TukeyHSD(LSP.anova.noLp)


tukey.WC.min.noLp <- TukeyHSD(WC_Min.anova.noLp)


tukey.WC.max.noLp <- TukeyHSD(WC_Max.anova.noLp)


tukey.chl.noLp <- TukeyHSD(Chl.anova.noLp)

tukey.optimal.range.noLp <- TukeyHSD(optimal.range.anova.noLp)


########### Generate CLD letter of significance - Wiout RV  ########################################

cld.NP.par.sat.noLp <- multcompLetters4(Max_NP_PARsat.anova.noLp ,tukeyNP.parsat.noLp)
print(cld.NP.par.sat.noLp)

cldDR.par.sat <- multcompLetters4(Max_DR_PARsat.anova.noLp,tukeyDR.parsat.noLp)
print(cldDR.par.sat)

cldLCP <- multcompLetters4(LCP.anova.noLp,tukeyLCP.noLp)
print(cldLCP)

cldLSP <- multcompLetters4(LSP.anova.noLp,tukeyLSP.noLp)
print(cldLSP)

cld.WC.min <- multcompLetters4(WC_Min.anova.noLp,tukey.WC.max.noLp)
print(cld.WC.min)

cld.WC.max <- multcompLetters4(WC_Max.anova.noLp,tukey.WC.max.noLp)
print(cld.WC.max)


cld.chl.no.lp <- multcompLetters4(WC_Max.anova.noLp, tukey.chl.noLp )
print(cld.WC.max)

cld.optimal.range.no.lp <- multcompLetters4(optimal.range.anova.noLp, tukey.optimal.range.noLp)
print(cld.optimal.range.no.lp)

cld.NPDR<- multcompLetters4(NPDR.anova,tukey.NPDR)
print(cld.NPDR)













#######################################################################################################################################

########## Normality testing - TWO WAY ANOVAS ---  without Lobaria pulmonaria and Ricasolia virens #########################
#######################################################################################################################################################################
#######################################################################################################################################################################
######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################



Cardinal_points_TABLE$Species <- factor(Cardinal_points_TABLE$Species, levels = c("Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata",   "Pectenia atlantica"))

groups <- with(Cardinal_points_TABLE,(type))

Cardinal_points_TABLE$type <- factor(Cardinal_points_TABLE$type, levels = c("cyanolichen", "chlorolichen"))



##### ANOVAs to generate residuals ###############

Max_NP_PARsat.anova.noLp <- aov(Max_NP_PARsat ~ type * Species, data = Cardinal_points_TABLE)
summary(Max_NP_PARsat.anova.noLp)

Max_DR_PARsat.anova.noLp <- aov(Max_DR_PARsat ~ type * Species, data = Cardinal_points_TABLE)
hist(Max_DR_PARsat.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(Max_DR_PARsat.anova.noLp$residuals)
summary(Max_DR_PARsat.anova.noLp)


LCP.anova.noLp <- aov(LCP ~  type * Species, data = Cardinal_points_TABLE)
hist(LCP.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(LCP.anova$residuals) 
summary(LCP.anova.noLp)


LSP.anova.noLp <- aov(LSP ~  type * Species, data = Cardinal_points_TABLE)
hist(LSP.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(LSP.anova.noLp$residuals)
summary(LSP.anova.noLp)


Chl.anova.noLp <- aov(Chl ~ type * Species, data = Cardinal_points_TABLE)
hist(Chl.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(Chl.anova.noLp$residuals)
summary(Chl.anova.noLp)


WC_Optimal_Range.anova.noLp <- aov(WC_Optimal_Range ~ type * Species, data = Cardinal_points_TABLE)
hist(WC_Optimal_Range.anova.noLp$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Optimal_Range.anova.noLp$residuals)
summary(WC_Optimal_Range.anova.noLp)


####### NORMALITY TESTING ##########################################

shapiro.test(Max_NP_PARsat.anova.noLp$residuals) 

shapiro.test(Max_DR_PARsat.anova.noLp$residuals) #

shapiro.test(LCP.anova.noLp$residuals) ## = 

shapiro.test(LSP.anova.noLp$residuals) ## =

shapiro.test(Chl.anova.noLp$residuals) ## =

shapiro.test(WC_Optimal_Range.anova.noLp$residuals) ## 

##############################################################################



## TRANSFORMATIONS HAVE BEEN MADE TO ABOVE CODE ###############################

Max_NP_PARsat_log10_no_Lp <-(log10(Cardinal_points_TABLE$Max_NP_PARsat))

LCP_log10_no_Lp <- (log10(Cardinal_points_TABLE$LCP))


WC_optimal_range_log10_no_Lp <- (log10(Cardinal_points_TABLE$WC_Optimal_Range))



################ redo ANOVAs with logged data, so I can get RESIDUALS for new Shapiro-Wilk tests (these are not the final ANOVAS) ######################
Max_NP_PARsat.anova.log10.noLp <- aov(Max_NP_PARsat_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)

LCP.anova.log10.noLp <- aov(LCP_log10_no_Lp~ factor(Species), data = Cardinal_points_TABLE)

WC_optimal.range.anova.log10.noLp <- aov(WC_optimal_range_log10_no_Lp ~ factor(Species), data = Cardinal_points_TABLE)



################################################################
########### redo S-W on logged data!!!!! ####################

shapiro.test(Max_NP_PARsat.anova.log10.noLp$residuals)## now all these are above 0.05 

shapiro.test(LCP.anova.log10.noLp$residuals) ## = 


shapiro.test(WC_optimal.range.anova.log10.noLp$residuals) #


#########################################################################################


########## ANOVAs REDONE####################################
############################ ANOVAS #############
##########ANOVAS #########################################

Max_NP_PARsat.anova.noLp <- aov(Max_NP_PARsat_log10_no_Lp ~ type * Species, data = Cardinal_points_TABLE)

summary(Max_NP_PARsat.anova.noLp)


Max_DR_PARsat.anova.noLp <- aov(Max_DR_PARsat ~ type * Species, data = Cardinal_points_TABLE)

summary(Max_DR_PARsat.anova.noLp)


LCP.anova.noLp <- aov(LCP_log10_no_Lp ~ type * Species, data = Cardinal_points_TABLE)

summary(LCP.anova.noLp)


LSP.anova.noLp <- aov(LSP ~ type * Species, data = Cardinal_points_TABLE)

summary(LSP.anova.noLp)

WC_Min.anova.noLp <- aov(WC_Min_log10_no_Lp ~ type * Species, data = Cardinal_points_TABLE)

summary(WC_Min.anova.noLp)

WC_Max.anova.noLp <- aov(WC_Max_log10_no_Lp ~ type * Species, data = Cardinal_points_TABLE)

summary(WC_Max.anova.noLp)


Chl.anova.noLp <- aov(Chl ~ type * Species, data = Cardinal_points_TABLE)

summary(Chl.anova.noLp)


optimal.range.anova.noLp <- aov(WC_optimal_range_log10_no_Lp ~ type * Species, data = Cardinal_points_TABLE)

summary(optimal.range.anova.noLp )


################### then TUKEY POST HOC TEST - to see where differences lie #########################

############ need species names as numeric to run ANOVAS, but then need them as characters to run Tukey post hocs.... no idea why ############
#####Having characters for species names when running the ANOVAs  does not give the interaction term, but you get the interaction value when running ANOVas using numbers for species names #########



Cardinal_data_Factors <- within(Cardinal_points_TABLE, {
  Species  <- factor(Species)
  type <- factor(type)
})



tukeyNP.parsat.noLp <- TukeyHSD(Max_NP_PARsat.anova.noLp, which = "type")
TukeyHSD(Max_NP_PARsat.anova.noLp, which = "Species")


tukeyDR.parsat.noLp <- TukeyHSD(Max_DR_PARsat.anova.noLp, which = "type")


tukeyLCP.noLp <- TukeyHSD(LCP.anova.noLp, which = "type")


tukeyLSP.noLp <- TukeyHSD(LSP.anova.noLp, which = "type")


tukey.WC.min.noLp <- TukeyHSD(WC_Min.anova.noLp, which = "type")


tukey.WC.max.noLp <- TukeyHSD(WC_Max.anova.noLp, which = "type")


tukey.chl.noLp <- TukeyHSD(Chl.anova.noLp, which = "type")


tukey.optimal.range.noLp <- TukeyHSD(optimal.range.anova.noLp, which = "type")


########### Generate CLD letter of significance - Wiout RV  ########################################

cld.NP.par.sat.noLp <- multcompLetters4(Max_NP_PARsat.anova.noLp ,tukeyNP.parsat.noLp)
print(cld.NP.par.sat.noLp)

cldDR.par.sat <- multcompLetters4(Max_DR_PARsat.anova.noLp,tukeyDR.parsat.noLp)
print(cldDR.par.sat)

cldLCP <- multcompLetters4(LCP.anova.noLp,tukeyLCP.noLp)
print(cldLCP)

cldLSP <- multcompLetters4(LSP.anova.noLp,tukeyLSP.noLp)
print(cldLSP)

cld.WC.min <- multcompLetters4(WC_Min.anova.noLp,tukey.WC.max.noLp)
print(cld.WC.min)

cld.WC.max <- multcompLetters4(WC_Max.anova.noLp,tukey.WC.max.noLp)
print(cld.WC.max)


cld.chl.no.lp <- multcompLetters4(WC_Max.anova.noLp, tukey.chl.noLp )
print(cld.WC.max)

cld.optimal.range.no.lp <- multcompLetters4(optimal.range.anova.noLp, tukey.optimal.range.noLp)
print(cld.optimal.range.no.lp)

cld.NPDR<- multcompLetters4(NPDR.anova,tukey.NPDR)
print(cld.NPDR)








########## Normality testing - TWO WAY ANOVAS ---  WITH Lobaria pulmonaria  #########################
#######################################################################################################################################################################
#######################################################################################################################################################################
######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################



Cardinal_points_TABLE$Species <- factor(Cardinal_points_TABLE$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata", "Pectenia atlantica"))

groups <- with(Cardinal_points_TABLE,(type))

Cardinal_points_TABLE$type <- factor(Cardinal_points_TABLE$type, levels = c("cyanolichen", "chlorolichen", "cephalolichen"))



##### ANOVAs to generate residuals ###############

Max_NP_PARsat.anova <- aov(Max_NP_PARsat ~ type * Species, data = Cardinal_points_TABLE)
summary(Max_NP_PARsat.anova)

Max_DR_PARsat.anova <- aov(Max_DR_PARsat ~ type * Species, data = Cardinal_points_TABLE)
hist(Max_DR_PARsat.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(Max_DR_PARsat.anova$residuals)
summary(Max_DR_PARsat.anova)


LCP.anova<- aov(LCP ~  type * Species, data = Cardinal_points_TABLE)
hist(LCP.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(LCP.anova$residuals) 
summary(LCP.anova)


LSP.anova <- aov(LSP ~  type * Species, data = Cardinal_points_TABLE)
hist(LSP.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(LSP.anova$residuals)
summary(LSP.anova)


Chl.anova <- aov(Chl ~ type * Species, data = Cardinal_points_TABLE)
hist(Chl.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(Chl.anova$residuals)
summary(Chl.anova)


WC_Optimal_Range.anova <- aov(WC_Optimal_Range ~ type * Species, data = Cardinal_points_TABLE)
hist(WC_Optimal_Range.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Optimal_Range.anova$residuals)
summary(WC_Optimal_Range.anova)


WC_Max.anova <- aov(WC_Max ~ type * Species, data = Cardinal_points_TABLE)
hist(WC_Max.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Max.anova$residuals)
summary(WC_Max.anova)


WC_Min.anova <- aov(WC_Min ~ type * Species, data = Cardinal_points_TABLE)
hist(WC_Min.anova$residuals)  ########### checking that data follows normal distribution
qqnorm(WC_Min.anova$residuals)
summary(WC_Min.anova)


WC_Optimal_percentage.anova <- aov(optimal_WC_percentage ~ type * Species, data = Cardinal_points_TABLE)
summary(WC_Optimal_percentage.anova)

NPDR.anova <- aov(NPDR ~ type * Species, data = Cardinal_points_TABLE)
summary(NPDR.anova)




####### NORMALITY TESTING ##############

############################

shapiro.test(Max_NP_PARsat.anova$residuals) 

shapiro.test(Max_DR_PARsat.anova$residuals) #

shapiro.test(LCP.anova$residuals) ## = 

shapiro.test(LSP.anova$residuals) ## =

shapiro.test(Chl.anova$residuals) ## =

shapiro.test(WC_Optimal_Range.anova$residuals)

shapiro.test(WC_Max.anova$residuals)

shapiro.test(WC_Min.anova$residuals)

shapiro.test(WC_Optimal_percentage.anova$residuals)


shapiro.test(NPDR.anova$residuals)
##############################################################################



## TRANSFORMATIONS HAVE BEEN MADE TO ABOVE CODE ###############################

Max_NP_PARsat_log10 <- (log10(Cardinal_points_TABLE$Max_NP_PARsat))


WC_optimal_range_log10 <- (log10(Cardinal_points_TABLE$WC_Optimal_Range))


WC_Max_log10 <- (log10(Cardinal_points_TABLE$WC_Max))

 
WC_Min_log10 <- (log10(Cardinal_points_TABLE$WC_Min))


WC_optimal_percentage_log10 <- (log10(Cardinal_points_TABLE$optimal_WC_percentage))




################ redo ANOVAs with logged data, so I can get RESIDUALS for new Shapiro-Wilk tests (these are not the final ANOVAS) ######################
Max_NP_PARsat.anova.log10 <- aov(Max_NP_PARsat_log10 ~ factor(Species), data = Cardinal_points_TABLE)


WC_optimal.range.anova.log10 <- aov(WC_optimal_range_log10 ~ factor(Species), data = Cardinal_points_TABLE)

WC_Max.anova.log10 <- aov(WC_Max_log10 ~ factor(Species), data = Cardinal_points_TABLE)

WC_Min.anova.log10 <- aov(WC_Min_log10 ~ factor(Species), data = Cardinal_points_TABLE)

WC_optimal.anova.log10.percentage <- aov(WC_optimal_percentage_log10 ~ factor(Species), data = Cardinal_points_TABLE)


################################################################
########### redo S-W on logged data!!!!! ####################




shapiro.test(Max_NP_PARsat.anova.log10$residuals) #

shapiro.test(WC_optimal.range.anova.log10$residuals) #

shapiro.test(WC_Max.anova.log10$residuals) #

shapiro.test(WC_Min.anova.log10$residuals) 

shapiro.test(WC_optimal.anova.log10.percentage$residuals) ##

#########################################################################################


########## ANOVAs REDONE####################################
############################ ANOVAS #############
##########ANOVAS #########################################

Max_NP_PARsat.anova <- aov(Max_NP_PARsat_log10 ~ type * Species, data = Cardinal_points_TABLE)

summary(Max_NP_PARsat.anova)


Max_DR_PARsat.anova <- aov(Max_DR_PARsat ~ type * Species, data = Cardinal_points_TABLE)

summary(Max_DR_PARsat.anova)


LCP.anova.noLp <- aov(LCP ~ type * Species, data = Cardinal_points_TABLE)

summary(LCP.anova)


LSP.anova<- aov(LSP ~ type * Species, data = Cardinal_points_TABLE)

summary(LSP.anova)


WC_Min.anova <- aov(WC_Min_log10 ~ type * Species, data = Cardinal_points_TABLE)

summary(WC_Min.anova)

WC_Max.anova.noLp <- aov(WC_Max_log10 ~ type * Species, data = Cardinal_points_TABLE)

summary(WC_Max.anova)


Chl.anova <- aov(Chl ~ type * Species, data = Cardinal_points_TABLE)

summary(Chl.anova)


optimal.range.anova <- aov(WC_optimal_range_log10 ~ type * Species, data = Cardinal_points_TABLE)

summary(optimal.range.anova)


optimal.percentage.anova <- aov(WC_optimal_percentage_log10~ type * Species, data = Cardinal_points_TABLE)

summary(optimal.percentage.anova)



NPDR.anova <- aov(NPDR~ type * Species, data = Cardinal_points_TABLE)

summary(NPDR.anova)




################### then TUKEY POST HOC TEST - to see where differences lie #########################

############ need species names as numeric to run ANOVAS, but then need them as characters to run Tukey post hocs.... no idea why ############
#####Having characters for species names when running the ANOVAs  does not give the interaction term, but you get the interaction value when running ANOVas using numbers for species names #########



Cardinal_data_Factors <- within(Cardinal_points_TABLE, {
  Species  <- factor(Species)
  type <- factor(type)
})



tukeyNP.parsat<- TukeyHSD(Max_NP_PARsat.anova, which = "type")
TukeyHSD(Max_NP_PARsat.anova, which = "Species")


tukeyDR.parsat<- TukeyHSD(Max_DR_PARsat.anova, which = "type")


tukeyLCP <- TukeyHSD(LCP.anova, which = "type")


tukeyLSP <- TukeyHSD(LSP.anova, which = "type")


tukey.WC.min <- TukeyHSD(WC_Min.anova, which = "type")


tukey.WC.max <- TukeyHSD(WC_Max.anova, which = "type")


tukey.chl<- TukeyHSD(Chl.anova, which = "type")


tukey.optimal.range <- TukeyHSD(optimal.range.anova, which = "type")


tukey.optimal.percentage <- TukeyHSD(optimal.percentage.anova, which = "type")

tukey.NPDR <- TukeyHSD(NPDR.anova, which = "type")




########### Generate CLD letter of significance - Wiout RV  ########################################

cld.NP.par.sat <- multcompLetters4(Max_NP_PARsat.anova.noLp ,tukeyNP.parsat.noLp)
print(cld.NP.par.sat.noLp)

cldDR.par.sat <- multcompLetters4(Max_DR_PARsat.anova.noLp,tukeyDR.parsat.noLp)
print(cldDR.par.sat)

cldLCP <- multcompLetters4(LCP.anova.noLp,tukeyLCP.noLp)
print(cldLCP)

cldLSP <- multcompLetters4(LSP.anova.noLp,tukeyLSP.noLp)
print(cldLSP)

cld.WC.min <- multcompLetters4(WC_Min.anova.noLp,tukey.WC.max.noLp)
print(cld.WC.min)

cld.WC.max <- multcompLetters4(WC_Max.anova.noLp,tukey.WC.max.noLp)
print(cld.WC.max)


cld.chl.no.lp <- multcompLetters4(WC_Max.anova.noLp, tukey.chl.noLp )
print(cld.WC.max)

cld.optimal.range.no.lp <- multcompLetters4(optimal.range.anova.noLp, tukey.optimal.range.noLp)
print(cld.optimal.range.no.lp)

cld.NPDR<- multcompLetters4(NPDR.anova,tukey.NPDR)
print(cld.NPDR)













