
library(plotly)
library(readxl)

install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")

library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(ggplot2)
library(mgcv)
library(mgcViz)
install.packages("readxl")
install.packages("tidyverse", dependencies = TRUE)
library(forcats)
install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(grid) 
install.packages("magrittr")
library(magrittr)
install.packages("ggtext")
library(ggtext)

install.packages("vioplot")
library(vioplot)
library(forcats)

##### make sre that dplyr is successfully installed, otherwise Tidyverse will not install proprtly!!######################
remove.packages("dplyr")
remove.packages("rlang")
remove.packages("cli")


install.packages("rlang")
library(rlang)
install.packages("cli")
library(cli)
install.packages("dplyr")
library(dplyr)


  
## wc max #####
WC.max.summary <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(WC_Max, na.rm = TRUE),
    WC = mean(WC_Max))
WC.max.summary


### wc min ####
WC.min.summary <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(WC_Min, na.rm = TRUE),
    WC = mean(WC_Min))
WC.min.summary


#### WC optimal RANGE ###########
WC.optimal <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(WC_Optimal_Range, na.rm = TRUE),
    WC = mean(WC_Optimal_Range))
 WC.optimal

 #### NP at wc optimal ####
 test <- Cardinal_points_TABLE222 %>%
   group_by(Species) %>%
  summarise(
    sd = sd(NP_WC_optimal, na.rm = TRUE),
    NP = mean(NP_WC_optimal))
test 

NP.wc.optimal.summary <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(NP_WC_optimal, na.rm = TRUE),
    NP = mean(NP_WC_optimal))
NP.wc.optimal.summary  
#### NP at wc optimal ####


### LCP #########
LCP <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(LCP, na.rm = TRUE),
    LCP = mean(LCP))
LCP

####### LSP #######
LSP <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(LSP, na.rm = TRUE),
    LSP = mean(LSP))
LSP

########### DR at WC optimal ###########  

DR.wc.optimal <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(DR_WC_optimal, na.rm = TRUE),
    DR = mean(DR_WC_optimal))
DR.wc.optimal

####### NP at PAR saturtaion ########
NP.at.PARsat <- Cardinal_points_TABLE %>%
  group_by(Species) %>%
  summarise(
    sd = sd(Max_NP_PARsat, na.rm = TRUE),
    NP = mean(Max_NP_PARsat))
NP.at.PARsat
   

##### DR at PAR Saturation ###########
DR.at.PARsat <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(Max_DR_PARsat, na.rm = TRUE),
    NP = mean(Max_DR_PARsat))
DR.at.PARsat



##### Chl ###########
Chl <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(Chl, na.rm = TRUE),
    Chl = mean(Chl))
Chl


##### Chl a ONLY ###########
Chl.a.only <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(Chla_only, na.rm = TRUE),
    Chl = mean(Chla_only))
Chl.a.only


##### NPDR ###########
NPDR <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(NPDR, na.rm = TRUE),
    NPDR = mean(NPDR))
NPDR



#### NP at wc optimal ####
test <- Cardinal_points_TABLE222 %>%
  group_by(Species) %>%
  summarise(
    sd = sd(NP_WC_optimal, na.rm = TRUE),
    NP = mean(NP_WC_optimal))
test 



######### FOR THESE TO PLOT CORRECTLY, DATA NEEDS TO HAVE SPECIES NAMES AS NUMBERS AND NOT WRITTEN OUT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #############
############# DR at OptimalWC ################################################################################################


Cardinal_points_TABLE$Species <- factor(Cardinal_points_TABLE$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica")) 




###### new summary of data with added letters ###############
numbers2 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(DR_WC_optimal, na.rm = TRUE),
    drmean = mean(DR_WC_optimal)) %>% 
  mutate(count = c("ab","a","b","ab","ab","a","a"))
numbers2
##################################################################

   DR.at.wc.optimal <- ggplot(data = numbers2, aes(x = as.factor(Species), y = drmean, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = drmean - sd, ymax = drmean + sd), width = 0.2, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  theme(legend.position = c(0.82, 0.86)) +
  theme(axis.title.y = element_markdown(size = 13)) +
  ylab("Maximum respiration <br>(nmol g<sup>-1</sup>s<sup>-1</sup>)")+
  scale_y_reverse(expand= c(0,0)) +          
  coord_cartesian(ylim=c(0,-10))  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  geom_text(data = numbers2, aes(x = Species, y = drmean, label = count), nudge_y = -0.04, vjust = -0.5, hjust = -0.7, colour ="grey16")##########


####### NP Max at Optimal WC ##############################
#numbers <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(NP_WC_optimal, na.rm = TRUE),
    NP = mean(NP_WC_optimal)) %>% 
  mutate(count = c("ab","b","a","b","b","b","b"))
numbers###########


###########################################################


NP.at.wc.optimal <- ggplot(data = numbers, aes(x = as.factor(Species), y = NP, colour = factor(Species), fill = factor(Species))) + 
geom_bar(stat="identity") +
geom_errorbar(aes(ymin = NP - sd, ymax = NP + sd), width = 0.2, color = "black", position = position_dodge(0.9))  +            
scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Ricasolia virens", "Sticta limbata", "Sticta sylvatica",  "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
theme(axis.text.x = element_blank()) +
theme(axis.title.x = element_blank()) +
theme(axis.ticks.x = element_blank()) +
theme(legend.position = "none") +
theme(axis.title.y = element_text(size = 8, face = "bold")) +
  ylab("Maximum net photosynthesis <br>(nmol g<sup>-1</sup>s<sup>-1</sup>)")+
scale_y_continuous(breaks=seq(0,70, by=5), expand= c(0,0)) +
coord_cartesian(ylim=c(0,60)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
geom_text(data = numbers, aes(x = Species, y = NP, label = count), vjust = -0.5, hjust = -1.1, colour ="grey16")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
Cardinal_points_TABLE$Species <- factor(Cardinal_points_TABLE$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica")) 

  
  
  
  
  
  
################################################################################################################################################
#################### NP at PAR SAT ##################################################################
  
  numbersno.rv <- Cardinal_points_TABLE %>% 
    group_by(Species) %>% 
    summarise (
      sd = sd(Max_NP_PARsat, na.rm = TRUE),
      NP = mean(Max_NP_PARsat)) %>% 
    mutate(count = c("ab","bc","a", "bc","bc","bc","c"))
  numbersno.rv

  
NP.at.PARSAT <- ggplot(data = numbersno.rv, aes(x = as.factor(Species), y = NP, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = NP - sd, ymax = NP + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +
scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_markdown(size = 14)) +
  theme(axis.text.y = element_markdown(size = 14)) +
  scale_y_continuous(breaks=seq(0,70, by=5), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,60)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  theme(plot.margin=unit(c(1,-0.1,0.4,1), "cm")) +
  geom_text(data = numbersno.rv, aes(x = Species, y = NP, label = count), vjust = -0.5, hjust = -0.6, size =5, colour ="grey16") +
ylab("Maximum net photosynthesis <br>(nmol g<sup>-1  </sup> s<sup>-1</sup> )")


#################################################################################################################################
#######################################################################################################
################ DR at PARSAT ##############################

numbers12 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(Max_DR_PARsat, na.rm = TRUE),
    drmean = mean(Max_DR_PARsat)) %>% 
  mutate(count = c("ab","a","b", "ab", "ab","a","a"))
numbers12



##################################################################

DR.PARSat <- ggplot(data = numbers12, aes(x = as.factor(Species), y = drmean, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = drmean - sd, ymax = drmean + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  theme(legend.position = c(0.82, 0.86)) +
  theme(axis.title.y = element_markdown(size = 14)) +
  theme(axis.text.y = element_markdown(size = 14)) +
  ylab("Maximum respiration <br>(nmol g<sup>-1 </sup> s<sup>-1</sup> )")+
  scale_y_reverse(expand= c(0,0)) +          
  coord_cartesian(ylim=c(0,-10))  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.1,0.4,1), "cm")) +
  geom_text(data = numbers12, aes(x = Species, y = drmean, label = count), nudge_y = -0.04, vjust = -0.45, hjust = -0.4, size =5,  colour ="grey16")


  
  



##########################################################################################

####### LCP ##########################################################

numbers3 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(LCP, na.rm = TRUE),
    LCP = mean(LCP)) %>% 
  mutate(count = c("a","a","a","a","a","a","a"))
numbers3


LCP.for.grid <- ggplot(data = numbers3, aes(x = Species, y = LCP, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = LCP - sd, ymax = LCP + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_markdown(size = 14)) +
  theme(axis.text.y = element_markdown(size = 14)) +
  ylab("Light compensation point <br>(&mu;mol photons m<sup>-2 </sup> s<sup>-1</sup> )")+ 
  scale_y_continuous(breaks=seq(0,200, by=50), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,200)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.1,0.4,1), "cm")) +
  geom_text(data = numbers3, aes(x = Species, y = LCP, label = count), nudge_y = -0.04, vjust = -0.6, hjust = -0.6, size =5,  colour ="grey16")

#### ########################### ###########################################



####################################################
####### LSP ####################################################################################

numbers4 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(LSP, na.rm = TRUE),
    LSP = mean(LSP)) %>% 
  mutate(count = c("a","a","a", "a", "a","a","a"))
numbers4



LSP.for.grid <- ggplot(data = numbers4, aes(x = Species, y = LSP, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = LSP - sd, ymax = LSP + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_markdown(size = 14)) +
  theme(axis.text.y = element_markdown(size = 14)) +
  ylab("Light saturation point <br>(&mu;mol photons m<sup>-2 </sup> s<sup>-1</sup> )")+ 
  scale_y_continuous(breaks=seq(0,1400, by=200), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,1400)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.1,0.4,1), "cm")) +
  geom_text(data = numbers4, aes(x = Species, y = LSP, label = count), nudge_y = -0.04, vjust = -0.4, hjust = -0.5, size =5,  colour ="grey16")

###############################################################################




###########################################################
########################## CHl ###############################################################################
numbers5 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(Chl, na.rm = TRUE),
    Chl = mean(Chl)) %>% 
  mutate(count = c("a","a","a", "a", "a","a","a"))
numbers5


Chl.for.grid <- ggplot(data = numbers5, aes(x = Species, y = Chl, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = Chl - sd, ymax = Chl + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_markdown(size = 14,)) +
  theme(axis.text.y = element_markdown(size = 14)) +
  ylab("Total chlorophyll content <br>(&mu;g Chl / mg DW<sup> -2</sup> )")+ 
  scale_y_continuous(breaks=seq(0,10, by=2), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.1,0.4,1), "cm")) +
  geom_text(data = numbers5, aes(x = Species, y = Chl, label = count), nudge_y = -0.04, vjust = -0.5, hjust = -0.55, size =5, colour ="grey16")

################################################################################




##############################################################################################
#################### NPDR#########################################################
Carbon.efficiency <- ggplot(data = NPDR, aes(x = Species, y = NPDR, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = NPDR - sd, ymax = NPDR + sd), width = 0.2, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica",  "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata","Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(size = 8, face = "bold"))+
  ylab(expression(Carbon~use~efficiency)) +  
  scale_y_continuous(breaks=seq(0,2, by=0.5), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,2)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm"))
####################################################################################################



######################## WC min #################################################################

numbers7 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC_Min, na.rm = TRUE),
    WC= mean(WC_Min)) %>% 
  mutate(count = c("c","bc","bc", "n/a","b","b","a"))
numbers7


WC_min <- ggplot(data = numbers7, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.2, color = "grey15", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 13,)) +
  ylab("Optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,1.5, by=0.2), expand= c(0,0))+
  coord_cartesian(ylim=c(0,1.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  geom_text(data = numbers7, aes(x = Species, y = WC, label = count), vjust = -0.8, hjust = -0.7, size =3,  colour ="black")

########################################################################


###################### WC Max ############################################################################
numbers8 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC_Max, na.rm = TRUE),
    WC= mean(WC_Max)) %>% 
  mutate(count = c("d","bcd","cd", "n/a","bc","b","a"))
numbers8



WC_max <- ggplot(data = numbers8, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.2, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica",  "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,1.5, by=0.2), expand= c(0,0))+
  coord_cartesian(ylim=c(0,1.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  geom_text(data = numbers8, aes(x = Species, y = WC, label = count), vjust = -0.8, hjust = -0.7, size =3,  colour ="grey16")
#########################################################################################################################################





######## WC optimal range  #####################################################################################


numbers6 <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC_Optimal_Range, na.rm = TRUE),
    WC= mean(WC_Optimal_Range)) %>% 
  mutate(count = c("b","ab","b","ab", "b", "ab","a"))
numbers6



wc.optimal.for.grid <- ggplot(data = numbers6, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "goldenrod4", "forestgreen",  "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_markdown(size = 13)) +
  theme(axis.text.y = element_markdown(size = 14)) +
  ylab("Optimal range of thallus water content <br>(precipitation equivalent mm H<sub> 2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,1.2, by=0.2), expand= c(0,0))+
  coord_cartesian(ylim=c(0,1.2)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.65, "cm")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.position = c(0.25, 0.68)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  geom_text(data = numbers6, aes(x = Species, y = WC, label = count), vjust = -0.5, hjust = -0.44, size = 5, colour ="black")

####################################################################################################################





theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +



 
### this is the one that works for aligning them all evenly ##########################
plot = plot_grid(NP.at.PARSAT, DR.PARSat, LCP.for.grid, LSP.for.grid, Chl.for.grid, wc.optimal.for.grid, ncol = 2, nrow = 3, align = "hv")
### !!!!!!!!!!!!!!!! THIS IS THE ONE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ###########





grid.arrange(plot, heights =c(5000,50)) 


cardinal.grid <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                       c(3,4),
                       c(5,6))

cardinal.grid <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                      c(3,4))
              


cardinal.grid2 <- rbind(c(1,2)) ##row 1, column 2 and 3 same size
                     
theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
grid.arrange(NP.at.PARSAT, DR.PARSat, LCP.for.grid, LSP.for.grid, Chl.for.grid, wc.optimal.for.grid, ncol = 2, nrow = 3, layout_matrix = cardinal.grid)




grid.arrange(NP.at.PARSAT, DR.PARSat, LCP.for.grid, LSP.for.grid, ncol = 2, nrow = 2, layout_matrix = cardinal.grid)

grid.arrange(Chl.for.grid, wc.optimal.for.grid, ncol = 2, nrow = 1, layout_matrix = cardinal.grid2)
























#####################################################################
######################################################################################################################################
############### both min and max WC on single plot to show range of values #######################
water.summary.new <- water_content_min_and_max %>%
  group_by(Species, level) %>%
  summarise(
    sd = sd(WC, na.rm = TRUE),
    WC = mean(WC))
water.summary.new

### then set species as a factor ########
water.summary.new$Species <- factor(water.summary.new$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens","Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"))


numbers88 <- water_content_min_and_max %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC, na.rm = TRUE),
    WC= mean(WC)) %>% 
  mutate(count = c("c","bc","bc","b","n/a","b","a")) %>%
mutate(count = c("d","bcd","cd","bc","n/a","b","a"))
numbers88



min.max.WC <- ggplot(data = water.summary.new, aes(x = Species, y = WC, colour = Species, fill = Species)) +
  geom_point(size = 4) +
  geom_line(size = 3)+
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"))+    
theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Range of optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,2.5, by=0.25), expand= c(0,0))+
  coord_cartesian(ylim=c(0,2.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
 geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, position = position_dodge(0.9)) +
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  geom_text(data = water.summary.new, aes(x = Species, y = WC, label = count), vjust = -0.8, hjust = -0.7, size =3,  colour ="grey16")












































########### ################        ##############   ##########   #################       ################        ############################      ##############################
### no lobaria pulmonaria in data BELOW ####################################################
#############################################################################################################################
###### no lobaria pulmonaria - new sigificance letters ##########################
##############################################################################################################################################


##### NP at PAR SAT ################
numbers.no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(Max_NP_PARsat, na.rm = TRUE),
    NP = mean(Max_NP_PARsat)) %>% 
  mutate(count = c("b","a","b","b","b","b"))
numbers.no.lp


NP.at.PARSAT.no.lp <- ggplot(data = numbers.no.lp, aes(x = as.factor(Species), y = NP, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = NP - sd, ymax = NP + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +
  scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_markdown(size = 12)) +
  ylab("Maximum net photosynthesis <br>(nmol g<sup>-1  </sup> s<sup>-1</sup> )")+
  scale_y_continuous(breaks=seq(0,70, by=5), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,60)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
geom_text(data = numbers.no.lp, aes(x = Species, y = NP, label = count), vjust = -0.5, hjust = 12, size =3, colour ="grey16")



grid.arrange(NP.at.PARSAT.no.lp, DR.PARSat.no.lp, LCP.for.grid.no.lp, LSP.for.grid.no.lp, Chl.for.grid.no.lp, wc.optimal.no.lp, ncol = 2, nrow = 3, layout_matrix = cardinal.grid)




##### DR at PAR SAT ##################################
numbers12.nolp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(Max_DR_PARsat, na.rm = TRUE),
    drmean = mean(Max_DR_PARsat)) %>% 
  mutate(count = c("a","b", "ab","ab","a","a"))
numbers12.nolp



DR.PARSat.no.lp <- ggplot(data = numbers12.nolp, aes(x = as.factor(Species), y = drmean, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = drmean - sd, ymax = drmean + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values =c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  theme(legend.position = c(0.82, 0.86)) +
  theme(axis.title.y = element_markdown(size = 12)) +
  ylab("Maximum respiration <br>(nmol g<sup>-1 </sup> s<sup>-1</sup> )")+
  scale_y_reverse(expand= c(0,0)) +          
  coord_cartesian(ylim=c(0,-10))  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
geom_text(data = numbers12.nolp, aes(x = Species, y = drmean, label = count), nudge_y = 1, vjust = 3, hjust = 8, size =3,  colour ="grey16")

###################################################################################################




numbers3.no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(LCP, na.rm = TRUE),
    LCP = mean(LCP)) %>% 
  mutate(count = c("a","a", "a","a","a","a"))
numbers3.no.lp


LCP.for.grid.no.lp <- ggplot(data = numbers3.no.lp, aes(x = Species, y = LCP, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = LCP - sd, ymax = LCP + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_markdown(size = 12)) +
  ylab("Light compensation point <br>(&mu;mol photons m<sup>-2 </sup> s<sup>-1</sup> )")+ 
  scale_y_continuous(breaks=seq(0,200, by=50), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,200)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
  geom_text(data = numbers3.no.lp, aes(x = Species, y = LCP, label = count), nudge_y = -0.04, vjust = -0.6, hjust = -0.9, size =3,  colour ="grey16")

#### ########################### ###########################################



####################################################
####### LSP ####################################################################################

numbers4.no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(LSP, na.rm = TRUE),
    LSP = mean(LSP)) %>% 
  mutate(count = c("a","a","a","a","a","a"))
numbers4.no.lp



LSP.for.grid.no.lp <- ggplot(data = numbers4.no.lp, aes(x = Species, y = LSP, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = LSP - sd, ymax = LSP + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c( "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c( "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_markdown(size = 12)) +
  ylab("Light saturation point <br>(&mu;mol photons m<sup>-2 </sup> s<sup>-1</sup> )")+ 
  scale_y_continuous(breaks=seq(0,1400, by=200), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,1400)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  geom_text(data = numbers4.no.lp, aes(x = Species, y = LSP, label = count), nudge_y = -0.04, vjust = -0.4, hjust = -0.7, size =3,  colour ="grey16")

######################
###############################################################################





#################### chl content ############################

numbers5.no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(Chl, na.rm = TRUE),
    Chl = mean(Chl)) %>% 
  mutate(count = c("a","a", "a","a","a","a"))
numbers5.no.lp


Chl.for.grid.no.lp <- ggplot(data = numbers5.no.lp, aes(x = Species, y = Chl, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = Chl - sd, ymax = Chl + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica",  "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c( "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Total chlorophyll content <br>(&mu;g mg DW<sup> -2</sup> )")+ 
  scale_y_continuous(breaks=seq(0,10, by=2), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
  geom_text(data = numbers5.no.lp, aes(x = Species, y = Chl, label = count), nudge_y = -0.04, vjust = -0.5, hjust = -0.75, size =3, colour ="grey16")

#########################
###########################################################################################################################################




######## WC Range of optimal water contents ############################

numbers6no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC_Optimal_Range, na.rm = TRUE),
    WC= mean(WC_Optimal_Range)) %>% 
  mutate(count = c("ab","b", "ab","ab","ab","a"))
numbers6no.lp



wc.optimal.no.lp <- ggplot(data = numbers6no.lp, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c( "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Range of optimal thallus water content <br>(precipitation equivalent mm H<sub> 2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,1.2, by=0.2), expand= c(0,0))+
  coord_cartesian(ylim=c(0,1.2)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  theme(legend.position = c(0.25, 0.68)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  geom_text(data = numbers6no.lp, aes(x = Species, y = WC, label = count), vjust = -0.5, hjust = -0.7, size = 3, colour ="black")

#######################################################################################################################################




grid.arrange(plot, heights =c(5000,50)) 


theme(plot.margin=unit(c(.2,1,.1,1),"cm"))


cardinal.grid <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                       c(3,4),
                       c(5,6))



grid.arrange(NP.at.PARSAT.no.lp, DR.PARSat.no.lp, LCP.for.grid.no.lp, LSP.for.grid.no.lp, Chl.for.grid.no.lp, wc.optimal.no.lp, ncol = 2, nrow = 3, layout_matrix = cardinal.grid)

##################################################################################################################################################################################


numbers100no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(optimal_WC_percentage, na.rm = TRUE),
    WC= mean(optimal_WC_percentage)) %>% 
  mutate(count = c("ab","b", "n/a","ab","ab","b"))
numbers100no.lp


wc.percentage.no.lp <- ggplot(data = numbers100no.lp, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +          
  scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c( "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Range of optimal thallus water content <br>(precipitation equivalent mm H<sub> 2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0, 450, by=50), expand= c(0,0))+
  coord_cartesian(ylim=c(0, 450)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  theme(legend.position = c(0.25, 0.68)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  geom_text(data = numbers6no.lp, aes(x = Species, y = WC, label = count), vjust = -0.5, hjust = -0.7, size = 3, colour ="black")


wc.percentage.no.lp <- ggplot(data = numbers100no.lp, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, color = "black", position = position_dodge(0.9))  +  
           scale_color_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c( "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c( "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Range of optimal thallus water content <br>(precipitation equivalent mm H<sub> 2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0.250, by=20), expand= c(0,0))+
  coord_cartesian(ylim=c(0, 250)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) +
  theme(legend.position = c(0.25, 0.68)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,1), "cm")) +
  geom_text(data = numbers6no.lp, aes(x = Species, y = WC, label = count), vjust = -0.5, hjust = -0.7, size = 3, colour ="black")

























optimal_WC% 




###### WC Min #########################

numbers7.nolp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC_Min, na.rm = TRUE),
    WC= mean(WC_Min)) %>% 
  mutate(count = c("bc","c","bc","n/a","b","a"))
numbers7.nolp

WC_min.no.lp <- ggplot(data = numbers7.no.lp, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.2, color = "black", position = position_dodge(0.9))  +            
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),,labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 13,)) +
  ylab("Optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,1.5, by=0.2), expand= c(0,0))+
  coord_cartesian(ylim=c(0,1.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  geom_text(data = numbers7, aes(x = Species, y = WC, label = count), vjust = -0.8, hjust = -0.7, size =3,  colour ="black")


###################### WC Max ############################################################################
numbers8.no.lp <- Cardinal_points_TABLE %>% 
  group_by(Species) %>% 
  summarise (
    sd = sd(WC_Max, na.rm = TRUE),
    WC= mean(WC_Max)) %>% 
  mutate(count = c("bc","c","bc","n/a","b","a"))
numbers8.no.lp

WC_max.no.lp <- ggplot(data = numbers8.no.lp, aes(x = Species, y = WC, colour = factor(Species), fill = factor(Species))) + 
  geom_bar(stat="identity") +
  c
  scale_color_manual(values = cc("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"), labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica", name = "Species") +
  scale_fill_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"),labels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"), name = "Species") +  
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 13,)) +
  ylab("Optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,1.5, by=0.2), expand= c(0,0))+
  coord_cartesian(ylim=c(0,1.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  geom_text(data = numbers8, aes(x = Species, y = WC, label = count), vjust = -0.8, hjust = -0.7, size =3,  colour ="black")
################################################################################################















##################### water range of optimal - attempt to plot three reps (as in claudes example) #####################

### use this summary only if need the means ###
water.three.reps <- Cardinal_points_TABLE %>%
  group_by(Species) %>%
  summarise(
    sd = sd(WC_Optimal_Range, na.rm = TRUE),
    WC = mean(WC_Optimal_Range))
water.three.reps


Cardinal_points_TABLE$Species <- factor(Cardinal_points_TABLE$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Ricasolia virens", "Sticta sylvatica", "Hypotrachyna laevigata", "Pectenia atlantica"))) 


ggplot(data = Cardinal_points_TABLE, aes(x = Species, y = WC_Optimal_Range, colour = Species, fill = Species)) +
  geom_point(size = 2) +
 
  scale_color_manual(values = cc("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"))+    
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Range of optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,3.5, by=0.1), expand= c(0,0))+
  coord_cartesian(ylim=c(0,3.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
+
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, position = position_dodge(0.9)) +
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) 



water_content_min_and_max$Species <- factor(water_content_min_and_max$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata", "Pectenia atlantica"))
water_content_min_and_max$level <- factor(water_content_min_and_max$level, levels = c("Min","Max")) 



ggplot(data = water_content_min_and_max, aes(x = factor(Species), y = WC, group = interaction (Species, level), colour = Species, fill = Species)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("slategray", "darkorange3", "purple4", "forestgreen", "goldenrod4", "deeppink4", "darkcyan"))+    
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 12,)) +
  ylab("Range of optimal thallus water content <br>(precipitation equivalent mm H<sub>2</sub>O</sub>)")+
  scale_y_continuous(breaks=seq(0,2.5, by=0.25), expand= c(0,0))+
  coord_cartesian(ylim=c(0,2.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_errorbar(aes(ymin = WC - sd, ymax = WC + sd), width = 0.1, position = position_dodge(0.9)) +
  theme(plot.margin=unit(c(1,-0.5,0.25,1), "cm")) +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.key.size = unit(0.45, "cm")) 










