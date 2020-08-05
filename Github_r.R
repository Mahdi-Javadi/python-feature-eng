library(ggplot2)
library(dplyr)
library(ggridges)
############################################## Admit Source #################################################################
patient_data= read.csv('data/plots_in_r.csv')
patient_data$Admitsource <- factor(patient_data$Admitsource,levels=c("Operating Room",
                                                         "Recovery Room",
                                                         "Chest Pain Center",
                                                         "Floor/Step-Down Unit (SDU)",
                                                         "Other ICU","Other Hospital",
                                                         "Direct Admit",
                                                         "Emergency Department"))
font_size = 20
title_size = 22
ggplot(patient_data, aes(x=Admitsource, y=apachescore, fill=unitdischargestatus)) +
  scale_fill_manual(values= c("#00BFC4","#F8766D"))+
  geom_boxplot(outlier.size =0.5 )+
  xlab("Admit source")+
  ylab("APACHE score")+
  guides(fill=guide_legend(title="ICU discharge status"))+
  theme(axis.text.x =element_text(size=font_size,angle = 45, hjust = 1),axis.title=element_text(size=title_size))+
  theme(axis.text.y = element_text(size = font_size))+
  theme(legend.text = element_text(colour="black", size = font_size))+
  theme(legend.title = element_text(colour="black", size=title_size))

############################################## Age groups #################################################################
patient_data$x_agegroup <- as.factor(patient_data$x_agegroup)

ggplot(patient_data, aes(x=x_agegroup, y=apachescore, fill=unitdischargestatus)) +
  scale_fill_manual(values= c("#00BFC4","#F8766D"))+
  geom_boxplot(outlier.size =0.5 )+
  xlab("Age group")+
  ylab("APACHE score")+
  guides(fill=guide_legend(title="ICU discharge status"))+
  theme(axis.text.x =element_text(size=font_size),axis.title=element_text(size=title_size))+
  theme(axis.text.y = element_text(size = font_size))+
  theme(legend.text = element_text(colour="black", size = font_size))
  theme(legend.title = element_text(colour="black", size=title_size))
############################################## Visit Number #################################################################
newdata <- subset(patient_data,Unitvisitnumber_cat=='1' | Unitvisitnumber_cat=='2 or more')

newdata$Unitvisitnumber_cat <- as.factor(newdata$Unitvisitnumber_cat)
ggplot(newdata, aes(y=factor(x_agegroup,levels=c("85+","75-84","65-74","55-64","45-54","35-44","25-34","15-24")),
                    x=apachescore,
                    fill=Unitvisitnumber_cat)) +
  geom_density_ridges(alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +  
  scale_x_continuous(expand = c(0, 0))+
  xlab("APACHE score")+
  ylab("Age groups")+
  theme(legend.position="right")+guides(fill=guide_legend(title="Number of visits"))+
  theme(axis.text.x =element_text(size=font_size),axis.title=element_text(size=title_size))+
  theme(axis.text.y = element_text(size = font_size))+
  theme(legend.text = element_text(colour="black", size = font_size))+
  theme(legend.title = element_text(colour="black", size=title_size))
