library(readxl)
library(tidyverse)


#########################rodal 1 y 2 general boxplots######################################333

df_r1 <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros")%>%
  group_by(plot,rodal) %>% 
  summarise(narb=n()*20,ab=sum(ab)*20,vol=sum(vol)*20)
  
  ab_ha<-ggplot(df_r1,aes(x=rodal,y=ab),colour=rodal)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Rodal",
       y=expression(paste("Área Basal (m "^2,"ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
    scale_y_continuous( breaks = seq(0,70,10),limits = c(0,70))+
  scale_x_discrete(labels=c("Rodal 1", "Rodal 2"))
ggsave('ab_ha.png', ab_ha , device= "png",height = 5,width = 5,units = "cm", dpi = 300)

#boxplot densidad

ggplot(df_r1,aes(x=rodal,y=narb),colour=rodal)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Rodal",
       y=expression(paste("Densidad (Árboles ","ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_y_continuous( breaks = seq(0,1800,200),limits = c(0,1800))+
  scale_x_discrete(labels=c("Rodal 1", "Rodal 2"))
ggsave('den_ha.png',den_ha , device= "png",height = 5,width = 5,units = "cm", dpi = 300)

##boxplots volumen

vol_ha<-ggplot(df_r1,aes(x=rodal,y=vol),colour=rodal)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Rodal",
       y=expression(paste("Volumen (m "^3,"ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_y_continuous( breaks = seq(0,700,100),limits = c(0,700))+
  scale_x_discrete(labels=c("Rodal 1", "Rodal 2"))
ggsave('vol_ha.png',vol_ha , device= "png",height = 5,width = 5,units = "cm", dpi = 300)
##########################---########################################
