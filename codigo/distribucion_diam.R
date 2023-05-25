library(tidyverse)
library(readxl)

df<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_distr_diam.xlsx")%>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal2")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  group_by(spp,plot,clases) %>% 
  summarise(densidad=n()*(10000/500),ab=sum(ab)*(10000/500),vol=sum(vol)*(10000/500)) %>% 
  group_by(clases,spp) %>% 
  summarise(densidad_ha=sum(densidad)/6,vol_ha=sum(vol)/6)

##crear los niveles de la variable spp
df$spp<-factor(df$spp,levels = c("Raulí","Trevo","Notro","Otras"))

rodal2<-ggplot(df,aes(x=clases,y=densidad_ha,pattern=spp,fill=spp))+
  geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,55,5),limits = c(0,55))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,240,25),limits = c(0,240))+
  scale_fill_manual(values = c("#000000","#666666","#CCCCCC","#FFFFFF"))+
  labs(x="Clase diamétrica (cm)",
       y=expression(paste("Densidad (árb ","ha"^-1,")")))+
  guides(fill=guide_legend(title="Especie")) +
  theme_bw(base_line_size=.2)+
  theme(legend.position = c(0.85,0.6),
        legend.margin = margin(c(5, 5, 5, 0)),
        axis.text=element_text(size=6,colour="black",family = "times"),
        axis.title.x = element_text(size=7,colour="black",family = "times"),
        axis.title.y=element_text(size=7,colour="black",family = "times"),
        legend.text = element_text(size = 6,colour="black",family="times"),
        legend.title = element_text(size = 6,colour="black",family="times"),
        legend.key.size = unit(0.2,"cm"),
        axis.line = element_line(size = .1,colour="black"),
        axis.ticks = element_line(size=.3,colour="black"))
ggsave('rodal2.jpg',rodal2 , device= "jpg",height = 4.5,width = 9,units = "cm", dpi = 300)
#######################rodal1#################

df_1<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_distr_diam.xlsx")%>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal1")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  group_by(spp,plot,clases) %>% 
  summarise(densidad=n()*(10000/500),ab=sum(ab)*(10000/500),vol=sum(vol)*(10000/500)) %>% 
  group_by(clases,spp) %>% 
  summarise(densidad_ha=sum(densidad)/3,vol_ha=sum(vol)/3)

df_1$spp <-factor(df_1$spp,levels = c("Raulí","Roble","Trevo","Otras"))

rodal1<-ggplot(df_1,aes(x=clases,y=densidad_ha,pattern=spp,fill=spp))+
  geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,70,5),limits = c(0,70))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,300,25),limits = c(0,300))+
  scale_fill_manual(values = c("#000000","#666666","#CCCCCC","#FFFFFF"))+
  labs(x="Clase diamétrica (cm)",
       y=expression(paste("Densidad (árb ","ha"^-1,")")))+
  guides(fill=guide_legend(title="Especie")) +
  theme_bw(base_line_size=.2)+
  theme(legend.position = c(0.85,0.6),
        legend.margin = margin(c(5, 5, 5, 0)),
        axis.text=element_text(size=6,colour="black",family = "times"),
        axis.title.x = element_text(size=7,colour="black",family = "times"),
        axis.title.y=element_text(size=7,colour="black",family = "times"),
        legend.text = element_text(size = 6,colour="black",family="times"),
        legend.title = element_text(size = 6,colour="black",family="times"),
        legend.key.size = unit(0.2,"cm"),
        axis.line = element_line(size = .1,colour="black"),
        axis.ticks = element_line(size=.3,colour="black"))

ggsave('rodal1.jpg',rodal1 , device= "jpg",height = 4.5,width = 9,units = "cm", dpi = 300)
