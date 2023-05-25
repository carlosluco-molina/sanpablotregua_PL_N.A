library(tidyverse)
library(readxl)

df<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx")

################rodal 1
##resumen dasometrico rodal 1 por parcela
df_1 <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal1")%>% 
group_by(plot) %>% 
  summarise(narb=n()*20,ab=sum(ab)*20,vol=sum(vol)*20) 
clipr::write_clip(df_1,object_type = c("character") ) 
##resumen dasometrico rodal 1 
df_1_tot<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal1")%>% 
  group_by(plot) %>% 
  summarise(narb=n()*20,ab=sum(ab)*20,vol=sum(vol)*20) %>% 
  summarise(n_plot=n(),densidad_ha=mean(narb),sd_densidad=sd(narb),ab_ha=mean(ab),sd_ab=sd(ab),vol_ha=mean(vol),sd_vol=sd(vol))
  
##cargamos rodal 2
  
  df_r2 <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
    mutate(ab=pi/40000*dap^2) %>% 
    filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal2") %>% 
    group_by(plot) %>% 
    summarise(narb=n()*20,ab=sum(ab)*20,vol=sum(vol)*20)  

  ##totales rodal 2
  df_r2tot <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
    mutate(ab=pi/40000*dap^2) %>% 
    filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal2") %>% 
    group_by(plot) %>% 
    summarise(narb=n()*20,ab=sum(ab)*20,vol=sum(vol)*20) %>% 
    summarise(n_plot=n(),densidad_ha=mean(narb),sd_densidad=sd(narb),ab_ha=mean(ab),sd_ab=sd(ab),vol_ha=mean(vol),sd_vol=sd(vol))
  
  