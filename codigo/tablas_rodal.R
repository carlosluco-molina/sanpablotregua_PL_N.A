library(tidyverse)
library(readxl)

df_r2<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal2")%>% 
drop_na(dap) %>% filter(dap>=5) %>%
mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  group_by(rodal,spp,plot,clases) %>% 
  summarise(densidad=n()*(10000/500),ab=sum(ab)*(10000/500),vol=sum(vol)*(10000/500)) %>% 
  group_by(rodal,clases,spp) %>% 
  summarise(densidad_ha=sum(densidad)/6,vol_ha=sum(vol)/6)

write_csv2(df_r2,"clases_diame_rodal2")

df_r1<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/pl_volumen.xlsx") %>% 
  mutate(ab=pi/40000*dap^2) %>% 
  filter(obs!="mp" & obs!="MP" & obs!="otros",rodal=="Rodal1")%>% 
  drop_na(dap) %>% filter(dap>=5) %>%
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  group_by(rodal,spp,plot,clases) %>% 
  summarise(densidad=n()*(10000/500),ab=sum(ab)*(10000/500),vol=sum(vol)*(10000/500)) %>% 
  group_by(rodal,clases,spp) %>% 
  summarise(densidad_ha=sum(densidad)/3,vol_ha=sum(vol)/3)

write_csv2(df_r1,"clases_diame_rodal1")
