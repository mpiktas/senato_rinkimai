library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(circlize)

source("03circle.R")

dtk <- read.csv("data/Balsai2x_tikslieji.csv")

dtk[,-1:-3] <- sapply(dtk[,-1:-3],function(x)as.numeric(as.character(x)))
dtk <- dtk[,-1]


filter <- dplyr:::filter
mtk <- gather(dtk,Vieta,Balsai,GMF:Viso) %>% filter(Vieta!="Viso") %>% na.omit

mtk %>% group_by(Kandidatas) %>% summarise(Viso1=sum(Balsai)) %>% arrange(-Viso1) %>% merge(dtk[,c("Kandidatas","Viso")]) %>%
    mutate(Check=Viso1-Viso) 

mtk$Vieta <- as.character(mtk$Vieta)

##1. Kiek balsavo kiekvienam fakultete
mtk %>% group_by(Vieta) %>% summarise(Viso=sum(Balsai)/6) %>% arrange(-Viso)

##2. Kas laimejo
fwin <- mtk %>% group_by(Kandidatas) %>% summarise(Viso1=sum(Balsai)) %>% arrange(-Viso1) %>% use_series("Kandidatas") %>% magrittr:::extract(1:6) %>% as.character
fwin_pdal<- dtk %>% filter(Kandidatas %in% fwin) %>% use_series("Padalinys") %>% as.character


##3. Laimėjusių balsų pasiskirstymai
qplot(x=Vieta,y=Balsai,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##4. Kiek surinko procentų fakultete
mtk <- mtk %>% group_by(Vieta) %>% mutate(Proc=Balsai/sum(Balsai)*7*100) 
qplot(x=Vieta,y=Proc,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##5. Kvorumas
fvis <- read.csv("data/Fiz_balsavo_viso.csv")
mtk <-  merge(mtk,fvis) %>% mutate(Proc1=Balsai/Balsavo*100)
qplot(x=Vieta,y=Proc1,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##6. Kas surinko virš 50% padalinyje
mtk %>% filter(Proc1>=50) %>% group_by(Kandidatas) %>% summarize(Daugumos=n()) %>% arrange(-Daugumos)

##7. Kuris padalinys balsavo už kurį:
finst <- mtk %>% group_by(Padalinys,Vieta) %>% summarize(Viso=sum(Balsai)) 
finst2 <- mtk %>% filter(Kandidatas %in% fwin)%>% group_by(Padalinys,Vieta) %>% summarize(Viso=sum(Balsai)) 
finst3 <- mtk %>% filter(!(Kandidatas %in% fwin))%>% group_by(Padalinys,Vieta) %>% summarize(Viso=sum(Balsai))

##8.
library(circlize)
#Visi balsai
do.circle(finst)
#Balsai už laimėjusius
do.circle(finst2)
#Pralaimėjusių balsai
do.circle(finst3)

