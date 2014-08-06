library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(circlize)

source("03circle.R")

dtk <- read.csv("data/Balsai2x_humanitariniai.csv")

dtk[,-1:-3] <- sapply(dtk[,-1:-3],function(x)as.numeric(as.character(x)))
dtk <- dtk[,-1]


filter <- dplyr:::filter
mtk <- gather(dtk,Vieta,Balsai,UKI:Viso) %>% filter(Vieta!="Viso") %>% na.omit

mtk %>% group_by(Kandidatas) %>% summarise(Viso1=sum(Balsai)) %>% arrange(-Viso1) %>% merge(dtk[,c("Kandidatas","Viso")]) %>%
    mutate(Check=Viso1-Viso) 

mtk$Vieta <- as.character(mtk$Vieta)

##1. Kiek balsavo kiekvienam fakultete
mtk %>% group_by(Vieta) %>% summarise(Viso=sum(Balsai)/6) %>% arrange(-Viso)

##2. Kas laimejo
fwin <- mtk %>% group_by(Kandidatas) %>% summarise(Viso1=sum(Balsai)) %>% arrange(-Viso1) %>% use_series("Kandidatas") %>% magrittr:::extract(1:7) %>% as.character
fwin_pdal<- dtk %>% filter(Kandidatas %in% fwin) %>% use_series("Padalinys") %>% as.character

##3. Laimėjusių balsų pasiskirstymai
qplot(x=Vieta,y=Balsai,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)
qplot(x=Padalinys,y=Balsai,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##4. Kiek surinko procentų fakultete
mtk <- mtk %>% group_by(Vieta) %>% mutate(Proc=Balsai/sum(Balsai)*7*100) 
qplot(x=Vieta,y=Proc,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##5. Kvorumas
#fvis <- read.csv("data/Fiz_balsavo_viso.csv")
#mtk <-  merge(mtk,fvis) %>% mutate(Proc1=Balsai/Balsavo*100)
#qplot(x=Vieta,y=Proc1,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##6. Kas surinko virš 50% padalinyje
#mtk %>% filter(Proc1>=50) %>% group_by(Kandidatas) %>% summarize(Daugumos=n()) %>% arrange(-Daugumos)

##7. Kuris padalinys balsavo už kurį:
finst <- mtk %>% group_by(Padalinys,Vieta) %>% summarize(Viso=sum(Balsai)) 
finst2 <- mtk %>% filter(Kandidatas %in% fwin)%>% group_by(Padalinys,Vieta) %>% summarize(Viso=sum(Balsai)) 
finst3 <- mtk %>% filter(!(Kandidatas %in% fwin))%>% group_by(Padalinys,Vieta) %>% summarize(Viso=sum(Balsai)) 

##8. Siuntė (ty atidavė) balsų ir gavo balsų
fsent <- finst %>% group_by(Vieta) %>% summarize(Sent=sum(Viso)) %>% arrange(-Sent) %>% set_names(c("Padalinys","Sent"))
frec <- finst %>% group_by(Padalinys) %>% summarize(Received=sum(Viso)) %>% arrange(-Received)

fsr <- merge(fsent,frec,all=TRUE)
fsr$Sent[is.na(fsr$Sent)] <- 0
fsr$Received[is.na(fsr$Received)] <- 0
fsr <- fsr %>% mutate(Padalinys=as.character(Padalinys)) %>% arrange(Padalinys)

fsr <- fsr %>% mutate(All=Sent+Received,col=brewer.pal(10,"Paired"))

##9.
library(circlize)
do.circle(finst)


aa <- finst %>% filter(Vieta=="MF")
aa$end <- cumsum(aa$Viso)
aa$start <- lag(aa$end,default=0)+1

