tk <- read.csv("data/Balsaix_Tikslieji.csv")
tk1 <- tk[[1]] %>% as.character
tk1 <- tk1[tk1!=""]

dl <- c(grep("^[0-9]+[.]$",tk1),length(tk1)+1)
ndl <- length(dl)-1
ii <- as.list(data.frame(matrix(dl[c(1,rep(2:ndl,each=2),ndl+1)],nrow=2)))

dtk <- ii %>% lapply(function(x)tk1[x[1]:(x[2]-1)]) %>% Reduce("rbind",.) %>% set_colnames(c("Nr.",tk1[1:13])) %>% data.frame(stringsAsFactors=FALSE)


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
fwin <- mtk %>% group_by(Kandidatas) %>% summarise(Viso1=sum(Balsai)) %>% arrange(-Viso1) %>% use_series(Kandidatas) %>% extract(1:6) %>% as.character
fwin_pdal<- dtk %>% filter(Kandidatas %in% fwin) %>% use_series("Padalinys") %>% as.character


##3. Laimėjusių balsų pasiskirstymai
qplot(x=Vieta,y=Balsai,data=filter(mtk,Kandidatas %in% fwin),geom="bar",stat="identity") + facet_wrap(~Kandidatas)

##4. Kiek surinko procentų fakultete
mtk <- mtk %>% group_by(Vieta) %>% mutate(Proc=Balsai/sum(Balsai)*100) 
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
finst <- finst3

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

