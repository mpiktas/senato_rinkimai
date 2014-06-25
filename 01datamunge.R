library(dplyr)
library(magrittr)

rr1<-readLines("data/rinkejai.txt")
rr1 <- rr1[rr1!="" & rr1!=" "]

hd<-c(grep("^\t",rr1,invert=TRUE),length(rr1))

nhd <- length(hd)-1
ii <- as.list(data.frame(matrix(hd[c(1,rep(2:nhd,each=2),nhd+1)],nrow=2)))

rr2 <- ii %>% lapply(function(x)data.frame(Padalinys=rr1[x[1]],Rinkėjai=rr1[(x[1]+1):(x[2]-1)])) %>%
    Reduce("rbind",.) %>%
    mutate(Rinkėjai=gsub("\t","",Rinkėjai)) %>% filter(!grepl("Rinkėjo",Rinkėjai))

fiz <- data.frame(Padalinys=readLines("data/fiz.txt"),Fiziniai=1)
hum <- data.frame(Padalinys=readLines("data/hum.txt"),Fiziniai=0)

sk <- rbind(fiz,hum)

rv <- rr2 %>% group_by(Padalinys) %>% summarise(Viso=n()) %>% merge(sk)
