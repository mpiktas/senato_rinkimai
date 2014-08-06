do.circle <- function(finst) {
    extract <- magrittr:::extract
    fsent <- finst %>% group_by(Vieta) %>% summarize(Sent=sum(Viso)) %>% arrange(-Sent) %>% set_names(c("Padalinys","Sent"))
    frec <- finst %>% group_by(Padalinys) %>% summarize(Received=sum(Viso)) %>% arrange(-Received)

    fsr <- merge(fsent,frec,all=TRUE)
    fsr$Sent[is.na(fsr$Sent)] <- 0
    fsr$Received[is.na(fsr$Received)] <- 0
    fsr <- fsr %>% mutate(Padalinys=as.character(Padalinys)) %>% arrange(Padalinys)
    
    fsr <- fsr %>% mutate(All=Sent+Received,col=brewer.pal(nrow(fsr),"Paired"))
    
    par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
    circos.par("default.track.height" = 0.1)
    circos.initialize(factors = fsr$Padalinys, xlim = cbind(0,fsr$All))
    
    circos.trackPlotRegion(factors = fsr$Padalinys, ylim=c(0,1),
                           panel.fun = function(x, y) {
                           sector.index = get.cell.meta.data("sector.index")
                           dd <- fsr %>% filter(Padalinys==sector.index)
                           circos.rect(0, 0, dd$Sent, 1,
                                       col = dd$col, border = "black", lty = par("lty"), lwd = par("lwd"))
                           col2 <- ifelse(sector.index %in% fwin_pdal,"#FFFF99","white")
                           circos.rect(dd$Sent+1, 0, dd$All, 1,
                                       col = col2, border = "black", lty = par("lty"), lwd = par("lwd"))
                           circos.text(x=dd$All/2,y=0.5,label=sector.index)                           
                       })
    

    ff <- finst %>% group_by(Padalinys) %>% mutate(Received.end=cumsum(Viso),Received.start=lag(Received.end,default=0))
    ff <- ff %>% group_by(Vieta) %>% mutate(Sent.end=cumsum(Viso),Sent.start=lag(Sent.end,default=0))
    
    places_from <- ff %>% use_series("Padalinys") %>% as.character %>% unique

    for(pfrom in places_from) {
        places_to <- ff%>% filter(Padalinys==pfrom) %>% use_series(Vieta)
        for (pto in places_to) {
            to <- ff%>% filter(Padalinys==pfrom & Vieta==pto)%>%extract(,c("Received.start","Received.end")) %>% as.numeric + fsr%>% filter(Padalinys==pfrom) %>% select(Sent) %>% as.numeric 
            from <- ff%>% filter(Padalinys==pfrom & Vieta==pto)%>%extract(,c("Sent.start","Sent.end")) %>% as.numeric
            lcol <- fsr%>% filter(Padalinys==pto) %>% use_series("col")
            circos.link(pto,from,pfrom,to,col=lcol)
        }
    }
    list(fsr=fsr,ff=ff)
}

