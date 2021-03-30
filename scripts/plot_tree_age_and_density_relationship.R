plot_tree_age_and_density_relationship <- function(myDF) {
    
    ### patcharea in unit of m2
    
    ### only look at trees
    subDF <- myDF[myDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9),]
 
    ### calculate sum of wood
    subDF$StemC <- subDF$SapC + subDF$HeartC
    
    
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    
    ### 
    p1 <- ggplot() +
        #geom_point(subDF, mapping = aes(Idens, Iage, col=StemC))+
        geom_hex(subDF, mapping = aes(Idens, Iage), bins=10)+
        geom_smooth(subDF, mapping=aes(Idens, Iage), 
                    method = "nls",formula = 'y ~ a*x^b', 
                    start=list(a=.1,b=-.5),
                    col="red",
                    span = 0.3)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Tree density")+
        ylab("Tree age")
    
     plot(p1)
    
    
     
     ### end
}
