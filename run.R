# Compare two query drugs
d1<-get.Names("Lipitor")
d2<-get.Names("Crestor")
d1se<-get.SideEffects(d1,"Lipitor")
d2se<-get.SideEffects(d2,"Crestor")
get.Vis(d1se,d2se,75,col=c("blue","red"),out="DS")
