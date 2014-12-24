<<<<<<< Updated upstream
# Compare two query drugs
d1<-get.Names("Aspirin")
d2<-get.Names("Atorvastatin")
d1se<-get.SideEffects(d1,"Aspirin")
d2se<-get.SideEffects(d2,"Atorvastatin")
get.Vis(d1se,d2se,top=50,color=("blue","red"))
=======
d1<-get.Names("Lipitor")
d2<-get.Names("Crestor")
d1se<-get.SideEffects(d1,"Lipitor")
d2se<-get.SideEffects(d2,"Crestor")
get.Vis(d1se,d2se,50,col=c("blue","red"))
>>>>>>> Stashed changes
