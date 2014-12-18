library(foreach)
library(doParallel)
c<-detectCores()
cl <- makeCluster(c)
registerDoParallel(cl)
getDoParWorkers()

# Read all the files from a quarter

#patient <- read.table("demo12q4.txt", sep = "$", header = T, fill = T, quote = "")
drug <- read.table("drug12q4.txt", sep = "$", header = T, fill = T, quote = "")
reaction <- read.table("reac12q4.txt", sep = "$", header = T, fill = T, quote = "")
outcomes <- read.table("outc12q4.txt", sep = "$", header = T, fill = T, quote = "")
indication <-read.table("indi12q4.txt",sep = "$", header = T, fill = T, quote = "")
#response <- read.table("rpsr12q4.txt",sep = "$", header = T, fill = T, quote = "")
#ther <-read.table("ther12q4.txt",sep = "$", header = T, fill = T, quote = "")

# Setting the brand names of drugs 
# Create a dataframe of same brand name drugs with Drugbank ID 
dbclass<-read.csv("dbatc.csv",header=TRUE,stringsAsFactors=FALSE)
dnames<-read.csv("brandnames.csv",header=TRUE,stringsAsFactors=FALSE)
dn<-gsub("\n","",dnames$Name)
dn<-gsub("^ ","",dn)
dn<-gsub(" $","",dn)
dname<-cbind(dnames[,1],as.data.frame(dn))
colnames(dname)<-c("dbid","Name")
bdname<-merge(dbclass,dname,by="dbid")

# Give a name of the drug and collect all the drugs of that class
# Collect names of all the drugs which have different name than the given name
bdname$Name<-as.character(bdname$Name)
head(bdname$Name)
df1 <- bdname[(grepl("Aspirin", bdname$Name, ignore.case = T)),]
count.dbid <- as.data.frame(table(as.character(df1$dbid)))
dbid<-count.dbid[order(count.dbid$Freq, decreasing = T),]
dbid.f<-as.character(dbid[1,1])
# get all the brand names for a given query drug
all.names<-as.character(bdname[bdname$dbid==dbid.f,]$Name)

# all the atc codes
atc<-unique(as.character(bdname[bdname$dbid==dbid.f,]$atc_code))

# You can find individual drugs and their reported adverse by specifying there names below

# There are commonly many names for a drug, see below where I put three grepl statements
# with text like, "put drug brand name here", you can replace this with drug brand names
# you can add or remote grepl statements depending on the number of brand names you want to look over.
# For all the names for a given query collect the data and bind to a dataframe

d.data<-data.frame()
d.data <- foreach(i=1:length(all.names), .combine=rbind) %dopar% {
  drug[(grepl(all.names[i], drug$drugname, ignore.case = T)) & drug$drug_seq == 1, ] }

df.d <- merge(d.data, reaction, by = "primaryid") # let's merge the drug file with reactions
df.d <- merge(df.d, outcomes, by = "primaryid") # we'll bring in outcomes, too
#df <- merge(df, patient, by = "primaryid")

# Checking on Adverse events for one drug only
d.adr <- as.data.frame(table(df.d$pt, df.d$outc_code)) # count the instances of reactions and their outcomes
names(d.adr) <- c("reaction", "outcome", "count")
d.adr <- d.adr[order(d.adr$count, decreasing = T), ]
n1.df<-d.adr[d.adr$count > 0,]
head(n1.df, 20)

## For the ATC class of drugs total adverse interactions calculations length(d.names)
## Need for calculation of PRR and ROR
all.data<-data.frame()
adrs<- foreach (i = 1:length(atc),.combine=rbind) %do% {
        s<-bdname[bdname$atc_code=="B",]
        print (i)
        d.names<-as.character(s$Name)
        all.data<-  foreach (j = 1:length(d.names),.combine=rbind) %dopar% { 
                    drug[(grepl(d.names[j], drug$drugname, ignore.case = T)) & drug$drug_seq == 1, ] }

        df <- merge(all.data, reaction, by = "primaryid") # let's merge the drug file with reactions
        df <- merge(df, outcomes, by = "primaryid")
        all.adr <- as.data.frame(table(df$pt, df$outc_code)) # count the instances of reactions and their outcomes
        names(all.adr) <- c("reaction", "outcome", "count")
        all.adr <- all.adr[order(all.adr$count, decreasing = T), ]
        n.adr<-all.adr[all.adr$count > 0,]
        n.adr$atc_code <- atc[i]
        return(n.adr) 
}    

## n1.df query drug and its outcome and reactions
## adrs all the drugs for the drug atc class. A drug can have multiple ATC classes .
## Explore the events based on each drug class.
adrs.b<-adrs[adrs$atc_code=="B",] 
adrs.n<-adrs[adrs$atc_code=="N",]
adrs.a<-adrs[adrs$atc_code=="A",]

adrs.b$drug<-"Blood and blood forming organs"
adrs.n$drug<- "Nervous system"
adrs.a$drug<- "Alimentary tract and metabolism"
#adrs<-rbind(adrs.b[1:50,],adrs.n[1:50,],adrs.a[1:50,])

## Plot only the ATC code B with Aspirin
adrs.a$atc_code<-NULL
## Plot Pyramid charts
adrs.a$count= -1 * adrs.a$count

## combine two dataframe and look for the top reactions

new.df<-rbind(adrs.b[1:55,],n1.df[1:55,])
new.df<-new.df[order(new.df$count, decreasing = T), ]
new.df$count<-as.numeric(new.df$count)
new.df$drug<-as.factor(new.df$drug)

p1<-pyramidPlot(new.df,colors=c("blue","red"))


## Checking on indications
colnames(indication)[3]<-"drug_seq"
df<-merge(df,indication,by=c("primaryid","caseid","drug_seq"))
df2 <- as.data.frame(table(df$indi_pt))
names(df2) <- c("indication", "count")
df2 <- df2[order(df2$count, decreasing = T), ]
n.df<-df2[df2$count > 0,]
head(n.df)
