# Initial adverse effect r code for data mining adverse events data 
patient <- read.table("demo12q4.txt", sep = "$", header = T, fill = T, quote = "")
drug <- read.table("drug12q4.txt", sep = "$", header = T, fill = T, quote = "")
reaction <- read.table("reac12q4.txt", sep = "$", header = T, fill = T, quote = "")
outcomes <- read.table("outc12q4.txt", sep = "$", header = T, fill = T, quote = "")

# You can find individual drugs and their reported adverse by specifying there names below

# There are commonly many names for a drug, see below where I put three grepl statements
# with text like, "put drug brand name here", you can replace this with drug brand names
# you can add or remote grepl statements depending on the number of brand names you want to look over.
df <- drug[(grepl("Aspirin", drug$drugname, ignore.case = T)) & drug$drug_seq == 1, ]

df <- drug[(grepl("Aspirin", drug$drugname, ignore.case = T) | # drug is likely to be entered as many different brand names, use this to capture them individually
              grepl("Humira", drug$drugname, ignore.case = T) | # enter drug names here, add or remove grepl() as needed with "or" statements \
              grepl("Gleevec", drug$drugname, ignore.case = T)) & drug$drug_seq == 1, ] # drug seq 1 == suspect drug of many possible that patient is taking

df <- merge(df, reaction, by = "primaryid") # let's merge the drug file with reactions
df <- merge(df, outcomes, by = "primaryid") # we'll bring in outcomes, too
#df <- merge(df, patient, by = "primaryid")
df2 <- as.data.frame(table(df$pt, df$outc_code)) # count the instances of reactions and their outcomes
names(df2) <- c("reaction", "outcome", "count")
df2 <- df2[order(df2$count, decreasing = T), ]
head(df2, 20)
