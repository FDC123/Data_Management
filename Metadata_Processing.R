#setwd

#load required packages
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(data.table)
library(xlsx)

#######Processing datasets from DNAST and Cross info#######

#Call DNAST and crossinfo datasets(now known as df.s and df.c respectively), this example only works in my computer, set directory accordingly.
df.s <- read_excel("file.dnast.xls",skip=6)
df.c<- read_excel("file.crossinfo.xlsx")
#Set dataframe from DNAST and crossinfo (df.s and df.c) as data.table (to avoid troubles with large data)
df.s<-as.data.table(df.s)
df.c<-as.data.table(df.c)
#Get Single Column frequency function on df.s/c
freqsdf.s<-mktabfreq(df.s)
freqsdf.c<-mktabfreq(df.c)

#Get Number and proportion of missing values for each df.s/c column
nullrepdf.s<-mkpctnull(df.s)
nullrepdf.c<-mkpctnull(df.c)
#Set not null minimum (treshold of "passing")
PCTNOTNULL<-.05

#Subset "real columns", and change names to avoid spaces
include.s<-as.character(nullrepdf.s$colname[nullrepdf.s$pctnotnull>=PCTNOTNULL])
colnames(df.s)<-as.character(nullrepdf.s$colname)
df.s.rc<-df.s[,include.s,with=F]

include.c<-as.character(nullrepdf.c$colname[nullrepdf.c$pctnotnull>=PCTNOTNULL])
colnames(df.c)<-as.character(nullrepdf.c$colname)
df.c.rc<-df.c[,include.c,with=F]

#Optional: Remove NAs from df.s.rc
df.s.rc<-na.omit(df.s.rc)

#Save Dataframes in same directory for future references
write.xlsx(df.s.rc, "dnast_rc.xls",row.names = FALSE,showNA = FALSE)
write.xlsx(df.c.rc, "crossinfo_rc.xls",row.names = FALSE,showNA = FALSE)


###### Processing germplasm-sample template imput#####

#Call germplasm-sample_tamplate_input(*.gsti)
df.t<- read_excel("file.gsti.xlsx")
df.t<-as.data.table(df.t)
#Check for empty cells in df.t
nullrepdf.t<-mkpctnull(df.t)
#Rename colnames in df.t to avoid spaces
colnames(df.t)<-as.character(nullrepdf.t$colname)

#Attach suffixes to column names of dataframes of interest, to know origin once merged.

colnames(df.t) <- paste(colnames(df.t), "gsti", sep = ".")
colnames(df.s.rc) <- paste(colnames(df.s.rc), "dnast", sep = ".")
colnames(df.c.rc) <- paste(colnames(df.c.rc), "crossinfo", sep = ".")


####Generate Final Germplasm-Sample Template####
#Merge gs_template_input and dnast dataframe (x,y,by.x=common column in x,by.y=common column in y)
df.t.s<-merge(df.t,df.s.rc,by.x="dnasample_num.gsti",by.y="Studysampleid.dnast")

#In case no unique ids are present in either dataframe.
##Generate concatenated proxy_id (first check a repeatable identifier and an unrepeatable identifier shared by both datasets)
df.t$proxy_id<-paste(df.t$GID.gsti,df.t$Plant.Number.gsti,sep="-")
df.s.rc$proxy_id<-paste(df.s.rc$GID.dnast,df.s.rc$Plant.Number.dnast,sep="-")
df.t.s<-merge(df.t,df.s.rc,by="proxy_id")

#Merge df.t.s with df.c.rc
df.t.s.c<-merge(df.t.s,df.c.rc,by.x="GID.dnast",by.y="GID.crossinfo")
#Special case of df.t.s.c (when cross.info output GIDs have errors)
df.t.s.c<-merge(df.t.s,df.c.rc,by.x="GID.gsti",by.y="GID.query.crossinfo")

#test for repeated columns
df.t.s.c<-as.data.frame(df.t.s.c)
dist<-stringdist::stringdistmatrix(df.t.s.c)/max(stringdist::stringdistmatrix(df.t.s.c))
h<-hclust(dist,method="average")
h$labels<-colnames(df.t.s.c)
require(graphics)
plot(as.dendrogram(h),horiz=TRUE,asp=0.04)

#Order columns by simmilaryty
new.col.order<-h$order
df.t.s.c<-df.t.s.c[,new.col.order]
sorted.colnames.list<-colnames(df.t.s.c)
write.table(sorted.colnames.list, "scl.txt",row.names = FALSE,quote=FALSE,sep="\t")

#From scl.txt, choose which colums to keep and the ones to keep out
##Generate your list of patterns (colnames/common string) to be removed: divided by pipe, all betwen quotes ex:"pattern1|pattern2".
pattern<-c("pattern1|pattern2")
#Apply filter.rep.col function, specifing the pattern to be removed and dataframe
df.f<-filter.rep.col(rem=pattern,df=df.t.s.c)


#Write final template
write.xlsx(df.f, "file.gs_final_template.xlsx",row.names = FALSE,showNA = FALSE)
