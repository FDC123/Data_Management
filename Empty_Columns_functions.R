#Single Column frequency function
mkcolfreq<-function(DT,BY){
  DT[,list(frequency=.N),by=BY][order(-frequency)][,percent:=100*frequency/sum(frequency)]
}

#Table frequency function: calls the first for every column in the table.
mktabfreq <- function(dt){
  nmes<-names(dt)
  freqtab<-rbindlist(lapply(nmes,function(n){
    freqcol<-mkcolfreq(dt,n)
    freqcol$colname<-names(freqcol)[1]
    setnames(freqcol,names(freqcol)[1],"colvalue")
    freqcol$colvalue<as.character(freqcol$colvalue)
    setcolorder(freqcol,c("colname","colvalue","frequency","percent"))
    freqcol
  }))
  freqtab
}

#function to compute the number and proportion of missing values for each data.table column
mkpctnull<-function(dt)
{
  dt<-data.frame(dt)
  nmes<-names(dt)
  ischar<-unlist(sapply(nmes,function(n) typeof(dt[,n]))=="character"
  )
  nullreport<-data.frame(
    colname=nmes,
    null=sapply(nmes,function(n)
      sum(is.na(dt[,n]))+ifelse(ischar[n],sum(nchar(trimws(dt[,n][!is.na(dt[,n])]))==0),0)
    ),
    nrows=sapply(nmes,function(n) length(dt[,n]))
  )
  nullreport$pctnotnull<-(nullreport$nrows-nullreport$null)/nullreport$nrows
  data.table(nullreport)
}