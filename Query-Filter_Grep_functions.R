#Filters out unwanted columns supplying a list of names
filter.rep.col<-function(df,rem){
  for(i in rem)
    filter.rep.col.df<-df[ -grep(get("i"), names(df))]
  return(filter.rep.col.df)
}


#Function to query tables in a SQL-like sintax
consult.df<-function(df,dfcol,q){
  for(i in q)
    query.result<-df[ grep(i, dfcol), ]
  return(query.result)
}
