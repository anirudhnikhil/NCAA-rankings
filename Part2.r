##This code involves using Colley's Bias Free Ranking Method to identify the effective number of wins in a given season. The function colley, created here, takes year as an input and lists the rankings of that year. 




load("Finaldf.rdata")
colley<-function(year){
  year_data<-Final_data[Final_data$season==year,]
  
  colley_mat=matrix(0,nrow=nrow(year_data),ncol=nrow(year_data))
  diag(colley_mat)<-(year_data$nwins+year_data$nloss+2)
  for (i in 1:(nrow(year_data))){
    for (j in year_data$opponents[[i]])
      colley_mat[i,j]=-1
  }
  print (colley_mat)
  b<-c()
  for (k in 1:nrow(year_data)){
    b[k]=(1+((year_data$nwins[k])-(year_data$nloss[k]))/2)
  }
  score<-solve(colley_mat,b)
  solution=data.frame("Team Name"=year_data$Teamname, "Score"=score)
  return(solution[order(-solution$Score),])
}
