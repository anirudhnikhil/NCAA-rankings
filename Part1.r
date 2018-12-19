##This code scrapes yearly fixture wise data from James Howell's college Football Scores database (http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/)
##and converts it into a dataframe listing the number of wins and losses along with the opponents listed in list form.




rm(list=ls())

df_year<-data.frame()
main_data<-data.frame()
Final_data<-data.frame()


for (i_year in 1960:2010){
      
      
      
      
      u<-paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",i_year,"gms.txt", sep="")
      main_data<-read.fwf(file=url(as.character(u)),col.names = c("Date","Home","Home Score","Away","Away Score","Venue"), widths=c(10,29,2,30,2,30))
      main_data$Home<-as.character(trimws(main_data$Home,which="both"))
      main_data$Away<-as.character(trimws(main_data$Away,which="both"))
      
      hometeam<-table(unlist(main_data$Home))
      awayteam<-table(unlist(main_data$Away))
      
      Final<-rbind(as.data.frame(hometeam),as.data.frame(awayteam))
      names(Final)<-c("Teams","games")
      Final<-aggregate(Final$games, by=list(Category=Final$Teams),FUN=sum)
      names(Final)<-c("Teams","games")
      Final<-Final[Final$games>5,]
      uniq_vect<-as.vector(unique(Final$Teams))
      new_env<-new.env()
      
      for (j in 1:length(uniq_vect)){
        new_env[[as.character(uniq_vect[j])]]=j
      }
      g=numeric(length(uniq_vect))
      
      for (k in 1:length(Final$Teams)){
        id<-new_env[[as.character(Final$Teams[k])]]
        g[id]=g[id]+Final$games[k]
      }
      main_data<-main_data[!(main_data$Home.Score==main_data$Away.Score),]
      audhak<-data.frame(Team1=main_data$Home,Team1.Score=main_data$Home.Score,Team2=main_data$Away,Team2.Score=main_data$Away.Score)
      asjkcd<-data.frame(Team1=main_data$Away,Team1.Score=main_data$Away.Score,Team2=main_data$Home,Team2.Score=main_data$Home.Score)
      i<-sapply(audhak,is.factor)
      audhak[i]<-lapply(audhak[i],as.character)
      i<-sapply(asjkcd,is.factor)
      asjkcd[i]<-lapply(asjkcd[i],as.character)
      askda<-rbind(audhak,asjkcd)
      Final_data_temp<-data.frame(Teamname=uniq_vect)
      Final_data_temp$season<-i_year
      Final_data_temp$nwins<-NA
      Final_data_temp$nloss<-NA
      Final_data_temp$opponents<-list(length=0)
    
      i<-sapply(Final_data_temp,is.factor)
      Final_data_temp[i]<-lapply(Final_data_temp[i],as.character)
      
      
      for (i in 1:nrow(Final_data_temp)){
        opplen<-c(which(uniq_vect %in% askda$Team2[askda$Team1==Final_data_temp$Teamname[i]]))
         print(i_year)
        if(length(opplen)>0){
        Final_data_temp$opponents[[i]]<-opplen
        }
        }
      
      
      for (i in 1:nrow(askda)){
        if (askda$Team1.Score[i]>askda$Team2.Score[i]){
          askda$winner[i]<-askda$Team1[i]
          askda$loser[i]<-askda$Team2[i]}
        else{
          askda$winner[i]<-askda$Team2[i]
          askda$loser[i]<-askda$Team1[i]
        }
      }
      for (i in 1:nrow(Final_data_temp)){
        Final_data_temp$nwins[i]<-(sum(askda$winner==Final_data_temp$Teamname[i]))/2
        Final_data_temp$nloss[i]<-(sum(askda$loser==Final_data_temp$Teamname[i]))/2
        
      }
      Final_data<-rbind(Final_data_temp,Final_data)
      }
      
