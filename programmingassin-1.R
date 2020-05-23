pollutantmean <- function(directory, pollutant, id = 1:332) {

    pathh <- paste(getwd(),"/",directory,"/",sep="")
    file_list <- list.files(pathh)
    data <-NA
    for (i in id) {
        
        fl<- paste(pathh,file_list[i],sep="")
        file_data <- read.csv(fl)
        data <- rbind(data,file_data)
    }
    mean(data[[pollutant]],na.rm = TRUE)
}


complete <-function(directory,id=1:332)
{
    path=paste(getwd(),"/",directory,"/",sep="")
    flst<-list.files(path)
    name<-c()
    ct<-c()
    for(i in id)
    {
        fl<-paste(path,flst[i],sep="")
        data<-read.csv(fl)
        name<-c(name,i)
        ct<-c(ct,sum(complete.cases(data)))
    }
    data.frame(id=name,nobs=ct)
    
}


corr <- function(directory,threshold=0)
{

    df<-complete(directory)
    data<-subset(df,df$nobs>threshold)
    path<-paste(getwd(),"/",directory,"/",sep="")
    flst<-list.files(path)
    corrn<-vector()
    for(i in data$id)
    {
       fl<-paste(path,flst[i],sep="")
       dta<-read.csv(fl)
       dta<-subset(dta,complete.cases(dta))
       corrn<-c(corrn,cor(dta$nitrate,dta$sulfate))
       
    }
    corrn
}