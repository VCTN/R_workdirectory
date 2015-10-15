pollutantmean<-function(directory,pollutant,id=1:332)
{
  ret<-0
  result<-c()
  for(t in id){
  num<-sprintf("%03d",t)
  paths<-paste(directory,"/",num,".csv",sep="")
  res<-read.csv(paths)
  if(pollutant=="sulfate")
  {
  tmp<-res[,2]
    
  }else{
    tmp<-res[,3]
  }
  flag<-is.na(tmp)
  
  tmp_arr=tmp[!flag]
  
  result<-c(result,tmp_arr)

  }
  
  
  ret<-mean(result)
  as.numeric(sprintf("%.3f",ret))
  
}