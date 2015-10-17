corr<-function(directory,threshold=0)
{
  ret<-c()
  for(i in 1:332)
  {
    tmp_res<-complete(directory,i)
    
    if(tmp_res[1,2]>threshold)
    {
     num_path<-sprintf("%03d",i)
     f_path<-paste(directory,"/",num_path,".csv",sep="")
     res<-read.csv(f_path)
     flag<-complete.cases(res)
     ret<-c(ret,cor(res[flag,2],res[flag,3]))
      
    }
    
    
  }
  
  ret
}