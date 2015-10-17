complete<-function(directory,id=1:332){
  
  times<-1
  
  for(i in id)
  {
   num<-sprintf("%03d",i)
   fpath<-paste(directory,"./",num,".csv",sep="")  
   tmp_res<-read.csv(fpath)
   t<-tmp_res[,2]
   flag<-complete.cases(tmp_res)
   nods_num<-length(t[flag]) 
   
   if(times==1){
     ret<-data.frame("id"=i,"nobs"=nods_num)
     times<-2
   }  
   else{
     ret<-rbind(ret,c(i,nods_num))
   }
   
  }
  
  ret
  
}