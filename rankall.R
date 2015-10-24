rankall<-function(outcome,num="best"){
  
  ot<-c("heart attack","pneumonia","heart failure")
  flg_o<-(outcome==ot)
  if(!any(flg_o)){
    stop("invalid outcome")
  }
  
  result<-read.csv("outcome-of-care-measures.csv",colClasses = "character",stringsAsFactors = FALSE)
  
  if(outcome=="heart attack"){
    result[,11]<-as.numeric(result[,11])
    res<-cbind(result[,11])
  }
  else if(outcome=="heart failure"){
    result[,17]<-as.numeric(result[,17])
    res<-cbind(result[,17])
  }
  else{
    result[,23]<-as.numeric(result[,23])
    res<-cbind(result[,23]) 
  }
  res<-cbind(result[,2],res)  #拼接成矩阵
  res<-cbind(result[,"State"],res)
  #res<-cbind(res,result$State)
  flag<-complete.cases(res)         #去掉NA数据的行
  res<-res[flag,]
  res<-as.data.frame(res,stringsAsFactors=FALSE)   #要转成data.frame才能划分
  res<-split(res,res[,1])
  res<-lapply(res,function(x) x[order(as.numeric(x[,3]),x[,2]),])  #对List中每个分组排序
  ret<-data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
  for(i in 1:54){

  tmp<-as.data.frame(res[[i]])
  
  if(is.numeric(num)){
  if(num>length(tmp[,1])){
    ret<-rbind(ret,c(NA,tmp[1,1]))
    next
  }
  ret<-rbind(ret,tmp[num,2:1])
 #   ret<-rbind(ret,tmp[num,])
  }
  else if(num=="best"){
    ret<-rbind(ret,tmp[1,2:1])
  }
  else if(num=="worst")
  {
    ret<-rbind(ret,tmp[length(tmp[,1]),2:1])
  }
  }
  colnames(ret)=c("hospital","state")
  ret
  
  
  
}