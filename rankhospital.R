rankhospital<-function(state,outcome,num="best"){
  
  
  ot<-c("heart attack","pneumonia","heart failure")
  flg_o<-(outcome==ot)
  if(!any(flg_o)){
    stop("invalid outcome")
  }
  
  result<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  result[,11]<-as.numeric(result[,11])
  idx<-duplicated(result$State)         #提取所有的州名
  states<-result$State[!idx]
  flg<-(state==states)
  if(!any(flg)){
    stop("invalid state")
  }
  
  c_outcome<-outcome

  sfg<-(result$State==state)
  if(outcome=="heart attack"){
    result[sfg,11]<-as.numeric(result[sfg,11])
    res<-cbind(result[sfg,11])
  }
  else if(outcome=="heart failure"){
    result[sfg,17]<-as.numeric(result[sfg,17])
    res<-cbind(as.numeric(result[sfg,17]))
  }
  else{
    result[sfg,23]<-as.numeric(result[sfg,23])
    res<-cbind(result[sfg,23]) 
  }
  res<-cbind(result[sfg,2],res)  #拼接成矩阵
  #res<-cbind(res,result$State)
  flag<-complete.cases(res)         #去掉NA数据的行
  res<-res[flag,]
  

  res[,2]<-as.numeric(res[,2])
  data_order<-res[order(as.numeric(res[,2]),res[,1]),]  #对数字排序之前，记得准换为numeric类型不然结果不对
  #   ()
  #   


  if(num=="best")
  {
    return(data_order[1,1])
  }
  else if(num=="worst"){
    return(data_order[length(data_order[,1]),1])
  }
  if(num<=length(data_order[,1]))
  {
  return(data_order[as.numeric(num),1])
  }else
  {
    return(NA)
  }

  #data_order[num,]
}

