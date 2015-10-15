myfunction <-function(){
  
  x<- rnorm(10000)
  mean(x)
  
  
}

secondfunction <-function(x){
  
  x + rnorm(length(x))
  
}