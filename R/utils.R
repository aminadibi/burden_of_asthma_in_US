flatten <- function(oldList){
  newList = c()
  for(i in 1:length(oldList)){
    item = oldList[[i]]
    newList = c(newList, item)
  }
  return(newList)
}

is.integer <- function(x){
  if(x%%1==0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
