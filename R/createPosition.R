createPosition <- function(creationtime,id){
  len <- length(creationtime)
  if((id %% 250000)==0){
    print(id)
  }
  # Spezialfall wenn funnelLength == 1 
  if (len==1){
    Position <- as.integer(1)
  }else{
    Position <- c(1:len)
  }
  
  data.table(Position)
}