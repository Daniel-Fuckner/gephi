createPosition <- function(creationtime){
  len <- length(creationtime)
  # Spezialfall wenn funnelLength == 1 
  if (len==1){
    Position <- as.integer(1)
  }else{
    Position <- c(1:len)
  }
  
  data.table(Position)
}