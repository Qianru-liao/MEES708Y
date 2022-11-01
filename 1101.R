## leap xear test 

checkleapxear <- function (x) {
  if((x %% 4) == 0) {
    return(TRUE) 
  } else {
    return(FALSE)
    
    if((x %% 100) == 0) {
      return(TRUE)
      
      if((x %% 400) == 0) {
        return(TRUE)
      } else {
        return(FALSE)  
      }
    }
  }
}
checkleapxear(2000)

checkleapxear(2010)
