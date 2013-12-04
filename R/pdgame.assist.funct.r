run.rep.game.assist.cap.numbers <- function(detailed.return, output, cap.numbers){
  restore.point("run.rep.game.assist.cap.numbers")
  
  #< Basic parameters not to be changed live
  default.cap.numbers = 2 #Number of digits to be rounded to in case of cap.number==TRUE
  #> Stop Basic parameters
  
  # Used in context with cap.numbers for better output
  transform.to.numeric <- function(df,cap.numbers=cap.numbers){
    restore.point("transform.to.numeric")
    as.data.frame(apply(df,MARGIN=2,FUN=function(x){
      pos.nas <- suppressWarnings(as.numeric(x))
      if(sum(is.na(pos.nas))==sum(is.na(x))){
        x <- round(as.numeric(x),cap.numbers)
      } else {
        #ignore 
      }
      x
    }))
  }
  #>
  
  #Basic checks of cap.number
  if(length(cap.numbers)>1){
    message("cap.numbers has to be single number. Please read the documentation.")
    stop()    
  }
  if(is.logical(cap.numbers)){ #So either the default value or it should be set to a default number
    if(cap.numbers==TRUE){
      cap.numbers <- as.integer(default.cap.numbers)
    } else {
      cap.numbers <- -1
    }
  } else if (is.numeric(cap.numbers)) { # number of digits explicitely given
    if(cap.numbers<0){
      message("cap.numbers has to be of positive value. Please read the documentation.")
      stop()
    }
    cap.numbers <- as.integer(cap.numbers) # to ensure that doubles do not interfere
  } else { # unexpected class of cap.numbers
    message("cap.numbers has to be either numerical or logical. Please read the documentation.")
    stop()
  }
  ## Now we have ensured, that cap.numbers is an integer, which is "greater or equal 0" or -1 (in the case of no cap.numbers)
  if (cap.numbers>=0 && detailed.return){ # If not detailed.return, we do not need this transformation
    output <- transform.to.numeric(df=output,cap.numbers=cap.numbers)  #Transform.to.numeric is a minifunction of this function an as such cap.numbers is known  
  } else { # cap.numbers==FALSE
    # do nothing
  }
  return(output)
}