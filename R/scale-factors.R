scaleComb <- function(){
  par_ <- par()
  x_ <- diff(par_$usr[1:2])/(par_$pin[1] * 2.54)
  y_ <- diff(par_$usr[3:4])/(par_$pin[2] * 2.54)
  return(list(x_ = x_,
              y_ = y_
              )
        )
}
