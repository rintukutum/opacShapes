source('./R/scale-factors.R')
point <- function(x,y,r=1,fill='grey80'){
  r <- r/10
  rad <- (0:360/180)*pi
  r_ <- scaleComb()
  r.x <- r * r_$x_
  #r.y <- (r * scaleRadius()) * scaleAxis()
  r.y <- r * r_$y_
  fillArea <- list(x = x + r.x * cos(rad),
                   y = y + r.y * sin(rad))
  polypath(x = fillArea$x,
           y = fillArea$y,
           border = NA,
           col = fill
           )
}
