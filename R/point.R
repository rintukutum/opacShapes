#' draw points rescaled according to plot
#' and device size
point <- function(
  x,
  y,
  r = 1,
  fill = 'grey80',
  xycor = FALSE, # draw only
  scale = TRUE # scaling for device size
  ){
  #
  r = r * 0.1
  rad <- (0:360/180)*pi
  # scaling area
  if(scale == TRUE){
    # scaled according to device 
    # width and height
    r_ <- scaleComb(r)
    fillArea <- list(
      x = x + r_$x * cos(rad),
      y = y + r_$y * sin(rad))
  }else{
    # no scaling applied
    fillArea <- list(
      x = x + r * cos(rad),
      y = y + r * sin(rad))

  }
  # draw only
  polypath(x = fillArea$x,
           y = fillArea$y,
           border = NA,
           col = fill
           )
  #
  
  if(xycor == TRUE){
    # if user need the coordinates 
    # P.S. Coordinates are scaled according to
    # device output size
    return(fillArea)
  }
}
