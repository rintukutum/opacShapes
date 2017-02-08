#' draw points rescaled according to plot
#' and device size
point <- function(
  x,
  y,
  r = 1,
  fill = 'grey80',
  scale = TRUE # scaling for device size
  ){
  #
  r = r * 0.1
  # scaling area
  if(scale == TRUE){
    # scaled according to device 
    # width and height
    r_ <- scaleComb(r)
    fillArea <- getXY(
      x = x,
      y = y,
      r.x = r_$x,
      r.y = r_$y
    )
  }else{
    # no scaling applied
    fillArea <- getXY(
      x = x,
      y = y,
      r.x = r,
      r.y = r
    )
  # draw only
  polypath(x = fillArea$x,
           y = fillArea$y,
           border = NA,
           col = fill
           )

}
