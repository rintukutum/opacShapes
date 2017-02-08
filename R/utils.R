#----------
# angle to radian
ang2rad <- function(angle){
	radian <- (angle/180)*pi
	return(radian)
}
#'-------------------------
#' This code allow user to plot
#' shapes that are scaled based on
#' axis and device width and height
scaleComb <- function(r){
 par_ <- par()
 x_ <- diff(par_$usr[1:2])/(par_$pin[1] * 2.54)
 y_ <- diff(par_$usr[3:4])/(par_$pin[2] * 2.54)
  return(
   list(
   	x_ = r * x_,
    y_ = r * y_
    )
  )
}
#--------
# get x and y coordinates 
# based on width and height
getXY <- function(
	x,
	y,
	r.x, # width
	r.y, # height
	start.angle = 0,
	end.angle =  360
	)
{

	ang <- start.angle:end.angle
	rad <- ang2rad(ang)
	coord <- list(
		x = x + r.x * cos(rad),
		y = y + r.y * sin(rad)
		)
	return(coord)
}
#------
