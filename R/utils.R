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

drawStroke <- function(
	x,
	y,
	r,
	stroke,
	stroke.size,
	stroke.position
){
	#------
	r <- r * 0.1
	stroke.size <- stroke.size * 0.1
	#-----
  	rad <- ((0:360)/180)*pi
  	#------
  	# radius
  	if(stroke.position == 'out'){
  		r_inner <- scaleComb(r)
		r_outer <- scaleComb(r+stroke.size)
  	}
	if(stroke.position == 'mid'){
  		r_inner <- scaleComb(r-(stroke.size/2))
		r_outer <- scaleComb(r+(stroke.size/2))
  	}
	#------
	# coordinates
	coor_inner <- list(
		x = x + r_inner$x * cos(rad),
		y = y + r_inner$y * sin(rad)
		)
	coor_outer <- list(
		x = x + r_outer$x * cos(rad),
		y = y + r_outer$y * sin(rad)
		)
	#----
	polypath(
		x = c(coor_inner$x, rev(coor_outer$x)),
		y = c(coor_inner$y, rev(coor_outer$y)),
		col = stroke,
		border = NA
	)
}

