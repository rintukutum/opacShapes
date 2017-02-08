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
# arc
drawFillArc <- function(
	x,
	y,
	w,
	h,
	s.angle,
	e.angle,
	fill,
	coord = FALSE
){
	w <- scaleComb(w*0.25)$x
	h <- scaleComb(h*0.25)$y
	###
	# fill area
	fillArea <- getXY(
		x = x,
		y = y,
		r.x = w,
		r.y = h,
		start.angle = s.angle,
		end.angle = e.angle
	)
	polypath(
		x = fillArea$x,
        y = fillArea$y,
        border = NA,
        col = fill
    )
    if(coord){
    	return(fillArea)
    }

}

drawStrokeArc <- function(
	x,
	y,
	w,
	h,
	stroke,
	stroke.size,
	s.angle,
	e.angle,
	fillArea,
	type
)
{
	r_x_outer <- scaleComb(
		r = (w*0.25 +  stroke.size * 0.05)
	)$x
	r_y_outer <- scaleComb(
		r = (h*0.25 +  stroke.size * 0.05)
	)$y
	strokeArea <- getXY(
		x = x,
      	y = y,
      	r.x = r_x_outer,
      	r.y = r_y_outer,
     	start.angle = s.angle,
		end.angle = e.angle 	
	)
	if(type == 'OPEN'){
		x_poly <- c(
			fillArea$x,
			rev(strokeArea$x)
		)
		y_poly <- c(
			fillArea$y,
			rev(strokeArea$y)
		)
	}
	if(type == 'CHORD'){
		x_poly <- c(
			fillArea$x, 
			fillArea$x[1],
			strokeArea$x[1],
			rev(strokeArea$x)
		)
		y_poly <- c(
			fillArea$y,
			rev(strokeArea$y)
		)
	}
	if(type == 'PIE'){
		x_poly <- c(
			fillArea$x,
			rev(strokeArea$x)
		)
		y_poly <- c(
			fillArea$y,
			rev(strokeArea$y)
		)
	}
	polypath(
			x = x_poly,
			y = y_poly,
			border = NA,
			col = stroke
		)
	
}