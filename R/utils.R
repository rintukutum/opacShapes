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
	coord = FALSE,
	scale = TRUE
){
	if(scale == TRUE)
	{
		w <- scaleComb(w*0.25)$x
		h <- scaleComb(h*0.25)$y
	}else{
		# w <- w
		# h <- h
	}
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
	add.coord <- findCoordArcChord(
		s.angle = s.angle,
		e.angle = e.angle,
		stroke.size = stroke.size,
		fillArea = fillArea
	)
	x_poly = c(
		fillArea$x,
		rev(add.coord$P2.coord$x),
		rev(strokeArea$x),
		add.coord$P1.coord$x,
		rev(add.coord$P2.coord$x)[1],
		rev(fillArea$x)[1],
		fillArea$x[1]
	)
	y_poly = c(
		fillArea$y,
		rev(add.coord$P2.coord$y),
		rev(strokeArea$y),
		add.coord$P1.coord$y,
		rev(add.coord$P2.coord$y)[1],
		rev(fillArea$y)[1],
		fillArea$y[1]
	)
	polypath(
			x = x_poly,
			y = y_poly,
			border = NA,
			col = stroke
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
findAngArcChord <- function(
	s.angle,
	e.angle
){
	# find diff angle
	diff.ang <- e.angle - s.angle
	if(diff.ang > 180){
		theta <- (360-diff.ang)
		ang.rotate <- 180 - (90 + (180-theta)/2)
		e.point <- c(
			s.ang = e.angle,
			e.ang = e.angle + ang.rotate
		)
		s.point <- c(
			s.ang = s.angle,
			e.ang = s.angle - ang.rotate
		)
	}
	return(list(
		start.point = s.point,
		end.point = e.point
		))
}

findCoordArcChord <- function(
	s.angle,
	e.angle,
	stroke.size,
	fillArea
){
	e.point.coord <- c(
		rev(fillArea$x)[1],
		rev(fillArea$y)[1]
	)
	rScaled <- scaleComb(stroke.size * 0.05)
	angS <- findAngArcChord(
		s.angle=s.angle,
		e.angle=e.angle
	)
	s.point <- angS$start.point
	P1.coord <- getXY(
		x = fillArea$x[1],
		y = fillArea$y[1],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = s.point['s.ang'],
		end.angle = s.point['e.ang']
	)
	e.point <- angS$end.point
	P2.coord <- getXY(
		x = rev(fillArea$x)[1],
		y = rev(fillArea$y)[1],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = e.point['s.ang'],
		end.angle = e.point['e.ang']
	)
	return(
		list(
			P1.coord = P1.coord,
			P2.coord = P2.coord
		)
	)
}