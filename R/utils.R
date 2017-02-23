#----------
# angle to radian
ang2rad <- function(angle){
	radian <- (angle/180)*pi
	return(radian)
}
Sin <- function(angle){
	val <- round(sin(ang2rad(angle)),10)
	return(val)
}
Cos <- function(angle){
	val <- round(cos(ang2rad(angle)),10)
	return(val)
}
Tan <- function(angle){
	val <- round(tan(ang2rad(angle)),10)
	return(val)
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
scaleFactor <- function(){
 par_ <- par()
 x_ <- diff(par_$usr[1:2])/(par_$pin[1] * 2.54)
 y_ <- diff(par_$usr[3:4])/(par_$pin[2] * 2.54)
  return(
   list(
   	fac.x = x_,
    fac.y = y_
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
	scale = TRUE,
	PIE = FALSE
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
	if(PIE == FALSE){
		polypath(
			x = fillArea$x,
        	y = fillArea$y,
        	border = NA,
        	col = fill
    	)
	}else{
		o <- c(x,y)
		polypath(
			x = c(o[1],fillArea$x),
        	y = c(o[2],fillArea$y),
        	border = NA,
        	col = fill
    	)
	}
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
	}
	if(type == 'PIE'){
		coords_ <- findAngArcPIE(
			x = x,
			y = y,
			stroke.size = stroke.size,
			s.angle = s.angle,
			e.angle = e.angle,
			fillArea = fillArea,
			strokeArea = strokeArea
		)
		x_poly = coords_$x_poly
		y_poly = coords_$y_poly
	}
	polypath(
			x = x_poly,
			y = y_poly,
			border = NA,
			col = stroke
		)
}
#--------------
# CHORD ARC
findAngArcChord <- function(
	s.angle,
	e.angle
){
	# find diff angle
	diff.ang <- e.angle - s.angle
	#if(diff.ang > 180){
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
	#}
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
#--------------
# PIE ARC
# third point in isosceles triangle
# http://stackoverflow.com/questions/3548039/find-the-coordinates-in-an-isosceles-triangle
findAngArcPIE <- function(
	x,
	y,
	stroke.size,
	s.angle,
	e.angle,
	fillArea,
	strokeArea
)
{
	a = c(
		fillArea$x[1],
		fillArea$y[1]
	)
	b = c(
		rev(fillArea$x)[1],
		rev(fillArea$y)[1]
	)
	o = c(
		x,
		y
	)
	rScaled <- scaleComb(stroke.size * 0.05)
	coor.o <- getXY(
		x = o[1],
		y = o[2],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = 0,
		end.angle = 360
	)
	polypath(coor.o)
	coor.a <- getXY(
		x = a[1],
		y = a[2],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = 0,
		end.angle = 360
	)
	polypath(coor.a)
	coor.b <- getXY(
		x = b[1],
		y = b[2],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = 0,
		end.angle = 360
	)
	polypath(coor.b)
	#--------------
	# find the side using sine laws of triangle
	# http://www.mathopenref.com/lawofsines.html
	#--------------
	diff.angle  <- e.angle - s.angle
	if(e.angle <= 90){
		thetha.turn <- 90 - diff.angle
		e_ <- 90
	}else if(e.angle <= 180){
		thetha.turn <- 180 - diff.angle
		e_ <- 180
	}else if(e.angle <= 270){
		thetha.turn <- 270 - diff.angle
		e_ <- 270
	}else if(e.angle <= 360){
		thetha.turn <- 360 - diff.angle
		e_ <- 360
	}
	#-----
	
	
	#---
	#
	# distance OA 
	 # find A coordinate
	coor.A <- getXY(
		x = o[1],
		y = o[2],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = e_ - 1,
		end.angle = e_
	)
	coord.A <- lapply(coor.A,function(x)rev(x)[1])
	OA <- sqrt(
		(coord.A$x - o[1])^2 + 
		(coord.A$y - o[2])^2
	 )
	# find OB
	#--------------------------
	fac <- scaleFactor()
	AB.x <- (OA * fac$fac.x) / Sin(180-(90+thetha.turn)) * Sin(90)
	AB.y <- (OA * fac$fac.y) / Sin(180-(90+thetha.turn)) * Sin(90)
	
	#---------------------------
	#-----
	# Find the coordinates
	# http://math.stackexchange.com/questions/543961/determine-third-point-of-triangle-when-two-points-and-all-sides-are-known
	#B.y <- (OA^2 + OB^2 - AB^2) / (2 * OA)
	#B.x <- -(sqrt(OB^2 - B.y^2))
	#------
	# http://math.stackexchange.com/questions/927802/how-to-find-coordinates-of-3rd-vertex-of-a-right-angled-triangle-when-everything
	
	B.x <- (coord.A$x - (AB.x * (o[2] - coord.A$y))/OA) 
	B.y <- (coord.A$y - (AB.y * (coord.A$x - o[1]))/OA)
	drawTriangle(
		o,
		coord.A,
		c(B.x,B.y)
		)
	#------------------------
	# find coordinate of B`
	opp.thetha <- s.angle - thetha.turn
	coor.A_ <- getXY(
		x = o[1],
		y = o[2],
		r.x = rScaled$x_,
		r.y = rScaled$y_,
		start.angle = 0,
		end.angle = opp.thetha
	)
	coord.A_ <- lapply(coor.A_,function(x)rev(x)[1])
	OA_ <- sqrt((coord.A_$x - o[1])^2 + (coord.A_$y - o[2])^2)
	# find OB_`
	OB_  <- OA_ /Sin(thetha.turn) * Sin(90)
	AB_ <- OB_ /Sin(180-(90+thetha.turn)) * Sin(90)

	B_.x <- coord.A_$x + (AB_ * (o[2] - coord.A_$y))/OA_
	B_.y <- coord.A_$y + (AB_ * (coord.A_$x - o[1]))/OA_
	
	drawTriangle(o,
		coord.A_,
		c(B_.x,B_.y))
	#-------
	# find C
	# http://math.stackexchange.com/questions/887095/find-the-4th-vertex-of-the-parallelogram
	C.x <- B.x + B_.x - o[1]
	C.y <- B.y + B_.y - o[2]

	ang.P1oP2 <- 360 - diff.angle
	if(diff.angle > 180){
		coord.P1 <- getXY(
			x = a[1],
			y = a[2],
			r.x = rScaled$x_,
			r.y = rScaled$y_,
			start.angle = e.angle,
			end.angle = e.angle + ang.P1oP2
		)
		a_ <- c(
			rev(coord.P1$x)[1],
			rev(coord.P1$y)[1]
		)
		#
		coord.P2 <- getXY(
			x = b[1],
			y = b[2],
			r.x = rScaled$x_,
			r.y = rScaled$y_,
			start.angle = e.angle,
			end.angle = e.angle + ang.P1oP2
		)
		b_ <- c(
			coord.P2$x[1],
			coord.P2$y[1]
		)
		coord.o <- getXY(
			x = o[1],
			y = o[2],
			r.x = rScaled$x_,
			r.y = rScaled$y_,
			start.angle = e.angle,
			end.angle = e.angle + ang.P1oP2
		)
	}

	x_poly = c(
		o[1],
		fillArea$x,
		o[1],
		C.x,
		rev(coord.P2$x),
		rev(strokeArea$x),
		rev(coord.P1$x),
		C.x,
		o[1]
	)
	y_poly = c(
		o[2],
		fillArea$y,
		o[2],
		C.y,
		rev(coord.P2$y),
		rev(strokeArea$y),
		rev(coord.P1$y),
		C.y,
		o[2]		
	)
	#return(list(
	#	x_poly = x_poly,
	#	y_poly = y_poly
	#	)
	#)
}

#----

x = 1
y = 3
width = 10
height = 10
fill = '#ffaaaaff'
stroke = '#0088aa8c'
s.angle = 0
e.angle = 200
stroke.size = 10
plot(-10:10,-10:10,type='n', xlim=c(-10,40))
type = 'PIE'
fillArea <- drawFillArc(
		x = x,
		y = y,
		w = width,
		h = height,
		fill = fill,
		s.angle = s.angle,
		e.angle = e.angle,
		coord = TRUE,
		PIE = TRUE
)



drawStrokeArc(
		x = x,
		y = y,
		w = width,
		h = height,
		stroke = stroke,
		stroke.size = stroke.size,
		s.angle = s.angle,
		e.angle = e.angle,
		fillArea = fillArea,
		type = type
)