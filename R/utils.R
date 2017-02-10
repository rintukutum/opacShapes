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
		#-----
		# P1 coordinates
		P1 <- c(
			rev(fillArea$x)[1],
			rev(fillArea$y)[1]
			)
		#-----
		# P2 coordinates
		P2 <- c(
			fillArea$x[1],
			fillArea$y[1]
			)

		#-----
		# find P3 coordinates
		P3 <- findP3(
			P1,
			P2,
			stroke.size * 0.05)
		#-----
		# draw for QC
		drawTriangle(
 			P1,
 			P2,
 			P3
		)
		# find P4 coordinates
		P4 <- findP3(
			P2,
			P1,
			stroke.size * 0.05)
		drawTriangle(
 			P1,
 			P2,
 			P4,
 			fill = '#2ca05a8c'
		)
		x_poly <- c(
			fillArea$x, 
			# need opimization
			fillArea$x[1],
			# need opimization
			strokeArea$x[1],

			rev(strokeArea$x)
		)
		y_poly <- c(
			fillArea$y,
			# need opimization
			fillArea$y[1],
			# need opimization
			strokeArea$y[1],

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
#-------------
# simple maths 
#-------------
# http://math.stackexchange.com/questions/64823/how-to-find-the-third-coordinate-of-a-right-triangle-given-2-coordinates-and-len
# answer by, Ross Millikan
# http://math.stackexchange.com/questions/927802/how-to-find-coordinates-of-3rd-vertex-of-a-right-angled-triangle-when-everything
# answer by MvG
# http://math.stackexchange.com/questions/187107/calculate-coordinates-of-3rd-point-vertex-of-a-scalene-triangle-if-angles-and
#-------------
#   P2
#   |\
#   | \
#  A|  \ C
#   |   \
#   |____\
# P1   B  P3
#------------
# Pn = c(Xn, Yn)
# slope of A, mA = (y2 - y1)/(x2 -x1)
# A _|_ B, mA * mB = -1
# slope B, mB = -1/mA = -(x2-x1)/(y2-y1)
# p3 coordinate
# x3 = (mA(y1 - y2)/mB) + x1
# y3 = (mA(x2 - x1)/mB) + y1 

findP3 <-  function(
	P1,
	P2,
	distP1P3
	){
	m.P1P2 = (P2[2] - P1[2])/(P2[1] - P1[1])
	m.P1P3 = -1/m.P1P2
	
	distScaled <- scaleComb(distP1P3)
	#x.P3 = (m.P1P2 * (P1[2] - P2[2])/m.P1P3) + P1[1]
	#y.P3 = (m.P1P2 * (P2[1] - P1[1])/m.P1P3) + P1[2]
	x.P3 = P1[1] + distScaled$x_ * (1/sqrt(1 + m.P1P3^2))
	y.P3 = P1[2] + distScaled$y_ * (m.P1P3/sqrt(abs(1 + m.P1P3^2)))

	return(
		c(
			x = x.P3,
			y = y.P3
		)
	)
}

findAngInPoint <- function(
	e.angle,
	P1 = TRUE
)
{
	angleLeft <- 360 - e.angle
	angle2rm <- angleLeft/2
	angleInP3 <- 180 - (90 + angle2rm)
	if(P1 == TRUE){
		s.angP3 <- 360 - (angleInP3*2)
		e.angP3 <- s.angP3 + angleInP3
	}else{
		s.angP3 <- 360 - angleInP3
		e.angP3 <- 360
	}
	return(c(s.angP3, e.angP3))
}

drawSmallFill <- function(
	x,
	y,
	r,
	s.angle,
	e.angle,
	fill
)
{
	r.scaled <- scaleComb(r*0.05)
	fillArea <- getXY(
		x = x,
		y = y,
		r.x = r.scaled$x_,
		r.y = r.scaled$y_,
		start.angle = s.angle,
		end.angle = e.angle
	)
	polypath(
		x = c(x, fillArea$x),
        y = c(y, fillArea$y),
        border = NA,
        col = fill
    )
}
#---------------------
drawTriangle <- function(
	P1,
	P2,
	P3,
	label = 'P3',
	fill = '#ff00008d')
{	

	polygon(
		x = c(P1[1],P2[1],P3[1],P1[1]),
		y = c(P1[2], P2[2], P3[2],P1[2]),
		col = fill,
		border = NA
	)
	text(P1[1],P1[2],'P1')
	text(P2[1],P2[2],'P2')
	text(P3[1],P3[2],label)

}


#P1 = c(-4,1)
#P2 = c(-4,-10)
#P3 = findP3(
#		P1 = P1,
#		P2 = P2, 
#		B = 2
#	)

#plot(-20:20,-20:20,type='n')
#drawTriangle(P1, P2, P3)
#
#P1 = c(1,-9)
#P2 = c(11,-2)
#P3 = findP3(
#		P1 = P1,
#		P2 = P2, 
#		B = 4
#	)
#drawTriangle(P1, P2, P3)
#---------------------