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
	fill,
	draw =FALSE,
	coord = TRUE
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
	if(draw == TRUE){
		polypath(
		x = c(x, fillArea$x),
        y = c(y, fillArea$y),
        border = NA,
        col = fill
    )
	}
	if(coord == TRUE){
		return(fillArea)
	}
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
	#text(P1[1],P1[2],'P1')
	#text(P2[1],P2[2],'P2')
	#text(P3[1],P3[2],label)

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
# chord arc
#	P1 <- c(
	#	rev(fillArea$x)[1],
	#	rev(fillArea$y)[1]
	#	)
	
	#angleP1 <- findAngInPoint(e.angle)
	# add sec coordinates
	#secP1 <-  drawSmallFill(
	#	x = P1[1],
	#	y = P1[2],
	#	r = stroke.size,
	#	s.angle = angleP1[1],
	#	e.angle = angleP1[2],
	#	fill = '#2ca05a8c',
	#	draw = FALSE
	#)
	#-----
	# P2 coordinates
	#P2 <- c(
	#	fillArea$x[1],
	#	fillArea$y[1]
	#)
	#secP2 <- drawSmallFill(
	#	x = P2[1],
	#	y = P2[2],
	#	r = stroke.size,
	#	s.angle = angleP2[1],
	#	e.angle = angleP2[2],
	#	fill = '#ff00008d',
	#	draw = FALSE
	#)
	#-----
	# pure angle based 
	# circle 
	
	#-----
	# combined coordinates
	# awesome :)
	#x_poly <- c(
	#	fillArea$x,
	#	rev(secP1$x),
	#	rev(strokeArea$x),
	#	rev(secP2$x),
	#	rev(secP1$x)[1],
	#	P1[1],
	#	P2[1]
	#)
	#y_poly <- c(
	#	fillArea$y,
	#	rev(secP1$y),
	#	rev(strokeArea$y),
	#	rev(secP2$y),
	#	rev(secP1$y)[1],
	#	P1[2],
	#	P2[2]
	#)