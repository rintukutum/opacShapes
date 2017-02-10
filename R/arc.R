#-----
# Different types arc
# OPEN, CHORD, PIE,
# inspiration and adopted from
# https://processing.org/reference/arc_.html
#-----
arc <- function(
	x,
	y,
	width =  1,
	height =  1,
	fill = 'grey80',
	stroke = 'grey10',
	stroke.size = 1,
	mode = 'OPEN',
	s.angle = 0,
	e.angle = 270,
	type = 'OPEN' # by default type is OPEN
				  # additional CHORD & PIE
)
{
	fillArea <- drawFillArc(
		x = x,
		y = y,
		w = width,
		h = height,
		fill = fill,
		s.angle = s.angle,
		e.angle = e.angle,
		coord = TRUE
	)
    ###
	# stroke area
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
}


#-----------------
svg('./vignettes/chordArcTest.svg',
	width = 6,
	height = 6
)
x = 1
y = 3
width = 5
height = 5
fill = '#ffaaaaff'
stroke = '#0088aa8c'
s.angle = 0
e.angle = 270
stroke.size = 20
plot(-10:10,-10:10,type='n')
fillArea <- drawFillArc(
		x = x,
		y = y,
		w = width,
		h = height,
		fill = fill,
		s.angle = s.angle,
		e.angle = e.angle,
		coord = TRUE
	)
type = 'OPEN'
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
	stroke.size * 0.05
	)
drawTriangle(
 	P1,
 	P2,
 	P3
)
P4 <- findP3(
			P2,
			P1,
			stroke.size * 0.05)
drawTriangle(
			P1,
 			P2,
 			P4,
 			label = 'P4',
 			fill = '#2ca05a8c'
)
#----
angleP1 <- findAngInPoint(e.angle)
drawSmallFill(
	x = P1[1],
	y = P1[2],
	r = stroke.size,
	s.angle = angleP1[1],
	e.angle = angleP1[2],
	fill = '#2ca05a8c'
)	
angleP2 <- findAngInPoint(e.angle, FALSE)
drawSmallFill(
	x = P2[1],
	y = P2[2],
	r = stroke.size,
	s.angle = angleP2[1],
	e.angle = angleP2[2],
	fill = '#ff00008d'
)	
dev.off()