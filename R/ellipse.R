#---------------
# ellipse =  circle with width and height attribute
# 			 in place of radius
#---------------
ellipse <- function(
	x,	# x-coordinate
	y,  # y-coordinate
	fill = 'grey80', # fill color
	stroke = 'skyblue', # stroke color
	width = 1, # width
	height = 1, # height
	stroke.size = 1,
	stroke.position = 'out' # 'in' = inside, mid = 'middle', out = 'outside'
){
	#----
	# re-scale according to device size
	r_x <- scaleComb(width*0.1)$x
	r_y <- scaleComb(height*0.1)$y
	#----------
	# x & y coordinate for fill area
	fillArea <- getXY(
		x = x,
      	y = y,
      	r.x = r_x,
      	r.y = r_y
    )
    # draw
	polypath(
		x = fillArea$x,
        y = fillArea$y,
        border = NA,
        col = fill
    )
	#-----
	# x & y coordinate for stroke area
	r_x_outer <- scaleComb(
		r = (width*0.1 +  stroke.size * 0.1)
	)$x
	r_y_outer <- scaleComb(
		r = (height*0.1 +  stroke.size * 0.1)
	)$y
	strokeArea <- getXY(
		x = x,
      	y = y,
      	r.x = r_x_outer,
      	r.y = r_y_outer
	)
	polypath(
		x = c(fillArea$x, rev(strokeArea$x)),
		y = c(fillArea$y, rev(strokeArea$y)),
		border = NA,
		col = stroke
	)
}
