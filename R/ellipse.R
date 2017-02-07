#---------------
# ellipse =  circle with width and height attribute
# 			 in place of radius
#---------------
ellispe <- function(
	x,	# x-coordinate
	y,  # y-coordinate
	fill = 'grey80', # fill color
	stroke = 'skyblue', # stroke color
	width = 1, # width
	height = 1, # height
	stroke.size = 1,
	stroke.position = 'out' # 'in' = inside, mid = 'middle', out = 'outside'
){
	r_x <- scaleComb(width)$x
	r_y <- scaleComb(height)$y
	
	fillArea <- getXY(
      x = x,
      y = y,
      r.x = r_x,
      r.y = r_y
    )
	polypath(x = fillArea$x,
           y = fillArea$y,
           border = NA,
           col = fill
           )
	#-----
	# do later

}
