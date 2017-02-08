#---------------
# circle =  point + stroke
#---------------
circle <- function(
	x,	# x-coordinate
	y,  # y-coordinate
	fill = 'grey80', # fill color
	stroke = 'skyblue', # stroke color
	r = 1, # radius
	stroke.size = 1,
	stroke.position = 'out' # 'in' = inside, mid = 'middle', out = 'outside'
){
	point(
		x = x,
		y = y,
		r = r,
		fill = fill
	)
	# draw stroke after point
	pos.implem <- c('mid','out')
	if(any(pos.implem == stroke.position))
	{
		drawStroke(
			x = x,
			y = y,
			r = r,
			stroke = stroke,
			stroke.size = stroke.size,
			stroke.position = stroke.position
		)
	}else{
		# eror msg
		stop(paste0(
			' In stroke.position\n',
			'"in", "mid" and "out" allowed!\n'
			))
	}

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
	ang <- 0:360
  	rad <- ang2rad(ang)
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