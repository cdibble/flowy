#Shift wind direction relative to "alongshore" (= at 320M degrees)
# calcs are made using true north. boon data are in true north  (per email w/ Marcel Losekoot on Oct. 24, 2016).
shift.wind <- function(df, coastline.angle = 320, dir = "dir", spe = "spe", time = "date_time_utc", verbose = FALSE){
			if(verbose == TRUE){cat("\nNOTE: Assuming a coastline angle of: \n", coastline.angle, "\nfor alongshore and cross-shore wind speed calculations!\n")
								cat("\nwind data has this many rows:", nrow(df), " and the number of rows being shifted is this many:", length(q1) + length(q2) + length(q3) + length(q4), "\n")
		}
							# tm <- proc.time()
			df$Wrel.dir <- df[[dir]] + (360-coastline.angle) #adjust "north" to coastline.angle
			df$Wrel.dir[which(df$Wrel.dir > 360)] <- df$Wrel.dir[which(df$Wrel.dir > 360)] - 360

			# # Quadrant indices
			q1 <- which(df$Wrel.dir >= 0 & df$Wrel.dir < (90))
			q2 <- which(df$Wrel.dir >= (270) & df$Wrel.dir <= 360)
			q3 <- which(df$Wrel.dir >= 180 & df$Wrel.dir < (270))
			q4 <- which(df$Wrel.dir >= (90) & df$Wrel.dir < (180))
			# Cartesian direction (0 degrees is east, increasing degress moves counterclockwise)
			df$cartesian.dir <- rep(NA, nrow(df))
			df$cartesian.dir[q1] <- 90 - df$Wrel.dir[q1] # need to fix angles so that the start at East and rotate counterclockwise
			df$cartesian.dir[q2] <- (360 - df$Wrel.dir[q2]) + 90 # need to fix angles so that the start at East and rotate counterclockwise
			df$cartesian.dir[q3] <- (360 - df$Wrel.dir[q3]) + 90 # need to fix angles so that the start at East and rotate counterclockwise
			df$cartesian.dir[q4] <- (360 - df$Wrel.dir[q4] ) + 90 # need to fix angles so that the start at East and rotate counterclockwise

			df$rad.dir <- df$cartesian.dir * pi / 180 # convert to radians
			df$xspe <- rep(NA, nrow(df)) # initialize cross-shore wind variable
			df$aspe <- rep(NA, nrow(df)) # initilaize alongshore wind variable

			# Breaking out Relative (to coastline) wind direction into alongshore and cross-shore components
			#  Compute along and cross shore wind speed
			df$xspe[q1] <- -cos(df$rad.dir[q1])*df[[spe]][q1]
			df$aspe[q1] <- -sin(df$rad.dir[q1])*df[[spe]][q1]
			df$xspe[q2] <- sin(df$rad.dir[q2] - (pi/2)) * df[[spe]][q2]
		  	df$aspe[q2] <- -cos(df$rad.dir[q2] - (pi/2)) * df[[spe]][q2]
			df$xspe[q3] <- cos(df$rad.dir[q3] - (pi)) * df[[spe]][q3]
		  	df$aspe[q3] <- sin(df$rad.dir[q3] - (pi)) * df[[spe]][q3]
			df$xspe[q4] <- -sin(df$rad.dir[q4] - (3*pi/2)) * df[[spe]][q4]
		 	df$aspe[q4] <- cos(df$rad.dir[q4] - (3*pi/2)) * df[[spe]][q4]

		 	df <- dplyr::arrange_(df, .dots = time)

						 	# time <- proc.time() - tm
						 	# print(time)
		  return(df)
}
