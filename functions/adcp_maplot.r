adcp_maplot.r

# Note this has been updated- new version on lab PC
adcp.mapplot <- function(adcp.df, map.df = NULL, tide.df = NULL, start.time = NULL, stop.time = NULL, time.step = 60, Bin.range = NULL,
	n.range = NULL, Bin.step = 1, filtered.Nor = NULL, filtered.Eas = NULL){
		require(grid) # grid.newpage() erases current plot, opens new blank one
		require(gridExtra)
		require(dplyr); require(ggplot2);
		cat("\nTime step: ", time.step, " (minutes)\n")
		# Set start and stop time to full extent of data sets if no user input
			if(is.null(start.time)){ 
				start.time <- min(adcp.df$Time.com, na.rm = TRUE)
			}
			if(is.null(stop.time)){
				stop.times <- max(adcp.df$Time.com, na.rm = TRUE)		
			}
		# pull in map data to draw coastline
			if(is.null(map.df)){ 
				cat("\nWARNING: Gonna be hella slow if you make it load the map file over and over again.\n")
				load(file  = "/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/flah_map_data.R")
				map.df <- coastline.df
			}
		# Get tide data
			if(is.null(tide.df)){
			tide.df <- read.csv(file = '/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Data/tides/CO-OPS__9415020__hr.csv',
				header = TRUE)
			tide.df$Date.Time <- as.POSIXct(tide.df$Date.Time, origin = "1970-01-01", format = "%Y-%m-%d %H:%M")
			}
		# Set map dimensions
			# Local Area Extent
			loc.dims <- data.frame(xmin = -123.12, xmax = -122.94, ymin = 38.25, ymax = 38.34)
			# Subset Local map from coastline.df by the dimensions specified in loc.dims
			map.df <- map.df[which(map.df$lat < loc.dims$ymax & map.df$lat > loc.dims$ymin),]
			map.df <- map.df[which(map.df$long > loc.dims$xmin & map.df$long < loc.dims$xmax),]
		# Draw Map
		map.layer <- ggplot(data = map.df) + geom_point(aes(x = long, y = lat), size = 0.2, alpha = 0.7) + 
			coord_equal(ratio = (68.71 / (cos(mean(c(loc.dims$ymin, loc.dims$ymax), na.rm = TRUE))*69.172))) + # ratio = y / x = lat / lon = 68.71 (miles/deg) / (cos(local_Lat) * 69.172 miles per deg lon at equator)
			ylab("Latitude") + xlab("Longitude") + 
			scale_y_continuous(expand = c(0.01,0.01), limits = c(loc.dims$ymin, loc.dims$ymax)) + scale_x_continuous(expand=c(0.01,0.01), limits = c(loc.dims$xmin, loc.dims$xmax)) +
			ylab("Latitude") + xlab("Longitude") + theme(text = element_text(size=20)) +
			# Scale Bar
			geom_spoke(aes(x = loc.dims$xmax,
				y = loc.dims$ymax - (0.5*(loc.dims$ymax-loc.dims$ymin)), # create scale bar
				angle = pi, radius = 500/15000), color = "black", size = 0.8, alpha = 0.8) +
			# Scale Bar Label
			geom_text(label = "\n0.5 m/s", # label the scale bar
				x = loc.dims$xmax - (0.1*(loc.dims$xmax-loc.dims$xmin)), y = loc.dims$ymax - (0.5*(loc.dims$ymax-loc.dims$ymin)), size = 3) +
			# Themes
			theme_bw() + theme( axis.text.x = element_text(size = 14, angle = 20),
				axis.text.y = element_text(size = 14), legend.position = "right",
				legend.title = element_blank(), legend.text = element_text(size = 14), plot.title = element_text(size = 14))
		# ADCP: gather data and draw map
		if(is.null(Bin.range)) Bin.range <- range(adcp.df$Bin, na.rm = TRUE)
	# IF: you want to use the raw "Dir" for the current direction, then compute Rad:
		if(is.null(filtered.Nor) == TRUE & is.null(filtered.Eas) == TRUE){
		# convert Dir to Rad by Computing Cartesian Coordinate System
			# # Quadrant indices
			# 	q1 <- which(adcp.df$Dir >= 0 & adcp.df$Dir < (90))
			# 	q2 <- which(adcp.df$Dir >= (270) & adcp.df$Dir < 360)
			# 	q3 <- which(adcp.df$Dir >= 180 & adcp.df$Dir < (270))
			# 	q4 <- which(adcp.df$Dir >= (90) & adcp.df$Dir < (180))
			# # Cartesian direction (0 degrees is east, increasing degress moves counterclockwise)
			# 	adcp.df$Cartesian.dir <- rep(NA, nrow(adcp.df))
			# 	adcp.df$Cartesian.dir[q1] <- 90 - adcp.df$Dir[q1] # need to fix angles so that the start at East and rotate counterclockwise
			# 	adcp.df$Cartesian.dir[q2] <- (360 - adcp.df$Dir[q2]) + 90 # need to fix angles so that the start at East and rotate counterclockwise
			# 	adcp.df$Cartesian.dir[q3] <- (360 - adcp.df$Dir[q3]) + 90 # need to fix angles so that the start at East and rotate counterclockwise
			# 	adcp.df$Cartesian.dir[q4] <- (360 - (adcp.df$Dir[q4])) + 90 # need to fix angles so that the start at East and rotate counterclockwise
			# # Convert to rad after computing cartesian
			# 	adcp.df$Rad <- adcp.df$Cartesian.dir * pi / 180 # convert to radians
		# Alternatively, compute Rad directly without moving through Cartesian computation
			adcp.df$Rad <- ifelse(adcp.df$Dir <= 90,
				pi*(90-adcp.df$Dir)/180, # first quadrant 
				pi*((360-adcp.df$Dir)+ 90)/180) # second, third, fourth quadrants
		}
	# IF: you want to use filtered data, reassembled the Dir from decomposed vector components
		if(is.null(filtered.Nor) == FALSE & is.null(filtered.Eas) == FALSE){
			adcp.df <- revec(adcp.df, Amag = filtered.Nor, Xmag = filtered.Eas)
			adcp.df$Rad <- ifelse(adcp.df[["Dir"]] <= 90,
				pi*(90-adcp.df[["Dir"]])/180, # first quadrant 
				pi*((360-adcp.df[["Dir"]])+ 90)/180) # second, third, fourth quadrants
		}
		# Assemble ADCP Data for Each Map Layer and Draw
		time.seq <- seq(from = as.POSIXct(start.time, origin = "1970-01-01"), to = as.POSIXct(stop.time, origin = "1970-01-01"), by = time.step*60)
		time.seq <- time.seq[which(time.seq %in% adcp.df$Time.com)]
			# loop through Time
			for(t in time.seq){
				print(as.POSIXct(t, origin = "1970-01-01"))
				if(format(as.POSIXct(t, origin = "1970-01-01"), format = '%Y-%m-%d') %in% format(adcp.df$Time.com, format = '%Y-%m-%d')){
					# data.plot will build up with mean values for plotting across different bins at a given time step
					# data.plot <- as.data.frame(matrix(nrow = length(unique(adcp.df$Name))*length(seq(min(Bin.range), max(Bin.range), by = Bin.step)), ncol = ncol(adcp.df))) # container for Time.com t and Bin b with each row a different ADCP
					# names(data.plot) <- names(adcp.df)
					data.plot <- adcp.df[0,]
					data.plot$Time.com <- as.POSIXct(data.plot$Time.com, origin = "1970-01-01")
					# Method for using numeric bin range, where each adcp uses the same bin values
						if(is.numeric(Bin.range)){
								# loop through Depths
							for(d in seq(min(Bin.range), max(Bin.range), by = Bin.step)){
									# data.t holds just the mean values for the current time step and bin.
								data.t <- as.data.frame(matrix(nrow = length(unique(adcp.df$Name)), ncol = ncol(adcp.df))) # container for Time.com t and Bin b with each row a different ADCP
								names(data.t) <- names(adcp.df)
								data.t$Time.com <- as.POSIXct(data.t$Time.com, origin = "1970-01-01")
								# loop through ADCPs
								for(i in 1:length(unique(adcp.df$Name))){
									adcp.temp <- dplyr::filter(adcp.df, Name == unique(adcp.df$Name)[i], 
										Time.com <= t & Time.com > (t - time.step*60)) # dplyr::filter to current data frame and observations prior to current time step but after previous time step
									adcp.temp <- dplyr::filter(adcp.temp, Bin <= d & Bin > (d - Bin.step)) # filter to Bin
									adcp.temp <- arrange(adcp.temp, Time.com) # chronological order
									if(nrow(adcp.temp) != 0){					
										data.t[i, ] <- adcp.temp[ nrow(adcp.temp) , ] # data.t is the current time-step data for each adcp. To be used for plotting arrows
										t.step.row <- max(which(adcp.temp$Time.com <= t), na.rm = TRUE) # row corresponding to current time for plot (which is the index 't')
										data.t[i, c("Tem", "Dep", "Dfb", "Bat", "Eas","Nor", "Nor.dtbf.m", "Eas.dtbf.m", "Ver","Err","Mag","Dir","aspe.1hr.tau","xspe.1hr.tau","spe.1hr.tau","dir.1hr.tau","Wrel.dir.1hr.tau","Cartesian.dir","Rad")] <-  colMeans(adcp.temp[ , c("Tem", "Dep", "Dfb", "Bat", "Eas","Nor", "Nor.dtbf.m", "Eas.dtbf.m", "Ver","Err","Mag","Dir","aspe.1hr.tau","xspe.1hr.tau","spe.1hr.tau","dir.1hr.tau","Wrel.dir.1hr.tau","Cartesian.dir","Rad")]) # find average over last time.interval of quantitative measures				
									}
								}	
								data.plot <- rbind(data.plot, data.t)
								# print(data.plot)
							}
						} # bin loop closure}

					# Bin.range <- list(Name = as.vector(unique(adcp.df$Name)), D.bot = rep(1, length(unique(adcp.df$Name))), D.sur = rep(15, length(unique(adcp.df$Name)))) # Test Case
					# Method for using Bin.range list, which allows different bins to be used for each ADCP (i.e., so you can define different surface bins)
						if(is.list(Bin.range)){
							if(names(Bin.range)[1] != "Name")cat("Bin.range list must contain 'Name' as first column. Columns therafter should be depth bins to plot.")
								# loop through Depths
								for(d in 2:(length(Bin.range))){
									# data.t holds just the mean values for the current time step and bin.
									data.t <- as.data.frame(matrix(nrow = length(unique(adcp.df$Name)), ncol = ncol(adcp.df))) # container for Time.com t and Bin b with each row a different ADCP
									names(data.t) <- names(adcp.df)
									data.t$Time.com <- as.POSIXct(data.t$Time.com, origin = "1970-01-01")
								# loop through ADCPs
									for(i in 1:length(unique(adcp.df$Name))){
										adcp.temp <- dplyr::filter(adcp.df, Name == unique(adcp.df$Name)[i], 
											Time.com <= t & Time.com > (t - time.step*60)) # dplyr::filter to current data frame and observations prior to current time step but after previous time step
										bin.temp <- Bin.range[[d]][which(Bin.range$Name == unique(adcp.df$Name)[i])] # find bottom bin for this unit
										adcp.temp <- dplyr::filter(adcp.temp, Bin == bin.temp) # filter to Bin
										adcp.temp <- arrange(adcp.temp, Time.com) # chronological order
										if(nrow(adcp.temp) != 0){					
											data.t[i, ] <- adcp.temp[ nrow(adcp.temp) , ] # data.t is the current time-step data for each adcp. To be used for plotting arrows
											t.step.row <- max(which(adcp.temp$Time.com <= t), na.rm = TRUE) # row corresponding to current time for plot (which is the index 't')
											data.t[i, c("Tem", "Dep", "Dfb", "Bat", "Eas","Nor", "Nor.dtbf.m", "Eas.dtbf.m", "Ver","Err","Mag","Dir","aspe.1hr.tau","xspe.1hr.tau","spe.1hr.tau","dir.1hr.tau","Wrel.dir.1hr.tau","Rad")] <-  colMeans(adcp.temp[ , c("Tem", "Dep", "Dfb", "Bat", "Eas","Nor", "Nor.dtbf.m", "Eas.dtbf.m", "Ver","Err","Mag","Dir","aspe.1hr.tau","xspe.1hr.tau","spe.1hr.tau","dir.1hr.tau","Wrel.dir.1hr.tau","Rad")]) # find average over last time.interval of quantitative measures				
										}
									}	
									data.plot <- rbind(data.plot, data.t)
									# print(data.plot)
								} 
							}
					# Rearrange Bin factors so they plot in order
						data.plot <- data.plot[!is.na(data.plot$Ens),] # get rid of NA values
						if(is.numeric(Bin.range))data.plot$Bin <- factor(data.plot$Bin, levels = sort(unique(data.plot$Bin), decreasing = TRUE))
						if(is.list(Bin.range))data.plot$Bin <- ifelse(data.plot$Bin %in% Bin.range[[2]] , "Near_Bottom", ifelse(data.plot$Bin %in% Bin.range[[length(Bin.range)]], "Near_Surface", "Mid_Water"))
						if(is.list(Bin.range))Bin.levels <- c(grep("_S", unique(data.plot$Bin), value = T), grep("_W", unique(data.plot$Bin), value = T),grep("_B", unique(data.plot$Bin), value = T))
						if(is.list(Bin.range))data.plot$Bin <-	factor(data.plot$Bin, levels = Bin.levels)
						# assemble full plot and print it.
									if(nrow(data.plot) > 0){
										# Create Plot: ADCP data for this Bin and this Time.com (data is average over last time step)
											map.plot <- map.layer + geom_spoke(data = data.plot, aes(x = Lon, y = Lat, angle = Rad, radius = Mag/15000, color = Name, group = Bin),
												arrow = arrow(length = unit(0.1, "cm")), size = 1.5, alpha = 0.8) + 
										# wind barb
											geom_spoke(data = data.plot, aes(x = loc.dims$xmin + (0.3*(loc.dims$xmax-loc.dims$xmin)), y = loc.dims$ymin + (0.2*(loc.dims$ymax-loc.dims$ymin)),
												angle = mean(rad.dir, na.rm = TRUE) + pi, # directions are wind origin, so need to add pi to get arrow pointing downwind
												radius = mean(spe.1hr.tau,na.rm = TRUE)*0.44704/1000), # wind is in mph, so need to convert to cm/s
											arrow = arrow(length = unit(0.1, "cm")), size = 0.7, alpha = 0.8) +
										# wind label
											geom_text(data = data.plot, aes(label = paste0(round(mean(spe.1hr.tau, na.rm = TRUE)*0.44704,2), " m/s"),
												x = loc.dims$xmin + (0.5*(loc.dims$xmax-loc.dims$xmin)),
												y = loc.dims$ymin + (0.2*(loc.dims$ymax-loc.dims$ymin))), size = 4) +
										# Title
											ggtitle(paste0("Current Velocity: ", range(data.t$Dfb, na.rm = TRUE)[1], " m from bottom.\n",
												"Time: ", range(data.plot$Time.com, na.rm = TRUE)[1]))
										# Facet By Grid
											if(length(unique(na.omit(data.plot$Bin))) > 1){map.plot <- map.plot + facet_grid(Bin ~ ., drop = FALSE)}
										# Add Tides
											tidePlot <- tide.plot(tide.df, mark.time = data.plot$Time.com[1])
										# Draw the output
											grid.newpage()
											v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
											if(length(unique(na.omit(data.plot$Bin))) == 1) v2 <- viewport(width = 0.25, height = 0.15, x = 0.65, y = 0.68) #plot area for the inset map
											if(length(unique(na.omit(data.plot$Bin))) == 2) v2 <- viewport(width = 0.25, height = 0.15, x = 0.64, y = 0.85) #plot area for the inset map
											if(length(unique(na.omit(data.plot$Bin))) == 3) v2 <- viewport(width = 0.175, height = 0.09, x = 0.534, y = 0.905) #plot area for the inset map

											print(map.plot, vp = v1)
											print(tidePlot, vp = v2)
									}
					}
			}
	}



		# TEST CASES
		# defaults for troubleshooting mapplot
		map.df = coastline.df; start.time = "2016-04-01"; stop.time = "2016-04-03"; Bin.range = c(1, 7, 13); Bin.step = 6; time.step = 60*4
		interval = 0.7; ani.height = 800; ani.width = 800; gif = TRUE; mp4 = FALSE
		Bin.range <- list(Name = as.vector(unique(adcp.data$Name)), D.bot = c(1,2,2,2,2,2), D.mid = c(10, 5, 9, 9, 12, 9), D.sur = c(20, 7, 13, 13, 21, 11))

		# 	# # IMPLEMENTING ADCP.MAPLOT()
			pdf(file = '/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/figures/ADCP_mapPlot_dtbf.pdf')
			adcp.mapplot(adcp.data, map.df = coastline.df, start.time = "2016-04-01", stop.time = "2016-04-02",
				filtered.Nor = "Nor.dtbf.m", filtered.Eas = "Eas.dtbf.m", Bin.range = Bin.range)
			dev.off()
		
# Conditional Averaging GIFS
		# 3. Condtion 3: Stronger Southward Surface Flow on Coast (mean - 3 sd cutoff)
			# Set condition by subsetting time stamps here. Then run the Plotting Code to produce plots. Don't forget to name it.
			start.time <- as.POSIXct("2016-03-17 00:00", origin = "1970-01-01")
			stop.time <- as.POSIXct("2016-06-17 23:30", origin = "1970-01-01")
			cutoff3 <- mean(dplyr::filter(adcp.data.bf, Name == "adcp.1")$Amag, na.rm = T) - 3 * sd(dplyr::filter(adcp.data.bf, Name == "adcp.1")$Amag, na.rm = T)
			southward.times <- adcp.data.bf$Time.com[which(adcp.data.bf$Name == "adcp.1" & adcp.data.bf$Bin == 23 & adcp.data.bf$Amag < cutoff3)]
			df.temp <- dplyr::filter(adcp.data.bf, Time.com %in% southward.times)
			Bin.range <- list(Name = as.vector(unique(adcp.data$Name)), D.bot = c(1,2,2,2,2,2) , D.sur = c(20, 7, 13, 13, 21, 10))

			pdf(file = '/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/figures/ADCP_mapPlot_Cond3.pdf')
			adcp.mapplot(df.temp, map.df = coastline.df, start.time = start.time, stop.time = stop.time, Bin.range = Bin.range)
			dev.off()


			# pdf(file = '/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/figures/ADCP_mapPlot_june.pdf')
			# adcp.mapplot(adcp.data, start.time = "2016-05-31", stop.time = "2016-06-18",time.step = 60*4, Bin.range = c(1, 10), Bin.step = 9)
			# dev.off()

			# pdf(file = '/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/figures/ADCP_mapPlot_june.pdf')
			# adcp.mapplot(adcp.data, start.time = "2016-05-31", stop.time = "2016-05-31",time.step = 60*12, Bin.range = c(1, 10), Bin.step = 9)
			# dev.off()
