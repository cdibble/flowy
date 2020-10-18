	
	# Simple Moving Average
		# k = number of time steps; sides = 1 filter coefs are for past vals only. sides = 2, filter coefs are for past and future values.
	mav <- function(x,k, filt.sides = 1){movav <- stats::filter(x, rep(1/k, k), sides = filt.sides); return(movav)}

	# takes any number of data frames as character list and rbinds them.
	cat.df <- function(..., append.name = FALSE){
		# data.list <- as.list(substitute(list(...)))[-1L]
		tm <- proc.time()
		data.list <- list(...)[[1]]
		df.out <- get(data.list[[1]])[0,]
		no.dfs <- length(data.list)
		for(i in 1:no.dfs){
			df <- get(data.list[[i]])
			if(append.name == TRUE) df <- mutate(df, name = as.character(data.list[[i]]))
			df.out <- rbind(df.out, df)
		}
		time <- proc.time() - tm
		print(time)
		return(df.out)
	}
	# Computing Upwelling Index
	# Function to compute upwelling index
	# u and v are zonal and meridional wind velocity componenets
	# u and v are in m/s
	upwell <- function(latitude, u, v){
		coriolis.param <- function(latitude, omega = 7.292e-5){f<-2*omega*sin(latitude);return(f)} # omega is earth's angular velocity in radians per second
		air.dens <- 1.22 #kg/m^3
		seawater.dens <- 1025 #kg/m^3
		drag.coef <- 1.4e-3 # no dims
	  	Q = (air.dens*drag.coef*sqrt(u^2 + v^2)*v)*(10^3) / (coriolis.param(latitude)*seawater.dens)
	  return(-Q)
	}
	# Convert wind speed in m/s to wind drag in N/m^2 (= Pascals)
		# c_d is the drag coefficient; varies with sea-state
		# ro_air is air density
	wind_drag <- function(u, c_d = 0.0013, ro_air = 1.22){
		tau <- c_d * ro_air * u^2 # c_d = drag coef; ro_air = air dens (kg/m^3)
		u_sign <- sign(u)
		tau <- u_sign * tau
		tau
	}
	# interpolate series using linear interpolation
		# this function takes as input a numeric vector with NA values and appends those NA values with the mean of the nearest neighbors
	# interpolate series
	linterp <- function(series, max.gap = 100){
		na.rows <- which(is.na(series))
		index.skip <- 0
		for(i in 1:length(na.rows)){
			if(length(na.rows) > 0){
				if(i > index.skip){
					start.index <- max(which(!is.na(series[1:na.rows[i]])))
					stop.index <- min(which(!is.na(series[na.rows[i]:length(series)]))) + na.rows[i] - 1
					if(stop.index - start.index > max.gap){
						print(paste0("Warning: will not interpolate more than max.gap time steps, in this case: ", stop.index - start.index))
						index.skip = stop.index
					}
					if(stop.index - start.index <= max.gap){
						index.skip = i - 1
						series[na.rows[i]] <- mean(c(series[start.index],
						series[stop.index]), na.rm = TRUE)
					}
				}
			}
		}
		return(series)
	}
	# Plot a frequency power spectrum - shows the amount of 'power' in a time series at various frequenceis
		# can take as input a data frame with columns "Freq" and "Amp" representing a range of frequencies and their respective amplitdues in the power spectrum
			# OR it can take a numeric vector that is the value returned from a fast-fourier-transform (i.e spec.plot(fft(your_unbroken_time_series), is.fft = TRUE) works nice)
	spec.plot <- function(df, is.fft = FALSE, f.min = 0, f.max = NULL, f.acq = 1, period = TRUE, time.units = "minutes", title = NULL, no.labs = 40){
		require(reshape2);require(dplyr)
		# is.fft : is the data frame input the output of an fft()? If so, spec.plot() appends frequencies and computes amplitudes; If not, spec.plot() looks for frequencies and amplitdues in the input df
		# f.min : minimum frequency to display on plot
		# f.max : maximum frequency to display on plot, if less than max frequency in series
		# f.acq : data acquisition frequency in minutes (default = 1, which corresponds to a period of 1 minute)
		if(is.fft == TRUE){
			spec.df <- data.frame("Freq" = 0:(length(df)-1)*1/(length(df)*f.acq), # Total record length used to compute frequency, so units are minutes and freq = 0.5 corresponds to period of 2 minutes.
				"Amp" = Mod(df)) # frequency values (integer array * fundamental frequency) and amplitdues (Mod of fft output)
			spec.df.m <- melt(spec.df, id.vars = "Freq")
		}else{spec.df.m <- df}

		if(is.null(f.max))f.max <- nrow(spec.df.m)*1/(nrow(spec.df.m)*f.acq)

		p <- ggplot(spec.df.m, aes(x = Freq, y = value, color = variable)) + geom_bar(stat = "identity", alpha = 0.5) +
			facet_grid(variable~.) + theme_bw() + theme(axis.text.x = element_text(angle = 90, margin = margin(t = 1, r = 2, b = 0, l = 0), size = 7)) + ylab("Amplitude")
		
		if(is.null(title)){p <- p + ggtitle("Amplitude Spectrum")}
		else(p <- p + ggtitle(paste0(title)))

		if(period == FALSE){p <- p + scale_x_continuous(limits = c(f.min, f.max), breaks = c(seq(f.min, f.max, length.out = no.labs)),
		 	labels = c(signif(seq(f.min, f.max, length.out = no.labs),4))) + xlab("Frequency (1/min)")}

		if(period == TRUE & time.units == "minutes"){p <- p + scale_x_continuous(limits = c(f.min, f.max), breaks = c(seq(f.min, f.max, length.out = no.labs)),
		 	labels = c(signif(1/seq(f.min, f.max, length.out = no.labs),4))) + xlab("Period (min)")
		}
		if(period == TRUE & time.units == "hours"){p <- p + scale_x_continuous(limits = c(f.min, f.max), breaks = c(seq(f.min, f.max, length.out = no.labs)),
		 	labels = c(signif(1/seq(f.min, f.max, length.out = no.labs)/60,4))) + xlab("Period (hours)")
		}
		if(period == TRUE & time.units == "days"){p <- p + scale_x_continuous(limits = c(f.min, f.max), breaks = c(seq(f.min, f.max, length.out = no.labs)),
		 	labels = c(signif(1/seq(f.min, f.max, length.out = no.labs)/60/24,4))) + xlab("Period (days)")
		}

		return(p)
	}

	# Deploy detiding as function
		# The oce::tidem() function does the heavy lifting.
		# you can also apply a butterworth filter with bf = TRUE, which triggers a centering process upon the detided series then, if defaults are kept,
			# a low-pass 6th-order butterworth filter with a frequency-cutoff of 0.0015 is applied both forwards and backwards to minimize phase shift
		# both the detided series and, if bf = TRUE, the detided + butterworth-filtered series are returned with suffixes ".dt" and ".dtbf", respectively
	detide <- function(df, t = "Time", x = "Signal", latitude = 38, bf = FALSE, f.cut = 0.0015, f.type = "low", bf.order = 6){
		tidal.signal <- oce::tidem(t = df[[t]], x = df[[x]], latitude = latitude)
		# var(predict(tidal.signal)) / (var(residuals(tidal.signal[['model']])) + var(predict(tidal.signal)))
		cat("Proportion of total signal variance explained by tidal fit:\n", var(predict(tidal.signal), na.rm = TRUE) /var(adcp.da[which(adcp.da$Name == unique(adcp.da$Name)[1]),]$Amag, na.rm = TRUE), "\n")
		df[[paste0(x, '.dt')]] <- df[[x]] - predict(tidal.signal)
		cat("Mean of signal:", mean(df[[x]], na.rm = TRUE), "\n Mean of tidal signal: ", mean(predict(tidal.signal), na.rm = TRUE), "\n")
		if(bf == TRUE){
			cat("\nButterworth filter is on de-meaned series with 5% two-tailed taper\n")
			bf.filt <- signal::butter(bf.order, f.cut, type = f.type)
			ser.demean <- df[[paste0(x, '.dt')]] - mean(df[[paste0(x, '.dt')]], na.rm = TRUE)
			# ser.std <- ser.demean / sd(ser.demean)
			df[[paste0(x,'.dtbf')]] <- NA
			df[!is.na(ser.demean) , paste0(x,'.dtbf')] <-  signal::filtfilt(bf.filt,
				stats::spec.taper(ser.demean[!is.na(ser.demean)] , p = 0.05))
		}
		return(df)
	}


	boon.wind.getter <- function(dff, time = "Time"){
		if(length(unique(format(dff[[time]], '%Y'))) > 1){cat("There is more than one year in the data and the function is not yet set up to deal with that. Pass it one year of data at a time and then rbind the resulting data frames")}
		    doc.sp <- paste0("http://boon.ucdavis.edu/data/bml/wind_speed/bml_wind_speed_", format(dff[[time]][1], '%Y'), "_hourly.csv") # define url with adcp.data timestamp inserted year
		    # print(doc.sp); print(dff[[time]])
		    doc.dir <- paste0("http://boon.ucdavis.edu/data/bml/wind_direction/bml_wind_direction_", format(dff[[time]][1], '%Y'), "_hourly.csv") # define url with adcp.data timestamp inserted year
		    # print(doc.dir)
		    wind.sp <- read.csv(url(doc.sp)) # connect to web and get wind speeds
		        wind.sp <- wind.sp[,-c(1,3)] # drop erroneous columns
		    wind.dir <- read.csv(url(doc.dir)) # connect to web and get wind directions
		        wind.dir <- wind.dir[,-c(1,3)] # drop erroneous columns
		    wind.data <- cbind(wind.dir, wind.sp[match(wind.dir[,1], wind.sp[,1]), 2]) # combine by matching date/time stamps
		    names(wind.data) <- c("Time.UTC", "dir","spe") # rename columns
		    wind.data$Time.UTC <- as.POSIXct(as.character(wind.data$Time.UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # convert time stamps to posix
		    wind.data$Time.com <- as.POSIXct(format(wind.data$Time.UTC, tz = "America/Los_Angeles"), origin = "1970-01-01")
		    wind.data$Hourly <- wind.data$Time.com
		    wind.data$spe <- wind.data$spe * 0.44704 # convert from mph to m/s
		    wind.data <- dplyr::filter(wind.data, Time.com <= max(dff[[time]], na.rm =TRUE), Time.com >= min(dff[[time]], na.rm = TRUE))
		return(wind.data)
	}

	t.dx <- function(df, start.loc, stop.loc, Name = "Name", Lat = "Lat", Lon = "Lon"){
		# Computes the distance between two points in a data frame, start.loc and stop.loc, each with a unique 'Name'
		require(maptools)# readShapeLines ft to read shape file of shoreline; loads depending package 'sp' which includes spatial object tools
		require(rgeos) # readWKT(), gDistanec()
		require(rgdal) # spTransform() methods; loaded as dependent by sp, which is dependent in maptools
		require(geosphere) # bearing() and bearingRhumb()
		# WGS 84 Proj4 CRS projection string:
		wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
		# Planar Proj4 CRS Projection string:
		epsg.2767 <- "+proj=lcc +lat_1=39.83333333333334 +lat_2=38.33333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000 +ellps=GRS80 +units=m +no_defs"
		# pull out Lat Lon in well known text format and make a spatial object in WGS84 projection
		LLstrings.start <- readWKT(paste('POINT(', df[[Lon]][which(df[[Name]] == start.loc)], df[[Lat]][which(df[[Name]] == start.loc)],')') , p4s = CRS(wgs.84))
		LLstrings.stop <- readWKT(paste('POINT(', df[[Lon]][which(df[[Name]] == stop.loc)], df[[Lat]][which(df[[Name]] == stop.loc)],')') , p4s = CRS(wgs.84))
		# transfrom Lat.Lon.strings to planar projection
		LLproj.start <- spTransform(LLstrings.start, CRS(epsg.2767))
		LLproj.stop <- spTransform(LLstrings.stop, CRS(epsg.2767))
		# compute distance
		dis <- gDistance(LLproj.start, LLproj.stop)
		# compute direction
		dir <- bearingRhumb(LLstrings.start, LLstrings.stop)
		# adjust distance to include a sign
		if(is.na(dir) == FALSE & dir >= 180 & dir <= 360){dis <- -dis}
		# cat("DIR: ", dir, "\n")
		return(dis)
	}