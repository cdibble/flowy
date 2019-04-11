adcp_binplot.r
# Function takes the adcp.data frame and plots subsets of the data as line plots.
	# Input: 
		# surf.bin is the surface bin to plot, default is 20% deeper than the shallowest bin
		# bot.bin is the bottom bin to plot over the surface bin. Default is the 3rd bin. 
			#  **** For a single Bin, use bot.bin == surf.bin *****
		# par is the parameter to plot
		# title is a plot title
		# filt = FALSE; apply butterworth filter to series before plotting?
			# but.order = buttworth filter order
			# but.freq = butterworth filter cutoff frequency (Low Pass Filter)
		# start.time/stop.time = time series to plot (can subset df)
		# lin.size = line size for plots (small for unfiltered plot,s but needs to be thicker for filtered plots)
adcp.binplot <- function(df, surf.bin = floor(max(df$Bin)-0.2*max(df$Bin)),	bot.bin = 3, par = "Nor", title = NULL, filt = FALSE, but.order = 6, but.freq = 0.0015, start.time = min(df$Time), stop.time = max(df$Time), lin.size = 0.1, point = FALSE, geom = FALSE){
	require(dplyr);require(ggplot2); require(scales);require(viridis)
	print("Defaults: Surface Bin = 20% from max; Bottom Bin = 3rd from bottom")
	bin.df <- dplyr::filter(df, Bin == surf.bin | Bin == bot.bin, Time >= start.time & Time <= stop.time)
	if(filt == TRUE){
		but.filt <- signal::butter(but.order, but.freq, type = "low") # low-pass fourth-order filter; for attenuation graph at different orders, see: http://www.electronics-tutorials.ws/filter/filter_8.html
		bin.df[,par] <- signal::filter(but.filt, bin.df[,par])
	}
	df.name <- substr(deparse(substitute(df)),1,6)
	if(geom == TRUE){
		p <- geom_line(bin.df, aes_string(x = "Time", y = par))+
			scale_x_datetime(expand = c(0,0), limits = c(start.time, stop.time), breaks = date_breaks(width = "1 week"), minor_breaks = date_breaks(width = "1 day")) +
			xlab("Time")+ylab(paste0(par,"(mm/sec)")) +
			theme_bw() + theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_line(size = 0.75, color = "grey60"), panel.grid.minor = element_line(size = 0.5, color = "grey75")) #+ theme(panel.grid=element_blank())
		}else{
			p <- ggplot(bin.df, aes_string(x = "Time", y = par))+
				scale_x_datetime(expand = c(0,0), limits = c(start.time, stop.time), breaks = date_breaks(width = "1 week"), minor_breaks = date_breaks(width = "1 day")) +
				xlab("Time")+ylab(paste0(par,"(mm/sec)")) +
				theme_bw() + theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_line(size = 0.75, color = "grey60"), panel.grid.minor = element_line(size = 0.5, color = "grey75")) #+ theme(panel.grid=element_blank())
		}
	if(point == FALSE){
		if(length(unique(bin.df$Bin)) > 1){
			p <- p + geom_line(aes(group = factor(Bin),colour=factor(Bin)),alpha=0.2, size = lin.size) +
				scale_colour_manual(name="Bin\n(~m from bottom)",
					labels=c(paste0("Deep Bin: ", bot.bin),
						paste0("Shallow Bin: ", surf.bin)),
					values = c("blue","black"))
			}

		if(length(unique(bin.df$Bin)) == 1){
			p <- p + geom_line(size = lin.size, alpha = 0.75)
		}
	}
	if(point == TRUE){
		if(length(unique(bin.df$Bin)) > 1){
			p <- p + geom_point(aes(group = factor(Bin),colour=factor(Bin)),alpha=0.2, size = lin.size) +
				scale_colour_manual(name="Bin\n(~m from bottom)",
					labels=c(paste0("Deep Bin: ", bot.bin),
						paste0("Shallow Bin: ", surf.bin)),
					values = c("blue","black"))
			}

		if(length(unique(bin.df$Bin)) == 1){
			p <- p + geom_point(size = lin.size, alpha = 0.75)
		}		
	}

	if(is.null(title)){
		p <- p + ggtitle(paste0("Bodega Head ADCP Data: ", df.name))
	}else{p <- p + ggtitle(title)}
	return(p)
}