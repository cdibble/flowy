tide_plot.r

tide.plot <- function(tide.data, mark.time, time.range = 48){
	require(ggplot2); require(dplyr);
	mark.time <- as.POSIXct(mark.time, origin = "1970-01-01")
	start.time <- mark.time - time.range/2*3600
	stop.time <- mark.time + time.range/2*3600
	plot.data <- dplyr::filter(tide.data, Date.Time >= start.time & Date.Time <= stop.time)
	tide.plot <- ggplot(plot.data, aes(x = Date.Time, y = Water.Level)) + geom_line(color = "darkblue") +
		geom_vline(xintercept = as.numeric(mark.time)) + geom_hline(yintercept = 0) +
		scale_y_continuous(limits = c(-0.6, 2.4), expand = c(0,0)) + 
		scale_x_datetime(expand = c(0,0)) + 
		# ggtitle("Pt. Reyes Tides") +
		theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(size = 6, hjust = 0),
			axis.text.y = element_blank(), axis.text.x = element_blank())
	return(tide.plot)
}