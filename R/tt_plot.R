points.TimeIntervalDataFrame <- function (x, y=NULL, cursor=NULL,
					type='p', lty=1:6, lwd=1, pch=1:25, col=NULL, ...) {
	if (is.null(cursor)) {
		if (is.null(y)) y <- names(x)
		y <- data.frame (x[y])

		# individual serial parameters
		lty <- rep (lty, length.out=length(y))
		lwd <- rep (lwd, length.out=length(y))
		pch <- rep (pch, length.out=length(y))
		if (is.null(col))
			col <- sample (colors(), length(y)) else
			col <- rep (col, length.out=length(y))
		
		# plotting
		if (type %in% c('l', 'b', 'o') ) {
			trash <- mapply (segments, y0=y[-nrow(x),,drop=FALSE], y1=y[-1,,drop=FALSE], col=col, lty=lty, lwd=lwd,
				MoreArgs=list(x1=start(x)[-1], x0=end(x)[-nrow(x)]) )
		}
		trash <- mapply (segments, y0=y, col=col, lty=lty, lwd=lwd,
			MoreArgs=list(x0=start(x), x1=end(x)) )
		if (type %in% c('p', 'b', 'o', 'h')) {
			type <- ifelse (type %in% c('b', 'o'), 'p', type)
			trash <- mapply (points, y=y, pch=pch, col=col, ..., MoreArgs=list(x=start(x), type=type))
			trash <- mapply (points, y=y, pch=pch, col=col, ..., MoreArgs=list(x=end(x), type=type))
		}
	} else {
		x <- as.TimeInstantDataFrame (x, cursor=cursor)
		points (x=x, y=y, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
	}
}

lines.TimeIntervalDataFrame <- function (x, y=NULL, cursor=NULL,
					type='l', lty=1:6, lwd=1, pch=1:25, col=NULL, ...)
{
	points (x=x, y=y, cursor=cursor, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
}

plot.TimeIntervalDataFrame <- function (x, y=NULL, cursor=NULL,
					type='p', lty=1:6, lwd=1, pch=1:25, col=NULL,
					xlim=NULL, ylim=NULL,
					log='', main='', sub='', xlab='', ylab='',
					ann=par('ann'), axes=TRUE, asp=NA, ...) {
	if (is.null(cursor))
	{
		if (is.null(y)) y <- names(x)
		y <- data.frame (x[y])

		# overal graph parameters
		if (is.null(xlim)) xlim <- range(c(start(x), end(x)))
		if (is.null(ylim)) ylim <- compute.lim (unlist(y), na.rm=TRUE)

		# prepare plot
		plot (NA, xlim=xlim, ylim=ylim, log=log, main=main, sub=sub,
		      xlab=xlab, ylab=ylab, ann=ann, axes=FALSE, asp=asp, ...)
		if (axes) {
			axis(2)
			axis.POSIXct (1, c(start(x), end(x)) ) 
			box()
		}
		points (x=x, y=names(y), cursor=cursor, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
	} else {
		x <- as.TimeInstantDataFrame (x, cursor=cursor)
		plot (x=x, y=y, type=type, lty=lty, lwd=lwd, pch=pch, col=col,
		      xlim=xlim, ylim=ylim, log=log,
		      main=main, sub=sub, xlab=xlab, ylab=ylab, ann=ann, axes=axes, asp=asp, ...)
	}
}

points.TimeInstantDataFrame <- function (x, y=NULL,
					type='p', lty=1:6, lwd=1, pch=1:25, col=NULL, ...) {
	if (is.null(y)) y <- names(x)
	y <- data.frame (x[y])

	# individual serial parameters
	type <- rep (type, length.out=length(y))
	lty <- rep (lty, length.out=length(y))
	lwd <- rep (lwd, length.out=length(y))
	pch <- rep (pch, length.out=length(y))
	if (is.null(col))
		col <- sample (colors(), length(y)) else
		col <- rep (col, length.out=length(y))
	
	# plotting
	trash <- mapply (points, y=y, pch=pch, col=col, type=type, lty=lty, lwd=lwd, ..., MoreArgs=list(x=when(x)))
}

lines.TimeInstantDataFrame <- function (x, y=NULL,
					type='l', lty=1:6, lwd=1, pch=1:25, col=NULL, ...) {
	points (x=x, y=y, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
}

plot.TimeInstantDataFrame <- function (x, y=NULL,
					type='p', lty=1:6, lwd=1, pch=1:25, col=NULL,
					xlim=NULL, ylim=NULL,
					log='', main='', sub='', xlab='', ylab='',
					ann=par('ann'), axes=TRUE, asp=NA, ...)
{
	if (is.null(y)) y <- names(x)
	y <- data.frame (x[y])

	# overal graph parameters
	if (is.null(xlim)) xlim <- range(when(x))
	if (is.null(ylim)) ylim <- compute.lim (unlist(y), na.rm=TRUE)

	# prepare plot
	plot (NA, xlim=xlim, ylim=ylim, log=log, main=main, sub=sub,
	      xlab=xlab, ylab=ylab, ann=ann, axes=FALSE, asp=asp, ...)
	if (axes) {
		axis(2)
		axis.POSIXct (1, when(x)) 
		box()
	}
	points (x=x, y=names(y), type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
}


points.SubtimeDataFrame <- function (x, y=NULL,
					type='p', lty=1:6, lwd=1, pch=1:25, col=NULL, ...) {
	if (is.null(y)) y <- names(x)
	y <- data.frame (x[y])

	# individual serial parameters
	type <- rep (type, length.out=length(y))
	lty <- rep (lty, length.out=length(y))
	lwd <- rep (lwd, length.out=length(y))
	pch <- rep (pch, length.out=length(y))
	if (is.null(col))
		col <- sample (colors(), length(y)) else
		col <- rep (col, length.out=length(y))
	
	# plotting
	trash <- mapply (points, y=y, pch=pch, col=col, type=type, ..., MoreArgs=list(x=when(x)))
}

lines.SubtimeDataFrame <- function (x, y=NULL,
					type='l', lty=1:6, lwd=1, pch=1:25, col=NULL, ...) {
	points (x=x, y=y, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
}

plot.SubtimeDataFrame <- function (x, y=NULL,
					type='p', lty=1:6, lwd=1, pch=1:25, col=NULL,
					xlim=NULL, ylim=NULL,
					log='', main='', sub='', xlab='', ylab='',
					ann=par('ann'), axes=TRUE, asp=NA, ...)
{
	if (is.null(y)) y <- names(x)
	y <- data.frame (x[y])

	# overal graph parameters
	if (is.null(xlim)) xlim <- range(as.numeric(when(x)))
	if (is.null(ylim)) ylim <- compute.lim (unlist(y), na.rm=TRUE)

	# prepare plot
	plot (NA, xlim=xlim, ylim=ylim, log=log, main=main, sub=sub,
	      xlab=xlab, ylab=ylab, ann=ann, axes=FALSE, asp=asp, ...)
	if (axes) {
		axis(2)
		axis(1, when(x), labels=as.character(when(x)))
		box()
	}
	points (x=x, y=names(y), type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
}
