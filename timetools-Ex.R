pkgname <- "timetools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('timetools')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("POSIXcti-class")
### * POSIXcti-class

flush(stderr()); flush(stdout())

### Name: POSIXcti-class
### Title: Class '"POSIXcti"'
### Aliases: POSIXcti-class POSIXcti length,POSIXcti-method
###   show,POSIXcti-method duration,POSIXcti-method start.POSIXcti
###   end.POSIXcti [.POSIXcti [<-.POSIXcti format.POSIXcti print.POSIXcti
###   c.POSIXcti
### Keywords: classes

### ** Examples

showClass("POSIXcti")



cleanEx()
nameEx("POSIXctp-class")
### * POSIXctp-class

flush(stderr()); flush(stdout())

### Name: POSIXctp-class
### Title: Class '"POSIXctp"'
### Aliases: POSIXctp-class POSIXctp length,POSIXctp-method
###   show,POSIXctp-method [.POSIXctp [<-.POSIXctp format.POSIXctp
###   print.POSIXctp c.POSIXctp
### Keywords: classes

### ** Examples

showClass("POSIXctp")



cleanEx()
nameEx("SubtimeDataFrame-class")
### * SubtimeDataFrame-class

flush(stderr()); flush(stdout())

### Name: SubtimeDataFrame-class
### Title: Class '"SubtimeDataFrame"'
### Aliases: SubtimeDataFrame-class $,SubtimeDataFrame-method
###   $<-,SubtimeDataFrame-method [[,SubtimeDataFrame-method
###   dim,SubtimeDataFrame-method lapply,SubtimeDataFrame-method
###   length,SubtimeDataFrame-method names,SubtimeDataFrame-method
###   names<-,SubtimeDataFrame-method ncol,SubtimeDataFrame-method
###   nrow,SubtimeDataFrame-method show,SubtimeDataFrame-method
###   [.SubtimeDataFrame [<-.SubtimeDataFrame [[<-.SubtimeDataFrame
###   as.data.frame.SubtimeDataFrame head.SubtimeDataFrame
###   merge.SubtimeDataFrame plot.SubtimeDataFrame points.SubtimeDataFrame
###   lines.SubtimeDataFrame print.SubtimeDataFrame
###   row.names.SubtimeDataFrame row.names<-.SubtimeDataFrame
###   split.SubtimeDataFrame summary.SubtimeDataFrame tail.SubtimeDataFrame
### Keywords: classes

### ** Examples

showClass("SubtimeDataFrame")



cleanEx()
nameEx("SubtimeDataFrame")
### * SubtimeDataFrame

flush(stderr()); flush(stdout())

### Name: SubtimeDataFrame
### Title: Create a SubtimeDataFrame from scratch
### Aliases: SubtimeDataFrame

### ** Examples

st <- subtime (1:4, 'wday')
SubtimeDataFrame (st, data.frame (test=sample (0:100, 4)))



cleanEx()
nameEx("TimeInstantDataFrame-class")
### * TimeInstantDataFrame-class

flush(stderr()); flush(stdout())

### Name: TimeInstantDataFrame-class
### Title: Class '"TimeInstantDataFrame"'
### Aliases: TimeInstantDataFrame-class $,TimeInstantDataFrame-method
###   $<-,TimeInstantDataFrame-method [[,TimeInstantDataFrame-method
###   dim,TimeInstantDataFrame-method length,TimeInstantDataFrame-method
###   names,TimeInstantDataFrame-method names<-,TimeInstantDataFrame-method
###   ncol,TimeInstantDataFrame-method nrow,TimeInstantDataFrame-method
###   show,TimeInstantDataFrame-method lapply,TimeInstantDataFrame-method
###   [.TimeInstantDataFrame [<-.TimeInstantDataFrame
###   [[<-.TimeInstantDataFrame as.data.frame.TimeInstantDataFrame
###   head.TimeInstantDataFrame merge.TimeInstantDataFrame
###   plot.TimeInstantDataFrame points.TimeInstantDataFrame
###   lines.TimeInstantDataFrame print.TimeInstantDataFrame
###   rbind.TimeInstantDataFrame row.names.TimeInstantDataFrame
###   row.names<-.TimeInstantDataFrame split.TimeInstantDataFrame
###   summary.TimeInstantDataFrame tail.TimeInstantDataFrame
### Keywords: classes

### ** Examples

showClass("TimeInstantDataFrame")



cleanEx()
nameEx("TimeInstantDataFrame")
### * TimeInstantDataFrame

flush(stderr()); flush(stdout())

### Name: TimeInstantDataFrame
### Title: Create a TimeInstantDataFrame from scratch
### Aliases: TimeInstantDataFrame

### ** Examples

TimeInstantDataFrame (
	c('2010-01-01', '2010-02-01'),
	'UTC', data.frame(ex=1:2) )

TimeInstantDataFrame (c('2010-01-01', '2010-02-01', '2010-02-02'), 'UTC')



cleanEx()
nameEx("TimeIntervalDataFrame-class")
### * TimeIntervalDataFrame-class

flush(stderr()); flush(stdout())

### Name: TimeIntervalDataFrame-class
### Title: Class '"TimeIntervalDataFrame"'
### Aliases: TimeIntervalDataFrame-class $,TimeIntervalDataFrame-method
###   $<-,TimeIntervalDataFrame-method [[,TimeIntervalDataFrame-method
###   dim,TimeIntervalDataFrame-method names,TimeIntervalDataFrame-method
###   names<-,TimeIntervalDataFrame-method
###   ncol,TimeIntervalDataFrame-method nrow,TimeIntervalDataFrame-method
###   length,TimeIntervalDataFrame-method
###   lapply,TimeIntervalDataFrame-method show,TimeIntervalDataFrame-method
###   [.TimeIntervalDataFrame [<-.TimeIntervalDataFrame
###   [[<-.TimeIntervalDataFrame as.data.frame.TimeIntervalDataFrame
###   end.TimeIntervalDataFrame head.TimeIntervalDataFrame
###   merge.TimeIntervalDataFrame plot.TimeIntervalDataFrame
###   points.TimeIntervalDataFrame lines.TimeIntervalDataFrame
###   print.TimeIntervalDataFrame rbind.TimeIntervalDataFrame
###   row.names.TimeIntervalDataFrame row.names<-.TimeIntervalDataFrame
###   split.TimeIntervalDataFrame start.TimeIntervalDataFrame
###   summary.TimeIntervalDataFrame tail.TimeIntervalDataFrame
### Keywords: classes

### ** Examples

showClass("TimeIntervalDataFrame")



cleanEx()
nameEx("TimeIntervalDataFrame")
### * TimeIntervalDataFrame

flush(stderr()); flush(stdout())

### Name: TimeIntervalDataFrame
### Title: Create a TimeIntervalDataFrame from scratch
### Aliases: TimeIntervalDataFrame

### ** Examples

TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01'), c('2010-02-01', '2010-02-02'),
	'UTC', data.frame(ex=1:2) )

TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01', '2010-02-02'), NULL,
	'UTC', data.frame(ex=1:2) )



cleanEx()
nameEx("changeSupport")
### * changeSupport

flush(stderr()); flush(stdout())

### Name: changeSupport
### Title: Function to change time support of TimeIntervalDataFrame
### Aliases: changeSupport
###   changeSupport,TimeIntervalDataFrame,character,numeric,ANY,ANY,missing,missing-method
###   changeSupport,TimeIntervalDataFrame,POSIXctp,numeric,ANY,ANY,missing,missing-method
###   changeSupport,TimeIntervalDataFrame,TimeIntervalDataFrame,numeric,ANY,ANY,ANY,ANY-method

### ** Examples

ti3 <- TimeIntervalDataFrame (
       c('2010-01-01', '2010-01-02', '2010-01-04'), NULL,
       'UTC', data.frame(ex3=c(6, 1.5)))

# weighted mean over a period of 3 days with at least 75% of
# coverage (NA is retunr if not)
ti3
d <- POSIXctp(unit='day')
changeSupport (ti3, 3L*d, 0.75)

ti4 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-01-02', '2010-01-04',
	  '2010-01-07', '2010-01-09', '2010-01-10'), NULL,
         'UTC', data.frame(ex4=c(6, 1.5, 5, 3, NA)))

# weighted mean over a period of 3 days with at least 75% of
# coverage (NA is retunr if not) or 50%
ti4
changeSupport (ti4, 3L*d, 0.75)
changeSupport (ti4, 3L*d, 0.5)

# use of split.from
ti1 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour')
ti1$value <- 1:nrow(ti1)
# we can calculate sliding mean over periods of 24 hours.
# first lets build the corresponding TimeIntervalDataFrame
ti2 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour', 'day')
# if we try to 'project' ti1 over ti2 it won't work :
summary (changeSupport (ti1, ti2, 0))
# all data are NA because 'spliting' is not enabled. Let's enable it :
summary (changeSupport (ti1, ti2, 0, split.from=TRUE))



cleanEx()
nameEx("included")
### * included

flush(stderr()); flush(stdout())

### Name: %included%
### Title: test inclusion of 2 'POSIXcti' objects
### Aliases: %included% %included%.POSIXcti

### ** Examples

# to see all existing methods :
methods ('%included%')



cleanEx()
nameEx("intersect")
### * intersect

flush(stderr()); flush(stdout())

### Name: %intersect%
### Title: intersects 2 'POSIXcti' objects
### Aliases: %intersect% %intersect%.POSIXcti

### ** Examples

# to see all existing methods :
methods ('%intersect%')



cleanEx()
nameEx("origin")
### * origin

flush(stderr()); flush(stdout())

### Name: origin
### Title: 1970-01-01 GMT
### Aliases: origin
### Keywords: chron data datasets

### ** Examples

origin
# "1970-01-01 GMT"



cleanEx()
nameEx("timetools-package")
### * timetools-package

flush(stderr()); flush(stdout())

### Name: timetools-package
### Title: provides objects and tools to manipulate irregular heterogeneous
###   time data and subtime data.
### Aliases: timetools-package timetools
### Keywords: package

### ** Examples

ti1 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01'), c('2010-02-01', '2010-02-02'),
	'UTC', data.frame(ex1=1:2) )

ti2 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01', '2010-02-02'), NULL,
	'UTC', data.frame(ex1=1:2) )

all.equal (ti1, ti2)

ti3 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-01-02', '2010-01-04'), NULL,
	'UTC', data.frame(ex3=c(6, 1.5)))

# weighted mean over a period of 3 days with at least 75% of
# coverage (NA is retunr if not)
ti3
d <- POSIXctp(unit='day')
changeSupport (ti3, 3L*d, 0.75)


ti4 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-01-02', '2010-01-04', 
	  '2010-01-07', '2010-01-09', '2010-01-10'), NULL,
	'UTC', data.frame(ex4=c(6, 1.5, 5, 3, NA)))

# weighted mean over a period of 3 days with at least 75% of
# coverage (NA is retunr if not) or 50%
ti4
changeSupport (ti4, 3L*d, 0.75)
changeSupport (ti4, 3L*d, 0.5)





### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
