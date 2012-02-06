# detach (package:timetools, unload=TRUE)
# detach (package:Qair, unload=TRUE)
# library (timetools)
# library (maptools)
# library (RJDBC)
# library (Qair, lib.loc='/home/vladislav/lib/R/library/')
# 
# xr.db <- xrConnect()
# 
# un <- xrGetContinuousData (xr.db, 'VER', '2010-01-01', '2010-12-31', polluants='NO2')
# deux <- changeTimeIntervalSupport (un, 'day', 0.75)
# system.time (deux <- changeTimeIntervalSupport (un, 'day', 0.75) )
# deux[names(deux)] <- round.a (data.frame (deux) )
# 
# int.from <- when (un)
# int.to <- when (deux)
# 
