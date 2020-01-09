###########################################################################
#				         Applied Finance
#     Statistical tools in algorithmic trading
#					          2018/2019
#				          dr Piotr Wojcik
#		University of Warsaw, Faculty of Economic Sciences                
###########################################################################
# 1. Dealing with time series data incl. intraday, data aggregation
###########################################################################

# lets install needed packages
install.packages("xts")   # for working with time series
install.packages("quantmod") # for gettinf Yahoo finance data automatically
install.packages("data.table") # for fread()
install.packages("pryr") # for otype()


# !!!!!!!!!!!!!!!!!
# xts: eXtensible Time Series. Using the xts package
#   http://joshuaulrich.github.io/xts/

# xts Cheat Sheet: Time Series in R
#   https://www.datacamp.com/community/blog/r-xts-cheat-sheet


# and load them into memory
library(xts)
library(quantmod)
library(data.table)
library(pryr)

# setting working directory, e.g. setwd(C:\\research\\data) or setwd(C:/research)
setwd("...")

# importing the data from csv file saved in working directory

quot_aapl <- read.csv("AAPL.csv")

head(quot_aapl)

# lets check the class of the Date column
class(quot_aapl$Date)

# lets check structure of the whole dataset
str(quot_aapl)

# Date column is not a date! It is stored as a factor column (qualitative variable)
# read.csv() function by default treats all string columns as factors

# lets change it by importing the data again with an additional parameter

quot_aapl <- read.csv("AAPL.csv", 
                      stringsAsFactors = FALSE)

class(quot_aapl$Date)

# now it is a character column
# lets transform it into date

quot_aapl$Date <- as.Date(quot_aapl$Date, 
                          format = "%Y-%m-%d")

# we have to give the format in which date is originally stored:
# %y means 2-digit year, 
# %Y means 4-digit year
# %m means a month
# %d means a day

class(quot_aapl$Date)

head(quot_aapl)

# looks the same, but is not the same - now R understands this column as dates

# lets convert the dataset to xts object

quot_aapl <- xts(quot_aapl[,-1], # data columns 
                 quot_aapl$Date) # date/time index


# lets check the result

str(quot_aapl)

head(quot_aapl)


#-------------------------------------------------------------------------
# DAILY data can be obtained in R directly from yahoo finance

# we will import General Electric's quotations since 1.01.2005

getSymbols("GE",
           from = "2005-01-01")

# it creates an object called GE (the same as the ticker)

# if one wants to control the name of the output object

rm(GE)

quot_ge <- getSymbols("GE", 
                      from = "2005-01-01",
                      to = "2018-10-09",
                      auto.assign = FALSE) 

# the data is automatically stored as xts object
str(quot_ge)

head(quot_ge)
tail(quot_ge)


# plot an xts object

plot(quot_ge)

# all columns are plotted by default
# - lets plot a single column and 
#   use some additional options

plot(quot_ge$GE.Adjusted,
     main = "Quotations of General Electric \n 2005-2018", # \n means a line break
     col = "darkblue",
     major.ticks = "years",   # frequency of ticks on x axis
     grid.ticks.on = "years", # frequency of gridlines on x axis
     grid.ticks.lty = 3,      # type of line for gridlines
     cex = 1.2)               # size of the text labels

# if you wich to always see English names on months, 
# you can set the appropriate system option

sessionInfo()

# the option LC_TIME is responsible for that

# lets change it
Sys.setlocale("LC_TIME", "English")
# below variant should also work
Sys.setlocale("LC_TIME", "C")

# and run the above plot function once again


# lets plot each series on a separate graph

plot(quot_ge,
     multi.panel = 1, # controlled by this argument
     main = "Quotations of GE in 2005-2018",
     col = c("#E41A1C", "#377EB8", "#4DAF4A", 
            "#984EA3", "brown", "gold"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     yaxis.same = F, # otherwise scale is the same for each column!
     cex = 1)

# more flexibility with the layout() function 

# lets define a a 3x2 panel (3 rows x 2 columns)

# this could be done by defining a matrix
# indicating where to put plots

matrix(1:6, nrow = 3, ncol = 2)

# and using it within layout() function

layout(matrix(1:6, nrow = 3, ncol = 2))

plot(quot_ge,
     multi.panel = 1,
     main = "", 
     col = c("#E41A1C", "#377EB8", "#4DAF4A", 
             "#984EA3", "brown", "gold"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     legend.loc = "bottomright",
     yaxis.same = FALSE, # do not use the same scale for all plots
     cex = 1)
# in the end lets restore the default layout
layout(matrix(1))


#----------------------------------------------------------
# load data of higher frequency

# sample data from Dukascopy

# the data are generated by the teacher from the website:
# https://www.dukascopy.com/swiss/english/marketwatch/historical/
# and saved as csv files on the course website

# To access the data directly from Dukascopy one needs to set up
# a free Dukascopy account

# XAUUSD (gold) ticks for a single day

# lets use a fact fread() function from data.table package

XAUUSD.ticks <- fread("http://wne.uw.edu.pl/pwojcik/hfd/XAUUSD_Ticks_09.10.2017-09.10.2017.csv",
                      stringsAsFactors = FALSE)

# lets see the range of the data
head(XAUUSD.ticks)

str(XAUUSD.ticks)

# as you saw above, the Gmt.time column includes 
# information about miliseconds

# to operate on the data easier, lets convert
# a data.table into a simple data.frame

XAUUSD.ticks <- data.frame(XAUUSD.ticks)

# lets see how to convert such data to a correct POSIX column

# we save few first datetimes for an example

(time_ <- XAUUSD.ticks$Gmt.time[1:5])

# if we wanted to keep time only up to seconds,
# we may use similar format as before
# and function strptime() instead of as.Date()

(time_2 <- strptime(time_, 
                    format = "%d.%m.%Y %H:%M:%S", 
                    tz = "GMT")) # time zone info can be added

# to show miliseconds in the index, we need to use
# %OS instead of %S

(time_3 <- strptime(time_, 
                    format = "%d.%m.%Y %H:%M:%OS", 
                    tz = "GMT"))

# the result looks the same, but miliseconds are stored here

# to see milliseconds we need to change the digits.secs option
# (it defines how many digits of a subsecond will be shown)

options(digits.secs = 3)

# compare datetime values again
time_3

time_2

# now the difference is clear

# lets format the whole Gmt.time column appropriately
XAUUSD.ticks$Gmt.time <- strptime(XAUUSD.ticks$Gmt.time, 
                                  format = "%d.%m.%Y %H:%M:%OS",
                                  tz = "GMT")

str(XAUUSD.ticks)

# change to xts
XAUUSD.ticks.xts <- xts(XAUUSD.ticks[,-1], 
                        XAUUSD.ticks$Gmt.time, 
                        tzone = "GMT")

head(XAUUSD.ticks.xts)

# lets compare the size of a data.frame object and xts
# including the same data

object_size(XAUUSD.ticks)
object_size(XAUUSD.ticks.xts)

# xts is half as large

# lets remove the unneeded data.frame

rm(XAUUSD.ticks)

# we can also run the garbage collector
gc()

# and finally make a plot
plot(XAUUSD.ticks.xts,
     multi.panel = 4,
     yaxis.same = FALSE)


#--------------------------------------------------------------
# 1-minute data (bid and ask separately) for 2 months

XAUUSD.1m.bid <- fread("http://wne.uw.edu.pl/pwojcik/hfd/XAGUSD_Candlestick_1_m_BID_01.01.2017-14.10.2017.csv")

XAUUSD.1m.ask <- fread("http://wne.uw.edu.pl/pwojcik/hfd/XAGUSD_Candlestick_1_m_ASK_01.01.2017-14.10.2017.csv")

# convert to simple data.frames
XAUUSD.1m.bid <- data.frame(XAUUSD.1m.bid)
XAUUSD.1m.ask <- data.frame(XAUUSD.1m.ask)

# lets see the range of the data
head(XAUUSD.1m.bid)
tail(XAUUSD.1m.bid)

head(XAUUSD.1m.ask)
tail(XAUUSD.1m.ask)

# here seconds and miliseconds are 0, but are included 
# in the Gmt.time column - we need to take this into account
# during conversion to POSIX format

# lets format the whole Gmt.time column appropriately
XAUUSD.1m.bid$Gmt.time <- strptime(XAUUSD.1m.bid$Gmt.time, 
                                   format = "%d.%m.%Y %H:%M:%OS", # miliseconds
                                   tz = "GMT")

XAUUSD.1m.ask$Gmt.time <- strptime(XAUUSD.1m.ask$Gmt.time, 
                                   format = "%d.%m.%Y %H:%M:%OS", # miliseconds
                                   tz = "GMT")


# change both to xts - lets keep just the close price
# (last ask/bid) for each interval

XAUUSD.1m.bid.xts <- xts(XAUUSD.1m.bid$Close,
                         XAUUSD.1m.bid$Gmt.time,
                         tzone = "GMT")

XAUUSD.1m.ask.xts <- xts(XAUUSD.1m.ask$Close,
                         XAUUSD.1m.ask$Gmt.time,
                         tzone = "GMT")

# lets put the data for bid and ask together

XAUUSD.1m.xts <- merge(XAUUSD.1m.bid.xts, XAUUSD.1m.ask.xts)

# remove objects which are not needed any more

rm(XAUUSD.1m.bid.xts, XAUUSD.1m.ask.xts,
   XAUUSD.1m.bid, XAUUSD.1m.ask)

# and empty the garbage collector (it cleans memory)
gc()

head(XAUUSD.1m.xts)
tail(XAUUSD.1m.xts)

# here miliseconds are not shown as they are equal to 0

names(XAUUSD.1m.xts) <- c("bid", "ask")

# and the plot

plot(XAUUSD.1m.xts,
     multi.panel = 2,
     yaxis.same = FALSE,
     legend.loc = "topleft")


################################################################
# aggregating the data to lower frequency - eg. to hours

# method for an xts object 
# WARNING! It has to be in a form of OHLC, ie.
# include columns called: open, high, low, close
# and optionally a column called volume
# IN THIS PARTICULAR ORDER !!

XAUUSD.1m_hourly <- to.hourly(XAUUSD.1m.xts)

head(XAUUSD.1m_hourly)

str(XAUUSD.1m_hourly)

# the results is also an xts object

#### lets aggregate the data to 5-min

XAUUSD.1m_5min <- to.minutes(XAUUSD.1m.xts, k = 5)

head(XAUUSD.1m_5min)

# there are wrappers:

# to.minutes(x, k)
# to.minutes3(x)
# to.minutes5(x)
# to.minutes10(x)
# to.minutes15(x)
# to.minutes30(x)
# to.hourly(x)
# to.daily(x, drop.time = TRUE)
# to.weekly(x, drop.time = TRUE)
# to.monthly(x)
# to.quarterly(x)
# to.yearly(x, drop.time = TRUE)

# or more general function
# to.period(x, 
#           period = 'seconds', # or "minutes", "hours", "days", "weeks",
#           k = 1,              #      "months", "quarters", "years"
#           OHLC = FALSE)  # do you want the result as Open, High, Low, Close?




#####################################################################
### Exercises

# Exercise 1.1
# Import quotations for three different companies (eg. MSFT, KO, PEP)
# from yahoo finance. Merge together all close prices into one xts
# object and plot them on one graph.




# Exercise 1.2
# sample data from truefx for EURGBP
# here sample for 1 week 22-29.09.2017

# Import the data from "http://wne.uw.edu.pl/pwojcik/hfd/EURGBP-2017-09.csv"
# CAUTION! there are no column names in the first row

# Assign column names: symbol, date, time, bid, ask.
# Create a correct date-time index and convert to xts.
# Compare the size of a data.frame and xts object.
# Play with different plots of the data.




# Exercise 1.3
# Aggregate the data for EURGBP to:
# - 15 sec data
# - 3 min data
# - 2 hourly data





# Exercise 1.4 (*)
# sample file from Bossa (PKOBP)
# Import the data from "http://wne.uw.edu.pl/pwojcik/hfd/PKOBP.prn"
# name the columns: ticker, null, date, time, open, high, low, 
#                   close, volume, null2

# create a correct date-time index and convert to xts
# Play with different plots of the data.


