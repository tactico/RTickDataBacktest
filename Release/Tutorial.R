


# You need the rJava Package
install.packages("rJava")
library("rJava")

# Then install the tactico package by going in packages/install and then browsing to the "com.tactico.backtest_0.1.tar" file.
# Then load that library
library(com.tactico.backtest)


######################################################################
#         Trade Class tests
######################################################################

# This is a trade object, used to hold the symbol, time, size and price of a trade
# Notice that to create a new object you need to call the "new" function and give it
# the string name of the object you want to create as the function's first argument.
trade <- new("ATrade","ERH",as.POSIXlt("2014-01-02 09:50:00"),100L,12.12)

# you can then call the function ToString on the trade object to get it's string representation
ToString(trade)



######################################################################
#         Position Class tests
######################################################################

# The position object is used to track our current position in any equity.
# In this case we create a new position object for the symbol "ERH"
position <- new("Position","ERH")

# We can then use the Trade function on the position object to update our current position.
# Notice that when we call a function that changes the object that it's called on, we need to
# set the function's return value to the object (the function returns a copy of the changed object)
position <- Trade(position, as.POSIXlt("2014-01-02 09:50:00"), 100L, 12.12)
GetPosition(position)

position <- Trade(position, as.POSIXlt("2014-01-02 09:55:00"), 300L, 12.49)
GetPosition(position)
GetRealized(position)
GetUnrealized(position)

position <- Trade(position, as.POSIXlt("2014-01-02 10:00:00"), -500L, 12.55)

# You see your current position, realized and unrealized gains using these functions.
# The list of all available functions is available in the package documentation.
GetPosition(position)
GetRealized(position)
GetUnrealized(position)



###############################################################
#       BarTimeSeries Tests
###############################################################

# the DataFetcher object is used to get data from TickData.com
# users need to input their username and password to get the data.
username <- ""
password <- ""
datafetcher <- DataFetcherObject("tickdata",username,password)

# Unlike all other objects the Data fetcher object is written in java,
# thus when calling this object use the "$" symbol followed by the function name.
# the GetData function will return a list of the file paths of all downloaded symbols.
data.files <- datafetcher$GetData("MHD", "US", "01/03/2014", "09/08/2015", 5)

# we can then use that csv file to create a Bar Timeseries object
bartimeseries <- new("BarTimeSeries", data.files)
while(!LastItem(bartimeseries)){
  # In a Bar Timeseries object (or any other Timeseries object), you can use the function
  # "Peek" to get the value of the next timestamp in our timeseries, (the peek function also
  #  returns a copy of the object in case more data was loaded in the object))
  Peek(bartimeseries)
  nextts <- nextTimeStamp(bartimeseries)
  # Based on that timestamp you can use the function "GoTo" to set your index in the object to
  # that perticular timestamp.
  GoTo(bartimeseries)
  print(ToString(bartimeseries))
}

# this example shows how to use the bartimeseries and the peek/goto functions
# to stream trough two pairs in a synced way
datafetcher <- DataFetcherObject("tickdata", username, password)

data.file1 <- datafetcher$GetData("MHD", "US", "01/03/2014", "01/03/2015", 1)
data.file2<- datafetcher$GetData("MUH", "US", "01/03/2014", "01/03/2015", 1)

bar1 <-  new("BarTimeSeries", data.file1)
bar2 <-  new("BarTimeSeries", data.file2)

while(!LastItem(bar1) ||!LastItem(bar2)){
  Peek(bar1)
  GoTo(bar1)
  Peek(bar2)
  GoTo(bar2)
  print(paste("MHD: ", Timestamp(bar1), Close(bar1), ", MUH: ", Timestamp(bar2), Close(bar2)))
}



###############################################################
#       QuoteTimeSeries Tests
#############################################################
datafetcher <- DataFetcherObject("tickdata", username, password)

# To download minute data the user needs to input the granularity in minutes in the
# GetData function as the last argument. For Quote data granularity needs to be set to -2
# and for Trade data -1.
data.files <- datafetcher$GetData("MHD", "US", "03/01/2013", "03/10/2013", -2)

# the QuoteTimeSeries can be used to stream trough quote data and return information like
# bid/ask sizes and prices, the timestamp or even the exchange the quote was on.
quotetimeseries <- new("QuoteTimeSeries",data.files)
while(!LastItem(quotetimeseries)){
  Peek(quotetimeseries)
  GoTo(quotetimeseries)
  print(paste(Timestamp(quotetimeseries),
              ", BidSize: ", QuoteBidSize(quotetimeseries),", BidPrice: ", QuoteBidPrice(quotetimeseries),
              ", AskPrice: ", QuoteAskPrice(quotetimeseries),", AskSize: ", QuoteAskSize(quotetimeseries)))
}


###############################################################
#       BBO Tests
###############################################################

# The BBO object (best bid offer), can be used to record an exchange's BBO.
# example in this case we can create a BBO object for the exchange "P" (ARCA).
bbo <- new("BBO", "P")
bbo <- SetBBO(bbo, 10.115, 12.13, 100L, 200L)

# we can also create a BBO for Q (nasdaq) and Z (BATS)
bbo2 <- new("BBO", "Q")
bbo2 <- SetBBO(bbo2, 10.12, 12.14, 100L, 200L)
bbo3 <- new("BBO", "Z")
bbo3 <- SetBBO(bbo3, 10.2, 12.21, 2L, 8L)

# using a hashtable you can then get the best bid and offer for each exchange.
# The reason for doing this is that we can use the quote timeseries object, to
# load data in a hashtable, then with that information we can know what the NBBO
# is at all times. In the following example that logic is used
hashtable <- list()
hashtable[[bbo@my.exchange]] <- bbo
hashtable[[bbo2@my.exchange]] <- bbo2
hashtable[[bbo3@my.exchange]] <- bbo3

bbo <- hashtable[[bbo3@my.exchange]]
bbo@my.bid.size
bbo@my.bid.price


###############################################################
#       NBBOTimeSeries
###############################################################
datafetcher <- DataFetcherObject("tickdata", username, password)
data.files <- datafetcher$GetData("MHD", "US", "03/01/2013", "03/10/2013", -2)

# The NBBO Timeseries object uses that logic and builds a NBBO object from the
# quote data. We can then stream trough that object and see the bid/ask price/size
# at any time in the trading day
nbbotimeseries <- new("NBBOTimeSeries",data.files)
while(!LastItem(nbbotimeseries)){
  Peek(nbbotimeseries)
  nextts <- nextTimeStamp(nbbotimeseries) #this is the timestep GoTo will move to
  GoTo(nbbotimeseries)
  print(ToString(nbbotimeseries))
}

########################################################################
#                   BollingerBand Backtest Tests
########################################################################

# The following backtest has been created using the objects viewed above. The Code for
# this strategy is provided so users can use it as a template to build their own backtests.

# The bollinger band strategy will enter a position when the stock hits an outer bollingerband
# set at x standard deviations of the average price (based on the past y periods). It will exit
# the position when the stocks value hits an inner bollinger band set at x - z standard deviation.
# In our example we enter a position when the price hits 2 standard deviations away from the average
# price of the last 20 periods (periods are built on 15 minute bars as per granularity). The positions
# are exited when the price comes back to 1 standard deviation away from the average price (aka moving average).
# We have also set a timeout of 300 periods, meaning that after 300 15minute bars of entering a position,
# if we are still in that position we get rid of it. Lastly when entry mode is set to true, the backtest waits
# untill the next timestamp and if the condition for entry is still met, it enters the possition.
# If emode is set to false, the position is entered as soon as the condition for entry is met.

#Bollinger Band Backtest is defined in  com.tactico.backtest/R/04BollingerBandBacktest.R
#Read there how to define your own strategy (Specifically the Evaluate method which is called at every timestamp)

symbol <- "BAC:US"
start.date <- "01/03/2014"
end.date <- "01/03/2015"
granularity <- 15L
period <- 20L
entry.thres <- 2
exit.thres <- 0.5
timeout <- 300L
entry.mode <- TRUE
shares <- 1000L


bollinger.band.backtest <- new("BollingerBandBacktest", symbol, start.date, end.date, granularity, period,
                         entry.thres , exit.thres, timeout, entry.mode, shares, username, password)
bollinger.band.backtest <- Start(bollinger.band.backtest)











