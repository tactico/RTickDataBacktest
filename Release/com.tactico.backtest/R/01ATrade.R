#' Records a Trade
#'
#' Takes in a symbol, timestamp, quantity and price of a trade.
#' @param symbol The equity's name
#' @param timestamp DateTime value of the trade
#' @param size Quantity traded
#' @param price Price of the transaction
#' @return A copy of the ATrade object
#' @export
ATrade <- setClass(
  "ATrade",

  slots = c(
    symbol = "character",
    timestamp = "POSIXlt",
    size = "integer",
    price = "numeric"
  ))
setMethod("initialize", "ATrade", function(.Object, symbol, timestamp, size, price){
  .Object@symbol <- symbol
  .Object@timestamp <- timestamp
  .Object@size <- as.integer(size)
  .Object@price <- price
  return(.Object)
})


#' String representation of a Trade
#' @param Obj ATrade object
#' @return A String representation of the Timestamp, quantity, symbol and price of the Trade
#' @export
setMethod(f="ToString",
          signature="ATrade",
          definition=function(Obj)
          {
            return (paste(Obj@timestamp, if(Obj@size<0){"SLD"}else{"BOT"}, Obj@size, Obj@symbol, "@", Obj@price))
          })
