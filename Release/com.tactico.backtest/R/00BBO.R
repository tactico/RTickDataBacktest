#' Best Bid Offer Class
#'
#' Tracks the Best Bid and Offer for an individual exchange
#' @param exchange The Exchange name
#' @return A copy of the BBO object
#' @export
BBO <- setClass(
  "BBO",

  slots = c(
    my.exchange = "character",
    my.bid.price = "numeric",
    my.ask.price = "numeric",
    my.bid.size = "integer",
    my.ask.size = "integer"
  ),

  prototype = list(
    my.bid.price = 0.0,
    my.ask.price = 99999.9,
    my.bid.size = 0L,
    my.ask.size = 0L
  ))
setMethod("initialize", "BBO", function(.Object, exchange){
  .Object@my.exchange <- exchange
  return(.Object)
})


#' Set BBO
#'
#' Sets the price and quantity of the Best Bid and Offer of the exchange
#' @param bid.price Bid Price
#' @param ask.price Ask Price
#' @param bid.size Bid Size
#' @param ask.size Ask Size
#' @return Copy of the BBO object
#' @export
setGeneric(name="SetBBO", def=function(Obj, bid.price, ask.price, bid.size, ask.size){standardGeneric("SetBBO")})
setMethod(f="SetBBO",
          signature="BBO",
          definition=function(Obj, bid.price, ask.price, bid.size, ask.size){
            Obj@my.bid.price <- bid.price
            Obj@my.ask.price <- ask.price
            Obj@my.bid.size <- bid.size
            Obj@my.ask.size <- ask.size
            return (Obj)
          })



