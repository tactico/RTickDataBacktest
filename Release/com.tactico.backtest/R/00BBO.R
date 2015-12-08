#' Best Bid Offer Class
#'
#' Tracks the Best Bid and Offer for an individual exchange
#' @param exchange The Exchange name
#' @return A copy of the BBO object
#' @export
BBO <- setClass(
  "BBO",

  slots = c(
    exchange = "character",
    bid.price = "numeric",
    ask.price = "numeric",
    bid.size = "integer",
    ask.size = "integer"
  ),

  prototype = list(
    bid.price = 0.0,
    ask.price = 99999.9,
    bid.size = 0L,
    ask.size = 0L
  ))
setMethod("initialize", "BBO", function(.Object, exchange){
  .Object@exchange <- exchange
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
            Obj@bid.price <- bid.price
            Obj@ask.price <- ask.price
            Obj@bid.size <- bid.size
            Obj@ask.size <- ask.size
            return (Obj)
          })



