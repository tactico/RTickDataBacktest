#' Position Tracker
#'
#' Tracks Position information using Trades
#' @param symbol The equity's name
#' @return A copy of the Trade object
#' @export
Position <- setClass(
  "Position",

  slots = c(
    symbol = "character",
    trades = "ANY",
    size = "integer",
    price = "numeric",
    unrealized = "numeric",
    realized = "numeric",
    realizedp = "numeric",
    realizedl = "numeric",
    lrealizedp = "numeric",
    lrealizedl = "numeric",
    srealizedp = "numeric",
    srealizedl = "numeric",
    tradep = "integer",
    tradel = "integer",
    ltradep = "integer",
    ltradel = "integer",
    stradep = "integer",
    stradel = "integer"
  ),

  prototype = list(
    trades = c(),
    size = 0L,
    price = 0.0,
    unrealized = 0.0,
    realized = 0.0,
    realizedp = 0.0,
    realizedl = 0.0,
    lrealizedp = 0.0,
    lrealizedl = 0.0,
    srealizedp = 0.0,
    srealizedl = 0.0,
    tradep = 0L,
    tradel = 0L,
    ltradep = 0L,
    ltradel = 0L,
    stradep = 0L,
    stradel = 0L
  ))
setMethod("initialize", "Position", function(.Object, symbol){
  .Object@symbol <- symbol
  return(.Object)
})

#' Gets the current position held
#' @param Obj Position object
#' @return The position held in the equity
#' @export
setGeneric(name="GetPositionSize", def=function(Obj){standardGeneric("GetPositionSize")})
setMethod(f="GetPositionSize",signature="Position",definition=function(Obj){return(Obj@size)})

#' Gets the current realized profit
#' @param Obj Position object
#' @return The realized profit/loss in the equity
#' @export
setGeneric(name="GetRealized", def=function(Obj){standardGeneric("GetRealized")})
setMethod(f="GetRealized",signature="Position",definition=function(Obj){return(Obj@realized)})

#' Gets the current un-realized profit
#' @param Obj Position object
#' @return The un-realized profit/loss in the equity
#' @export
setGeneric(name="GetUnrealized", def=function(Obj){standardGeneric("GetUnrealized")})
setMethod(f="GetUnrealized",signature="Position",definition=function(Obj){return(Obj@unrealized)})

#' Gets the Cost Price
#' @param Obj Position object
#' @return The Cost Price of the equity
#' @export
setGeneric(name="GetPrice", def=function(Obj){standardGeneric("GetPrice")})
setMethod(f="GetPrice",signature="Position",definition=function(Obj){return(Obj@price)})

#' Calculate PnL
#'
#' Calculates unrealized gains/losses using last price the equity has traded at
#' @param Obj Position object
#' @param price current price the equity is trading at
#' @return A copy of the Position Object
#' @export
setGeneric(name="CalcPl", def=function(Obj,price){standardGeneric("CalcPl")})
setMethod(f="CalcPl",
          signature="Position",
          definition=function(Obj,price){
            Obj@unrealized <- (Obj@size * (price - Obj@price))
            return (Obj)
          })

#' Record Trade
#'
#' Updates position information using the trade information provided
#' @param Obj Position object
#' @param timestamp timestamp of the trade
#' @param size quantity traded
#' @param price price of the trade
#' @return A Copy of the Position Object
#' @export
setGeneric(name="Trade", def=function(Obj, timestamp, size, price){standardGeneric("Trade")})
setMethod(f="Trade",
          signature="Position",
          definition=function(Obj, timestamp, size, price){
            size <- as.integer(size)
            if(size == 0){
              return(Obj)
            }
            Obj@trades <- c(Obj@trades, new("ATrade", Obj@symbol, timestamp, size, price))
            size.left <- size

            if(Obj@size > 0 && size.left < 0){
              size.traded <- min(-1L*size.left, Obj@size)
              profit <- size.traded * (price - Obj@price)
              Obj@realized <- Obj@realized + profit
              if(profit > 0){
                Obj@realizedp <- Obj@realizedp + profit
                Obj@lrealizedp <- Obj@lrealizedp + profit
                Obj@ltradep <- Obj@ltradep + 1L
                Obj@tradep <- Obj@tradep + 1L
              }else{
                Obj@realizedl <- Obj@realizedl + profit
                Obj@lrealizedl <- Obj@lrealizedl + profit
                Obj@ltradel <- Obj@ltradel + 1L
                Obj@tradel <- Obj@tradel + 1L
              }
              Obj@size <- Obj@size - size.traded
              size.left <- size.left + size.traded
            }

            if(Obj@size < 0 && size.left > 0){
              size.traded <- min(size.left, -1L*Obj@size)
              profit <- size.traded *(Obj@price - price)
              Obj@realized <- Obj@realized + profit
              if(profit > 0){
                Obj@realizedp <- Obj@realizedp + profit
                Obj@srealizedp <- Obj@srealizedp + profit
                Obj@stradep <- Obj@stradep + 1L
                Obj@tradep <- Obj@tradep + 1L
              }else{
                Obj@realizedl <- Obj@realizedl + profit
                Obj@srealizedl <- Obj@srealizedl + profit
                Obj@stradel <- Obj@stradel + 1L
                Obj@tradel <- Obj@tradel + 1L
              }
              Obj@size <- Obj@size + size.traded
              size.left <- size.left - size.traded
            }

            if(size.left != 0){
              Obj@price <- (Obj@price * Obj@size + price * size.left)/(Obj@size + size.left)
              Obj@size <- Obj@size + size.left
            }

            Obj <- CalcPl(Obj,price)
            return (Obj)
          })
