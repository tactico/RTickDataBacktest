#' DataFetcher Class
#'
#' Fetches Data
#' @param data.source Data Source
#' @param username UserName
#' @param password Password
#' @return .Object Copy of the created object
#' @export
DataFetcherObject <- function(data.source,username,password){
  library("rJava")
  .jinit(".\\JavaBacktestPackage.jar")
  if(username == "" && password == ""){
    obj <- .jnew("com.tactico.backtest.DataFetcher", data.source)

  }else{
    obj <- .jnew("com.tactico.backtest.DataFetcher", data.source, username, password, paste(getwd(), .Platform$file.sep, "data", sep=""))
  }
  GetDirectory <-function(){
    return(.jcall(obj,"S","getDirectory"))
  }
  setDirectory <-function(directory){
    return(.jcall(obj,"V","setDirectory",directory))
  }
  GetUsername <-function(){
    return(.jcall(obj,"S","getUsername"))
  }
  SetUsername <-function(username){
    return(.jcall(obj,"V","setUsername",username))
  }
  GetPassword <-function(){
    return(.jcall(obj,"S","getPassword"))
  }
  SetPassword <-function(password){
    return(.jcall(obj,"V","setPassword",password))
  }

  GetData <-function(symbol,exchange,start.date,end.date,granularity){
    return(gsub("[\\]", .Platform$file.sep, .jcall(obj,"[S","getDataToR",symbol,exchange,start.date,end.date,as.integer(granularity))))
  }

  list(GetDirectory = GetDirectory, setDirectory = setDirectory, GetUsername = GetUsername,
       SetUsername = SetUsername, GetPassword = GetPassword, SetPassword = SetPassword, GetData = GetData)
}
