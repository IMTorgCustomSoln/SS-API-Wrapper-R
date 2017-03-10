#' ScrumSaga API Wrapper
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords api
#' @export
#' @examples
#' Account()



library(httr)
library(jsonlite)





Account <- function(email, password){
    
    thisEnv <- environment()
    
    URL = "http://api.scrumsaga.com/v1"
    URL_LOCAL = '144.76.39.53:3001/v1'
    
    rteLogin = "/login"
    rteAcctData = '/acctData'
    rteAcctDiagram = '/acctDiagram'
    
    EMAIL <- email
    PASSWORD <- password
    TOKEN = 'TOKEN'

      me <- list(
          thisEnv = thisEnv,
          getToken = function(){
              return(get("TOKEN",thisEnv))
          },
          setToken = function(value){
              return(assign("TOKEN",value,thisEnv))
          },
          api_status = function(){
              uri = URL_LOCAL
              resp = GET(uri)
              if (http_type(resp) != "application/json") {
                  stop("API did not return json", call. = FALSE)
              }
              parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
              structure(
                  list(
                      msg = parsed$msg,
                      path = uri,
                      response = resp
                  ),
                  class = "resp"
              )   
          },
          login = function() {
              uri = paste(URL_LOCAL, rteLogin, sep='')
              resp = POST(uri, body = list(email=EMAIL, password=PASSWORD), encode = "json")
              if (http_type(resp) != "application/json") {
                  stop("API did not return json", call. = FALSE)
              }
              parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
              me$setToken( paste('JWT',parsed$token ) )
              structure(
                  list(
                      msg = parsed$msg,
                      path = uri,
                      response = resp
                  ),
                  class = "resp"
              )
          },
          check_data = function() {
              TOKEN = me$getToken()
              uri = paste(URL_LOCAL, rteAcctData, sep='')
              resp = POST(uri, add_headers(Authorization=TOKEN))
              if (http_type(resp) != "application/json") {
                  stop("API did not return json", call. = FALSE)
              }
              parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
              structure(
                  list(
                      msg = parsed$msg,
                      path = uri,
                      response = resp,
                      data = parsed$data
                  ),
                  class = "resp"
              )
          },
          check_diagram = function() {
              TOKEN = me$getToken()
              uri = paste(URL_LOCAL, rteAcctDiagram, sep='')
              resp = POST(uri, add_headers(Authorization=TOKEN))
              if (http_type(resp) != "application/json") {
                  stop("API did not return json", call. = FALSE)
              }
              parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
              structure(
                  list(
                      msg = parsed$msg,
                      path = uri,
                      response = resp,
                      data = parsed$data
                  ),
                  class = "resp"
              )
          }
          
                
      )

      ## Define the value of the list within the current environment.
      assign('this',me,envir=thisEnv)

      ## Set the name for the class
      class(me) <- append(class(me),"Account")
      return(me)
}
