#' ScrumSaga API Wrapper
#'
#' Container and associated functionaity for Projects
#' @param 
#' @keywords api
#' @import jsonlite
#' @import httr
#' @export
#' @examples
#' Project()







Project <- function(account, repo_namespace, repo_email, repo_name){
    
    thisEnv <- environment()
    
    # system (class) config
    argGroups = c('project', 'size', 'author', 'entity_structure', 'entity_characteristic', 'complexity', 'relation', 'error','quality','tag','process_log')
    argSelection = c('current', 'all')
    # API paths
    rtePathLoadGroup = '/load/'
    rtePathExtract = '/extract'
    
    ACCT = account
    PAYLOAD=list(
        namespace = repo_namespace,
        email = repo_email,
        repo = repo_name
    )

      me <- list(
          thisEnv = thisEnv,
          getGroups = function(){
              return(get("argGroups",thisEnv))
          },
          getAcct = function(){
              return(get("ACCT",thisEnv))
          }, 
          getPayload = function(){
              return(get("PAYLOAD",thisEnv))
          },
          extract = function(select='all'){
              ACCT = me$getAcct()
              PAYLOAD = me$getPayload()
              PAYLOAD$selection = select
              TOKEN = ACCT$getToken()
              rteExtractData = 'extract'
              uri = paste( ACCT$getUrl(), rteExtractData, sep='/')
              resp = POST(uri, add_headers(Authorization=TOKEN), body=PAYLOAD , encode = "json")
              if (http_type(resp) != "application/json") {
                  stop("API did not return json", call. = FALSE)
              }
              parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
              structure(
                  list(
                      msg = parsed$message,
                      path = uri,
                      response = resp,
                      data = parsed$data
                  ),
                  class = "resp"
              )
          },
          load_group = function(group='project') {
              ACCT=me$getAcct()
              PAYLOAD=me$getPayload()
              TOKEN = ACCT$getToken()
              rteLoadData = 'load'
              uri = paste( ACCT$getUrl(), rteLoadData, group, sep='/')
              resp = POST(uri, add_headers(Authorization=TOKEN), body=PAYLOAD , encode = "json")
              if (http_type(resp) != "application/json") {
                  stop("API did not return json", call. = FALSE)
              }
              data = data.frame(jsonlite::fromJSON(content(resp,'text'))$data)
            return(data)
          }
        
      )

      ## Define the value of the list within the current environment.
      assign('this',me,envir=thisEnv)

      ## Set the name for the class
      class(me) <- append(class(me),"Project")
      return(me)
}
