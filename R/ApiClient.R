#' This module contains client that wrap requests and response to Knoema API
#' if the parameters "app_id" and "app_secret" are not are not defined, then a public user with a limit of 50 requests is created
#' This class configures knoema api.

#' The class contains fields:

#' host -- the host where kneoma is going to connect

#' app_id -- application id that will have access to knoema.
#' Application should be created by knoema user or administrator

#' app_secret -- code that can be done after application will be created.
#' Should be set up together with app_id

#' @export
#' @importFrom digest hmac
#' @importFrom base64enc base64encode
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr content_type
#' @importFrom httr http_error
#' @importFrom httr http_status

ApiClient <- function(host="knoema.com", app_id = "",app_secret = "") {
  client = list(
    host = host,
    app_id = app_id,
    app_secret = app_secret
  )
  #Add a few more methods

  client$getUrl <- function(apipath){
    return (sprintf("http://%1s/%2s", client$host, apipath))
  }

  client$get_authorization <- function(){
    if (client$app_id != "" || client$app_secret != "")
    {
      key <- format(Sys.time(), "%d-%m-%y-%H", tz = "UCT")
      hash <-  hmac(key, client$app_secret, "sha1", raw = TRUE)
      secrethash <- base64encode(hash)
      auth = sprintf("Knoema %1s:%2s:1.2", client$app_id, secrethash)
      return (auth)
    }
    return (NULL)
  }

  client$apiGet <- function(apipath, query=NULL){
    url <- client$getUrl(apipath)
    if (!is.null(query))
    {
      url <- sprintf("%1s?%2s", url, query)
    }
    auth <- client$get_authorization()
    if (is.null(auth))
      response <- GET(url, add_headers("Content-Type"="application/json"))
    else
      response <- GET(url, add_headers("Content-Type"="application/json", "Authorization"= auth))
    if (http_error(response))
    {
        e <- simpleError(http_status(response)$message)
        stop(e)
    }
    res = content(response, as = "parsed");
    return (res)
  }

  client$apiPost <- function(apipath, request){
    url <- client$getUrl(apipath)
    auth <- client$get_authorization()
    if (is.null(auth))
      p <- POST(url, content_type("application/json"), body = request, encode = "json")
    else
      p <- POST(url,content_type("application/json"),add_headers("Authorization"= auth), body = request,encode = "json")
    if (http_error(p))
    {
        e <- simpleError(http_status(p)$message)
        stop(e)
    }
    res <- content(p, as = "parsed")
    return(res)
  }

  #The method is getting information about dataset byt it's id
  client$get_dataset <- function(datasetId){
    path <- 'api/1.0/meta/dataset/%1s'
    return (client$apiGet(sprintf(path, datasetId)))
  }

  #The method is getting information about dimension with items
  client$get_dimension <- function(datasetId, dimensionName){
    path <- 'api/1.0/meta/dataset/%1s/dimension/%2s'
    return (client$apiGet(sprintf(path, datasetId, dimensionName)))
  }

  #The method is getting data by pivot request
  client$get_data <- function (pivotrequest){
    path <- 'api/1.0/data/pivot'
    return (client$apiPost(path, pivotrequest))
  }

  client <- list2env(client)
  class(client) <- "ApiClient"
  return(client)
}
