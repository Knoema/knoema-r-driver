# This module contains client that wrap requests and response to Knoema API
# if the parameters "client.id" and "client.secret" are not defined, then a public user with a limit of 50 requests is created
# This class configures knoema api.

# The class contains fields:

# host -- the host where knoema is going to connect
# The following parameters can be obtained from the knoema's application.
# Application should be created by knoema user or administrator after registering on the site knoema.com, in the section "My profile - Apps - create new".
#
# client.id --  client id.
# client.secret -- client secret code.
# client.id  and client.secret should be set up together.


#' @importFrom digest hmac
#' @importFrom base64enc base64encode
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr content_type
#' @importFrom httr http_error
#' @importFrom httr http_status

ApiClient <- function(host = "knoema.com", client.id = "", client.secret = "") {
  client = list(
    host = host,
    client.id = client.id,
    client.secret = client.secret
  )
  #Add a few more methods

  client$GetUrl <- function(apipath) {
    return (sprintf("https://%1s/%2s", client$host, apipath))
  }

  client$GetAuthorization <- function() {
    if (client$client.id != "" && client$client.secret != "") {
      key <- format(Sys.time(), "%d-%m-%y-%H", tz = "UCT")
      hash <-  hmac(key, client$client.secret, "sha1", raw = TRUE)
      secrethash <- base64encode(hash)
      auth = sprintf("Knoema %1s:%2s:1.2", client$client.id, secrethash)
      return (auth)
    }
    return (NULL)
  }

  client$CheckCorrectHost <- function() {
    if (client$host == 'knoema.com')
      return (NULL)
    url = client$GetUrl('api/1.0/frontend/tags')
    out <- tryCatch(GET(url, add_headers("Content-Type"="application/json")),
                    error = function(e)
                     stop(simpleError(sprintf("The specified host %1s does not exist", client$host))))
    return (out)
  }

  client$ApiGet <- function(apipath, query = NULL) {
    url <- client$GetUrl(apipath)
    if (!is.null(query)) {
      url <- sprintf("%1s?%2s", url, query)
    }
    auth <- client$GetAuthorization()
    if (is.null(auth)) {
      response <- GET(url, add_headers("Content-Type"="application/json"))
    } else {
      response <- GET(url, add_headers("Content-Type"="application/json", "Authorization"= auth))
    }
    if (http_error(response)) {
      e <- simpleError(http_status(response)$message)
      stop(e)
    }
    res = content(response, as = "parsed");
    return (res)
  }

  client$ApiPost <- function(apipath, request) {
    url <- client$GetUrl(apipath)
    auth <- client$GetAuthorization()
    if (is.null(auth)) {
      p <- POST(url, content_type("application/json"), body = request, encode = "json")
    } else {
      p <- POST(url, content_type("application/json"), add_headers("Authorization"= auth), body = request, encode = "json")
    }
    if (http_error(p)) {
      e <- simpleError(http_status(p)$message)
      stop(e)
    }
    res <- content(p, as = "parsed")
    if (is.character(res)) {
      e <- simpleError(res)
      stop(e)
    }
    return(res)
  }

  #The method is getting information about dataset by its id
  client$GetDataset <- function(dataset.id) {
    path <- "api/1.0/meta/dataset/%1s"
    return (client$ApiGet(sprintf(path, dataset.id)))
  }

  #The method is getting information about dimension with items
  client$GetDimension <- function(dataset.id, dimension.name) {
    path <- "api/1.0/meta/dataset/%1s/dimension/%2s"
    return (client$ApiGet(sprintf(path, dataset.id, dimension.name)))
  }

  #The method is getting data by pivot request
  client$GetData <- function(pivot.request) {
    path <- "api/1.0/data/pivot"
    return (client$ApiPost(path, pivot.request))
  }

  #The method is getting data by streaming request
  client$GetRawData <- function(pivot.request) {
    path <- "api/1.0/data/raw"
    raw.data <- client$ApiPost(path, pivot.request)
    raw.series <- raw.data$data
    token <- raw.data$continuationToken
    while (!is.null(token)) {
      raw.data.with.token <- client$GetRawDataWithToken(token)
      raw.series <- c(raw.series, raw.data.with.token$data)
      token <- raw.data.with.token$continuationToken
    }
    return (raw.series)
  }

  client$GetRawDataWithToken <- function(token) {
    path = "api/1.0/data/raw/?continuationToken=%1s"
    return (client$ApiGet(sprintf(path,token)))
  }

  client$GetMnemonics <- function(mnemonics) {
    path <- "api/1.0/data/mnemonics?mnemonics=%1s"
    return (client$ApiGet(sprintf(path, mnemonics)))
  }

  client <- list2env(client)
  class(client) <- "ApiClient"
  return(client)
}
