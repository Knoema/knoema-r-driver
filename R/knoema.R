#' knoema package!
#' This package works with datasets from knoema.com
#' @param dataset.id is Dataset's ID specified as a string
#' @param selection is list where all the dimensions of the dataset are listed and a selection from them
#' @param mnemonics are mnemonics values specified as a string separated by a semicolon
#' @param type is optional. By default equals "ts". Other supported variants are "xts","zoo","DataFrame","MetaDataFrame","DataTable","MetaDataTable"
#' @param host is optional. You can use "knoema.com" or other supported knoema's portals
#' @param client.id is client id from knoema application
#' @param client.secret is secret client code from knoema application
#' By default uses public user for knoema.com
#' By default the package allows you to work only with public datasets from the site knoema.com.
#' If you want to work with private datasets or from other hosts, you need to set optional parameters host, client.id and client.secret
#' You can get parameters client.id and client.secret after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications)
#' @return the list of timeseries in the selected format from the dataset
#' @examples
#' Knoema("IMFWEO2017Oct", list(country = "512;914", subject = "NGDP_RPCH"),
#'            client.id = "bHcV5UkOVyKcBw",
#'            client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
#' Knoema("IMFWEO2017Oct", list(country = "512;914", subject = "NGDP_RPCH", frequency = "A"),
#'            type = "xts",
#'            client.id = "bHcV5UkOVyKcBw",
#'            client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
#' Knoema("IMFWEO2017Oct", list(country = "512;914", subject = "NGDP_RPCH", timerange = "2010-2015"),
#'            client.id = "bHcV5UkOVyKcBw",
#'            client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
#' Knoema(mnemonics="512NGDP_A_in_test_dataset",
#'            client.id = "bHcV5UkOVyKcBw",
#'            client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
#' @export

Knoema <- function(dataset.id = NULL, selection = NULL,  mnemonics = NULL, type = "ts", host = "", client.id = "", client.secret = "") {
  if (host == "") {
    if (Sys.getenv("KNOEMA_HOST") == "") {
      host = "knoema.com"
    } else {
      host = Sys.getenv("KNOEMA_HOST")
    }
  }
  if (!is.character(dataset.id) && is.null(mnemonics)) {
    e <- simpleError(sprintf("dataset.id should be a string. Can't be %1s",typeof(dataset.id)))
    stop(e)
  }
  if (!is.null(mnemonics) && !is.null(selection)) {
    e <- simpleError("The function does not support specifying mnemonics and selection in a single call")
    stop(e)
  }

  client <- ApiClient(host, client.id, client.secret)
  client$CheckCorrectHost()

  if (!is.null(dataset.id)) {
    dataset <- client$GetDataset(dataset.id)
    #case for dataset with error or without access
    if (is.character(dataset)) {
      error <- simpleError(dataset)
      stop(error)
    }
    ds <- Dataset(dataset)
  } else {
    ds <- NULL
  }
  data.reader <- NULL
  if (!is.null(mnemonics)){
    data.reader <- MnemonicsDataReader(client, mnemonics)
  } else {
    if (ds$type == "Regular"){
      data.reader <- StreamingDataReader(client, selection)
    } else {
      data.reader <- PivotDataReader(client, selection)
    }
  }

  data.reader$set("dataset", ds)
  series <- data.reader$GetObjectByType(type)
  return (series)
}
