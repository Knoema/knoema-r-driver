#' knoema package!
#' This package works with datasets from knoema.com
#' @param datasetId is Dataset's ID specified as a string
#' @param selection is list where all the dimensions of the dataset are listed and a selection from them
#' @param type is optional. By default equal "ts". Other supported variants are "xts" and "zoo"
#' @param client is optional. By default use public user for knoema.com
#' By default the package allows you to work only with public datasets from the site knoema.com.
#' If you want work with private datasets or from other hosts, you need create ApiClient
#' client = ApiClient("host","some_app_id","some_app_secret") and then use this client in funcion knoema.get
#' You can get parameters app_id and app_secret after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications)
#' @examples
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"))
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"),type="xts")
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"),type="zoo", app_id="some app_id", app_secret="some app_secret")
#' @export

knoema.get <- function(datasetId, selection,type="ts", host = "", app_id="", app_secret="")
{
  if (host == "")
    if (Sys.getenv("KNOEMA_HOST") == "")
      host = "knoema.com"
    else
      host = Sys.getenv("KNOEMA_HOST")
  client = ApiClient(host, app_id, app_secret)
  if (!is.character(datasetId))
  {
    e = simpleError(sprintf("DatasetId should be a string. Can't be %1s",typeof(datasetId)))
    stop(e)
  }
  dataset <- client$get_dataset(datasetId)
  if (is.character(dataset))
  {
    error = simpleError(dataset)
    stop(error)
  }
  data_reader <- DataReader(client,dataset,selection)
  series <- data_reader$get_frame(type)
  return (series)
}
