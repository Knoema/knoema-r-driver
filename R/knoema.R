#' knoema package!
#' This package works with datasets from knoema.com
#' @param datasetId is Dataset's ID specified as a string
#' @param selection is list where all the dimensions of the dataset are listed and a selection from them
#' @param type is optional. By default equals "ts". Other supported variants are "xts" and "zoo"
#' @param host is optional. You can use "knoema.com" or other supported knoema's portals
#' @param app.id is client id from knoema application
#' @param app.secret is secret client code from knoema application
#' By default uses public user for knoema.com
#' By default the package allows you to work only with public datasets from the site knoema.com.
#' If you want to work with private datasets or from other hosts, you need to set optional parameters host, app.id and app.secret
#' You can get parameters app.id and app.secret after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications)
#' @return the list of timeseries in the selected format from the dataset
#' @examples
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"))
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH", frequency="A"),type="xts")
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH", timerange = "2010-2015"))
#' @export

knoema.get <- function(datasetId, selection,type="ts", host = "", app.id="", app.secret="")
{
  if (host == "")
    if (Sys.getenv("KNOEMA_HOST") == ""){
      host = "knoema.com"
    } else {
      host = Sys.getenv("KNOEMA_HOST")
    }
  client = ApiClient(host, app.id, app.secret)
  if (!is.character(datasetId)){
    e = simpleError(sprintf("DatasetId should be a string. Can't be %1s",typeof(datasetId)))
    stop(e)
  }
  dataset <- client$GetDataset(datasetId)
  if (is.character(dataset)){
    error = simpleError(dataset)
    stop(error)
  }
  data.reader <- DataReader(client,dataset,selection)
  series <- data.reader$GetFrame(type)
  return (series)
}
