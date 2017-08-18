# This class reads data from Knoema and transforms it to ts,xts or zoo

#' @importFrom lubridate day
#' @importFrom lubridate week
#' @importFrom lubridate month
#' @importFrom lubridate quarter
#' @importFrom lubridate year
#' @importFrom xts as.xts
#' @importFrom xts xts
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.yearqtr
#' @importFrom zoo as.zooreg
#' @importFrom zoo zoo
#' @importFrom zoo as.zoo
#' @importFrom zoo is.zoo
#' @importFrom xts is.xts
#' @importFrom zoo coredata
#' @importFrom stats ts
#' @importFrom methods is


DataReader <- function(client, dataset, selection){
  reader <- list(
    client = client,
    dataset = Dataset(dataset),
    selection = selection
  )

  reader$FindDimension <- function (dim.name.or.id){
    dim <- reader$dataset$FindDimensionByName(dim.name.or.id)
    if (is.null(dim)) {
      dim <- reader$dataset$FindDimensionById(dim.name.or.id)
    }
    return (dim)
  }
  reader$EnsureAllDimenionsInFilter <- function(filter.dims){
    dims <- list()
    for (item in reader$dataset$dimensions) {
      dims <- c(dims,item$name)
    }
    dims.from.filter <- list()
    for (item in filter.dims) {
      dims.from.filter <- c(dims.from.filter,item$name)
    }
    list.condition <- sapply(dims, function(x) ! x %in% dims.from.filter)
    out.of.filter.dim.names <- dims[list.condition]
    if (length (out.of.filter.dim.names) > 0) {
      error <- simpleError(sprintf("The following dimension(s) are not set: %1s",paste(out.of.filter.dim.names,sep="", collapse =",")))
      stop(error)
    }
  }


  reader$GetDimMembers <- function(dim, split.values){
    members <- c()
    for (value in split.values) {
      if (is.null(value)) {
        error <- simpleError(sprintf("Selection for dimension %1s is empty",dim$name))
        stop(error)
      }
      member <- dim$FindMemberById(value)
      if (is.null(member))
        member <- dim$FindMemberByName(value)

      if (is.null(member)& !is.na(suppressWarnings(as.numeric(value))))
           member <- dim$FindMemberByKey(as.numeric(value))
      members <- c(members, member$key)
    }
    return (members)
  }

  reader$CheckCorrectFrequencies <- function (values){
    correct.freq <- list("A","H","Q","M","W","D")
    list.condition <- !values %in% correct.freq
    list.err <- values[list.condition]
    if (length(list.err)>0) {
      error <- simpleError(sprintf("The following frequencies are not correct: %1s",paste(list.err,sep="", collapse =",")))
      stop(error)
    }
    return (TRUE)
  }

  reader$CreatePivotRequest <- function (){
    request <- PivotRequest(dataset$id)
    filter.dims <- list()
    time.range <- NULL
    for (item in names(reader$selection)) {
      value <- reader$selection[item][[1]]
      if (item == "timerange") {
        time.range <- value
        next
      }
      splited.values <- as.list(strsplit(value, ';')[[1]])
      if (item == "frequency") {
        if (reader$CheckCorrectFrequencies(splited.values)) {
          request$set("frequencies", splited.values)
          next
        }
      }
      dim <- reader$FindDimension(item)
      if (is.null(dim))
      {
         error <- simpleError(sprintf("Dimension with id or name %1s is not found",item))
         stop(error)
      }
      filter.dims <- c(filter.dims,dim)
      dim2 <- Dimension(reader$client$GetDimension(reader$dataset$id, dim$id))
      members <- reader$GetDimMembers(dim2, splited.values)
      if (length(members) == 0) {
        e = simpleError(sprintf("Selection for dimension %1s is empty",dim$name))
        stop(e)
      }
      l <- c(request$get("stub"),PivotItem(dim$id, members))
      request$set("stub", l)
    }

    reader$EnsureAllDimenionsInFilter(filter.dims)
    if (length(time.range != 0)){
      l <- c(request$get("header"),PivotTimeItem("Time", time.range, "range"))
      request$set("header", l)
    } else {
      l <- c(request$get("header"),PivotTimeItem("Time", list(), "allData"))
      request$set("header", l)
    }
    return (request)

  }


  reader$CreateTs <-function(series){
    result <- list()
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (i in 1:length(series)) {
      title <- names(series[i])
      freq <- substring(title, 1, 1)
      freq.seq <- unlist(frequencies.seq)[freq]
      dates <- names(series[[i]])
      if (length(dates) == 0) {
        next
      }
      min.date <- as.Date(min(dates))
      max.date <- as.Date(max(dates))
      all.dates <- seq(min.date,max.date, by = freq.seq)
      values <- c()
      for (j in 1:length(all.dates)) {
        if (as.Date(all.dates[j]) %in% as.Date(dates)) {
          value <- as.numeric(unlist(series[[i]])[as.character(all.dates[j])])
          values <- c(values, value)
        } else {
          values <- c(values, NA)
        }
      }
      if (freq == "A")
        start.by.freq <- c(year(min.date),1)
      if (freq == "H") {
        half.year = (month(min.date)-1)%/%6+1
        start.by.freq <- c(year(min.date), half.year)
      }
      if (freq == "Q")
        start.by.freq <- c(year(min.date), quarter(min.date))
      if (freq == "M")
        start.by.freq <- c(year(min.date), month(min.date))
      if (freq == "W")
        start.by.freq <- c(year(min.date), week(min.date))
      if (freq == "D")
        start.by.freq <- c(year(min.date), day(min.date))

      result[[title]] <- ts(values, start = start.by.freq, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateXts <-function(series){
    result <- list()
    for (i in 1:length(series)) {
      title <- names(series[i])
      if (length(names(series[[i]])) == 0){
        next
      }
      freq <- substring(title, 1, 1)
      if (freq == "Q")
        dates <- as.yearqtr(as.Date(names(series[[i]])))
      if (freq == "M")
        dates <- as.yearmon(as.Date(names(series[[i]])))
      if (freq == "A" || freq =="H" || freq == "W" || freq == "D")
        dates <- as.Date(names(series[[i]]))
      values <- as.numeric(unlist(series[[i]]))
      result[[title]] <- xts(values,order.by = dates,frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateZoo <-function(series){
    result <- list()
    for (i in 1:length(series)) {
      title <- names(series[i])
      if (length(names(series[[i]])) == 0) {
        next
      }
      freq <- substring(title, 1, 1)
      if (freq == "A")
        dates <- as.numeric(format(as.Date(names(series[[i]])), "%Y"))
      if (freq == "Q")
        dates <- as.yearqtr(as.Date(names(series[[i]])))
      if (freq == "M")
        dates <- as.yearmon(as.Date(names(series[[i]])))
      if (freq =="H" || freq == "W" || freq == "D")
        dates <- as.Date(names(series[[i]]))
      values <- as.numeric(unlist(series[[i]]))
      result[[title]] <- zoo(values,order.by = dates,frequency = FrequencyToInt(freq))
    }
    return (result)
  }


  reader$GetSeries <- function (resp, type){
    series <- list()
    if (length(resp$data) == 0){
      warning(simpleError("Dataset do not have data by this selection"))
      return (series)
    } else {
      for (i in 1:length(resp$data)){
        frequency <- resp$data[[i]]$Frequency
        name <- frequency
        # get name of time series
        for (j in 1:length(resp$stub)){
          dim <- resp$stub[[j]]$dimensionId
          name <-  paste(name, resp$data[[i]][[dim]], sep = " - ");
        }
        # create key-value list where time is the key
        if (is.null(series[[name]])){
          series[[name]] <- list()
        }
        time <- format(as.Date(resp$data[[i]]$Time), "%Y-%m-%d")
        values <- list(time=time,value=resp$data[[i]]$Value)
        series[[name]][time] <- resp$data[[i]]$Value
      }
      if (type == "zoo")
        result <- reader$CreateZoo(series)
      if (type == "xts")
        result <- reader$CreateXts(series)
      if (type == "ts")
        result <- reader$CreateTs(series)
      return (result)
    }
  }
  reader$GetFrame <-function(type){
    pivot.request <- reader$CreatePivotRequest()
    pivot.request.json <- pivot.request$SaveToJson()
    data <- reader$client$GetData(pivot.request.json)
    series <- reader$GetSeries(data, type)
    return (series)
  }

  reader <- list2env(reader)
  class(reader) <- "DataReader"
  return(reader)
}
