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
#' @importFrom stats ftable


DataReader <- function(client, dataset, selection){
  reader <- list(
    client = client,
    dataset = Dataset(dataset),
    selection = selection,
    dimensions = list()
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
    dims <- lapply(1:length(reader$dataset$dimensions),function(x) reader$dataset$dimensions[[x]]$name)
    dims.from.filter <- list()
    dims.from.filter <-lapply(1:length(filter.dims),function(x) filter.dims[[x]]$name)
    list.condition <- sapply(dims, function(x) ! x %in% dims.from.filter)
    out.of.filter.dim.names <- dims[list.condition]
    if (length (out.of.filter.dim.names) > 0) {
      error <- simpleError(sprintf("The following dimension(s) are not set: %1s",paste(out.of.filter.dim.names,sep="", collapse =",")))
      stop(error)
    }
  }


  reader$GetDimMembers <- function(dim, split.values){
    members <- NULL
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
      for (dimension in reader$dimensions){
        if(dimension$dim.model$id == dim$id)
        {
          dim2 <- dimension
          break
        }
      }
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
      values <- NULL
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
      for (serie.point in resp$data){
        if (is.null(serie.point$Value)){
          next
        }
        frequency <- serie.point$Frequency
        name <- frequency
        # get name of time series
        for (stub in resp$stub){
          dim <- stub$dimensionId
          name <-  paste(name, serie.point[[dim]], sep = " - ")
        }
        # create key-value list where time is the key
        if (is.null(series[[name]])){
          series[[name]] <- list()
        }
        time <- format(as.Date(serie.point$Time), "%Y-%m-%d")
        series[[name]][time] <- serie.point$Value
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

  reader$CreateMatrixForFrameOrTable <- function(data.rows, series){
    list.for.matrix <- NULL
    for (name.serie in names(series)){
      for (date in data.rows){
        if (date %in% names(series[[name.serie]])){
          list.for.matrix <- c(list.for.matrix, series[[name.serie]][[date]])
        } else {
          list.for.matrix <- c(list.for.matrix, NA)
        }
      }
    }
    matrix <- matrix(list.for.matrix, nrow = length(data.rows), ncol = length(series))
    return (matrix)
  }

  reader$GetFTableByData <- function (data.rows, data.columns, series, row.equal.dates = TRUE){
    matrix <- reader$CreateMatrixForFrameOrTable(data.rows, series)
    length.by.columns <- unlist(lapply(1:length(data.columns), function(x) length(data.columns[[x]])), recursive = F)
    length.of.all.dimension <- c(length(data.rows), length.by.columns)
    matrix.arr <- array(matrix, length.of.all.dimension)
    if (row.equal.dates){
      dimnames(matrix.arr) <- c(list(date = data.rows), data.columns)
    } else {
      dimnames(matrix.arr) <- c(list(attributes = data.rows), data.columns)
    }
    data.table <- ftable(matrix.arr, row.vars = 1, col.vars = 2:(length(data.columns) + 1))
    return (data.table)
  }

  reader$GetDataFrameByData <- function(data.rows, series){
    matrix <- reader$CreateMatrixForFrameOrTable(data.rows, series)
    data.frame <- as.data.frame(matrix, row.names = data.rows, stringsAsFactors = FALSE)
    colnames(data.frame) <- names(series)
    return (data.frame)
  }

  reader$GetSeriesNameWithMetadata <- function(series.point){
    names <- list()
    for (dim in reader$dimensions){
      for (item in dim$items){
        if (item$name == series.point[dim$dim.model$id]){
          dim.attr <- item$fields
          break
        }
      }
      for (attr in dim$fields){
        if (!attr$isSystemField){
          for (i in 1: length(dim.attr)){
            if (IsEqualStringsIgnoreCase(names(dim.attr[i]), attr$name)){
              names[[paste(dim$dim.model$name, attr$displayName, sep=' ')]] <- dim.attr[[i]]
            }
          }
        }
      }
    }
    names[['Unit']] <- series.point$Unit
    names[['Scale']] <- series.point$Scale
    names[['Mnemonics']] <- ifelse(is.null(series.point$Mnemonics), 'NULL', series.point$Mnemonics)
    for (attr in reader$dataset$time.series.attributes){
        names[[attr$name]] <- series.point[[attr$name]]
    }
    return (names)
  }

  reader$CreateAttributesNamesForMetadata <- function(){
    data.rows <- NULL
    for (dim in reader$dimensions){
      for (attr in dim$fields){
        if (!attr$isSystemField){
          data.rows <- c(data.rows, paste(dim$dim.model$name, attr$displayName, sep = ' '))
        }
      }
    }
    data.rows <- c(data.rows, c('Unit', 'Scale', 'Mnemonics'))
    for (attr in reader$dataset$time.series.attributes){
      data.rows <- c(data.rows, attr$name)
    }
    return(data.rows)
  }

  reader$CreateMetaDataTable <- function(resp){
    series <- list()
    data.rows <- reader$CreateAttributesNamesForMetadata()
    data.columns <- list()

    for (serie.point in resp$data){
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (j in 1:length(resp$stub)){
        dim <- resp$stub[[j]]$dimensionId
        dim.name <- reader$FindDimension(dim)$name
        name.element = serie.point[[dim]]
        if (!name.element %in% data.columns[[dim.name]]){
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      if (!frequency %in% data.columns[['Frequency']]){
        data.columns[['Frequency']] <- c(data.columns[['Frequency']],frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <-  name.meta
      }
    }

    data.table <- reader$GetFTableByData(data.rows, data.columns, series, FALSE)
    return (data.table)
  }

  reader$CreateMetaDataFrame <- function(resp){
    series <- list()
    data.rows <- reader$CreateAttributesNamesForMetadata()
    for (serie.point in resp$data){
      if (is.null(serie.point$Value)){
        next
      }
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (j in 1:length(resp$stub)){
        dim <- resp$stub[[j]]$dimensionId
        name.element = serie.point[[dim]]
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <-  name.meta
      }
    }
    data.table <- reader$GetDataFrameByData(data.rows, series)
    return (data.table)
  }

  reader$CreateDataTable <- function(resp){
    series <- list()
    data.rows <- NULL
    data.columns <- list()
    for (serie.point in resp$data){
      name <- NULL
      # create columns with dimension names for frable object
      for (stub in resp$stub){
        dim <- stub$dimensionId
        dim.name <- reader$FindDimension(dim)$name
        name.element = serie.point[[dim]]
        if (!name.element %in% data.columns[[dim.name]]){
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      if (!frequency %in% data.columns[['Frequency']]){
        data.columns[['Frequency']] <- c(data.columns[['Frequency']], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      time <- format(as.Date(serie.point$Time), "%Y-%m-%d")
      if (!time %in% data.rows){
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    data.rows <- sort(data.rows)
    data.table <- reader$GetFTableByData(data.rows, data.columns, series)
    return (data.table)
  }

  reader$CreateDataFrame<- function(resp){
    series <- list()
    data.rows <- NULL
    for (serie.point in resp$data){
      if (is.null(serie.point$Value)){
        next
      }
      name <- NULL
      # create columns with dimension names for frable object
      for (stub in resp$stub){
        dim <- stub$dimensionId
        name.element = serie.point[[dim]]
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      time <- format(as.Date(serie.point$Time), "%Y-%m-%d")
      if (!time %in% data.rows){
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    data.rows <- sort(data.rows)
    data.table <- reader$GetDataFrameByData(data.rows, series)
    return (data.table)
  }

  reader$GetObjectByType <- function(type){
    for (dim in reader$dataset$dimensions){
      reader$dimensions <- c(reader$dimensions, Dimension(reader$client$GetDimension(reader$dataset$id, dim$id)))
    }
    pivot.request <- reader$CreatePivotRequest()
    pivot.request.json <- pivot.request$SaveToJson()
    data <- reader$client$GetData(pivot.request.json)
    if (length(data$data) == 0){
      warning(simpleError("Dataset do not have data by this selection"))
      return (NULL)
    }
    if (type == 'DataTable'){
      table <- reader$CreateDataTable(data)
      return (table)
    }
    if (type == 'MetaDataTable'){
      table <- reader$CreateMetaDataTable(data)
      return (table)
    }
    if (type == 'DataFrame'){
      table <- reader$CreateDataFrame(data)
      return (table)
    }
    if (type == 'MetaDataFrame'){
      table <- reader$CreateMetaDataFrame(data)
      return (table)
    }
    series <- reader$GetSeries(data, type)
    return (series)
  }
  reader <- list2env(reader)
  class(reader) <- "DataReader"
  return(reader)
}
