# This class reads data from Knoema and transforms it to ts,xts or zoo

#' @importFrom lubridate day
#' @importFrom lubridate days
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

    reader$AddFullSelectionByEmptyDimValues(filter.dims, request)
    if (length(time.range != 0)){
      l <- c(request$get("header"),PivotTimeItem("Time", time.range, "range"))
      request$set("header", l)
    } else {
      l <- c(request$get("header"),PivotTimeItem("Time", list(), "allData"))
      request$set("header", l)
    }
    return (request)

  }

  reader$AddFullSelectionByEmptyDimValues <- function(filter.dims, request)
  {
    dims <- lapply(1:length(reader$dataset$dimensions),function(x) reader$dataset$dimensions[[x]]$id)
    dims.from.filter <- lapply(1:length(filter.dims),function(x) filter.dims[[x]]$id)
    list.condition <- sapply(dims, function(x) ! x %in% dims.from.filter)
    out.of.filter.dim.names <- dims[list.condition]
    for (id in out.of.filter.dim.names)
    {
      l <- c(request$get("stub"),PivotItem(id, list()))
      request$set("stub", l)
    }
  }


  reader$CreateTs <- function(series){
    result <- list()
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")

    for (i in 1:length(series)) {
      title <- names(series[i])
      freq <- substring(title, 1, 1)
      freq.seq <- unlist(frequencies.seq)[freq]
      dates <- as.Date(names(series[[i]]))
      if (length(dates) == 0) {
        next
      }
      min.date <- min(dates)
      max.date <- max(dates)
      all.dates <- seq(min.date,max.date, by = freq.seq)
      values <- sapply(1:length(all.dates), function(x) {
          dat = all.dates[x]
          cond.v <- as.Date(dat) %in% dates
          return(ifelse (!cond.v, NA,  series[[i]][[as.character(dat)]]))
      })

      start.by.freq <- switch(freq,
                             "A"= c(year(min.date),1),
                             "H" = c(year(min.date), (month(min.date)-1)%/%6+1),
                             "Q" = c(year(min.date), quarter(min.date)),
                             "M" = c(year(min.date), month(min.date)),
                             "W" = c(year(min.date), week(min.date)),
                             "D" = c(year(min.date), day(min.date)))

      result[[title]] <- ts(values, start = start.by.freq, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateXts <- function(series){
    result <- list()
    for (i in 1:length(series)) {
      title <- names(series[i])
      if (length(names(series[[i]])) == 0){
        next
      }
      dates <- as.Date(names(series[[i]]))
      freq <- substring(title, 1, 1)
      dates.xts <- switch (freq,
                       "Q" = as.yearqtr(dates),
                       "M" = as.yearmon(dates),
                       dates)
      values <- unlist(series[[i]], use.names = FALSE)
      result[[title]] <- xts(values,order.by = dates.xts, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateZoo <-function(series){
    result <- list()
    for (i in 1:length(series)) {
      title <- names(series[i])
      dates <- as.Date(names(series[[i]]))
      if (length(dates) == 0) {
        next
      }
      freq <- substring(title, 1, 1)
      dates.zoo <- switch (freq,
                       "A" = as.numeric(format(dates, "%Y")),
                       "Q" = as.yearqtr(dates),
                       "M" = as.yearmon(dates),
                       dates
                       )
      values <- unlist(series[[i]], use.names = FALSE)
      result[[title]] <- zoo(values, order.by = dates.zoo, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateSeriesForTsXtsZooForPivotApi <- function (resp, series)
  {
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
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"),error = function(e) stop(simpleError("Types ts, xts, zoo are not supported for flat datasets")))
      series[[name]][time] <- serie.point$Value
    }
    return (series)
  }

  reader$CreateSeriesForTsXtsZooForStreamingApi <- function (resp, series)
  {
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (serie.point in resp){
      all.values <- serie.point$values
      frequency <- serie.point$frequency
      name <- frequency
      # get name of time series
      for (dim in reader$dimensions){
        name <- paste(name, serie.point[[dim$dim.model$id]]$name, sep = " - ")
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      data.begin <- as.Date(serie.point$startDate)
      data.end <- as.Date(serie.point$endDate)
      if (frequency == "W"){
        data.begin <- data.begin - days(as.numeric(strftime(data.begin,"%u"))-1)
        data.end <- data.end - days(as.numeric(strftime(data.end,"%u"))-1)
      }
      all.dates<- seq(data.begin,data.end, by = frequencies.seq[[frequency]])
      for (i in 1:length(all.dates)){
        val <- all.values[[i]]
        if (!is.null(val)){
          time <- format(as.Date(all.dates[[i]], "%Y-%m-%d"))
          series[[name]][time] <- val
        }
      }
    }
    return (series)
  }

  reader$GetSeries <- function (resp, type){
    if (length(resp$data) == 0){
      warning(simpleError("Dataset do not have data by this selection"))
      return (series)
    } else {
      series <- reader$CreateSeriesForTS(resp, list())
      return (switch(type,
             "zoo" = reader$CreateZoo(series),
             "xts" = reader$CreateXts(series),
             "ts" = reader$CreateTs(series)))
    }
  }

  reader$CreateMatrixForFrameOrTable <- function(data.rows, series){
    list.for.matrix <- sapply(1:length(series), function (i) {
      dates <- names(series[[i]])
      t <- sapply (1:length(data.rows), function (j){
        date <- data.rows[[j]]
        cond <- date %in% dates
        ifelse(cond, series[[i]][[date]],NA)
      })
      })

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

  reader$GetSeriesNameWithMetadataForPivotApi <- function(series.point){
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
              names[[paste(dim$dim.model$name, attr$displayName, sep=" ")]] <- dim.attr[[i]]
            }
          }
        }
      }
    }
    names[["Unit"]] <- series.point$Unit
    names[["Scale"]] <- series.point$Scale
    names[["Mnemonics"]] <- ifelse(is.null(series.point$Mnemonics), "NULL", series.point$Mnemonics)
    for (attr in reader$dataset$time.series.attributes){
        names[[attr$name]] <- series.point[[attr$name]]
    }
    return (names)
  }

  reader$GetSeriesNameWithMetadataForStreamingApi <- function(series.point){
    names <- list()
    for (dim in reader$dimensions){
      for (item in dim$items){
        if (item$name == series.point[[dim$dim.model$id]]$name){
          dim.attr <- item$fields
          break
        }
      }
      for (attr in dim$fields){
        if (!attr$isSystemField){
          for (i in 1: length(dim.attr)){
            if (IsEqualStringsIgnoreCase(names(dim.attr[i]), attr$name)){
              names[[paste(dim$dim.model$name, attr$displayName, sep=" ")]] <- dim.attr[[i]]
            }
          }
        }
      }
    }
    names[["Unit"]] <- series.point$unit
    names[["Scale"]] <- series.point$scale
    names[["Mnemonics"]] <- ifelse(is.null(series.point$mnemonics), "NULL", series.point$mnemonics)
    for (attr in reader$dataset$time.series.attributes){
      names[[attr$name]] <- series.point$timeseriesAttributes[[attr$name]]
    }
    return (names)
  }

  reader$CreateAttributesNamesForMetadata <- function(){
    data.rows <- NULL
    for (dim in reader$dimensions){
      for (attr in dim$fields){
        if (!attr$isSystemField){
          data.rows <- c(data.rows, paste(dim$dim.model$name, attr$displayName, sep = " "))
        }
      }
    }
    data.rows <- c(data.rows, c("Unit", "Scale", "Mnemonics"))
    for (attr in reader$dataset$time.series.attributes){
      data.rows <- c(data.rows, attr$name)
    }
    return(data.rows)
  }

  reader$CreateSeriesForMetaDataTableForPivotApi <- function(resp, series, data.columns){
    for (serie.point in resp$data){
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (j in 1:length(resp$stub)){
        dim <- resp$stub[[j]]$dimensionId
        dim.name <- reader$FindDimension(dim)$name
        name.element <- serie.point[[dim]]
        if (!name.element %in% data.columns[[dim.name]]){
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadataForPivotApi(serie.point)
      if (!frequency %in% data.columns[["Frequency"]]){
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]],frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- name.meta
      }
    }
    return (list(series, data.columns))
  }

  reader$CreateSeriesForMetaDataTableForStreamingApi <- function(resp, series, data.columns){
    for (serie.point in resp){
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (dim in reader$dimensions){
        name.element <- serie.point[[dim$dim.model$id]]$name
        dim.name <- dim$dim.model$name
        if (!name.element %in% data.columns[[dim.name]]){
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadataForStreamingApi(serie.point)
      if (!frequency %in% data.columns[["Frequency"]]){
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]],frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- name.meta
      }
    }
    return (list(series, data.columns))
  }

  reader$CreateSeriesForMetaDataFrameForPivotApi <- function (resp, series)
  {
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
      name.meta <- reader$GetSeriesNameWithMetadataForPivotApi(serie.point)
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- name.meta
      }
    }
    return (series)
  }

  reader$CreateSeriesForMetaDataFrameForStreamingApi <- function (resp, series)
  {
    for (serie.point in resp){
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (dim in reader$dimensions){
        name.element <- serie.point[[dim$dim.model$id]]$name
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadataForStreamingApi(serie.point)
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- name.meta
      }
    }
    return (series)
  }

  reader$CreateMetaDataFrame <- function(resp){
    data.rows <- reader$CreateAttributesNamesForMetadata()
    series <- reader$CreateSeriesForMetaDataFrame (resp, list())[[1]]
    data.table <- reader$GetDataFrameByData(data.rows, series)
    return (data.table)
  }

  reader$CreateSeriesForDataTableForPivotApi <- function (resp, series, data.rows, data.columns)
  {
    for (serie.point in resp$data){
      name <- NULL
      # create columns with dimension names for ftable object
      for (stub in resp$stub){
        dim <- stub$dimensionId
        dim.name <- reader$FindDimension(dim)$name
        name.element <- serie.point[[dim]]
        if (!name.element %in% data.columns[[dim.name]]){
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      if (!frequency %in% data.columns[["Frequency"]]){
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"),error = function(e) serie.point$Time)
      if (!time %in% data.rows){
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    return (list(series, data.rows, data.columns))
  }

  reader$CreateSeriesForDataTableForStreamingApi <- function (resp, series, data.rows, data.columns)
  {
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (serie.point in resp){
      all.values <- serie.point$values
      name <- NULL
      # create columns with dimension names for ftable object
      for (dim in reader$dimensions){
        name.element <- serie.point[[dim$dim.model$id]]$name
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
        dim.name <- dim$dim.model$name
        if (!name.element %in% data.columns[[dim.name]]){
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      if (!frequency %in% data.columns[["Frequency"]]){
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      data.begin <- as.Date(serie.point$startDate)
      data.end <- as.Date(serie.point$endDate)
      if (frequency == "W"){
        data.begin <- data.begin - days(as.numeric(strftime(data.begin,"%u"))-1)
        data.end <- data.end - days(as.numeric(strftime(data.end,"%u"))-1)
      }
      all.dates<- seq(data.begin,data.end, by = frequencies.seq[[frequency]])
      for (i in 1:length(all.dates)){
        val <- all.values[[i]]
        if (!is.null(val)){
          time <- format(as.Date(all.dates[[i]], "%Y-%m-%d"))
          if (!time %in% data.rows){
            data.rows <- c(data.rows, time)
          }
          series[[name]][time] <- val
        }
      }
    }
    return (list(series, data.rows, data.columns))
  }


  reader$CreateDataTable <- function(resp){
    result <- reader$CreateSeriesForDataTable(resp, list(), NULL, list())
    series <- result[[1]]
    data.rows <- result[[2]]
    data.columns <- result[[3]]
    data.rows <- sort(data.rows)
    data.table <- reader$GetFTableByData(data.rows, data.columns, series)
    return (data.table)
  }

  reader$CreateSeriesForDataFrameForPivotApi <- function(resp, series, data.rows)
  {
    for (serie.point in resp$data){
      if (is.null(serie.point$Value)){
        next
      }
      name <- NULL
      # create columns with dimension names for frable object
      for (stub in resp$stub){
        dim <- stub$dimensionId
        name.element <- serie.point[[dim]]
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"),error = function(e) serie.point$Time)
      if (!time %in% data.rows){
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    return (list(series, data.rows))
  }

  reader$CreateSeriesForDataFrameForStreamingApi <- function(resp, series, data.rows)
  {
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (serie.point in resp){
      all.values <- serie.point$values
      name <- NULL
      # create columns with dimension names for ftable object
      for (dim in reader$dimensions){
        name.element <- serie.point[[dim$dim.model$id]]$name
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])){
        series[[name]] <- list()
      }
      data.begin <- as.Date(serie.point$startDate)
      data.end <- as.Date(serie.point$endDate)
      if (frequency == "W"){
        data.begin <- data.begin - days(as.numeric(strftime(data.begin,"%u"))-1)
        data.end <- data.end - days(as.numeric(strftime(data.end,"%u"))-1)
      }
      all.dates<- seq(data.begin,data.end, by = frequencies.seq[[frequency]])
      for (i in 1:length(all.dates)){
        val <- all.values[[i]]
        if (!is.null(val)){
          time <- format(all.dates[[i]], "%Y-%m-%d")
          if (!time %in% data.rows){
            data.rows <- c(data.rows, time)
          }
          series[[name]][time] <- val
        }
      }
    }
    return (list(series, data.rows))
  }

  reader$CreateDataFrame<- function(resp){
    result  <- reader$CreateSeriesForDataFrame(resp, list(), NULL)
    series <- result[[1]]
    data.rows <- sort(result[[2]])
    data.table <- reader$GetDataFrameByData(data.rows, series)
    return (data.table)
  }

  reader$CreateResultObjectByType <- function (result, type)
  {
     switch (type,
      "DataFrame"={
       series = result[[1]]
       data.rows <- sort(result[[2]])
       data.table <- reader$GetDataFrameByData(data.rows, series)
       return (data.table)
      },
      "MetaDataFrame" ={
        data.rows <- reader$CreateAttributesNamesForMetadata()
        data.table <- reader$GetDataFrameByData(data.rows, result)
        return (data.table)
      },
      "DataTable" = {
        series = result [[1]]
        data.rows <- sort(result[[2]])
        data.columns <- result[[3]]
        data.table <- reader$GetFTableByData(data.rows, data.columns, series)
        return (data.table)
      },
      "MetaDataTable"= {
        series = result[[1]]
        data.rows <- reader$CreateAttributesNamesForMetadata()
        data.columns <- result[[2]]
        data.table <- reader$GetFTableByData(data.rows, data.columns, series, FALSE)
        return (data.table)
      },
      "zoo"={
        return (reader$CreateZoo(result))
      },
      "xts"={
        return (reader$CreateXts(result))
      },
      "ts"={
        return (reader$CreateTs(result))
      }
    )
  }

  reader$CreateResultSeriesForPivotApi <- function(data, result, type){
    switch (type,
            "DataTable" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              data.columns <- result[[3]]
              return(reader$CreateSeriesForDataTableForPivotApi(data, series, data.rows, data.columns))
            },
            "MetaDataTable" ={
              series <- result [[1]]
              data.columns <- result[[2]]
              return(reader$CreateSeriesForMetaDataTableForPivotApi(data, series, data.columns))
            },
            "DataFrame" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForDataFrameForPivotApi(data, series, data.rows))
            },
            "MetaDataFrame"={
              return(reader$CreateSeriesForMetaDataFrameForPivotApi(data, result))
            },
            "ts"={
              return(reader$CreateSeriesForTsXtsZooForPivotApi(data, result))
            },
            "xts"={
              return(reader$CreateSeriesForTsXtsZooForPivotApi(data, result))
            },
            "zoo"={
              return(reader$CreateSeriesForTsXtsZooForPivotApi(data, result))
            },
            {
              error <- simpleError(sprintf("Unknown type %1s",type))
              stop(error)
            }
    )
  }


  reader$CreateResultSeriesForStreamingApi <- function(data, result, type){
    switch (type,
         "DataTable" = {
           series <- result[[1]]
           data.rows <- result[[2]]
           data.columns <- result[[3]]
           return(reader$CreateSeriesForDataTableForStreamingApi(data, series, data.rows, data.columns))
         },
         "MetaDataTable" ={
           series <- result [[1]]
           data.columns <- result[[2]]
           return(reader$CreateSeriesForMetaDataTableForStreamingApi(data, series, data.columns))
         },
         "DataFrame" = {
           series <- result[[1]]
           data.rows <- result[[2]]
           return(reader$CreateSeriesForDataFrameForStreamingApi(data, series, data.rows))
         },
         "MetaDataFrame"={
           return(reader$CreateSeriesForMetaDataFrameForStreamingApi(data, result))
         },
         "ts"={
           return(reader$CreateSeriesForTsXtsZooForStreamingApi(data, result))
         },
         "xts"={
           return(reader$CreateSeriesForTsXtsZooForStreamingApi(data, result))
         },
         "zoo"={
           return(reader$CreateSeriesForTsXtsZooForStreamingApi(data, result))
         },
         {
           error <- simpleError(sprintf("Unknown type %1s",type))
           stop(error)
         }
    )
  }

  reader$GetObjectForSearchingByMnemonics <- function(type, result){
    mnemonics.resp <- reader$client$GetMnemonics(reader$selection$mnemonics)
    if (length(mnemonics.resp)==0)
    {
      warning(simpleError("Series by these mnemonics don't found"))
      return (NULL)
    }
    for (item in mnemonics.resp)
    {
      data <- item$pivot
      result <- reader$CreateResultSeriesForPivotApi(data, result, type)
    }
    return (reader$CreateResultObjectByType(result, type))
  }

  reader$GetObjectForRegularDataset <- function (type, result){
    pivot.request <- reader$CreatePivotRequest()
    pivot.request.json <- pivot.request$SaveToJson()
    data <- reader$client$GetRawData(pivot.request.json)
      if (length(data) == 0){
        warning(simpleError("Dataset do not have data by this selection"))
        return (NULL)
      }
      result <- reader$CreateResultSeriesForStreamingApi(data, result, type)
      return (reader$CreateResultObjectByType(result, type))
  }

  reader$GetObjectForFlatDataset <- function (type, result){
    pivot.request <- reader$CreatePivotRequest()
    pivot.request.json <- pivot.request$SaveToJson()
    data <- reader$client$GetData(pivot.request.json)
    if (length(data$data) == 0){
      warning(simpleError("Dataset do not have data by this selection"))
      return (NULL)
    }
    result <- reader$CreateResultSeriesForPivotApi(data, result, type)
    return (reader$CreateResultObjectByType(result, type))
  }


  reader$GetObjectByType <- function(type){
    for (dim in reader$dataset$dimensions){
      reader$dimensions <- c(reader$dimensions, Dimension(reader$client$GetDimension(reader$dataset$id, dim$id)))
    }
    # initial values
    result <- switch (type,
              "MetaDataTable"= {
                list (list(), list())
              },
              "DataTable"={
                list (list(), NULL, list())
              },
              "DataFrame" = {
                list (list(), NULL, list())
              },
              list()
              )

    if (length(reader$selection)==1 & !is.null(reader$selection$mnemonics))    {
      return (reader$GetObjectForSearchingByMnemonics(type, result))
    }
    if (reader$dataset$type == "Regular")    {
      return (reader$GetObjectForRegularDataset(type, result))
    }
    return (reader$GetObjectForFlatDataset(type, result))
  }

  reader <- list2env(reader)
  class(reader) <- "DataReader"
  return(reader)
}
