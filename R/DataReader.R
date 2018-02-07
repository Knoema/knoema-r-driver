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
#' @importFrom stats setNames


DataReader <- function(client) {

    reader <- list(
    client = client,
    get = function(x)  reader[[x]],
    set = function(x, value)  reader[[x]] <<- value
  )

  reader$CreateTs <- function(series) {
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
      all.dates <- seq(min.date, max.date, by = freq.seq)
      values <- series[[i]][as.character(all.dates)]
      values[sapply(values,is.null)] <- NA
      values <- unname(unlist(values))

      start.by.freq <- switch(freq,
                             "A" = c(year(min.date), 1),
                             "H" = c(year(min.date), (month(min.date)-1)%/%6+1),
                             "Q" = c(year(min.date), quarter(min.date)),
                             "M" = c(year(min.date), month(min.date)),
                             "W" = c(year(min.date), week(min.date)),
                             "D" = c(year(min.date), day(min.date)))

      result[[title]] <- ts(values, start = start.by.freq, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateXts <- function(series) {
    result <- list()
    for (i in 1:length(series)) {
      title <- names(series[i])
      if (length(names(series[[i]])) == 0) {
        next
      }
      dates <- as.Date(names(series[[i]]))
      freq <- substring(title, 1, 1)
      dates.xts <- switch (freq,
                       "Q" = as.yearqtr(dates),
                       "M" = as.yearmon(dates),
                       dates)
      values <- unlist(series[[i]], use.names = FALSE)
      result[[title]] <- xts(values, order.by = dates.xts, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateZoo <- function(series) {
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

  reader$CreateSeriesForTsXtsZoo <- function (resp, series) {
    for (serie.point in resp$data) {
      if (is.null(serie.point$Value)) {
        next
      }
      frequency <- serie.point$Frequency
      name <- frequency
      # get name of time series
      for (stub in resp$stub) {
        dim <- stub$dimensionId
        name <-  paste(name, serie.point[[dim]], sep = " - ")
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"), error = function(e) stop(simpleError("Types ts, xts, zoo are not supported for flat datasets")))
      series[[name]][time] <- serie.point$Value
    }
    return (series)
  }

  reader$CreateMatrixForFrameOrTable <- function(data.rows, series) {
    list.for.matrix <- sapply(1:length(series), function (i) {
      t <- lapply(data.rows, function(j) {
       series[[i]][[j]]})
      t[sapply(t,is.null)]<-NA
      unlist(t)
    })

    matrix <- matrix(list.for.matrix, nrow = length(data.rows), ncol = length(series))
    return (matrix)
  }

  reader$GetFTableByData <- function (data.rows, data.columns, series, row.equal.dates = TRUE) {
    matrix <- reader$CreateMatrixForFrameOrTable(data.rows, series)
    length.by.columns <- unlist(lapply(1:length(data.columns), function(x) length(data.columns[[x]])), recursive = F)
    length.of.all.dimension <- c(length(data.rows), length.by.columns)
    matrix.arr <- array(matrix, length.of.all.dimension)
    if (row.equal.dates) {
      dimnames(matrix.arr) <- c(list(date = data.rows), data.columns)
    } else {
      dimnames(matrix.arr) <- c(list(attributes = data.rows), data.columns)
    }
    data.table <- ftable(matrix.arr, row.vars = 1, col.vars = 2:(length(data.columns) + 1))
    return (data.table)
  }

  reader$GetDataFrameByData <- function(data.rows, series) {
    matrix <- reader$CreateMatrixForFrameOrTable(data.rows, series)
    data.frame <- as.data.frame(matrix, row.names = data.rows, stringsAsFactors = FALSE)
    colnames(data.frame) <- names(series)
    return (data.frame)
  }

  reader$GetXtsByData <- function(data.rows, series) {
    matrix <- reader$CreateMatrixForFrameOrTable(data.rows, series)
    data.frame <- xts(matrix, order.by = as.Date(data.rows))
    colnames(data.frame) <- names(series)
    return (data.frame)
  }

  reader$GetSeriesNameWithMetadata <- function(series.point) {
    names <- list()
    for (dim in reader$get("dimensions")) {
      for (item in dim$items) {
        if (item$name == series.point[dim$dim.model$id]) {
          dim.attr <- item$fields
          break
        }
      }
      for (attr in dim$fields) {
        if (!attr$isSystemField) {
          for (i in 1: length(dim.attr)) {
            if (IsEqualStringsIgnoreCase(names(dim.attr[i]), attr$name)) {
              names[[paste(dim$dim.model$name, attr$displayName, sep=" ")]] <- dim.attr[[i]]
            }
          }
        }
      }
    }
    names[["Unit"]] <- series.point$Unit
    names[["Scale"]] <- series.point$Scale
    names[["Mnemonics"]] <- ifelse(is.null(series.point$Mnemonics), "NULL", series.point$Mnemonics)
    for (attr in reader$dataset$time.series.attributes) {
        names[[attr$name]] <- series.point[[attr$name]]
    }
    return (names)
  }

  reader$CreateAttributesNamesForMetadata <- function() {
    data.rows <- NULL
    for (dim in reader$dimensions) {
      for (attr in dim$fields) {
        if (!attr$isSystemField) {
          data.rows <- c(data.rows, paste(dim$dim.model$name, attr$displayName, sep = " "))
        }
      }
    }
    data.rows <- c(data.rows, c("Unit", "Scale", "Mnemonics"))
    for (attr in reader$dataset$time.series.attributes) {
      data.rows <- c(data.rows, attr$name)
    }
    return(data.rows)
  }

  reader$CreateSeriesForMetaDataTable <- function(resp, series, data.columns) {
    for (serie.point in resp$data) {
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (j in 1:length(resp$stub)) {
        dim <- resp$stub[[j]]$dimensionId
        dim.name <- reader$FindDimension(dim)$name
        name.element <- serie.point[[dim]]
        if (!name.element %in% data.columns[[dim.name]]) {
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      if (!frequency %in% data.columns[["Frequency"]]) {
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- name.meta
      }
    }
    return (list(series, data.columns))
  }

  reader$CreateSeriesForMetaDataFrame <- function (resp, series) {
    for (serie.point in resp$data) {
      if (is.null(serie.point$Value)) {
        next
      }
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (j in 1:length(resp$stub)) {
        dim <- resp$stub[[j]]$dimensionId
        name.element <- serie.point[[dim]]
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- name.meta
      }
    }
    return (series)
  }

  reader$CreateMetaDataFrame <- function(resp) {
    data.rows <- reader$CreateAttributesNamesForMetadata()
    series <- reader$CreateSeriesForMetaDataFrame (resp, list())[[1]]
    data.table <- reader$GetDataFrameByData(data.rows, series)
    return (data.table)
  }

  reader$CreateSeriesForDataTable <- function (resp, series, data.rows, data.columns) {
    for (serie.point in resp$data) {
      name <- NULL
      # create columns with dimension names for ftable object
      for (stub in resp$stub) {
        dim <- stub$dimensionId
        dim.name <- reader$FindDimension(dim)$name
        name.element <- serie.point[[dim]]
        if (!name.element %in% data.columns[[dim.name]]) {
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      if (!frequency %in% data.columns[["Frequency"]]) {
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"), error = function(e) serie.point$Time)
      if (!time %in% data.rows) {
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    return (list(series, data.rows, data.columns))
  }

  reader$CreateDataTable <- function(resp) {
    result <- reader$CreateSeriesForDataTable(resp, list(), NULL, list())
    series <- result[[1]]
    data.rows <- result[[2]]
    data.columns <- result[[3]]
    data.rows <- sort(data.rows)
    data.table <- reader$GetFTableByData(data.rows, data.columns, series)
    return (data.table)
  }

  reader$CreateSeriesForDataFrame <- function(resp, series, data.rows) {
    for (serie.point in resp$data) {
      if (is.null(serie.point$Value)) {
        next
      }
      name <- NULL
      # create columns with dimension names for frable object
      for (stub in resp$stub) {
        dim <- stub$dimensionId
        name.element <- serie.point[[dim]]
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$Frequency
      name <- paste(name, frequency, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"), error = function(e) serie.point$Time)
      if (!time %in% data.rows) {
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    return (list(series, data.rows))
  }

  reader$CreateDataFrame <- function(resp) {
    result  <- reader$CreateSeriesForDataFrame(resp, list(), NULL)
    series <- result[[1]]
    data.rows <- sort(result[[2]])
    data.table <- reader$GetDataFrameByData(data.rows, series)
    return (data.table)
  }

  reader$CreateResultObjectByType <- function (result, type) {
     switch (type,
      "DataFrame" = {
       series = result[[1]]
       data.rows <- sort(result[[2]])
       data.table <- reader$GetDataFrameByData(data.rows, series)
       return (data.table)
      },
      "MetaDataFrame" = {
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
      "MetaDataTable" = {
        series = result[[1]]
        data.rows <- reader$CreateAttributesNamesForMetadata()
        data.columns <- result[[2]]
        data.table <- reader$GetFTableByData(data.rows, data.columns, series, FALSE)
        return (data.table)
      },
      "zoo" = {
        return (reader$CreateZoo(result))
      },
      "xts" = {
        series = result[[1]]
        data.rows <- sort(result[[2]])
        data.table <- reader$GetXtsByData(data.rows, series)
        return (data.table)
      },
      "ts" = {
        return (reader$CreateTs(result))
      }
    )
  }

  reader$CreateResultSeries <- function(data, result, type) {
    switch (type,
            "DataTable" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              data.columns <- result[[3]]
              return(reader$CreateSeriesForDataTable(data, series, data.rows, data.columns))
            },
            "MetaDataTable" = {
              series <- result [[1]]
              data.columns <- result[[2]]
              return(reader$CreateSeriesForMetaDataTable(data, series, data.columns))
            },
            "DataFrame" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForDataFrame(data, series, data.rows))
            },
            "MetaDataFrame" = {
              return(reader$CreateSeriesForMetaDataFrame(data, result))
            },
            "ts" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result))
            },
            "xts" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result))
            },
            "zoo" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result))
            },
            {
              error <- simpleError(sprintf("Unknown type %1s", type))
              stop(error)
            }
    )
  }

  reader$LoadDimensions <- function () {
    l <- list ()
    for (dim in reader$get("dataset")$dimensions) {
      d <- Dimension(reader$client$GetDimension(reader$get("dataset")$id, dim$id))
      l <- c(l, d)
    }
    reader$set("dimensions", l)
  }

  reader <- list2env(reader)
  return(reader)
}

SelectionDataReader  <- function(client, selection) {
  reader <- DataReader (client)
  reader$set("selection", selection)

  reader$CheckCorrectFrequencies <- function (values) {
    correct.freq <- list("A","H","Q","M","W","D")
    list.condition <- !values %in% correct.freq
    list.err <- values[list.condition]
    if (length(list.err)>0) {
      error <- simpleError(sprintf("The following frequencies are not correct: %1s", paste(list.err, sep="", collapse =",")))
      stop(error)
    }
    return (TRUE)
  }

  reader$FindDimension <- function (dim.name.or.id) {
    dim <- reader$dataset$FindDimensionByName(dim.name.or.id)
    if (is.null(dim)) {
      dim <- reader$dataset$FindDimensionById(dim.name.or.id)
    }
    return (dim)
  }

  reader$GetDimMembers <- function(dim, split.values) {
    members <- NULL
    for (value in split.values) {
      if (is.null(value)) {
        error <- simpleError(sprintf("Selection for dimension %1s is empty", dim$name))
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

  reader$AddFullSelectionByEmptyDimValues <- function(filter.dims, request) {
    dims <- lapply(1:length(reader$dataset$dimensions), function(x) reader$dataset$dimensions[[x]]$id)
    if (length(filter.dims)>0) {
      dims.from.filter <- lapply(1:length(filter.dims), function(x) filter.dims[[x]]$id)
      list.condition <- sapply(dims, function(x) ! x %in% dims.from.filter)
      out.of.filter.dim.names <- dims[list.condition]
    } else {
      out.of.filter.dim.names <- dims
    }

    for (id in out.of.filter.dim.names)
    {
      l <- c(request$get("stub"), PivotItem(id, list()))
      request$set("stub", l)
    }
  }

  reader$CreatePivotRequest <- function () {
    request <- PivotRequest(reader$dataset$id)
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
        error <- simpleError(sprintf("Dimension with id or name %1s is not found", item))
        stop(error)
      }
      filter.dims <- c(filter.dims, dim)
      for (dimension in reader$dimensions) {
        if(dimension$dim.model$id == dim$id)
        {
          dim2 <- dimension
          break
        }
      }
      members <- reader$GetDimMembers(dim2, splited.values)
      if (length(members) == 0) {
        e = simpleError(sprintf("Selection for dimension %1s is empty", dim$name))
        stop(e)
      }
      l <- c(request$get("stub"), PivotItem(dim$id, members))
      request$set("stub", l)
    }

    reader$AddFullSelectionByEmptyDimValues(filter.dims, request)
    if (length(time.range != 0)) {
      l <- c(request$get("header"), PivotTimeItem("Time", time.range, "range"))
      request$set("header", l)
    } else {
      l <- c(request$get("header"), PivotTimeItem("Time", list(), "allData"))
      request$set("header", l)
    }
    return (request)

  }

  return (reader)
}

PivotDataReader <- function(client, selection) {
  reader <- SelectionDataReader (client, selection)

  reader$GetObjectByType <- function(type) {
    reader$LoadDimensions()
    # initial values
    result <- switch (type,
                      "MetaDataTable" = {
                        list (list(), list())
                      },
                      "DataTable" = {
                        list (list(), NULL, list())
                      },
                      "DataFrame" = {
                        list (list(), NULL, list())
                      },
                      list()
    )
    return (reader$GetObjectForFlatDataset(type, result))
  }

  reader$GetObjectForFlatDataset <- function (type, result) {
    pivot.request <- reader$CreatePivotRequest()
    pivot.request.json <- pivot.request$SaveToJson()
    data <- reader$client$GetData(pivot.request.json)
    if (length(data$data) == 0) {
      warning(simpleError("Dataset do not have data by this selection"))
      return (NULL)
    }
    result <- reader$CreateResultSeries(data, result, type)
    return (reader$CreateResultObjectByType(result, type))
  }

  return (reader)
}

StreamingDataReader <- function(client, selection) {
  reader <- SelectionDataReader (client, selection)

  reader$CreateSeriesForTsXtsZoo <- function (resp, series) {
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (serie.point in resp) {
      all.values <- serie.point$values
      frequency <- serie.point$frequency
      name <- frequency
      # get name of time series
      for (dim in reader$dimensions) {
        name <- paste(name, serie.point[[dim$dim.model$id]]$name, sep = " - ")
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      data.begin <- as.Date(serie.point$startDate)
      data.end <- as.Date(serie.point$endDate)
      if (frequency == "W") {
        data.begin <- data.begin - days(as.numeric(strftime(data.begin, "%u"))-1)
        data.end <- data.end - days(as.numeric(strftime(data.end, "%u"))-1)
      }
      all.dates<- seq(data.begin, data.end, by = frequencies.seq[[frequency]])
      index.without.nan  <- which(!all.values %in% list(NULL))
      dates <- format(all.dates[index.without.nan], "%Y-%m-%d")
      serie <- setNames(all.values[index.without.nan], dates)
      series[[name]] <- serie
      }
    return (series)
  }

  reader$GetSeriesNameWithMetadata <- function(series.point) {
    names <- list()
    for (dim in reader$dimensions) {
      for (item in dim$items) {
        if (item$name == series.point[[dim$dim.model$id]]$name) {
          dim.attr <- item$fields
          break
        }
      }
      for (attr in dim$fields) {
        if (!attr$isSystemField) {
          for (i in 1: length(dim.attr)) {
            if (IsEqualStringsIgnoreCase(names(dim.attr[i]), attr$name)) {
              names[[paste(dim$dim.model$name, attr$displayName, sep=" ")]] <- dim.attr[[i]]
            }
          }
        }
      }
    }
    names[["Unit"]] <- series.point$unit
    names[["Scale"]] <- series.point$scale
    names[["Mnemonics"]] <- ifelse(is.null(series.point$mnemonics), "NULL", series.point$mnemonics)
    for (attr in reader$dataset$time.series.attributes) {
      names[[attr$name]] <- series.point$timeseriesAttributes[[attr$name]]
    }
    return (names)
  }

  reader$CreateSeriesForMetaDataTable <- function(resp, series, data.columns) {
    for (serie.point in resp) {
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (dim in reader$dimensions) {
        name.element <- serie.point[[dim$dim.model$id]]$name
        dim.name <- dim$dim.model$name
        if (!name.element %in% data.columns[[dim.name]]) {
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      if (!frequency %in% data.columns[["Frequency"]]) {
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- name.meta
      }
    }
    return (list(series, data.columns))
  }

  reader$CreateSeriesForMetaDataFrame <- function (resp, series) {
    for (serie.point in resp) {
      name <- NULL
      name.meta <- list()
      # get name of time series
      for (dim in reader$dimensions) {
        name.element <- serie.point[[dim$dim.model$id]]$name
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- name.meta
      }
    }
    return (series)
  }

  reader$CreateSeriesForDataTable <- function (resp, series, data.rows, data.columns) {
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (serie.point in resp) {
      all.values <- serie.point$values
      name <- NULL
      # create columns with dimension names for ftable object
      for (dim in reader$dimensions) {
        name.element <- serie.point[[dim$dim.model$id]]$name
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
        dim.name <- dim$dim.model$name
        if (!name.element %in% data.columns[[dim.name]]) {
          data.columns[[dim.name]] <- c(data.columns[[dim.name]], name.element)
        }
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      if (!frequency %in% data.columns[["Frequency"]]) {
        data.columns[["Frequency"]] <- c(data.columns[["Frequency"]], frequency)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      data.begin <- as.Date(serie.point$startDate)
      data.end <- as.Date(serie.point$endDate)
      if (frequency == "W") {
        data.begin <- data.begin - days(as.numeric(strftime(data.begin,"%u"))-1)
        data.end <- data.end - days(as.numeric(strftime(data.end,"%u"))-1)
      }
      all.dates<- seq(data.begin, data.end, by = frequencies.seq[[frequency]])
      index.without.nan  <- which(!all.values %in% list(NULL))
      dates <- format(all.dates[index.without.nan ], "%Y-%m-%d")
      data.rows <- unique(c(data.rows, dates))
      serie <- setNames(all.values[index.without.nan], dates)
      series[[name]] <- serie
    }
    return (list(series, data.rows, data.columns))
  }

  reader$CreateSeriesForDataFrame <- function(resp, series, data.rows) {
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")
    for (serie.point in resp) {
      all.values <- serie.point$values
      name <- NULL
      # create columns with dimension names for ftable object
      for (dim in reader$dimensions) {
        name.element <- serie.point[[dim$dim.model$id]]$name
        name <- ifelse(is.null(name), name.element, paste(name, name.element, sep = " - "))
      }
      frequency <- serie.point$frequency
      name <- paste(name, frequency, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      data.begin <- as.Date(serie.point$startDate)
      data.end <- as.Date(serie.point$endDate)
      if (frequency == "W") {
        data.begin <- data.begin - days(as.numeric(strftime(data.begin, "%u"))-1)
        data.end <- data.end - days(as.numeric(strftime(data.end, "%u"))-1)
      }
      all.dates<- seq(data.begin, data.end, by = frequencies.seq[[frequency]])

      index.without.nan  <- which(!all.values %in% list(NULL))
      dates <- format(all.dates[index.without.nan ], "%Y-%m-%d")
      data.rows <- unique(c(data.rows, dates))
      serie <- setNames(all.values[index.without.nan], dates)
      series[[name]] <- serie
    }
    return (list(series, data.rows))
  }

  reader$CreateResultSeries <- function(data, result, type) {
    switch (type,
            "DataTable" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              data.columns <- result[[3]]
              return(reader$CreateSeriesForDataTable(data, series, data.rows, data.columns))
            },
            "MetaDataTable" = {
              series <- result [[1]]
              data.columns <- result[[2]]
              return(reader$CreateSeriesForMetaDataTable(data, series, data.columns))
            },
            "DataFrame" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForDataFrame(data, series, data.rows))
            },
            "MetaDataFrame" = {
              return(reader$CreateSeriesForMetaDataFrame(data, result))
            },
            "ts" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result))
            },
            "xts" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForDataFrame(data, series, data.rows))
            },
            "zoo" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result))
            },
            {
              error <- simpleError(sprintf("Unknown type %1s", type))
              stop(error)
            }
    )
  }

  reader$GetObjectForRegularDataset <- function (type, result) {
    pivot.request <- reader$CreatePivotRequest()
    pivot.request.json <- pivot.request$SaveToJson()
    data <- reader$client$GetRawData(pivot.request.json)
    if (length(data) == 0) {
      warning(simpleError("Dataset do not have data by this selection"))
      return (NULL)
    }
    result <- reader$CreateResultSeries(data, result, type)
    return (reader$CreateResultObjectByType(result, type))
  }

  reader$GetObjectByType <- function(type) {
    reader$LoadDimensions()
    # initial values
    result <- switch (type,
                      "MetaDataTable" = {
                        list (list(), list())
                      },
                      "DataTable" = {
                        list (list(), NULL, list())
                      },
                      "DataFrame" = {
                        list (list(), NULL, list())
                      },
                      "xts" = {
                        list (list(), NULL, list())
                      },
                      list()
    )
      return (reader$GetObjectForRegularDataset(type, result))
  }

  return (reader)
}

MnemonicsDataReader<- function(client, mnemonics) {
  reader <- DataReader (client)
  reader$set("mnemonics", mnemonics)

  reader$CreateTs <- function(series) {
    result <- list()
    frequencies.seq <- list(A = "year", H = "6 month", Q = "quarter", M = "month", W = "week", D = "day")

    for (i in 1:length(series)) {
      title.with.freq <- names(series[i])
      freq <- substring(title.with.freq, 1, 1)
      title <- substring(title.with.freq, 5)
      freq.seq <- unlist(frequencies.seq)[freq]
      dates <- as.Date(names(series[[i]]))
      if (length(dates) == 0) {
        next
      }
      min.date <- min(dates)
      max.date <- max(dates)
      all.dates <- seq(min.date, max.date, by = freq.seq)
      values <- sapply(1:length(all.dates), function(x) {
        dat <- all.dates[x]
        cond.v <- dat %in% dates
        return(ifelse (cond.v, series[[i]][[as.character(dat)]], NA))
      })

      start.by.freq <- switch(freq,
                              "A" = c(year(min.date), 1),
                              "H" = c(year(min.date), (month(min.date)-1)%/%6+1),
                              "Q" = c(year(min.date), quarter(min.date)),
                              "M" = c(year(min.date), month(min.date)),
                              "W" = c(year(min.date), week(min.date)),
                              "D" = c(year(min.date), day(min.date)))

      result[[title]] <- ts(values, start = start.by.freq, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateXts <- function(series) {
    result <- list()
    for (i in 1:length(series)) {
      title.with.freq <- names(series[i])
      if (length(names(series[[i]])) == 0) {
        next
      }
      dates <- as.Date(names(series[[i]]))
      freq <- substring(title.with.freq, 1, 1)
      title <- substring(title.with.freq, 5)
      dates.xts <- switch (freq,
                           "Q" = as.yearqtr(dates),
                           "M" = as.yearmon(dates),
                           dates)
      values <- unlist(series[[i]], use.names = FALSE)
      result[[title]] <- xts(values, order.by = dates.xts, frequency = FrequencyToInt(freq))
    }
    return (result)
  }

  reader$CreateZoo <- function(series) {
    result <- list()
    for (i in 1:length(series)) {
      title.for.freq <- names(series[i])
      dates <- as.Date(names(series[[i]]))
      if (length(dates) == 0) {
        next
      }
      freq <- substring(title.for.freq, 1, 1)
      title <- substring(title.for.freq, 5)
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

  reader$CreateSeriesForTsXtsZoo <- function (resp, series, mnemonic) {
    for (serie.point in resp$data) {
      if (is.null(serie.point$Value)) {
        next
      }
      frequency <- serie.point$Frequency
      name <- paste(frequency, mnemonic, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"), error = function(e) stop(simpleError("Types ts, xts, zoo are not supported for flat datasets")))
      series[[name]][time] <- serie.point$Value
    }
    return (series)
  }

  reader$GetDataFrameByData <- function(data.rows, series) {
    matrix <- reader$CreateMatrixForFrameOrTable(data.rows, series)
    data.frame <- as.data.frame(matrix, row.names = data.rows, stringsAsFactors = FALSE)
    colnames(data.frame) <- substring(names(series),5)
    return (data.frame)
  }

  reader$CreateSeriesForMetaDataTable <- function(resp, series, data.columns, data.rows, mnemonic) {
    for (serie.point in resp$data) {
      name.meta <- list()
      frequency <- serie.point$Frequency
      name <- paste(frequency, mnemonic, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      if (!mnemonic %in% data.columns[["Mnemonics"]]) {
        data.columns[["Mnemonics"]] <- c(data.columns[["Mnemonics"]], mnemonic)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- name.meta
      }
    }
    return (list(series, data.rows, data.columns))
  }

  reader$CreateSeriesForMetaDataFrame <- function (resp, series, mnemonic) {
    for (serie.point in resp$data) {
      if (is.null(serie.point$Value)) {
        next
      }
      name.meta <- list()
      frequency <- serie.point$Frequency
      name <- paste(frequency, mnemonic, sep = " - ")
      name.meta <- reader$GetSeriesNameWithMetadata(serie.point)
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- name.meta
      }
    }
    return (series)
  }

  reader$CreateSeriesForDataTable <- function (resp, series, data.rows, data.columns, mnemonic) {
    for (serie.point in resp$data) {
      frequency <- serie.point$Frequency
      name <- paste(frequency, mnemonic, sep = " - ")
      if (!mnemonic %in% data.columns[["Mnemonics"]]) {
        data.columns[["Mnemonics"]] <- c(data.columns[["Mnemonics"]], mnemonic)
      }
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"), error = function(e) serie.point$Time)
      if (!time %in% data.rows) {
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    return (list(series, data.rows, data.columns))
  }

  reader$CreateSeriesForDataFrame <- function(resp, series, data.rows, mnemonic) {
    for (serie.point in resp$data) {
      if (is.null(serie.point$Value)) {
        next
      }
      # create columns with dimension names for frable object
      frequency <- serie.point$Frequency
      name <- paste(frequency, mnemonic, sep = " - ")
      # create key-value list where time is the key
      if (is.null(series[[name]])) {
        series[[name]] <- list()
      }
      time <- tryCatch(format(as.Date(serie.point$Time), "%Y-%m-%d"), error = function(e) serie.point$Time)
      if (!time %in% data.rows) {
        data.rows <- c(data.rows, time)
      }
      series[[name]][time] <- serie.point$Value
    }
    return (list(series, data.rows))
  }

  reader$CreateAttributesNamesForMetadata <- function(data.rows) {
    for (dim in reader$dimensions) {
      for (attr in dim$fields) {
        if (!attr$isSystemField) {
          value <- paste(dim$dim.model$name, attr$displayName, sep = " ")
          if (!value %in% data.rows) {
          data.rows <- c(data.rows, value)
          }
        }
      }
    }
    if (!"Unit" %in% data.rows) {
      data.rows <- c(data.rows, c("Unit", "Scale", "Mnemonics"))
    }
    for (attr in reader$dataset$time.series.attributes) {
      if (!attr$name %in% data.rows) {
      data.rows <- c(data.rows, attr$name)
      }
    }


    return(data.rows)
  }

  reader$CreateResultObjectByType <- function (result, type) {
    switch (type,
            "DataFrame" = {
              series <- result[[1]]
              data.rows <- sort(result[[2]])
              data.table <- reader$GetDataFrameByData(data.rows, series)
              return (data.table)
            },
            "MetaDataFrame" = {
              series <- result[[1]]
              data.rows <- sort(result[[2]])
              data.table <- reader$GetDataFrameByData(data.rows, series)
              return (data.table)
            },
            "DataTable" = {
              series <- result [[1]]
              data.rows <- sort(result[[2]])
              data.columns <- result[[3]]
              data.table <- reader$GetFTableByData(data.rows, data.columns, series)
              return (data.table)
            },
            "MetaDataTable" = {
              series <- result[[1]]
              data.rows <- sort(result[[2]])
              data.columns <- result[[3]]
              data.table <- reader$GetFTableByData(data.rows, data.columns, series, FALSE)
              return (data.table)
            },
            "zoo" = {
              return (reader$CreateZoo(result))
            },
            "xts" = {
              series <- result[[1]]
              data.rows <- sort(result[[2]])
              data.table <- reader$GetXtsByData(data.rows, series)
              return (data.table)
            },
            "ts" = {
              return (reader$CreateTs(result))
            }
    )
  }

  reader$CreateResultSeries <- function(data, result, type, mnemonic) {
    switch (type,
            "DataTable" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              data.columns <- result[[3]]
              return(reader$CreateSeriesForDataTable(data, series, data.rows, data.columns, mnemonic))
            },
            "MetaDataTable" = {
              series <- result [[1]]
              data.columns <- result[[3]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForMetaDataTable(data, series, data.columns, data.rows, mnemonic))
            },
            "DataFrame" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForDataFrame(data, series, data.rows, mnemonic))
            },
            "MetaDataFrame" = {
              res <- reader$CreateSeriesForMetaDataFrame(data, result[[1]], mnemonic)
              return (list(res, result[[2]]))
            },
            "ts" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result, mnemonic))
            },
            "xts" = {
              series <- result[[1]]
              data.rows <- result[[2]]
              return(reader$CreateSeriesForDataFrame(data, series, data.rows, mnemonic))
            },
            "zoo" = {
              return(reader$CreateSeriesForTsXtsZoo(data, result, mnemonic))
            },
            {
              error <- simpleError(sprintf("Unknown type %1s", type))
              stop(error)
            }
    )
  }

  reader$GetObjectForSearchingByMnemonicsInOneDataset <- function(type, result) {
    mnemonics.resp <- reader$client$GetMnemonics(reader$mnemonics)
    if (length(mnemonics.resp)==0) {
      warning(simpleError("Series by these mnemonics don't found"))
      return (NULL)
    }
    for (item in mnemonics.resp) {
      data <- item$pivot
      if (!IsEqualStringsIgnoreCase(reader$get("dataset")$id, data$dataset)) {
        next
      }
      mnemonic <- item$mnemonics
      if (type == "MetaDataFrame" || type == "MetaDataTable") {
        result[[2]] <- reader$CreateAttributesNamesForMetadata (result[[2]])
      }
      result <- reader$CreateResultSeries(data, result, type, mnemonic)
    }
    return (reader$CreateResultObjectByType(result, type))
  }

  reader$GetObjectForSearchingByMnemonicsAccrossDatasets <- function(type, result) {
    mnemonics.resp <- reader$client$GetMnemonics(reader$mnemonics)
    if (length(mnemonics.resp)==0) {
      warning(simpleError("Series by these mnemonics don't found"))
      return (NULL)
    }
    datasets.list <- list()
    dimensions.list <- list()
    for (item in mnemonics.resp) {
      data <- item$pivot
      if (is.null(data)) {
        next
      }
      mnemonic <- item$mnemonics
      if (type == "MetaDataFrame" || type == "MetaDataTable") {
        dataset.id <- data$dataset
        if (!dataset.id %in% names(datasets.list)) {
          dataset <- Dataset(reader$client$GetDataset(dataset.id))
          reader$set("dataset", dataset)
          datasets.list[[dataset.id]] <- dataset
          reader$LoadDimensions()
          dimensions.list[[dataset.id]] <- reader$get("dimensions")
          result[[2]] <- reader$CreateAttributesNamesForMetadata (result[[2]])
        } else {
          reader$set("dataset", datasets.list[[dataset.id]])
          reader$set("dimensions", dimensions.list[[dataset.id]])
        }
      }
      result <- reader$CreateResultSeries(data, result, type, mnemonic)
    }
    return (reader$CreateResultObjectByType(result, type))
  }

  reader$GetObjectByType <- function(type) {
    # initial values
    result <- switch (type,
                      "MetaDataTable" = {
                        list (list(), NULL, list())
                      },
                      "MetaDataFrame" = {
                        list (list(), NULL)
                      },
                      "DataTable" = {
                        list (list(), NULL, list())
                      },
                      "DataFrame" = {
                        list (list(), NULL, list())
                      },
                      "xts" = {
                        list (list(), NULL, list())
                      },
                      list()
    )

    if (!is.null(reader$dataset)) {
      reader$LoadDimensions()
      return (reader$GetObjectForSearchingByMnemonicsInOneDataset(type, result))
    }
    return (reader$GetObjectForSearchingByMnemonicsAccrossDatasets(type, result))
  }

  return (reader)
}
