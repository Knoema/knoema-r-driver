#' This class read data from Knoema and transform it to ts,xts or zoo
#' @export
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


DataReader <- function(client, dataset, selection){
  reader <- list(
    client = client,
    dataset = Dataset(dataset),
    selection = selection
  )
  #Add a few more methods

  reader$find_dmension <- function (dim_name_or_id){
    dim <- reader$dataset$find_dimension_by_name(dim_name_or_id)
    if (is.null(dim))
    {
      dim <- reader$dataset$find_dimension_by_id(dim_name_or_id)
    }
    return (dim)
  }
  reader$ensure_alldimenions_in_filter <- function(filter_dims){
    dims <- list()
    for (item in reader$dataset$dimensions)
    {
      dims <- c(dims,item$name)
    }
    dims_from_filter <- list()
    for (item in filter_dims)
    {
      dims_from_filter <- c(dims_from_filter,item$name)
    }
    list.condition <- sapply(dims, function(x) ! x %in% dims_from_filter)
    out_of_filter_dim_names <- dims[list.condition]
    if (length (out_of_filter_dim_names)>0)
        {
            error <- simpleError(sprintf('The following dimension(s) are not set: %1s',paste(out_of_filter_dim_names,sep="", collapse =",")))
            stop(error)
        }
  }


  reader$get_dim_members <- function(dim, splited_values){
    members <- c()
    for (value in splited_values)
    {
     if (is.null(value))
     {
        error <- simpleError(sprintf('Selection for dimension %1s is empty',dim$name))
        stop(error)
      }
      member <- dim$findmember_by_id(value)
      if (is.null(member))
        member <- dim$findmember_by_name(value)

      if (is.null(member)& !is.na(suppressWarnings(as.numeric(value))))
           member <- dim$findmember_by_key(as.numeric(value))
      members <- c(members, member$key)
    }
    return (members)
  }

  reader$check_correct_frequencies <- function (values){
    correct_freq <- list("A","H","Q","M","W","D")
    list_condition <- !values %in% correct_freq
    list_err <- values[list_condition]
    if (length(list_err)>0)
    {
      error <- simpleError(sprintf('The following frequencies are not correct: %1s',paste(list_err,sep="", collapse =",")))
      stop(error)
    }
    return (TRUE)
  }

  reader$create_pivot_request <- function ()    {
    request <- PivotRequest(dataset$id)
    filter_dims <- list()
    time_range <- NULL
    for (item in names(reader$selection))
    {
      value <- reader$selection[item][[1]]
      if (item == "timerange")
      {
        time_range <- value
        next
      }
      splited_values <- as.list(strsplit(value, ';')[[1]])
      if (item == "frequency")
      {
        if (reader$check_correct_frequencies(splited_values))
        {
          request$set('frequencies', splited_values)
          next
        }
      }
      dim <- reader$find_dmension(item)
      if (is.null(dim))
      {
         error <- simpleError(sprintf('Dimension with id or name %1s is not found',item))
         stop(error)
      }
      filter_dims <- c(filter_dims,dim)
      dim2 <- Dimension(reader$client$get_dimension(reader$dataset$id, dim$id))
      members <- reader$get_dim_members(dim2, splited_values)
      if (length(members) == 0)
      {
        e = simpleError(sprintf('Selection for dimension %1s is empty',dim$name))
        stop(e)
      }
      l <- c(request$get('stub'),PivotItem(dim$id, members))
      request$set('stub', l)
    }
    reader$ensure_alldimenions_in_filter(filter_dims)
    if (length(time_range != 0))
    {
      l <- c(request$get('header'),PivotTimeItem('Time', time_range, 'range'))
      request$set('header', l)
    }
    else
    {
      l <- c(request$get('header'),PivotTimeItem('Time', list(), 'allData'))
      request$set('header', l)
    }
    return (request)

  }


  reader$create_ts <-function(series){
    result <- list()
    frequencies_seq <- list(A = "year",H = "6 month",Q = "quarter",M = "month", W = "week",D = "day")
    for (i in 1:length(series))
    {
      title <- names(series[i])
      freq <- substring(title, 1, 1)
      freq_seq <- unlist(frequencies_seq)[freq]
      dates <- names(series[[i]])
      if (length(dates) == 0)
      {
        #e = sprintf('Warning: Timeserie %1s is empty',title)
        #print(e)
        next
      }
      min_date <- as.Date(min(dates))
      max_date <- as.Date(max(dates))
      all_dates <- seq(min_date,max_date, by = freq_seq)
      values <- c()
      for (j in 1:length(all_dates))
      {
        if (as.Date(all_dates[j]) %in% as.Date(dates))
        {
          value <- as.numeric(unlist(series[[i]])[as.character(all_dates[j])])
          values <- c(values,value)
        }
        else
        {
          values <- c(values,NA)
        }
      }
      if (freq == "A")
        start_by_freq <- c(year(min_date),1)
      if (freq == "H")
      {
        half_y = (month(min_date)-1)%/%6-1
        start_by_freq <- c(year(min_date),half_y)
      }
      if (freq == "Q")
        start_by_freq <- c(year(min_date),quarter(min_date))
      if (freq == "M")
        start_by_freq <- c(year(min_date),month(min_date))
      if (freq == "W")
        start_by_freq <- c(year(min_date),week(min_date))
      if (freq == "D")
        start_by_freq <- c(year(min_date),day(min_date))

      result[[title]] <- ts(values, start = start_by_freq, frequency = frequency_to_int(freq))
    }
    return (result)
  }

  reader$create_xts <-function(series){
    result <- list()
    for (i in 1:length(series))
    {
      title <- names(series[i])
      if (length(names(series[[i]])) == 0)
          {
            #e = sprintf('Warning: Timeserie %1s is empty',title)
            #print(e)
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
      result[[title]] <- xts(values,order.by = dates,frequency = frequency_to_int(freq))
    }
    return (result)
  }

  reader$create_zoo <-function(series){
    result <- list()
    for (i in 1:length(series))
    {
      title <- names(series[i])
      if (length(names(series[[i]])) == 0)
      {
        #e = sprintf('Warning: Timeserie %1s is empty',title)
        #print(e)
        next
      }
      freq <- substring(title, 1, 1)
      if (freq == "A")
        dates <- as.numeric(format(as.Date(names(series[[i]])),"%Y"))
      if (freq == "Q")
        dates <- as.yearqtr(as.Date(names(series[[i]])))
      if (freq == "M")
        dates <- as.yearmon(as.Date(names(series[[i]])))
      if (freq =="H" || freq == "W" || freq == "D")
        dates <- as.Date(names(series[[i]]))
      values <- as.numeric(unlist(series[[i]]))
      result[[title]] <- zoo(values,order.by = dates,frequency = frequency_to_int(freq))
    }
    return (result)
  }


  reader$get_series <- function (resp, type){
    series <- list()
    if (length(resp$data) == 0)
    {
      warning(simpleError("Dataset do not have data by this selection"))
      return (series)
    }
    else
    {
      for (i in 1:length(resp$data)){
        frequency <- resp$data[[i]]$Frequency
        name <- frequency
        # get name of time series
        for (j in 1:length(resp$stub))
        {
          dim <- resp$stub[[j]]$dimensionId
          name <-  paste(name, resp$data[[i]][[dim]], sep = ' - ');
        }

        # create key-value list where time is the key
        if (is.null(series[[name]]))
          series[[name]] <- list()

        time <- format(as.Date(resp$data[[i]]$Time), "%Y-%m-%d")
        values <- list(time=time,value=resp$data[[i]]$Value)
        series[[name]][time] <- resp$data[[i]]$Value
      }
      if (type == "zoo")
        result <- reader$create_zoo(series)
      if (type == "xts")
        result <- reader$create_xts(series)
      if (type == "ts")
        result <- reader$create_ts(series)
      return (result)
    }
  }
  reader$get_frame <-function(type){
    pivot_request <- reader$create_pivot_request()
    pivot_request_json <- pivot_request$save_to_json()
    data <- reader$client$get_data(pivot_request_json)
    series <- reader$get_series(data, type)
    return (series)
  }

  reader <- list2env(reader)
  class(reader) <- "DataReader"
  return(reader)
}
