# This file contains metadata definitions for Knoema API

# The class contains dimension description

#' @importFrom  jsonlite toJSON
DimensionModel <- function (data) {
  dimension.model = list(
    key = data$key,
    id = data$id,
    name = data$name
  )

  dimension.model <- list2env(dimension.model)
  class(dimension.model) <- "DimensionModel"
  return(dimension.model)
}

DimensionModelList <- function (data) {
  dimension.model.list <- lapply(1:length(data), function(x) DimensionModel(data[[x]]))
  class(dimension.model.list) <- "DimensionModelList"
  return(dimension.model.list)
}

# The class contains dimension member information
DimensionMember <- function(data) {
  dimension.member = list(
    key = data$key,
    name = data$name,
    level = data$level,
    hasdata = data$hasData,
    fields = data$fields
  )
  return(dimension.member)
}

# The class contains dimension description and dimension items
Dimension <- function (data) {
  dimension = list(
    dim.model = DimensionModel(data),
    items = list(),
    fields = list(),
    key.map = list(),
    id.map = list(),
    name.map = list()
  )
  dimension$items <- lapply(1:length(data$items), function(x) DimensionMember(data$items[x][[1]]))
  dimension$fields <- data$fields

  dimension$key.map <- sapply(dimension$items, function(x) split(x, x$key))
  dimension$name.map <- sapply(dimension$items, function(x) split(x, toupper(x$name)))
  dimension$id.map <- sapply(dimension$items, function(x) if ('id' %in% names(x$fields)) split(x, toupper(x$fields$id)))

  # The method searches member of dimension by given member key
  dimension$FindMemberByKey <- function(member.key) {
    return (dimension$key.map[[as.character(as.integer(member.key))]])
  }

  # The method searches member of dimension by given member id
  dimension$FindMemberById <- function(member.id) {
    return (dimension$id.map[[toupper(as.character(member.id))]])
  }

  # The method searches member of dimension by given member name
  dimension$FindMemberByName <- function (member.name) {
    return (dimension$name.map[[toupper(member.name)]])
  }
  dimension <- list2env(dimension)
  class(dimension) <- "Dimension"
  return(dimension)
}

# The class contains information about timeseries attributes
TimeSeriesAttribute <- function (data) {
  return (list(
    name = data$name,
    type = data$type,
    allowedValues = data$allowedValues
  ))
}


# The class contains dataset description
Dataset <-function (data) {
  dataset = list(
    id = data$id,
    type = data$type,
    dimensions = DimensionModelList(data$dimensions),
    time.series.attributes = list()
   )

if (!is.null(data$timeseriesAttributes) && length(data$timeseriesAttributes) != 0) {
    dataset$time.series.attributes <- lapply(1:length(data$timeseriesAttributes), function(x) TimeSeriesAttribute(data$timeseriesAttributes[x][[1]]))
  }
  # The method searching dimension with a given name
  dataset$FindDimensionByName <- function (dim.name) {
    for (dim in dataset$dimensions) {
      if (IsEqualStringsIgnoreCase(dim$name, dim.name)) {
        return (dim)
      }
    }
    return (NULL)
  }

  # The method searching dimension with a given id
  dataset$FindDimensionById <- function (dim.id) {
    for (dim in dataset$dimensions) {
      if (IsEqualStringsIgnoreCase(dim$id, dim.id)) {
        return (dim)
      }
    }
    return (NULL)
  }
  dataset <- list2env(dataset)
  class(dataset) <- "Dataset"
  return(dataset)
}

# The class contains pivot request item
PivotItem <- function(dimension.id, members) {
  pivot.item = list(
    dimension.id = dimension.id,
    members = members
  )
  pivot.item <- list2env(pivot.item)
  class(pivot.item) <- "PivotItem"
  return(pivot.item)
}

# The class contains pivot request item
PivotTimeItem <- function(dimension.id=NULL, members=NULL, uimode=NULL) {
  pivot.time.item = list(
    dimension.id = dimension.id,
    members = members,
    uimode = uimode
  )
  pivot.time.item  <- list2env(pivot.time.item)
  class(pivot.time.item) <- "PivotTimeItem"
  return(pivot.time.item)
}

# The class contains pivot request
PivotRequest <- function(dataset) {
  pivot.request = list(
    dataset = dataset,
    header = list(),
    stub = list(),
    filter = list(),
    frequencies = list(),
    get = function(x) pivot.request[[x]],
    set = function(x, value) pivot.request[[x]] <<- value
  )
  # Methods
  pivot.request$GetItemArray <- function (items) {
    arr <- list()
    i <- 1
    if (length(items)>0) {
      for (item in items) {
        item.values <- list(
          "DimensionId" = item$dimension.id,
          "Members" = item$members
        )
        if (is(item, "PivotTimeItem")) {
          item.values["UiMode"] <- item$uimode
        }
        arr[[i]] <- item.values
        i <- i + 1
      }
    }
    return (arr)
  }
  pivot.request$SaveToJson <- function() {
    request.values <- list(
      "Dataset"= pivot.request$get("dataset"),
      "Header" = pivot.request$GetItemArray(pivot.request$get("header")),
      "Filter" = pivot.request$GetItemArray(pivot.request$get("filter")),
      "Stub" = pivot.request$GetItemArray(pivot.request$get("stub")),
      "Frequencies" = pivot.request$get("frequencies")
    )

    return (toJSON(request.values, auto_unbox = TRUE))
  }
  class(pivot.request) <- "PivotRequest"
  return(pivot.request)
}



