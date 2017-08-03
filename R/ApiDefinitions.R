#' This module contains metadata definitions for Knoema API

#' The class contains dimension description
#' @export
#' @importFrom  jsonlite toJSON
DimensionModel <- function (data){
  dimensionModel=list(
    key = data$key,
    id = data$id,
    name = data$name
  )

  dimensionModel <- list2env(dimensionModel)
  class(dimensionModel) <- "DimensionModel"
  return(dimensionModel)
}

#' @export
DimensionModelList <- function (data){
  dimensionModelList=list()

  for (dim in data)
  {
    dimModel <- DimensionModel(dim)
    dimensionModelList<-c(dimensionModelList, dimModel)
  }
  class(dimensionModelList) <- "DimensionModelList"
  return(dimensionModelList)
}

#' The class contains dimension member information
#' @export
DimensionMember <- function(data){
  dimensionMember=list(
    key = data$key,
    name = data$name,
    level = data$level,
    hasdata = data$hasData,
    fields = data$fields
  )
  dimensionMember <- list2env(dimensionMember)
  class(dimensionMember) <- "DimensionMember"
  return(dimensionMember)
}

#' The class contains dimension description and dimnesion items
#' @export
Dimension <- function (data){
  dimension=list(
    items=list()
  )
  for (i in 1:length(data$items))
  {
    item <- data$items[i][[1]]
    member <- DimensionMember(item)
    dimension$items <-c (dimension$items,member)
  }

  # The method searches member of dimension by given member key
  dimension$findmember_by_key <- function(member_key){
    for (item in dimension$items)
      if (item$key == member_key)
        return (item)
    return (NULL)
  }

  # The method searches member of dimension by given member id
  dimension$findmember_by_id <- function(member_id){
    for (item in dimension$items)
    {
      if ("id" %in% names(item$fields) & isequal_strings_ignorecase(item$fields$id, member_id))
        return (item)
    }
    return (NULL)
  }

  # The method searches member of dimension by given member name
  dimension$findmember_by_name <-function (member_name){
    for (item in dimension$items)
      if (isequal_strings_ignorecase(item$name, member_name))
        return (item)
    return (NULL)
  }
  dimension <- list2env(dimension)
  class(dimension) <- "Dimension"
  return(dimension)
}

#' The class contains dataset description
#' @export
Dataset <-function (data){
  dataset = list(
    id = data$id,
    dimensions = DimensionModelList(data$dimensions)
  )

  # The method searching dimension with a given name
  dataset$find_dimension_by_name <- function (dim_name){
    for (dim in dataset$dimensions)
    {
      if (isequal_strings_ignorecase(dim$name, dim_name))
      {
        return (dim)
      }
    }
    return (NULL)
  }

  # The method searching dimension with a given id
  dataset$find_dimension_by_id <- function (dim_id){
    for (dim in dataset$dimensions)
    {
      if (isequal_strings_ignorecase(dim$id, dim_id))
      {
        return (dim)
      }
    }
    return (NULL)
  }
  dataset <- list2env(dataset)
  class(dataset) <- "Dataset"
  return(dataset)
}

#' The class contains pivot request item
#' @export
PivotItem <- function(dimensionid,members){
  pivotItem = list(
    dimensionid = dimensionid,
    members = members
  )
  pivotItem <- list2env(pivotItem)
  class(pivotItem) <- "PivotItem"
  return(pivotItem)
}

#' The class contains pivot request item
#' @export
PivotTimeItem <- function(dimensionid=NULL, members=NULL, uimode=NULL){
  pivotTimeItem = list(
    dimensionid = dimensionid,
    members = members,
    uimode = uimode
  )
  pivotTimeItem  <- list2env(pivotTimeItem)
  class(pivotTimeItem) <- "PivotTimeItem"
  return(pivotTimeItem)
}

#' The class contains pivot request
#' @export
PivotRequest <- function(dataset){
  pivot_Request = list(
    dataset = dataset,
    header = list(),
    stub = list(),
    filter = list(),
    frequencies = list(),
    get = function(x) pivot_Request[[x]],
    set = function(x, value) pivot_Request[[x]] <<- value
  )
  # Methods
  pivot_Request$get_item_array <- function (items){
    arr <- list()
    i <- 1
    if (length(items)>0)
    {
      for (item in items)
      {
        itemvalues <- list(
          'DimensionId' = item$dimensionid,
          'Members'= item$members
        )
        if (is(item, "PivotTimeItem"))
          itemvalues['UiMode'] <- item$uimode
        arr[[i]] <- itemvalues
        i <- i + 1
      }
    }
    return (arr)
  }
  pivot_Request$save_to_json <- function(){
    requestvalues <- list(
      'Dataset'= pivot_Request$get('dataset'),
      'Header' = pivot_Request$get_item_array(pivot_Request$get('header')),
      'Filter' = pivot_Request$get_item_array(pivot_Request$get('filter')),
      'Stub' = pivot_Request$get_item_array(pivot_Request$get('stub')),
      'Frequencies'= pivot_Request$get('frequencies')
    )

    return (toJSON(requestvalues, auto_unbox = TRUE))
  }
  class(pivot_Request) <- "PivotRequest"
  return(pivot_Request)
}
