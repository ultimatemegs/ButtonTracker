library("SOAR")
countNAs <-
  function (object, ...) 
    UseMethod("countNAs")
countNAs.data.frame <-
  function (object, ...) 
    colSums(is.na(object))
countNAs.default <-
  function (object, ...) 
    stop("no method available for objects of class ", class(object))
countNAs.list <-
  function (object, ...) 
    sapply(object, function(x) sum(is.na(x)), ...)
countNAs.matrix <-
  function (object, ...) 
    colSums(is.na(object))
countNAs.numeric <-
  function (object, ...) 
    sum(is.na(object))

Store(countNAs, countNAs.data.frame, countNAs.default, countNAs.default, countNAs.list, countNAs.matrix, 
      countNAs.numeric)