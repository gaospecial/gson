#' Transform the gene id in `gson` object
#'
#' @param x a json object
#' @param .f a function fun, a quosure style lambda ~ fun(.) or a list of either form.
#' @param ... additional arguments for the function call in .f. 
#'
#' @return
#' @export
#'
#' @examples
mutate_gene_id = function(x, .f, ...){
  if (!inherits(x, what = "GSON")) simpleError("x should be a GSON object.")
  slotname = methods::slotNames(x)
  empty_slot = sapply(slotname, function(y) is.null(methods::slot(x, name = y)))
  geneslot = slotname[stringr::str_detect(slotname, "gene") & !empty_slot]
  for (i in seq_along(geneslot)){
    methods::slot(x, name = geneslot[[i]], check = TRUE) = 
      methods::slot(x, name = geneslot[[i]]) %>%
      dplyr::mutate_at(dplyr::matches("gene"), .funs = .f, ...)
  }
  return(x)
}