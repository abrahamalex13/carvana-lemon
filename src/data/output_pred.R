#output_pred.R

output_pred <- function(Yhat, identifiers, filename_out) {
  
  if (!missing(identifiers)) out <- as.data.frame( cbind(identifiers, "Yhat" = Yhat) )
  else out <- Yhat
  
  if (!is.null(filename_out)) write_csv(out, path = filename_out)
  
  out
}