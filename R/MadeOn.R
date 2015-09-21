#' Calculating Date of Conception from Date of Birth
#'
#' @param x a vector of birth dates
#' @param format a text string explicating the date format for x, defaults to "%d.%m.%Y"
#' @param get either `point` or `interval` for just the point estimate or the interval boundaries, using a standard deviation of 13 days, respectively
#' @param sd indicates the width of the uncertainty interval as multiples of the standard deviation (13 days)
#' @return a vector of estimated dates of conception (for `point`) or a data.frame of dates of conception and lower and upper boundaries
#' @keywords birthday conception naegele
#' @export
#' @examples
#' MadeOn("31.05.1984")
#' MadeOn(c("24.04.1984","01.12.1983"),get="interval",sd=1)

MadeOn <- function(x,format="%d.%m.%Y",get="point",sd=1.96) {
  birthDate <- as.Date(x,format=format)
  conceptionDate <- birthDate - 266
  i_low <- conceptionDate - sd*13
  i_high <- conceptionDate + sd*13
  
  if (get=="point"){
    output <- format(conceptionDate,format)
  }
  if (get=="interval"){
    output <- data.frame(format(conceptionDate,format),format(i_low,format),format(i_high,format))
    colnames(output) <- c("Date of Conception","lower estimate","upper estimate")
  }
  return(output)
}