#' Calculating Date of Birth from the First Day of the Last Menstrual Cycle
#'
#' @param x a vector, first day of the last menstrual cycle
#' @param format a text string explicating the date format for x, defaults to "%d.%m.%Y"
#' @param get either `point` or `interval` for just the point estimate or the interval boundaries, using a standard deviation of 13 days, respectively
#' @param sd indicates the width of the uncertainty interval as multiples of the standard deviation (13 days)
#' @return a vector of estimated dates of birth (for `point`) or a data.frame of dates of birth and lower and upper boundaries
#' @keywords birthday naegele
#' @export
#' @examples
#' BornBy("31.12.1984")
#' BornBy(c("11.11.1984","08.07.1990"),get="interval",sd=1)

BornBy <- function(x,format="%d.%m.%Y",get="point",sd=1.96) {
  MenDate <- as.Date(x,format=format)
  birthDate <- MenDate + 280
  i_low <- birthDate - sd*13
  i_high <- birthDate + sd*13
  
  if (get=="point"){
    output <- format(birthDate,format)
  }
  if (get=="interval"){
    output <- data.frame(format(birthDate,format),format(i_low,format),format(i_high,format))
    colnames(output) <- c("Date of Birth","lower estimate","upper estimate")
  }
  return(output)
}