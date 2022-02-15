# generate dates, including the gaps
seq_date <- function(date){
  mindate <- min(date) %>% 
    as.Date() %>% 
    format("%Y-%m") %>% 
    paste0("-01") %>% 
    as.Date()
  maxdate <- max(date) %>% 
    as.Date() %>% 
    format("%Y-%m") %>% 
    paste0("-01") %>% 
    as.Date()
  out <- seq.Date(mindate, maxdate, by = "month") %>% 
    as.Date() %>% 
    format("%Y-%m") %>% 
    data.frame(ym = .)
  return(out)
}


# add n months to a vector of dates
add_months <- function(date_list, n = 3){
  lastdate <- date_list %>% 
    paste0("-01") %>% 
    max() %>% 
    as.Date()
  maxdate <- seq.Date(lastdate, by = "month", length.out = n + 1) %>% 
    # as.Date() %>% 
    format("%Y-%m")
  return(c(date_list, maxdate[-1]))
}


# transform month to a circular variables
circular_month <- function(x, increment, sin = TRUE){
  y <- x %>% 
    add_months(increment) %>% 
    ym() %>% 
    month()
  if(sin){
    sin(2 * 3.141593 * (y / 12))
  } else{
    cos(2 * 3.141593 * (y / 12))
  }
}
