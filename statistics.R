library(dplyr)


statistics <- function(my_data, dimensions, measurement, filters) {
  
  if(length(dimensions) > 3) {
    return(NULL)
  }
  if(length(measurement) > 1) {
    return(NULL)
  }
  
  my_data <- my_data %>%
    select(dimensions, measurement)
  
  for(i in filters) {
    if(!is.null(i$filter)) {
      my_data <- my_data %>%
        filter(my_data[,i$dimension] %in% i$filter)
    }
  }
  
  statistics <- my_data %>%
    dplyr::group_by_at(vars(dimensions)) %>%
    dplyr::summarise_at(vars(measurement), 
                        funs(mean = mean(.,na.rm=T), 
                             sd = sd(., na.rm = T), 
                             n = sum(!is.na(.)))) %>%
    dplyr::mutate(se = sd / sqrt(n),
                  lower.ci = mean - qt(1-(0.05/2), n - 1) * se,
                  upper.ci = mean + qt(1-(0.05/2), n - 1) * se)
  
  return(statistics)
}