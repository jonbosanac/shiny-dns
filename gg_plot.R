library(ggplot2)
library(dplyr)

gg_plot_gen <- function(my_data, dimensions, measurement, filters) {
  
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
  
  

  if (length(dimensions) == 1)  {
    p <- ggplot(my_data, aes_string(x=dimensions[1], y=measurement, fill=dimensions[1]))
  }
  else {
    if(length(dimensions) == 2) {
      p <- ggplot(my_data, aes_string(x=dimensions[1], y=measurement, fill=dimensions[2]))
    }     
    else{
      p <- ggplot(my_data, aes_string(x=dimensions[1], y=measurement, fill=dimensions[2])) + 
        facet_wrap(dimensions[3])
    }
  }
  p <- p + 
    geom_violin(scale = "width", position = position_dodge(1)) + 
    scale_y_log10() +
    ylab(paste("log10 of", measurement))
  
  return(p)
}