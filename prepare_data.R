source("format_data.R")
filenames <- list.files(path="../data/", pattern="*.csv")
my_data <- process_data(filenames)
saveRDS(my_data,"my_data.Rda")