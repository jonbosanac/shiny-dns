# Format data --------------------

# Data format
# timestamp | hostname       | dns_resolver         * | root_ip | root_response | root_icmp_response |
# tld_ip    | tld_response   | tld_icmp_response      | auth_ip | auth_response | auth_icmp_response |
# cname_ip  | cname_response | cname_icmp_response    | final_response *
# The schema between ** can be repeated N times

library(data.table)
library(dplyr)

define_provider <- function(col_names) {
  auth_name_server <- col_names[7]
  split_auth_name_server <- strsplit(auth_name_server, "\\.")[[1]]
  provider <- split_auth_name_server[2]
  return(provider)
}

transform_data <- function(filepath) {
  col_names <- list("host"=character(0), 
                    "dns_resolver"=character(0), 
                    "level"=character(0), 
                    "response"=numeric(0), 
                    "ip"=character(0), 
                    "icmp_response"=numeric(0),
                    "nameserver"=character(0),
                    "dns_provider"=character(0))
  
  df <- data.frame(col_names, stringsAsFactors = FALSE)
  df <- tryCatch({
    my_data <- fread(file = paste("../data/", filepath,sep=""),
                     sep = ",",
                     drop = c(1),
                     na.strings = c("","N/A"),
                     fill=TRUE)
    my_data <- my_data[c(-1,-2),]
    host <- my_data$Hostname
    dns_resolver <- my_data$`DNS Resolver`
    my_data <- my_data[,c(-1,-2)]
    my_data <- Filter(function(x) !(all(is.na(x))), my_data)
    total_rows <- nrow(my_data)
    for(i in seq(0,ncol(my_data)-1, 13)) {
      lower_bound <- i+1
      upper_bound <- i+13
      old_data <- my_data[,lower_bound:upper_bound, with=FALSE]
      provider <- define_provider(colnames(old_data))
      root <- data.frame("ip"=old_data[[1]],
                         "response"=as.numeric(old_data[[2]]),
                         "icmp_response"=as.numeric(old_data[[3]]),
                         "nameserver"=rep(colnames(old_data[,1]), total_rows),
                         "dns_provider"=rep(provider, total_rows),
                         "host"=host,
                         "dns_resolver"=dns_resolver,
                         "level"=rep("root", total_rows),
                         stringsAsFactors = FALSE)
      tld <- data.frame("ip"=old_data[[4]],
                        "response"=as.numeric(old_data[[5]]),
                        "icmp_response"=as.numeric(old_data[[6]]),
                        "nameserver"=rep(colnames(old_data[,4]), total_rows),
                        "dns_provider"=rep(provider, total_rows),
                        "host"=host,
                        "dns_resolver"=dns_resolver,
                        "level"=rep("tld", total_rows),
                        stringsAsFactors = FALSE)
      auth <- data.frame("ip"=old_data[[7]],
                         "response"=as.numeric(old_data[[8]]),
                         "icmp_response"=as.numeric(old_data[[9]]),
                         "nameserver"=rep(colnames(old_data[,7]), total_rows),
                         "dns_provider"=rep(provider, total_rows),
                         "host"=host,
                         "dns_resolver"=dns_resolver,
                         "level"=rep("auth", total_rows),
                         stringsAsFactors = FALSE)
      cname <- data.frame("ip"=old_data[[10]],
                          "response"=as.numeric(old_data[[11]]),
                          "icmp_response"=as.numeric(old_data[[12]]),
                          "nameserver"=rep(colnames(old_data[,10]), total_rows),
                          "dns_provider"=rep(provider, total_rows),
                          "host"=host,
                          "dns_resolver"=dns_resolver,
                          "level"=rep("cname", total_rows),
                          stringsAsFactors = FALSE)
      final <- data.frame("ip"=rep(NA, total_rows),
                          "response"=as.numeric(old_data[[13]]),
                          "icmp_response"=as.numeric(rep(NA, total_rows)),
                          "nameserver"=rep(colnames(old_data[,13]), total_rows),
                          "dns_provider"=rep(provider, total_rows),
                          "host"=host,
                          "dns_resolver"=dns_resolver,
                          "level"=rep("final", total_rows),
                          stringsAsFactors = FALSE)
      new_entry <- bind_rows(root, tld, auth, cname, final)
      df <- bind_rows(df, new_entry)
    }
    not_factors <- c("response", "icmp_response")
    df[,!names(df) %in% not_factors] <- lapply(
      df[,!names(df) %in% not_factors], factor)
    levels <- c("root", "tld", "auth", "cname", "final")
    df$level <- factor(df$level, levels=levels)
    df
  }, error = function(e) {
    print(paste("An error happened while transforming",filepath,
                "check if the data format is correct"))
    return(data.frame(col_names, stringsAsFactors = FALSE))
  })
  
}

process_data <- function(filepaths) {
  my_data <- bind_rows(lapply(filepaths, transform_data))
}