library("e1071")
library(ggplot2)
library(reshape2)
library(gtable)
library(scales)
library(lattice)
library("caret")
library('ipred')
library('dplyr')


source("./input_dataset.R")
source("./kmeans.R")
folder_source <- "./trainig_dataset/"
total <- input_dataset(folder_source)



start_time <- Sys.time()
modelo_svm <-
  svm(
    category ~ .,
    data = total ,
    type = 'C-classification',
    nu = 0.10,
    scale = TRUE,
    kernel = "polynomial"
  )
end_time <- Sys.time()
end_time - start_time


svm_classifier_percent <- function(a, b) {
  df <- data.frame()
  
  interval <- b
  finish <- nrow(a) / interval
  
  for (i in 0:finish) {
    if (i == as.integer(finish)) {
      x <- (i * interval)
      y <- nrow(a)
    } else{
      x <- (i * interval)
      y <- ((i * interval + interval) - 1)
    }
    
    teste <- predict(modelo_svm, a[x:y, ])
    result <- a$category[x:y]
    
    tab <- table(teste, result)
    for (h in 1:nrow(tab)) {
      row <-
        data.frame(
          interval = as.numeric(i),
          resource = as.factor(rownames(tab)[h]),
          per = as.integer(((tab[h, 1] / (
            y - x + 1
          )) * 100))
        )
      df <- bind_rows(df, row)
    }
    
  }
  return(df)
}

svm_classifier_class <- function(a, b) {
  result <- svm_classifier_percent(a, b)
  df = data.frame()
  control <- 0
  interval <- 0
  
  for (h in 1:nrow(result)) {
    if (result[h, 1] != interval) {
      interval <- interval + 1
      control <- 0
      row <-
        data.frame(interval = as.numeric(interval * b),
                   resource = as.factor(resource))
      df <- bind_rows(df, row)
      
    }
    if (result[h, 3] > control) {
      resource <- result[h, 2]
      control <- result[h, 3]
    }
    
    if (h == nrow(result)) {
      interval <-  interval + 1
      row <-
        data.frame(interval = as.numeric(interval * b),
                   resource = as.factor(resource))
      df <- bind_rows(df, row)
    }
    
  }
  return(df)
}

svm_classifier_level <- function(a, b) {
  df <- data.frame()
  df_cpu <- data.frame()
  df_mem <- data.frame()
  df_disk <- data.frame()
  df_net <- data.frame()
  df_cache <- data.frame()
  cpu <- ""
  mem <- ""
  disk <- ""
  net <- ""
  cache <- ""
  v_cpu <- vector()
  v_mem <- vector()
  v_disk <- vector()
  v_net <- vector()
  v_cache <- vector()
  
  interval <- b
  finish <- nrow(a) / interval
  
  for (i in 0:finish) {
    if (i == as.integer(finish)) {
      x <- (i * interval) + 1
      y <- nrow(a)
    } else{
      if (i == 0)
        x <- (i * interval) + 1
      else
        x <- (i * interval)
      y <- ((i * interval + interval) - 1)
    }
    for (j in x:y) {
      predict <- predict(modelo_svm, a[j, ])
      if (predict == "cpu") {
        df_cpu <- bind_rows(df_cpu, a[j, ])
        
      } else if (predict == "mem") {
        df_mem <- bind_rows(df_mem, a[j, ])
        
      } else if (predict == "disk") {
        df_disk <- bind_rows(df_disk, a[j, ])
        
      } else if (predict == "net") {
        df_net <- bind_rows(df_net, a[j, ])
        
      } else if (predict == "cache") {
        df_cache <- bind_rows(df_cache, a[j, ])
        
      }
    }
    
    
    if (nrow(df_cpu) > 0) {
      v_cpu <- c(v_cpu, predict_cpu.kmeans(cl_cpu, df_cpu[, 1:7]))
      aa <- data.frame()
      aa <-
        cbind(c("low", "mod", "hig"), c(sum(str_count(v_cpu, "low")), sum(str_count(v_cpu, "mod")), sum(str_count(v_cpu, "hig"))))
      if (which.max(aa[, 2]) == 1)
      {
        cpu <- "low"
      } else if (which.max(aa[, 2]) == 2)
      {
        cpu <- "mod"
      } else if (which.max(aa[, 2]) == 3)
      {
        cpu <- "hig"
      }
      
    } else{
      cpu <- "abs"
    }
    if (nrow(df_mem) > 0) {
      v_mem <- c(v_mem, predict_mem.kmeans(cl_mem, df_mem[, 1:7]))
      aa <- data.frame()
      aa <-
        cbind(c("low", "mod", "hig"), c(sum(str_count(v_mem, "low")), sum(str_count(v_mem, "mod")), sum(str_count(v_mem, "hig"))))
      if (which.max(aa[, 2]) == 1)
      {
        mem <- "low"
      } else if (which.max(aa[, 2]) == 2)
      {
        mem <- "mod"
      } else if (which.max(aa[, 2]) == 3)
      {
        mem <- "hig"
      }
    } else{
      mem <- "abs"
    }
    if (nrow(df_disk) > 0) {
      v_disk <- c(v_disk, predict_disk.kmeans(cl_disk, df_disk[, 1:7]))
      aa <- data.frame()
      aa <-
        cbind(c("low", "mod", "hig"), c(sum(str_count(v_disk, "low")), sum(str_count(v_disk, "mod")), sum(str_count(v_disk, "hig"))))
      if (which.max(aa[, 2]) == 1)
      {
        disk <- "low"
      } else if (which.max(aa[, 2]) == 2)
      {
        disk <- "mod"
      } else if (which.max(aa[, 2]) == 3)
      {
        disk <- "hig"
      }
    } else{
      disk <- "abs"
    }
    if (nrow(df_net) > 0) {
      v_net <- c(v_net, predict_net.kmeans(cl_net, df_net[, 1:7]))
      aa <- data.frame()
      aa <-
        cbind(c("low", "mod", "hig"), c(sum(str_count(v_net, "low")), sum(str_count(v_net, "mod")), sum(str_count(v_net, "hig"))))
      if (which.max(aa[, 2]) == 1)
      {
        net <- "low"
      } else if (which.max(aa[, 2]) == 2)
      {
        net <- "mod"
      } else if (which.max(aa[, 2]) == 3)
      {
        net <- "hig"
      }
    } else{
      net <- "abs"
    }
    if (nrow(df_cache) > 0) {
      v_cache <-
        c(v_cache, predict_cache.kmeans(cl_cache, df_cache[, 1:7]))
      aa <- data.frame()
      aa <-
        cbind(c("low", "mod", "hig"), c(sum(str_count(
          v_cache, "low"
        )), sum(str_count(
          v_cache, "mod"
        )), sum(str_count(
          v_cache, "hig"
        ))))
      if (which.max(aa[, 2]) == 1)
      {
        cache <- "low"
      } else if (which.max(aa[, 2]) == 2)
      {
        cache <- "mod"
      } else if (which.max(aa[, 2]) == 3)
      {
        cache <- "hig"
      }
    } else{
      cache <- "abs"
    }
    
    row <-
      data.frame(interval = as.numeric(i),
                 resource = "cpu",
                 per = cpu)
    df <- bind_rows(df, row)
    row <-
      data.frame(interval = as.numeric(i),
                 resource = "mem",
                 per = mem)
    df <- bind_rows(df, row)
    row <-
      data.frame(interval = as.numeric(i),
                 resource = "disk",
                 per = disk)
    df <- bind_rows(df, row)
    row <-
      data.frame(interval = as.numeric(i),
                 resource = "net",
                 per = net)
    df <- bind_rows(df, row)
    row <-
      data.frame(interval = as.numeric(i),
                 resource = "cache",
                 per = cache)
    df <- bind_rows(df, row)
    
  }
  
  return(df)
  
  
}

