# install and load dependencies
install.packages("pacman")
library(pacman)
pacman::p_load(rio, plotly, GGally, dplyr, tidyverse)
theme_set(theme_bw())

# path the the csv file
out <- import("C:\\Users\\Administrator\\Desktop\\Unisa\\Ansible\\data.csv")
options(scipen = 999)
df <- out

# filter out the abnormal rps values. It can happens when the benchmark fails
df <- filter(df, rps < 100000)
paths <- unique(df$path)
servers <- unique(df$server)

gen_correlation <- function(path_) {
  # This will be shown in the "Plots" tab and not in "Viewer"
  # when it gives an error on "x must be an integer", you need to run again from line 1
  p <- ggpairs(
    filter(df, df$path == path_),
    upper = list(discrete = "cor"),
    mapping = aes(color = server),
    columns = c("rps",
                "conc",
                "time",
                "memtotal_mb",
                "disk",
                "vcpus")
  ) +
    labs(title = paste("Correlation for", path_))

  return(p)
}

gen_correlation(paths[1])
gen_correlation(paths[2])
gen_correlation(paths[3])

# converting to character values to use them as categories in the plots
df$vcpus <- as.character(df$vcpus)
df$memtotal_mb <- as.character(ceiling(df$memtotal_mb / 1E+3))
df$disk <- as.character(as.integer(df$disk / 1E+9))


gen_bar_plot <- function(x, fill, fill_label) {
  ymax_val <- 14 * 2 * 10 + 14 * 3
  p <- ggplot(df,
              aes(
                x,
                fill = fill,
                ymax = ymax_val,
                ymin = ymax_val
              )) +
    geom_bar() +
    geom_errorbar() +
    geom_text(stat = "count",
              aes(label = prettyNum(after_stat(count))),
              position = position_stack(vjust = 0.5)) +
    ggtitle("Number of completed benchmarks") +
    labs(fill = fill_label, x = "server")

  return(ggplotly(p, dynamicTicks = TRUE))
}

gen_bar_plot(df$server, df$arc, "architecture")
gen_bar_plot(df$server, df$memtotal_mb, "memory GB")
gen_bar_plot(df$server, df$vcpus, "vcpus")
gen_bar_plot(df$server, df$disk, "disk GB")
gen_bar_plot(df$server, as.character(df$conc), "concurrency")
gen_bar_plot(df$server, df$path, "static file")


gen_facet_grid <-
  function(y,
           color_,
           title,
           x_label,
           y_label,
           color_label,
           x = df$conc) {
    p <- ggplot(df, aes(x, y, color = color_)) +
      geom_point(size = 2, alpha = 0.7) +
      facet_grid(factor(df$path,
                        levels = paths) ~ server, scales = "free",) +
      labs(
        title = title,
        x = x_label,
        y = y_label,
        color = color_label
      )

    return(ggplotly(p, dynamicTicks = TRUE))
  }

title <- "Requests per second"
x_label <- "concurrency"
y_label <- "requests per second"

gen_facet_grid(df$rps,
               df$arc,
               title,
               x_label,
               y_label,
               "architecture")
gen_facet_grid(df$rps,
               df$memtotal_mb,
               title,
               x_label,
               y_label,
               "memory GB")
gen_facet_grid(df$rps,
               as.character(df$vcpus),
               title,
               x_label,
               y_label,
               "vcpus")
gen_facet_grid(df$rps,
               df$disk,
               title,
               x_label,
               y_label,
               "disk GB")


# this algorithm is not efficient
gen_rps_best_path <- function(n) {
  results <- list()
  for (p in 1:length(paths)) {
    f_path <- filter(df, df$path == paths[p])
    for (s in 1:(length(servers))) {
      i <- length(results) + 1
      results[[i]] <- filter(f_path, f_path$server == servers[s])
      results[[i]] <-
        results[[i]][order(results[[i]]$rps, decreasing = T),]
      results[[i]] <- results[[i]] %>% slice(seq(n))
      results[[i]]$pos <- c(1:n)
    }
  }

  return(results)
}

list_best_rps <- gen_rps_best_path(5)
df_best_rps <- bind_rows(list_best_rps)

p <-
  ggplot(df_best_rps, aes(pos, rps, color = server)) +
    geom_line() +
    geom_point(size = 2, alpha = 0.7) +
    facet_grid(factor(df_best_rps$path,
                      levels = paths), scales = "free",) +
    labs(
      title = "Rank of the best requests per second",
      x = "position",
      y = "requests per second",
      color = "server"
    )

ggplotly(p, dynamicTicks = TRUE)


title <- "Time to complete a benchmark"
y_label <- "time in seconds"

gen_facet_grid(df$time,
               df$arc,
               title,
               x_label,
               y_label,
               "architecture")
gen_facet_grid(df$time,
               df$memtotal_mb,
               title,
               x_label,
               y_label,
               "memory GB")
gen_facet_grid(df$time,
               as.character(df$vcpus),
               title,
               x_label,
               y_label,
               "vcpus")
gen_facet_grid(
  x = df$rps,
  df$time,
  as.character(df$vcpus),
  title,
  "requests per second",
  y_label,
  "vcpus"
)
gen_facet_grid(df$time,
               df$disk,
               title,
               x_label,
               y_label,
               "disk GB")

gen_percentile <- function(server_) {
  f_df <- filter(df, server == server_)
  p <-
    ggplot(f_df,
           aes(color = vcpus, alpha = 0.7)) +
      geom_point(aes(`50`, 50)) +
      geom_point(aes(`66`, 66)) +
      geom_point(aes(`75`, 75)) +
      geom_point(aes(`80`, 80)) +
      geom_point(aes(`90`, 90)) +
      geom_point(aes(`95`, 95)) +
      geom_point(aes(`98`, 98)) +
      geom_point(aes(`99`, 99)) +
      geom_point(aes(`100`, 100)) +
      facet_grid(~factor(f_df$path,
                         levels = paths), scales = "free") +
      labs(
        title = paste(server_, "percentile response time"),
        x = "response time (ms)",
        y = "percentile",
        color = "vcpus",
        alpha = NULL,
        yaxis = list(tickformat = "%")
      )

  return(ggplotly(p, dynamicTicks = TRUE))
}

gen_percentile(servers[1])
gen_percentile(servers[2])
gen_percentile(servers[3])
gen_percentile(servers[4])
gen_percentile(servers[5])


p <- ggplot(df, aes(color = vcpus, alpha = 0.7)) +
  geom_point(aes(`50`, 50)) +
  geom_point(aes(`66`, 66)) +
  geom_point(aes(`75`, 75)) +
  geom_point(aes(`80`, 80)) +
  geom_point(aes(`90`, 90)) +
  geom_point(aes(`95`, 95)) +
  geom_point(aes(`98`, 98)) +
  geom_point(aes(`99`, 99)) +
  geom_point(aes(`100`, 100)) +
  facet_grid(factor(df$path,
                    levels = paths) ~ server, scales = "free",) +
  labs(
    title = "Percentile response time",
    x = "response time (ms)",
    y = "percentile",
    color = "vcpus",
    alpha = NULL
  )

ggplotly(p, dynamicTicks = TRUE)


p <- ggplot(df, aes(color = vcpus, alpha = 0.7)) +
  geom_point(aes(`50`, 50)) +
  geom_point(aes(`66`, 66)) +
  geom_point(aes(`75`, 75)) +
  geom_point(aes(`80`, 80)) +
  geom_point(aes(`90`, 90)) +
  geom_point(aes(`95`, 95)) +
  geom_point(aes(`98`, 98)) +
  geom_point(aes(`99`, 99)) +
  geom_point(aes(`100`, 100)) +
  facet_grid(factor(df$path,
                    levels = paths) ~ server, scales = "free",) +
  labs(
    title = "Percentile response time",
    x = "response time (ms)",
    y = "percentile",
    color = "vcpus",
    alpha = NULL
  )

ggplotly(p, dynamicTicks = TRUE)
