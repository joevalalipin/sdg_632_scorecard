library(shiny)
library(dplyr)
library(plotly)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)

# prep data for pie chart
data_for_pie <- function(data, wb_type) {
  data %>% 
    filter(`Water body type` == wb_type) %>% 
    group_by(`Water body type`,
             `Parameter group`) %>% 
    summarise(n_good = sum(good),
              n = n(),
              prop_good = n_good / n) %>% 
    ungroup() %>% 
    mutate(Status = cut(prop_good,
                        breaks = c(-Inf, 0.50, 0.65, 0.80, 0.95, Inf),
                        labels = c("Poor", "Marginal", "Fair", "Good", "Excellent"))) %>% 
    arrange(desc(Status)) %>%
    mutate(width = 1,
           position = cumsum(width) - 0.5)
}

# pie chart function
cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE
pie_param <- function(data) {
  data %>% 
    ggplot(aes(x = "", y = width, fill = Status)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    cp +
    geom_text(aes(y = position, label = `Parameter group`)) +
    labs(x = "", y = "", fill = "") +
    scale_fill_manual(values = c("Poor" = brewer.pal(5, "RdYlGn")[1], 
                                 "Marginal" = brewer.pal(5, "RdYlGn")[2], 
                                 "Fair" = brewer.pal(5, "RdYlGn")[3], 
                                 "Good" = brewer.pal(5, "RdYlGn")[4], 
                                 "Excellent" = brewer.pal(5, "RdYlGn")[5]),
                      drop = FALSE) +
    theme_void() +
    theme(aspect.ratio = 1)
}

