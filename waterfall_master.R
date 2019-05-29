library(tidyverse)



df <- tribble(
    ~Category, ~Value, 
    # --------- header record ----------
    "Prev Budget", 5,
    "Salaries", 0.1,
    "CapEx", 0.175,
    "Travel", -0.2,
    "Contracting", -0.1,
    "Operations", -0.2,
    "RealEstate", -0.1,
    "Gap to Target", -0.175,
    "Current Budget", -4.5
)
# You want the values to sum to zero.

levels <- df$Category

data1 <- df  %>%
    mutate(ymin = cumsum(Value),
           ymax = lag(cumsum(Value), default = 0),
           Category = factor(Category, levels = levels),
           xmin = c(head(Category, -1), NA),
           xmax = c(tail(Category, -1), NA),
           Impact = ifelse(Category %in% c(as.character(df$Category[1]), as.character(df$Category[nrow(df)])),"Budget",
                          ifelse(Value > 0, "Increase", "Decrease")
           ))

w <- 0.4  #use to set width of bars

ggplot(data1) +
    theme_bw()+
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Millions", x = "Expense Category", title = "Explanation of Budget Gap Closure") +
    geom_rect(aes(xmin = as.integer(Category) - w/2,
                  xmax = as.integer(Category) + w/2, ymin = ymin, ymax = ymax,
                  fill = Impact), colour = "black") +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(values = (c("Decrease" = "blue", "Increase" = "orange", "Budget" = "black")))+
    geom_segment(data = data1[1:(nrow(data1) -1),],aes(x = xmin,
                                                       xend = xmax,
                                                       y = ymin,
                                                       yend = ymin)) +
    
    coord_cartesian(ylim=c(4, max(data1$ymax)+.025)) 

