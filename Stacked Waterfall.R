library(dplyr)
library(lubridate)
library(ggplot2)

# Create mock data
month_range <- -2:3
df <- data.frame()

for (r in month_range){
    
    g1 <- sample(3:8,1)
    g2 <- sample(3:15,1)
    g3 <- sample(3:12,1)
    
    group <- data.frame(Group = c(rep("G1",g1), rep("G2", g2), rep("G3", g3)))
    month <- data.frame(Month = c(rep(floor_date(today() %m+% months(r), "month"), sum(g1, g2, g3))))
    set <- data.frame(Group = group, Month = month)
    df <- rbind(df, set)
}


df$Value <- -1

df2 <- df %>% 
    select(Group, Value) %>% 
    group_by(Group) %>% 
    summarise(Value = sum(-Value)) %>% 
    ungroup() %>% 
    mutate(Month = floor_date(min(df$Month) %m+% (months(-1)))) %>%
    select(Group, Month, Value)

df <- rbind(df2,df)

df$Month <- ymd(df$Month)  #This date conversion is necessary for scale_x_date when we go to plot

all.equal(sum(df$Value), 0)   # check that the sum of the values is zero


data1 <- df %>% 
    select(Month, Value) %>% 
    group_by(Month) %>% 
    summarise(Value = sum(Value)) %>% 
    ungroup()

data1 <- data1  %>%
    mutate(
        yend = round(cumsum(Value), 3),
        ystart = lag(cumsum(Value), default = 0),
        xstart = c(head(Month, -1), NA),
        xend = c(tail(Month, -1), NA))

data2 <- df %>%
    select(Month, Group, Value) %>% 
    group_by(Month, Group) %>% 
    summarise(Value = sum(Value)) %>% 
    ungroup()

data2 <- data2 %>%
    mutate(
        yend2 = cumsum(Value),
        ystart2 = lag(cumsum(Value), default = 0) 
    )

data2 <- data2 %>% 
    left_join(data1, by = "Month") %>%
    select(-c("Value.x", "Value.y"))  # These variables are no longer needed



month_label <- c("Start Point", tail(format(as.Date(seq(from = min(data1$Month), to = max(data1$Month), 
                                                        by = "month")), "%b-%y"), -1))

ggplot(data2) +
    geom_rect(aes(xmin = ymd(Month) - days(12),
                  xmax = ymd(Month) + days(12), ymin = yend2, ymax = ystart2,
                  fill = Group),
              colour = "black") +
    geom_segment(data = data1[1:(nrow(data1) -1),],aes(x = ymd(xstart),
                                                       xend = ymd(xend),
                                                       y = yend,
                                                       yend = yend)) +
    geom_hline(yintercept = 0, linetype=2) +
    geom_vline(xintercept = floor_date(today(), "month"), linetype=1, colour = "red", 
               show.legend = TRUE) +
    annotate("text", x = floor_date(today(), "month"), y = -3, label = "Current Month", size = 3) +
    labs(y = "Number of Deliverables", x = "Delivery Timeframe", 
         title = "Waterfall Schedule of Deliverables") +
    scale_x_date(breaks = seq(from = min(data1$Month), to = max(data1$Month), by = "month"), labels = month_label) +
    theme_bw()+
    theme(legend.position = "right", panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = .5))