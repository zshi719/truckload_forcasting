# load packages
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(tidycensus)
library(rnaturalearth)
library(conflicted)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflict_prefer("box", "shinydashboard")


ibtl <- read_excel("data/IB TL Data.xlsx")
ibtl_2020 <- ibtl %>% filter(`Ship Year` == 2020)
  
# select pairs that have data each month
ibtl_2020 %>%
  group_by(`Pick State`, `Drop State`) %>%
  summarize(dist_month = n_distinct(`Ship Month`)) %>%
  filter(dist_month == 12) -> df_dist_month

# select top 5 origins
ibtl_2020 %>%
  filter(`Pick State` %in% df_dist_month$`Pick State`, `Drop State` %in% df_dist_month$`Drop State`) %>%
  group_by(`Pick State`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  arrange(desc(total_n)) %>%
  head(5) -> top5_origin
top5_origin # So the top five states are IA, IL, WI, TN, GA

# look for the top five destinations for each of the five origins
ibtl_2020 %>%
  filter(`Pick State` %in% top5_origin$`Pick State`) %>%
  group_by(`Pick State`, `Drop State`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  top_n(5, total_n) %>%
  arrange(desc(total_n), .by_group = TRUE) -> final_comb

# get the final dataset that has origin, destination, monthly shipment
ibtl_2020 %>%
  filter(`Pick State` %in% final_comb$`Pick State`, `Drop State` %in% final_comb$`Drop State`) %>%
  group_by(`Pick State`, `Drop State`, `Ship Month`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  mutate(`Ship Month` = as.factor(`Ship Month`))-> monthly_count

# plot line & bar
bar_line_dff <- monthly_count %>%
  filter(`Pick State` == "IA", `Drop State`== "IA") 

bar_line_dff %>%
  ggplot()+
  geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
  geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
  theme_light()+
  labs(y = "Shipment Count", title = "Trend of shipment count")+
  scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
  #scale_y_continuous(breaks = trans_breaks(bar_line_dff$total_n, bar_line_dff$total_n, n = 5))
  geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)

