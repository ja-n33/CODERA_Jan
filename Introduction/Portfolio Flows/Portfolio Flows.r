
##Portfolio Flows
packages <- c("grid",
              "scales",
              "stringr",
              "ggtext",
              "png",
              "econdatar",
              "plyr",
              "readxl",
              "lubridate",
              "readODS",
              "readr",
              "tidyr",
              "dplyr",
              "ggplot2",
              "plotly",
              "haven",
              "openxlsx",
              "zoo",
              "countrycode",
              "haven",
              "patchwork",
              "ggridges",
              "tcltk") 

invisible(lapply(packages, library, c=TRUE))
setwd(Sys.getenv("CODERA_Jan"))
root<-Sys.getenv("CODERA_Jan")
save_path <- file.path(root, "Introduction", "Portfolio Flows")
custom_base_size <- 20


# Data
net <- read_dataset(
  id = "QB_BALNPAY",
  series_key = "KBP5684K+KBP5756K+KBP5757K+KBP5759K+KBP5758K.Q.N.N.LVL")|>
  as_tibble()|>
  rename(net_portfolio = KBP5684K.Q.N.N.LVL,
  equity0 = 	KBP5756K.Q.N.N.LVL, 
  debt0 = KBP5757K.Q.N.N.LVL,
  equity1 = KBP5758K.Q.N.N.LVL,
  debt1 = KBP5759K.Q.N.N.LVL)%>%
  group_by(time_period)%>%
  mutate( debt = debt0 + debt1,
  equity = equity0 + equity1) %>%
  filter(time_period >= "1997-01-01") %>%
  select(time_period, debt, equity, net_portfolio) %>%
  ungroup()


##Expenditure on gross domestic product, Seasonally adjusted data, not calendar adjusted
ngdp <- read_dataset(
  id = "QB_NATLACC",
  series_key = "KBP6045L.Q.N.S.LVL") %>%
  as_tibble() %>%
  rename(ngdp = KBP6045L.Q.N.S.LVL)


net_adjusted <- left_join(net, ngdp, by = "time_period")


net_ma <- net_adjusted %>%
arrange(time_period)%>%
mutate(debt_ma4 = rollmean(debt, k = 4, fill = NA, align = "right"), ##rollmean computes the average over the currrent obs and the last 3 (we use align=right)
equity_ma4 = rollmean(equity, k = 4, fill = NA, align = "right"), 
net_ma4 = rollmean(net_portfolio, k = 4, fill = NA, align = "right"),
ngdp_ma4 = rollmean(ngdp, k = 4, fill = NA, align = "right"))%>%
filter(if_all(everything(), ~!is.na(.))) %>%
mutate(across(contains("ma4"), ~(./ngdp_ma4)*100)) ##across applies function across all columns containing ma4 (all of the moving averages)

net_long <- net_ma %>%
    select(time_period, contains("ma4"), -ngdp_ma4)|>
    pivot_longer(-time_period, names_to = "type", values_to = "value")


#plot
source("Jan/utils.r")


max_val <- net_long %>%
  filter(type == "net_ma4") %>%
  pull(value) |>
  max(na.rm = TRUE)
min_val <- net_long %>%
  filter(type == "net_ma4") %>%
  pull(value) %>%
  min(na.rm = TRUE)


if (TRUE) {
ggplot_flow <- ggplot(data = net_long, aes(x = time_period, y = value, fill = type)) +
base_graph +
geom_bar(data = net_long |> filter(type != "net_ma4"), stat = "identity", position = "stack") +
geom_line(data = net_long |> filter(type == "net_ma4"), linewidth = 1.2, aes(color = type)) +
scale_color_manual(values = c("net_ma4" = "black"), labels = c("net_ma4" = "Net Portofolio") ) +
scale_fill_manual(values = color_list, labels = c("debt_ma4" = "Debt", "equity_ma4" = "Equity", "net_ma4" = "")) +
scale_y_continuous(limits = c(min_val*1.15, max_val* 1.1),
    breaks = seq(floor(min_val * 1.15), ceiling(max_val * 1.1), by = 0.5),  labels = function(x) paste0(x, "%")) +
      scale_x_date( breaks = c(seq(as.Date("1997-01-01"), as.Date("2025-01-01"), by = "1 year"), as.Date("2025-01-01")),
  date_labels = "%Y",
  expand = expansion(mult = c(0, 0.05))) +
labs(title = "South Africa Net Portfolio Investment (by flow type)", 
caption = "Source: SARB, EconData", 
y = "% of GDP (4 quarter moving average)") +
theme_minimal(base_size = 10)+
theme(plot.title = element_text(size = rel(0.995), hjust = 0.5, face = "bold", margin = margin(b = 4, l = -4)),
          plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
          axis.title.y = element_text(margin = margin(r = 8)),
          panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_blank(),
          legend.spacing.y = unit(1.5, 'cm'),
          plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.line   = element_line(color = "black"),
          axis.ticks = element_line(color = "black")) +
    codera_logo(net_long|> filter(type == "net_ma4"), xpos = 0.5, ypos = 0.3, wdth = 6.5)

    ggplotgrob_flows <- ggplotGrob(ggplot_flow)
  ggplotgrob_flows$layout[grepl("panel", ggplotgrob_flows$layout$name), ]$clip <- "off"

  png(filename = file.path(save_path, "Portfolio Flows (by type).png"),
      width = 1250,
      height = 650,
      res = 190)
  grid.newpage()
  grid.draw(ggplotgrob_flows)
  dev.off()
}

##now without moving average
ratio_gdp <- net_adjusted %>%
    group_by(time_period)%>%
    mutate(across(debt:ngdp,  ~(./ngdp)*100))%>%
    select(-ngdp)%>%
    pivot_longer(-time_period, names_to = "type", values_to = "value")
    mi_val <- ratio_gdp %>%
    filter(type == "net_portfolio") %>%
    pull(value) %>%
    min(na.rm = TRUE)


ma_val <- ratio_gdp %>%
  filter(type == "net_portfolio") %>%
  pull(value) %>%
  max(na.rm = TRUE)


if (TRUE) {
ggplot_flow1 <- ggplot(data = ratio_gdp, aes(x = time_period, y = value, fill = type)) +
base_graph +
geom_bar(data = ratio_gdp |> filter(type != "net_portfolio"), stat = "identity", position = "stack") +
geom_line(data = ratio_gdp|> filter(type == "net_portfolio"), linewidth = 1, aes(color = type)) +
scale_color_manual(values = c("net_portfolio" = "black"), labels = c("net_portfolio" = "Net Portofolio") ) +
scale_fill_manual(values = color_list, labels = c("debt" = "Debt", "equity" = "Equity", "net_portfolio" = "")) +
scale_y_continuous(limits = c(mi_val*1.1, ma_val* 1.1),
    breaks = seq(floor(mi_val * 1.1), ceiling(ma_val * 1.1), by = 1),  labels = function(x) paste0(x, "%")) +
      scale_x_date( breaks = c(seq(as.Date("1997-01-01"), as.Date("2025-01-01"), by = "1 year"), as.Date("2025-01-01")),
  date_labels = "%Y",
  expand = expansion(mult = c(0, 0.05))) +
labs(title = "South Africa Net Portfolio Investment (by flow type)", 
caption = "Source: SARB, EconData", 
y = "Percentage of GDP ") +
theme_minimal(base_size = 10)+
theme(plot.title = element_text(size = rel(0.995), hjust = 0.5, face = "bold", margin = margin(b = 4, l = -4)),
          plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
          axis.title.y = element_text(margin = margin(r = 8)),
          panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_blank(),
          legend.spacing.y = unit(1.5, 'cm'),
          plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.line   = element_line(color = "black"),
          axis.ticks = element_line(color = "black")) +
    codera_logo(ratio_gdp, xpos = 0.5, ypos = 0.3, wdth = 6.5)

    ggplotgrob_flows1 <- ggplotGrob(ggplot_flow1)
  ggplotgrob_flows1$layout[grepl("panel", ggplotgrob_flows1$layout$name), ]$clip <- "off"

  png(filename = file.path(save_path, "Portfolio Flows (by type) (not 4qma).png"),
      width = 1250,
      height = 650,
      res = 190)
  grid.newpage()
  grid.draw(ggplotgrob_flows1)
  dev.off()
}


##Where are inflows and outflows going
net_type <- read_dataset(
  id = "QB_BALNPAY",
  series_key = "KBP5646K+KBP5647K+KBP5648K+KBP5649K+KBP5663K+KBP5664K+KBP5665K.Q.N.N.LVL")|>
  as_tibble()%>%
  rename(
  general_government_liabilities = KBP5646K.Q.N.N.LVL,
  public_corporations_liabilities = KBP5647K.Q.N.N.LVL,
  banking_liabilities = KBP5648K.Q.N.N.LVL,
  nonbanking_liabilities = KBP5649K.Q.N.N.LVL,
  public_corporations_assets = KBP5663K.Q.N.N.LVL,
  banking_assets = KBP5664K.Q.N.N.LVL,
  nonbanking_assets = KBP5665K.Q.N.N.LVL)


  net_type <- net_type %>%
  arrange(time_period)%>%
  mutate(Public_Sector = (general_government_liabilities +(public_corporations_liabilities + public_corporations_assets)),
  Private_Nonbanking_Sector = (nonbanking_liabilities + nonbanking_assets),
  Banking_Sector =  (banking_liabilities + banking_assets)) #Finally got it. Need to add them. Search for Portfolio Invest in QB, Need to look at BOP. They describe inflow and outflows.

#write.xlsx(net_type, "Testing.xlsx")
net_type_adjusted <- left_join(net_type, ngdp, by = "time_period")

#create moving average

net_type_dataset <- net_type_adjusted %>%
mutate(Public_Sector_ma4 = rollmean(Public_Sector, k = 4, fill = NA, align = "right"),
Private_Nonbanking_Sector_ma4 = rollmean(Private_Nonbanking_Sector, k = 4, fill = NA, align = "right"),
Banking_Sector_ma4 = rollmean(Banking_Sector, k = 4, fill = NA, align = "right"),
ngdp_ma4 = rollmean(ngdp, k = 4, fill = NA, align = "right"))%>%
filter(if_all(everything(), ~!is.na(.))) %>%
mutate(across(contains("ma4"), ~(./ngdp_ma4)*100))


net_type_long <- left_join(net_type_dataset |> select(time_period, contains("ma4")), net_ma|> select(time_period, net_ma4), by = "time_period")|>
select(-ngdp_ma4)|>
pivot_longer(-time_period, names_to = "type", values_to = "value")|>
filter(time_period >= "1997-10-01")


if (TRUE) {
ggplot_type <- ggplot(data = net_type_long, aes(x = time_period, y = value, fill = type)) +
base_graph +
geom_bar(data = net_type_long |> filter(type != "net_ma4"),stat = "identity", position = "stack") +
geom_line(data = net_type_long |> filter(type == "net_ma4"), linewidth = 1, aes(color = type)) +
scale_color_manual(values = c("net_ma4" = "black"), labels = c("net_ma4" = "Net Portofolio") ) +
scale_fill_manual(values = color_list, labels = c("Public_Sector_ma4" = "Public Sector", "Private_Nonbanking_Sector_ma4" = "Private Nonbanking Sector", 
"Banking_Sector_ma4" = "Banking Sector", "net_ma4" = "")) +
scale_y_continuous( labels = function(x) paste0(x, "%"),
limits = c(min(net_type_long$value)*1.2, max(net_type_long$value)*1.2),
breaks = seq(-3.5, 2, by = 0.5),
expand = c(0,0)) +
scale_x_date(breaks = seq(min(net_type_long$time_period), max(net_type_long$time_period), by = "2 years"),
  date_labels = "%Y",
  expand = c(0,0)) +
labs(title = "South Africa Net Portfolio Investment (by flow type) (1997 - 2024)", 
caption = "Source: SARB, EconData", 
y = "% of GDP (4 quarter moving average)") +
theme_minimal(base_size = 10)+
theme(plot.title = element_text(size = rel(0.995), hjust = 0.5, face = "bold", margin = margin(b = 4, l = -4)),
          plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
          axis.title.y = element_text(margin = margin(r = 8)),
          panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_blank(),
          legend.spacing.y = unit(1.5, 'cm'),
          plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
          axis.text.x = element_text(angle = 90,hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.line = element_line(color = "black")) +
    codera_logo(net_type_long, xpos = 0.5, ypos = 0.3, wdth = 6.5)

    ggplotgrob_type <- ggplotGrob(ggplot_type)
  ggplotgrob_type$layout[grepl("panel", ggplotgrob_type$layout$name), ]$clip <- "off"

  png(filename = file.path(save_path, "Portfolio Flows (by entity).png"),
      width = 1250,
      height = 650,
      res = 190)
  grid.newpage()
  grid.draw(ggplotgrob_type)
  dev.off()
}


##non-rolling
not_rollmean <- left_join(net |> select(time_period, net_portfolio), net_type|> select(time_period, Public_Sector, Private_Nonbanking_Sector, Banking_Sector))|>
pivot_longer(-time_period, names_to = "type", values_to = "value")|>
filter(if_all(everything(), ~!is.na(.)))


if (TRUE) {
ggplot_type1 <- ggplot(data = not_rollmean, aes(x = time_period, y = value, fill = type)) +
base_graph +
geom_bar(data = not_rollmean |> filter(type != "net_portoflio"),stat = "identity", position = "stack") +
geom_line(data = not_rollmean |> filter(type == "net_prtfolio"), linewidth = 1, aes(color = type)) +
scale_color_manual(values = c("net_portfolio" = "black"), labels = c("net_portfolio" = "Net Portofolio") ) +
scale_fill_manual(values = color_list, labels = c("Public_Sector" = "Public Sector", "Private_Nonbanking_Sector" = "Private Nonbanking Sector", 
"Banking_Sector" = "Banking Sector", "net_portfolio" = "")) +
scale_y_continuous( labels = function(x) paste0(x, "%"),
limits = c(min(not_rollmean$value)*1.2, max(not_rollmean$value)*1.2),
breaks = seq(min(not_rollmean$value)*1.2, max(not_rollmean$value)*1.2, by = 100000),
expand = c(0,0)) +
scale_x_date(breaks = seq(min(not_rollmean$time_period), max(not_rollmean$time_period), by = "2 years"),
  date_labels = "%Y",
  expand = c(0,0)) +
labs(title = "South Africa Net Portfolio Investment (by flow type) (1997 - 2024)", 
caption = "Source: SARB, EconData", 
y = "% of GDP") +
theme_minimal(base_size = 10)+
theme(plot.title = element_text(size = rel(0.995), hjust = 0.5, face = "bold", margin = margin(b = 4, l = -4)),
          plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
          axis.title.y = element_text(margin = margin(r = 8)),
          panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_blank(),
          legend.spacing.y = unit(1.5, 'cm'),
          plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
          axis.text.x = element_text(angle = 90,hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.line = element_line(color = "black")) +
    codera_logo(not_rollmean, xpos = 0.5, ypos = 0.3, wdth = 6.5)

    ggplotgrob_type1 <- ggplotGrob(ggplot_type1)
  ggplotgrob_type1$layout[grepl("panel", ggplotgrob_type1$layout$name), ]$clip <- "off"

  png(filename = file.path(save_path, "Portfolio Flows (by entity)(not rollmean).png"),
      width = 1250,
      height = 650,
      res = 190)
  grid.newpage()
  grid.draw(ggplotgrob_type1)
  dev.off()
}



net_type <- net_type %>%
pivot_longer(-time_period, names_to = "type", values_to = "value")

write.xlsx(
  list(
    entity = net_type_long,
    type = net_long
  ),
  file = file.path(save_path, "Portfolio_Flows.xlsx"),
  overwrite = TRUE
)

