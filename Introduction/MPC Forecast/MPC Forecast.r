##Plotting past MPC forecasts


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
save_path <- file.path("Introduction", "MPC Forecast")
custom_base_size <- 20


first_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2018-01 (forecast date 2017-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2018-01-01")


second_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2019-01 (forecast date 2018-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2019-01-01")

third_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2020-01 (forecast date 2019-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2020-01-01")

fourth_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2021-01 (forecast date 2020-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2021-01-01")
##typo corrected in fourht
fifth_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2022-01 (forecast date 2021-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2022-01-01")

sixth_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2023-01 (forecast date 2022-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2023-01-01")


seventh_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2024-01 (forecast date 2023-10-01)")%>%
  as_tibble() %>%
  rename(growth = "GDP.A") %>%
         filter(time_period == "2024-01-01")


eighth_gdp_pot <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2025-01 (forecast date 2024-10-01)")%>%
  as_tibble()%>%
  rename(growth = "GDP.A") %>%
  filter(time_period >= "2025-01-01")%>%
  mutate(type = "growth")

##I was considering adding the latest forecast but the October release would create an inconsistency


gdp <- read_dataset(
  id = "MPC",
  version = "1.1.0",
  series_key = "GDP.A",
  release = "2025-03 (forecast date 2025-01-01)")%>%
  as_tibble() %>%
  rename(gdp = "GDP.A") %>%
  mutate(type = "gdp")


 # Combine all GDP datasets into one
gdp_combined <- bind_rows(
      first_gdp_pot %>%
        mutate(type = "growth"),
      second_gdp_pot %>%
         mutate(type = "growth"),
      third_gdp_pot %>%
        mutate(type = "growth"),
      fourth_gdp_pot %>%
         mutate(type = "growth"),
      fifth_gdp_pot %>%
       mutate(type = "growth"),
      sixth_gdp_pot %>%
       mutate(type = "growth"),
      seventh_gdp_pot %>%
       mutate(type = "growth"),
     eighth_gdp_pot %>%
      mutate(type = "growth")) 


graph <- left_join(gdp_combined, gdp, by = "time_period")%>%
select(time_period, growth, gdp) %>%
pivot_longer(cols = c("growth", "gdp"), names_to = "type", values_to = "value") %>%
 filter(type != "gdp" | time_period < as.Date("2025-01-01"))

#Plot
source("Jan/utils.r")

if (TRUE){
  ggplot_own <-  ggplot(graph, aes(x = time_period, y = value, fill = type)) +
  geom_bar(data = graph  ,stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_list, labels = c("gdp" = "Real GDP Growth", "growth" = "SARB Projection")) +
 scale_y_continuous(labels = function(x) paste0(format(x, nsmall = 0), "%"),
  limits = c(min(graph$value), max(graph$value) * 1.3),
  breaks = seq(-8, 6, by = 2)) +
coord_cartesian(ylim = c(-8, 6), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0.5)) +
  labs(title = "South African Real GDP Growth Outcomes Compared to SARB Projections",
       caption = "Source: SARB QB MPC Forecast, EconData.\nNote: January MPC each year, compared to 2025 vintage GDP.",
       y = "Growth (Year-on-Year)") +
theme_minimal(base_size = 10) +
        theme(plot.title = element_text(size = rel(0.995), hjust = 0.5, face = "bold", margin = margin(b = 4, l = -4)),
          plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
          axis.title.y = element_text(margin = margin(r = 8)),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(hjust = 0.5, vjust = 1), 
          axis.title.x = element_blank()) +
    codera_logo(graph, xpos = 0.6, ypos = 0.25, wdth = 6.5) 

    ggplotgrob_own <- ggplotGrob(ggplot_own)
    ggplotgrob_own$layout[grepl("panel", ggplotgrob_own$layout$name), ]$clip <- "off"
    png(filename = file.path(save_path, "GDP Forecast (option).png"),
        width = 1150,
        height = 650,
        res = 190)
    grid.newpage()
    grid.draw(ggplotgrob_own)
    dev.off()
}

