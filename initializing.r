####
##Load Packages & Data
####

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


ports<-c("BFN", "CPT", "DUR", "HLA", )
for (port in ports)
df<-read_dataset(id="SARS_TRADE",
                    )
View(df)
df<-df %>%
    mutate(year=str_sub(period, 1, 4),
    month=str_sub(period, 6, 7)) 

                
df_years<-df %>%
    group_by(year)

View(df)


ggplot(df_years, aes(x=period, y=value), color=port, group=year) +
    geom_line(linewidth = 0.75) + 

    labs(title = "Exports from Cape Town", 
        subtitle = "2010-2025",
        x = "Date",
        y = "Total Exports from Cape Town") +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        plot.margin = margin(15, 15, 15, 15)
    )

ggsave("cpt_exp.png", 
       width = 8, 
       height = 5, 
       dpi = 300)

