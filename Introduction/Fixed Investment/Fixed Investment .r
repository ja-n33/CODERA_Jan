# # Fixed Investment 
# ## A concern with replicating the image provided, is that gfcf consists of more components than just government, public and private.
# # The image provided only shows these three components. Therefore, I used all available gfcf components available on EconData.

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
              "tcltk") ## needed for tibble()

invisible(lapply(packages, library, c=TRUE))
setwd(Sys.getenv("CODERA_Jan"))
root<-Sys.getenv("CODERA_Jan")
save_path <- file.path(root, "Introduction", "Fixed Investment")
custom_base_size <- 20

# # Load Data

data1 <- read_dataset(
   id = "QB_NATLACC",
   series_key = "KBP6114D+KBP6122D+KBP6106D+KBP6130D+KBP6118D+KBP6110D+KBP6126D+KBP6109D+KBP6100D.Q.R.S.LA") 

data1<-data1 %>%
    rename(general_government = KBP6100D.Q.R.S.LA, 
        public_corporations = KBP6106D.Q.R.S.LA,
        private_business_enterprises = KBP6109D.Q.R.S.LA,
        residential_buildings = KBP6110D.Q.R.S.LA,
        nonresidential_buildings = KBP6114D.Q.R.S.LA,
        construction_works = KBP6118D.Q.R.S.LA,
        transport_equipment_total = KBP6122D.Q.R.S.LA,
        machinery = KBP6126D.Q.R.S.LA,
        transfer_costs = KBP6130D.Q.R.S.LA) #Everything is in real terms and in Millions of Rands

data1$gfcf <- rowSums(select(data1,2:10), na.rm = TRUE) # Making a  gfcf column. 
        
rgdp <- read_dataset(
   id = "NATL_ACC_SARB",
   series_key = "KBP6006.R.S")

rgdp<-rgdp %>%
        rename(rgdp = KBP6006.R.S)

 # Prepare Data
 data_raw <- left_join(data1, rgdp, by = "time_period") 

final <- data_raw %>%
   mutate(gov = general_government/gfcf*100,
   pub = public_corporations/gfcf*100,
   priv = private_business_enterprises/gfcf*100,
   res = residential_buildings/gfcf*100,
   nonres = nonresidential_buildings/gfcf*100,
   const = construction_works/gfcf*100,
   trans = transport_equipment_total/gfcf*100,
   mach = machinery/gfcf*100,
   tc = transfer_costs/gfcf*100,
   gfc = gfcf/rgdp*100)
  
# Save the Data
write.xlsx(final, file.path(save_path, "Data.xlsx"))

# Pivot Data (for grpahing purposes)
 final_pivot <- final %>%
 group_by(time_period) %>%
   pivot_longer(cols = c(gfc, res, nonres, const, trans, mach, tc),
                names_to = "type",
                values_to = "percentage") %>%
   mutate(type = factor(type, levels = c("gfc", "res", "nonres", "const","trans", "mach", "tc"))) |>
   select(time_period, type, percentage)


# Graph
#source("utils.R") ##Not sure what this is supposed to do

color_list <- RColorBrewer::brewer.pal(9, "Set1")
 ##using this in the absence of color list from utils.R

scaling <- max(filter(final_pivot, type != "gfc")$percentage, na.rm = TRUE) / max(filter(final_pivot, type == "gfc")$percentage, na.rm = TRUE)
##Divide all types by gfc

if (TRUE) { ##This is a development tool that allows us to comment out large blocks in 1 go
ggplot_fixed <- ggplot() +
   #base_graph + ##This is part of utils.R which i dont have
   geom_area(data = filter(final_pivot, type != "gfc"), 
             aes(x = time_period, y = percentage, fill = type), 
             alpha = 0.8) +
   geom_line(data = filter(final_pivot, type == "gfc"), 
             aes(x = time_period, y = percentage * scaling, color = type), 
             linewidth = 1.2) +
   scale_fill_manual(
     values = color_list, 
     labels = c("General Government", "Public Corporations", "Private Enterprises", 
                "Residential Buildings", "Non-Residential Buildings", 
                "Construction Works", "Transport Equipment", "Machinery", 
                "Transfer Costs")
   ) +
   scale_color_manual(
     values = c("gfc" = "black"),
     labels = c("gfc" = "Gross Fixed \nCapital Formation (Total) (RHS)")
   ) +
   scale_y_continuous(
     name = "% of Total GFCF",  
     sec.axis = sec_axis(~ . / scaling, name = "GFCF as % of Real GDP")  
   ) +
   labs(
     y = "% of total GFCF", 
     title = "Gross Fixed Capital Formation by Sector",
     caption = "Source: StatsSA"
   ) +
   theme_minimal(base_size = 10) +
   theme(
     plot.title = element_text(size = rel(0.995), hjust = 0.5, face = "bold", margin = margin(b = 4, l = -4)),
     plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
     plot.title.position = "plot",
     plot.caption.position = "plot",
     plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
     axis.title.y = element_text(margin = margin(r = 8)),
     panel.grid = element_blank(),
     legend.position = "right",
     legend.title = element_blank(),
     legend.text = element_text(size = 8), 
     legend.key.size = unit(0.4, "cm"),
     plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
     axis.line = element_line(color = "black"),
     axis.ticks = element_line(color = "black"),
     axis.text.x = element_text(margin = margin(r = 8)),
     axis.title.x = element_blank()
   ) +
 guides(fill = guide_legend(ncol = 1)) #+
 #codera_logo(final_pivot, xpos = 0.7, ypos = 0.2, wdth = 6.5)

 ggplotgrob_fixed <- ggplotGrob(ggplot_fixed)
 ggplotgrob_fixed$layout[grepl("panel", ggplotgrob_fixed$layout$name), ]$clip <- "off"

png(filename = file.path(save_path, "Fixed Investments.png"),
     width = 1137,
     height = 650,
     res = 190)

grid.newpage()
grid.draw(ggplotgrob_fixed)
dev.off()
 }
print(ggplotgrob_fixed)
# Plotting 
 sector_plot <- final %>%
 select(time_period, general_government, public_corporations, private_business_enterprises, gfc) %>%
 group_by(time_period) |>
 mutate(total_investment = general_government+public_corporations+private_business_enterprises) |>
 mutate( Government = general_government/total_investment*100,
Public_Corporations = public_corporations/total_investment*100,
 Private_Business = private_business_enterprises/total_investment*100)|>
 select(Government, Public_Corporations, Private_Business, gfc, time_period) |>
 pivot_longer(cols = c(Government, Public_Corporations, Private_Business, gfc), names_to = "Sector", values_to = "Value")

# # Plot
if (TRUE){
ggplot_fixed <- ggplot() +
#     base_graph +
   geom_area(data = filter(sector_plot, Sector != "gfc"), 
             aes(x = time_period, y = Value, fill = Sector), 
             alpha = 0.8) +
   # Line plot for "gfc" only
   geom_line(data = filter(sector_plot, Sector == "gfc"), 
             aes(x = time_period, y = Value, color = "Total GFCF"), 
             linewidth = 1.2) +
    scale_fill_manual(
     values = color_list, 
     labels = c("General Government", "Public Corporations", "Private Enterprises")
   ) +
   scale_color_manual(
     values = c("Total GFCF" = "black"),
     labels = c("Total GFCF" = "Gross Fixed Capital Formation (RHS)") 
   ) +
    scale_y_continuous(
     name = "% of Total GFCF",  
     sec.axis = sec_axis(~ ., name = "GFCF as % of Real GDP") # I am not able to get the y axis to only relate to gfc
   ) +
   scale_x_date(breaks = seq(min(sector_plot$time_period), max(sector_plot$time_period), by = "10 years"),
     date_labels = "%Y"  
       ) +
   labs(
     y = "% of total GFCF", 
     x = "Year",
     title = "Gross Fixed Capital Formation by Sector for South Africa",
     caption = "Source: StatsSA, EconData. \n The three sectors are combined to form the total GFCF,     with each sector's contribution shown as a percentage of the total GFCF.") +
     theme_minimal(base_size = 15) +
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
     axis.title = element_text(size = 8),  
     axis.text = element_text(size = 8), 
     axis.title.y = element_text(margin = margin(r = 8)),
     axis.title.x = element_blank(),
     axis.line = element_line(color = "black"),
     axis.ticks.x = element_line(color = "black"),
     legend.position = "top",
     legend.direction = "horizontal",
     legend.text = element_text(size = 7), 
     legend.key.size = unit(0.4, "cm"),
     legend.title = element_blank(),
     plot.caption = element_text(size = 8, hjust = 0),
     panel.grid = element_blank()
   ) +
   guides(
     fill = guide_legend(nrow = 1),  
     color = guide_legend(nrow = 1)  
   )
   #  codera_logo(sector_plot, xpos=0.3, ypos=0.8, wdth=6.5)

 ggplotgrob_fixed1 <- ggplotGrob(ggplot_fixed)
 ggplotgrob_fixed1$layout[grepl("panel", ggplotgrob_fixed1$layout$name), ]$clip <- "off"
 png(filename = file.path(save_path, "Fixed Investments by Sector.png"),
     width = 1137,
     height = 650,
     res = 190       )
 grid.newpage()
 grid.draw(ggplotgrob_fixed1)
 dev.off()
 }

