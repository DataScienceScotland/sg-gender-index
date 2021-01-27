gi_col_palette <- c(gi_white = "#FFFFFF",
                    gi_grey = "#8e979c", # From SG logo
                    gi_dark = "#0065bd", # From SG logo
                    gi_orange = "#bd4500", #complement of SG blue
                    gi_light = "#80c3ff",
                    work_dark = RColorBrewer::brewer.pal(12, "Paired")[2],  #1F78B4
                    work_light = RColorBrewer::brewer.pal(12, "Paired")[1], #A6CEE3
                    knowledge_dark = RColorBrewer::brewer.pal(12, "Paired")[4], #33A02C
                    knowledge_light = RColorBrewer::brewer.pal(12, "Paired")[3], #B2DF8A
                    power_dark = RColorBrewer::brewer.pal(12, "Paired")[6], #E31A1C
                    power_light = RColorBrewer::brewer.pal(12, "Paired")[5], #FB9A99
                    #time_dark = RColorBrewer::brewer.pal(12, "Paired")[8], #FF7F00
                    time_dark = "#E67300",
                    time_light = RColorBrewer::brewer.pal(12, "Paired")[7], #FDBF6F
                    health_dark = RColorBrewer::brewer.pal(12, "Paired")[10], #6A3D9A 
                    health_light = RColorBrewer::brewer.pal(12, "Paired")[9], #CAB2D6
                    money_dark = "#993366", 
                    money_light = "#ecc6d9",
                    justice_dark = "#669900",#"#77b300", 
                    justice_light = "#e6ffb3",
                    whealth_dark = "#008080", 
                    whealth_mid = "#00cccc", 
                    whealth_light = "#ccffff")

ggplot(tibble(colname = names(gi_col_palette), col = gi_col_palette),
       aes(x=colname,fill=colname),
       y=10) + 
  geom_bar() +
  coord_flip() +
  scale_fill_manual(values=gi_col_palette)
  

