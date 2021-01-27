data[["Scotland"]] <- data[["Scotland"]] %>%
  filter(domain %in% c("Work", "Money", "Knowledge", "Time", "Power", "Health", "Total")) %>%
  mutate(domain = domain %>% replace(domain == "Total", "Scotland"),
         domain = domain %>% fct_relevel("Work", "Money", "Time", "Knowledge", "Power", "Health", "Scotland") %>% fct_rev(),
         colour = case_when(domain == "Time Use" ~ "time",
                            domain == "Scotland" ~ "gi",
                            TRUE ~ tolower(domain)))

ds <- data[["Scotland"]] %>% filter(domain == "Scotland") %>% mutate(domain = domain %>% fct_drop())
ds_light <- bind_rows(ds %>% mutate(score = 1),
                      ds %>% mutate(score = 100))
plots[["Scotland1"]] <- plot_ly(ds_light,
                               y = ~ domain,
                               x = ~ score,
                               height = 200,
                               hovertext = ~ text,
                               hoverinfo = ~ "text",
                               colors = gi_col_palette) %>%
  add_trace(type = "scatter",
            mode = "lines+markers",
            color = ~ paste0(colour, "_light"),
            marker = list(size = 10),
            showlegend = FALSE) %>%  
  # add_trace(type = "scatter",
  #           mode = "text",
  #           color = "gi_white",
  #           text = ~ round(score,0),
  #           showlegend = FALSE) %>%  
  add_trace(data = ds,
            type = "scatter",
            mode = "markers",
            marker = list(size = 30),
            color = ~paste0(colour, "_dark"),
            showlegend = FALSE)%>%  
  add_trace(data = ds,
            type = "scatter",
            mode = "text",
            color = "gi_white",
            hoverinfo = ~ "none",
            text = ~ round(score,0),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(annotations = tibble(x = c(1, 100), y = "Scotland", text = c("1\nNo equality", "100\nFull Equality"), xshift = c(-10, 10), yshift = -25, showarrow = FALSE, xanchor = c("left", "right"), align = c("left", "right")),
         yaxis = list(range = c(-0.5, 0.5)),
         xaxis = list(title = "",
                      zeroline = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      showgrid = FALSE),
         margin = list(t= 0, b = 0))

ds <- data[["Scotland"]] %>% filter(domain != "Scotland") %>% mutate(domain = domain %>% fct_drop(), col = paste0(colour, "_dark"))
ds_light <- bind_rows(ds %>% mutate(score = 1),
                      ds %>% mutate(score = 100)) %>%
  mutate(col = paste0(colour, "_light"))

ggplot(ds_light, aes(y = domain, x = score)) +
  geom_line(aes(color = col), size = 2) +
  geom_point(aes(color = col), size = 5) +
  geom_point(data = ds, aes(color = col), size = 15) +
  geom_text(data = ds, aes(label = round(score)), colour = "white", fontface = "bold", size = 7) +
  scale_color_manual(values = gi_col_palette) + 
  scale_x_continuous(name = NULL, breaks = c(1,100), labels = c("1\nNo Equality", "100\nFull Equality"), limits = c(1,110)) +
  ggplot2::theme(
    # Declutter
    panel.background = ggplot2::element_blank(),
    legend.position = "none",
    axis.title = ggplot2::element_blank(),
    axis.ticks = element_blank(),
    
    #margin
    plot.margin = grid::unit(c(0,5,5,0), "mm"),
    
    # Text
    text = element_text(family = "Arial",size=20),
    title = element_text(size = 20),
    plot.subtitle=element_text(colour="#4B4B4B"),
    axis.text = element_text(size = 20,
                             colour = "#222222")
  )
ggsave("key_findings.svg", height = 5, width = 8)
  
  

plots[["Scotland2"]] <- plot_ly(ds_light,
                               y = ~ domain,
                               x = ~ score,
                               height = 400,
                               hovertext = ~ text,
                               hoverinfo = ~ "text",
                               colors = gi_col_palette) %>%
  add_trace(type = "scatter",
            mode = "lines+markers",
            color = ~ paste0(colour, "_light"),
            marker = list(size = 10),
            showlegend = FALSE) %>%  
  # add_trace(type = "scatter",
  #           mode = "text",
  #           color = "gi_white",
  #           text = ~ round(score,0),
  #           showlegend = FALSE) %>%  
  add_trace(data = ds,
            type = "scatter",
            mode = "markers",
            marker = list(size = 30),
            color = ~paste0(colour, "_dark"),
            showlegend = FALSE)%>%  
  add_trace(data = ds,
            type = "scatter",
            mode = "text",
            color = "gi_white",
            hoverinfo = ~ "none",
            text = ~ round(score,0),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(annotations = tibble(x = c(1, 100), y = "Health", text = c("1\nNo equality", "100\nFull Equality"), xshift = c(-10, 10), yshift = -35, showarrow = FALSE, xanchor = c("left", "right"), align = c("left", "right")),
         xaxis = list(title = "",
                      zeroline = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      showgrid = FALSE),
         margin = list(t= 0, b = 0))

