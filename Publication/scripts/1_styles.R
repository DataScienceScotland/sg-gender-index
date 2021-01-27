# Chart styles ------------------------------------------------------------

add_style_chart <- function(p) {
  p %>% config(displayModeBar = FALSE,
               showAxisDragHandles = FALSE) %>%
    layout(
      xaxis = list(
        linecolor = rgb(255, 255, 255, maxColorValue = 255),
        width = 0,
        fill = NA,
        fixedrange = TRUE,
        bty = "n",
        showline = FALSE,
        title = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        showline = FALSE,
        title = "",
        showgrid = FALSE,
        tick0 = 0,
        zeroline = FALSE,
        rangemode = "tozero",
        tickformat = ","
      ),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      margin = list(l = 0,
                    r = 0)
    ) %>%
    htmlwidgets::onRender(
      "function(el, x) {
      Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'default')}"
    )
}

# Chart templates ---------------------------------------------------------

plot_domain <- function(ds) {
  ds <- ds %>%
    mutate(subdomain_label = subdomain %>% 
             str_wrap(20) %>%
             gsub("\\n", "<br>", .) %>%
             as_factor() %>%fct_rev() %>% 
             fct_relabel(~case_when(.x == "Total" ~ "         Domain total", TRUE ~ .x)))
  plot_ly(
    x = ~ score,
    y = ~ subdomain_label,
    text = ~ text,
    height = 300,
    hoverinfo = ~ "text"
  ) %>%
    add_trace(type = "bar",
              data = filter(ds, subdomain != "Total"),
              showlegend = FALSE) %>%
    add_trace(type = "bar",
              data = filter(ds, subdomain == "Total"),
              showlegend = FALSE) %>%
    add_style_chart() %>%
    layout(colorway = c(col_palette["sg_dark"], col_palette["sg_light"]),
           barmode = "stack",
           xaxis = list(range = c(0, 105)))
}

plot_subdomain <- function(ds) {
  ds <- ds %>%
    mutate(indicator_label = indicator %>% 
             str_wrap(20) %>%
             gsub("\\n", "<br>", .) %>%
             as_factor() %>%fct_rev() %>% 
             fct_relabel(~case_when(.x == "Total" ~ "   Sub-domain total", TRUE ~ .x)))
  plot_ly(
    x = ~ score,
    y = ~ indicator_label,
    text = ~ text,
    height = 300,
    hoverinfo = ~ "text"
  ) %>%
    add_trace(type = "bar",
              data = filter(ds, indicator != "Total"),
              showlegend = FALSE) %>%
    add_trace(type = "bar",
              data = filter(ds, indicator == "Total"),
              showlegend = FALSE) %>%
    add_style_chart() %>%
    layout(colorway = c(col_palette["sg_dark"], col_palette["sg_light"]),
           barmode = "stack",
           xaxis = list(range = c(0, 105)))
}

plot_indicator <- function(ds, var = "gender") {
  Encoding(ds$text) <- "UTF-8" #markdown uses UTF-8 rather than Windows encoding - any character variables containing special characters need converting to UTF-8
  ds %>%
    rename(var = contains(var)) %>%
    mutate(var = case_when(var == "women" ~ "Women",
                              var == "men" ~ "Men",
                              TRUE ~ var) %>%
             as.factor()
    ) %>%
    plot_ly(
      x = ~ value,
      y = ~ var,
    text = ~ text,
    height = 200,
    hoverinfo = ~ "text"
  ) %>%
    add_trace(type = "bar",
              showlegend = FALSE) %>%
    add_style_chart() %>%
    layout(#title = ~description[1],
           colorway = c(col_palette["sg_dark"]),
           barmode = "stack",
           margin = list(t= 0, b = 0))
}

plot_indicator_time <- function(ds, var = "gender", nudge = 10) {
  Encoding(ds$text) <- "UTF-8" #markdown uses UTF-8 rather than Windows encoding - any character variables containing special characters need converting to UTF-8
  ds <- ds %>%
    rename(var = contains(var)) %>%
    mutate(year = sub("^.*(\\d{2})$", "\\1", ref_period) %>% as.numeric(),
           var = case_when(var == "women" ~ "Women",
                           var == "men" ~ "Men",
                              TRUE ~ var)
    )
  width = max(ds$year, na.rm = TRUE) - min(ds$year, na.rm = TRUE) 
  ds %>%
    plot_ly(
      x = ~ year,
      y = ~ value,
      name = ~ var,
      text = ~ text,
      height = 200,
      hoverinfo = ~ "text"
    ) %>%
    add_trace(type = "scatter",
              mode = "lines",
              showlegend = FALSE) %>%
    add_style_chart() %>%
    layout(#title = ~description[1],
      colorway = c(col_palette["sg_light"], col_palette["sg_dark"]),
      margin = list(t= 0, b = 0),
      annotations = ds %>% 
        filter(year == max(year, na.rm = TRUE)) %>% 
        select(y = value, x = year, text = var) %>% 
        mutate(showarrow = FALSE, xanchor = "right", yshift = nudge, align = "right"),
      xaxis = list(range = ~range(year) + c(-0.04, 0.1) * width,
                   tickvals = ~range(year),
                   ticktext = ~range(ref_period))
    )
}

plot_indicator_barbell <- function(ds) {
  Encoding(ds$text) <- "UTF-8" #markdown uses UTF-8 rather than Windows encoding - any character variables containing special characters need converting to UTF-8
  ds <- ds %>%
    mutate(gender = case_when(gender == "women" ~ "Women",
                              gender == "men" ~ "Men",
                              TRUE ~ gender) %>%
             as.factor()
    ) %>%
    group_by(breakdown) %>%
    mutate(colour = ifelse(gender[which.max(value)] == "Women", "sg_dark", "sg_light"))
    plot_ly(ds,
      x = ~ value,
      y = ~ breakdown,
      name = ~ breakdown,
      text = ~ text,
      height = 200,
      hoverinfo = ~ "text",
      colors = c(col_palette["sg_light"], col_palette["sg_dark"])
    ) %>%
      add_trace(type = "scatter",
                mode = "lines",
                color = ~colour,
                showlegend = FALSE) %>%
      add_trace(type = "scatter",
                mode = "markers",
                marker = list(size = 10),
                color = ~ ifelse(gender=="Women", "sg_dark", "sg_light"),
                showlegend = FALSE) %>%
    add_style_chart() %>%
    layout(#title = ~description[1],
      margin = list(t= 0, b = 0))
}

