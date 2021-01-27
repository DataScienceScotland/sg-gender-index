

# Women Specific Health ------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["whealth_dark"]],
                 sg_mid = gi_col_palette[["whealth_mid"]],
                 sg_light = gi_col_palette[["whealth_light"]])

plots[["termination_deprivation"]] <-
  all_data[["termination_deprivation"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = " terminations")),
         breakdown = breakdown %>% fct_rev()) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 4200), tickformat = ","),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["termination_age"]] <-
  all_data[["termination_age"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = " terminations")),
         breakdown = breakdown %>% fct_rev()) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 4200), tickformat = ","),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["positive_physical"]] <-
  all_data[["positive_physical"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(breakdown, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = "%")),
         breakdown = breakdown %>% fct_relevel(c("Now", "During pregnancy", "Before Pregnancy"))
  ) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 109), ticksuffix="%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["positive_mental"]] <-
  all_data[["positive_mental"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(breakdown, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = "%")),
         breakdown = breakdown %>% fct_relevel(c("Now", "During pregnancy", "Before Pregnancy"))
  ) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 109), ticksuffix="%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["maternal_smoke"]] <-
  all_data[["maternal_smoke"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(breakdown, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = "%")),
         breakdown = breakdown %>% fct_relevel(c("Current", "Former", "Never"))
  ) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 105), ticksuffix="%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["maternal_BMI"]] <-
  all_data[["maternal_BMI"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(breakdown, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = "%", scale = 1.000001)),
         breakdown = breakdown %>% fct_relevel(c("Obese", "Overweight", "Healthy", "Underweight"))
  ) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 105), ticksuffix="%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )


plots[["contraception_deprivation"]] <-
  all_data[["contraception_deprivation"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = " women per 1000 aged 15-49")),
  breakdown = breakdown %>% fct_rev()
  ) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 70)),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["contraception_age"]] <-
  all_data[["contraception_age"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = " women per 1000")),
 breakdown = breakdown %>% fct_rev()
 ) %>%
  plot_ly(x = ~ value, 
          y = ~ breakdown,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    xaxis = list(range = c(0, 70)),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

all_data[["IVF_time"]] <- all_data[["IVF_time"]] %>%
  mutate(.year = as.Date(as.numeric(ref_period), origin = "1899-12-30"),
         ref_range = ifelse(max(ref_period) == min(ref_period), .year, paste(min(.year), "to", max(.year))),
         title_text = paste0("**",description,"**"),
         .text = paste0(breakdown, "\n", "<b>", format(.year, "%B %Y"), ": </b>", value %>% comma(1, suffix = " women"))
  ) 
plots[["IVF_time"]] <- all_data[["IVF_time"]] %>%
  filter(gender == "women") %>%
  plot_ly(x = ~ .year, 
          y = ~ value, 
          name = ~ breakdown,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "scatter",
            mode = "lines",
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~ description,
    colorway = c(col_palette["sg_light"], col_palette["sg_dark"], col_palette["sg_mid"]),
    annotations = all_data[["IVF_time"]] %>% 
      filter(.year == max(.year, na.rm = TRUE)) %>% 
      select(y = value, x = .year, text = breakdown) %>% 
      mutate(showarrow = FALSE, xanchor = "right", yshift = c(-10, -10, 20), xshift = c(0, -180, -180), align = "right"),
    xaxis = list(range = range(all_data[["IVF_time"]]$.year) + c(-1, 1)
                 #                 tickvals = range(all_data[["IVF_time"]]$.year),
                 #                 ticktext = range(all_data[["IVF_time"]]$.year)
    ))

