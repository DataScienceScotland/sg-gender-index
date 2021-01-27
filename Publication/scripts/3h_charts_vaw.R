# Justice domain ----------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["justice_dark"]],
                 sg_light = gi_col_palette[["justice_light"]])

all_data[["partner_abuse"]] <- all_data[["partner_abuse"]] %>%
  filter(gender == "women") %>%
  mutate(.year = sub("^.*(\\d{2})$", "\\1", ref_period) %>% as.numeric(),
         .text = paste0("% of women who experienced partner abuse (psychological or physical) in the previous 12 months", "\n", "<b>", ref_period, ": </b>", value %>% comma(0.1, suffix = "%"))
  ) 
plots[["partner_abuse"]] <- all_data[["partner_abuse"]] %>%
  plot_ly(x = ~ .year, 
          y = ~ value, 
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "scatter",
            mode = "lines",
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    colorway = c(col_palette["sg_dark"]),
    yaxis = list(range = c(0,5.5), ticksuffix = "%"),
    xaxis = list(range = range(all_data[["partner_abuse"]]$.year) + c(-0.5, 1),
                 tickvals = unique(all_data[["partner_abuse"]]$.year),
                 ticktext = unique(all_data[["partner_abuse"]]$ref_period)
    ))

all_data[["partner_abuse_impact"]] <-
  bind_rows(all_data[["partner_abuse_physical"]],
            all_data[["partner_abuse_psych"]]
  ) %>%
  filter(gender == "women") %>%
  mutate(.type = case_when(variable == "partner_abuse_physical" ~ "Physical",
                           variable == "partner_abuse_psych" ~ "Psychological"),
         title_text = paste0("**% of women who experienced at least one type of physical or psychological effect as a result of the most recent incident of partner abuse in the previous 12 months, ", max(ref_period), "**"),
         .text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = "%"))
  )
plots[["partner_abuse_impact"]] <- all_data[["partner_abuse_impact"]] %>%
  plot_ly(x = ~ value, 
          y = ~ .type,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"),
         colorway = c(col_palette["sg_light"], col_palette["sg_dark"]),
         barmode = "group"
  )

all_data[["sexual_victimization"]] <-
  bind_rows(all_data[["sexual_harass"]],
            all_data[["sexual_assault"]]
  ) %>%
  filter(gender == "women") %>%
  mutate(.type = case_when(variable == "sexual_harass" ~ "Sexual harassment",
                           variable == "sexual_assault" ~ "More serious sexual assault"),
         title_text = paste0("**% of women who have experienced a more serious sexual assault or sexual harassment in the previous 12 months, ", max(ref_period), "**"),
         .text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(0.1, suffix = "%"))
  )
plots[["sexual_victimization"]] <- all_data[["sexual_victimization"]] %>%
  plot_ly(x = ~ value, 
          y = ~ .type,
          name = ~ ref_period,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "bar",
            #marker = list(color = ~ref_period),
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(xaxis = list(range = c(0, 17),
                      ticksuffix = "%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["sexual_assault_disclosure"]] <-
  all_data[["sexual_assault_disclosure"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(breakdown, "\n", "<b>", ref_period, ": </b>", value %>% comma(0.1, suffix = "%")),
         breakdown = breakdown %>% fct_reorder(value) %>% fct_relevel("None of these")
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
    xaxis = list(range = c(0, 105),
                 ticksuffix = "%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

plots[["partner_abuse_disclosure"]] <-
  all_data[["partner_abuse_disclosure"]] %>%
  filter(!is.na(value)) %>%
  mutate(.text = paste0(breakdown, "\n", "<b>", ref_period, ": </b>", value %>% comma(0.1, suffix = "%")),
         breakdown = breakdown %>% fct_reorder(value)  %>% fct_relevel("None of these")
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
    xaxis = list(range = c(0, 105),
                 ticksuffix = "%"),
    colorway = c(col_palette["sg_dark"]),
    barmode = "group"
  )

all_data[["femicide"]] <- all_data[["femicide"]] %>%
  filter(gender == "women") %>%
  mutate(.year = sub("^.*(\\d{2})$", "\\1", ref_period) %>% as.numeric(),
         .text = paste0(description, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = " women"))
  ) 
plots[["femicide"]] <- all_data[["femicide"]] %>%
  plot_ly(x = ~ .year, 
          y = ~ value, 
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "scatter",
            mode = "lines",
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    colorway = c(col_palette["sg_dark"]),
    xaxis = list(range = range(all_data[["femicide"]]$.year) + c(-0.5, 1),
                 tickvals = range(all_data[["femicide"]]$.year),
                 ticktext = range(all_data[["femicide"]]$ref_period)
    ))


all_data[["safe_night"]] <- all_data[["safe_night"]] %>%
  mutate(.year = sub("^.*(\\d{2})$", "\\1", ref_period) %>% as.numeric(),
         .text = paste0(gender, "\n", "<b>", ref_period, ": </b>", value %>% comma(1, suffix = "%"))
  ) 
plots[["safe_night"]] <- all_data[["safe_night"]] %>%
  plot_ly(x = ~ .year, 
          y = ~ value, 
          name = ~ gender,
          text = ~ .text,
          height = 300,
          hoverinfo = ~ "text") %>%
  add_trace(type = "scatter",
            mode = "lines",
            showlegend = FALSE) %>%
  add_style_chart() %>%
  layout(#title = ~description[1],
    colorway = c(col_palette["sg_light"], col_palette["sg_dark"]),
    annotations = all_data[["safe_night"]] %>% 
      filter(.year == max(.year, na.rm = TRUE)) %>% 
      select(y = value, x = .year, text = gender) %>% 
      mutate(showarrow = FALSE, xanchor = "right", yshift = c(-10, 10), align = "right"),
    xaxis = list(range = range(all_data[["safe_night"]]$.year) + c(-0.5, 1),
                 tickvals = range(all_data[["safe_night"]]$.year),
                 ticktext = range(all_data[["safe_night"]]$ref_period)),
    yaxis = list(range = c(0, 105),
                 ticksuffix = "%")
  )


