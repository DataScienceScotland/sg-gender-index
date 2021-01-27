
# Time Use Domain ---------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["time_dark"]],
                 sg_light = gi_col_palette[["time_light"]])

plots[["time_use"]] <- data[["time_use"]] %>% plot_domain()

plots[["time_use_care"]] <- data[["time_use_care"]] %>% plot_subdomain()

plots[["time_use_time"]] <- data[["time_use_time"]] %>% plot_subdomain()


plots[["child_careDev"]] <- data[["child_careDev"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = " minutes"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(
    #range = c(0,1.05),
    #ticksuffix = " mins"
  ))

plots[["child_careNDev"]] <- data[["child_careNDev"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = " minutes"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(
    #range = c(0,1.05),
    #ticksuffix = " mins"
  ))

plots[["hhld_management"]] <- data[["hhld_management"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = " minutes"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(
    #range = c(0,1.05),
    #ticksuffix = " mins"
  ))

plots[["housework"]] <- data[["housework"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = " minutes"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(
    #range = c(0,1.05),
    #ticksuffix = " mins"
  ))

plots[["leisure"]] <- data[["leisure"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = " minutes"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(
    #range = c(0,1.05),
    #ticksuffix = " mins"
  ))

plots[["adult_care"]] <- data[["adult_care"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["volunteer"]] <- data[["volunteer"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))




