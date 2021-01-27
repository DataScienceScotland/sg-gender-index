# Power domain ------------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["power_dark"]],
                 sg_light = gi_col_palette[["power_light"]])


plots[["power"]] <- data[["power"]] %>% plot_domain()

plots[["power_economic"]] <- data[["power_economic"]] %>% plot_subdomain()

plots[["power_social"]] <- data[["power_social"]] %>% plot_subdomain()

plots[["power_political"]] <- data[["power_political"]] %>% plot_subdomain()

plots[["power_social"]] <- data[["power_social"]] %>% plot_subdomain()

plots[["private_sector"]] <- data[["private_sector"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()%>%
layout(xaxis = list(range = c(0, 52)))

plots[["public_body"]] <- data[["public_body"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()

plots[["council_leader"]] <- data[["council_leader"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()

plots[["ministers"]] <- data[["ministers"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()

plots[["msps"]] <- data[["msps"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()

plots[["police_judges"]] <- data[["police_judges"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()

plots[["media"]] <- data[["media"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()

plots[["sports_body"]] <- data[["sports_body"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator()


