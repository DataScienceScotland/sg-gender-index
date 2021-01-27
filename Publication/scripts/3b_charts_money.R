# Money domain ------------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["money_dark"]],
                 sg_light = gi_col_palette[["money_light"]])

plots[["money"]] <- data[["money"]] %>% plot_domain()

plots[["money_income"]] <- data[["money_income"]] %>% plot_subdomain()

plots[["money_limited_independent_resources"]] <- data[["money_limited_independent_resources"]] %>% plot_subdomain()

plots[["money_wealth"]] <- data[["money_wealth"]] %>% plot_subdomain()


plots[["earnings_median_all"]] <- all_data[["earnings_median_all"]] %>% 
  mutate(text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": </b>", value %>% comma(0.01, prefix = "£"))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(#range = c(0,1.05),
    tickprefix = "£"))

plots[["earnings_median_ft"]] <- all_data[["earnings_median_ft"]] %>% 
  mutate(text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": </b>", value %>% comma(0.01, prefix = "£"))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(#range = c(0,1.05),
    tickprefix = "£"))

plots[["inc_pool"]] <- data[["inc_pool"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["inc_pool_age"]] <- all_data[["inc_pool_age"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")

plots[["inc_pool_simd"]] <- all_data[["inc_pool_simd"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")


plots[["living_wage"]] <- all_data[["living_wage"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": </b>", value %>% percent(1))) %>%
  plot_indicator_time(nudge=c(-10,10)) %>%
  layout(yaxis = list(range = c(0,1.05),
                      tickformat = "%"))

plots[["mat_dep"]] <- data[["mat_dep"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["poverty"]] <- data[["poverty"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["spend"]] <- all_data[["spend"]]  %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["spend_age"]] <- all_data[["spend_age"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")

plots[["spend_simd"]] <- all_data[["spend_simd"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")

plots[["spend_lrg"]] <- all_data[["spend_lrg"]]  %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% percent(1))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 1.05), tickformat = "%"))

plots[["wealth_pension"]] <- all_data[["wealth_pension"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, prefix = "£"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(#range = c(0, 105),
    tickprefix = "£"))

plots[["wealth_net"]] <- all_data[["wealth_net"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, prefix = "£"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 2200),
    tickformat = ",",
    tickprefix = "£"))


