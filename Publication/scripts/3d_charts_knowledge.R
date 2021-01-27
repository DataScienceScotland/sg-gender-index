# Knowledge domain --------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["knowledge_dark"]],
                 sg_light = gi_col_palette[["knowledge_light"]])

plots[["knowledge"]] <- data[["knowledge"]] %>% plot_domain()

plots[["knowledge_attainment"]] <- data[["knowledge_attainment"]] %>% plot_subdomain()

plots[["knowledge_subject_segregation"]] <- data[["knowledge_subject_segregation"]] %>% plot_subdomain()

plots[["attain_secondary"]] <- data[["attain_secondary"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%", scale = 1.000001))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["degree"]] <- data[["degree"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["attain_pay"]] <- data[["attain_pay"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, prefix = "£"))) %>%
  plot_indicator()

plots[["attain_secondary"]] <- data[["attain_secondary"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%", scale = 1.000001))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["degree"]] <- data[["degree"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["ma"]] <- data[["ma"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1))) %>%
  plot_indicator() %>%
  layout(xaxis = list(tickformat = ",")) #comma formating

plots[["stem_secondary"]] <- data[["stem_secondary"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 105), ticksuffix = "%"))

plots[["stem_degree"]] <- data[["stem_degree"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% percent(1))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 1.05), tickformat = "%"))

plots[["stem_degree_subject"]] <- all_data[["stem_degree_subject"]] %>% 
  filter(!grepl("_n", gender)) %>%
  mutate(breakdown = fct_reorder(breakdown,value,max),
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", breakdown, ": </b>", value %>% percent(0.1))) %>%
  plot_indicator_barbell()%>%
  layout(xaxis = list(range = c(0, 0.22),
                      tickformat = "%"))

