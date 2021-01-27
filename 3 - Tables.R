library(openxlsx)
options("openxlsx.minWidth" = 8.43)
sg_tables <- function(lst_ds, sheet_titles, source_links = list(), footnotes = list(), number_fmt = list()) {
  wb <- createWorkbook()
  
  #Fix sheet names if missing
  if(is.null(names(sheet_titles))) names(sheet_titles) <- rep("", length(sheet_titles))
  names(sheet_titles) <- ifelse(names(sheet_titles) == "", paste("Sheet", seq_along(sheet_titles)), names(sheet_titles))
  
  #table of contents
  addWorksheet(wb, sheetName = "TOC")
  setColWidths(wb, "TOC", width = c(8.43, "auto"), cols = 1:2, ignoreMergedCells = TRUE)
  addStyle(wb, "TOC", rows = 1, cols = 1, style = createStyle(fontSize = 14, textDecoration = "bold"))
  writeData(wb, "TOC", x = "Table of Contents") #title
  pwalk(list(sheet_titles, names(sheet_titles), seq_along(sheet_titles)), ~ {
    writeFormula(wb, "TOC", startRow = 1 + ..3,
                 x = makeHyperlinkString(sheet = ..2, row = 1, col = 1, text = ..2))
    writeData(wb, "TOC", startRow = 1 + ..3, startCol = 2, x = ..1) #table
  })
  
  pwalk(list(lst_ds, names(sheet_titles), sheet_titles), ~{
    #set up sheet
    data_table <- ..1
    sheet_name <- ..2
    sheet_title <- ..3
    sheet_number_formats = as.list(number_fmt[[sheet_name]])
    
    ncols <- max(1, length(data_table))
    nrows <- nrow(data_table)
    addWorksheet(wb, sheetName = sheet_name)
    setColWidths(wb, sheet_name, width = c(rep("auto", ncols )), cols = 1:ncols, ignoreMergedCells = TRUE)
    
    
    addStyle(wb, sheet_name, rows = 1, cols = 1, style = createStyle(fontSize = 14, wrapText = TRUE, textDecoration = "bold")) #title
    
    #write spreadsheet
    if(ncols > 1) mergeCells(wb, sheet_name, cols = 1:ncols, rows = 1)
    writeData(wb, sheet_name, x = sheet_title) #title
    #writeData(wb, sheet_name, x = link, startRow = 2) #source/link
    if(!is.null(footnotes[[sheet_name]])) {
      addStyle(wb, sheet_name, rows = nrows + 4, col = 1:ncols, style = createStyle(wrapText = TRUE)) 
      if(ncols > 1) mergeCells(wb, sheet_name, cols = 1:ncols, rows = nrows + 4)
      writeData(wb, sheet_name, x = paste0("[", seq_along(footnotes[[sheet_name]]), "]", footnotes[[sheet_name]]), startRow = nrows + 4, startCol = 1) #footnotes
    }
    
    if(nrows == 0) return(NULL)
    
    #table formatting
    addStyle(wb, sheet_name, rows = 3, cols = seq_len(ncols), style = createStyle(wrapText = TRUE, textDecoration = "bold", border = c("top", "bottom"))) #header border
    addStyle(wb, sheet_name, rows = nrows + 3, cols = seq_len(ncols), style = createStyle(border = c("bottom"))) #footer border
    
    #numeric formatting
    pwalk(list(data_table, names(data_table), seq_along(data_table)),
          function(x, name, id, sheet = sheet_name, numFmt = sheet_number_formats[[name]]) {
            if(is.numeric(x) & !grepl("^year$", name, ignore.case = TRUE)) {
              addStyle(wb, sheet, rows = 3, col = id, 
                       style = createStyle(halign = "right", wrapText = TRUE, textDecoration = "bold", border = c("top", "bottom"))) #right align column headings for numbers
              x <- replace_na(x, 0)
              if(any(abs(x) > 1000)) { #add comma formatting.
                addStyle(wb, sheet, rows = (1:nrows) + 3, col = id, style = createStyle(numFmt = "#,##0"))
                addStyle(wb, sheet, rows = nrows + 3, col = id, style = createStyle(border = "bottom", numFmt = "#,##0"))
              } else if(all(x == floor(x))) {
                addStyle(wb, sheet, rows = (1:nrows) + 3, col = id, style = createStyle(numFmt = "#,##0"))
                addStyle(wb, sheet, rows = nrows + 3, col = id, style = createStyle(border = "bottom", numFmt = "#,##0"))
              } else if(all((x * 10) == floor(x * 10))) {#add number formatting with 1 or 2 d.p. if data contains unrounded values
                addStyle(wb, sheet, rows = (1:nrows) + 3, col = id, style = createStyle(numFmt = "0.0"))
                addStyle(wb, sheet, rows = nrows + 3, col = id, style = createStyle(border = "bottom", numFmt = "0.0"))
              } else {
                addStyle(wb, sheet, rows = (1:nrows) + 3, col = id, style = createStyle(numFmt = "0.00"))
                addStyle(wb, sheet, rows = nrows + 3, col = id, style = createStyle(border = "bottom", numFmt = "0.00"))
              }
            }
            if(!is.null(numFmt)) {
              addStyle(wb, sheet, rows = (1:nrows) + 3, col = id, style = createStyle(numFmt = numFmt))
              addStyle(wb, sheet, rows = nrows + 3, col = id, style = createStyle(border = "bottom", numFmt = numFmt))
            }
          }
    )
    
    writeData(wb, sheet_name, x = data_table, startRow = 3, startCol = 1) #table
  })
  
  return(wb)
}

# data_tables <- gi_data %>% group_by(variable)  #%>% filter(max(ref_period) != min(ref_period) | max("", breakdown, na.rm = TRUE) != min("", breakdown, na.rm = TRUE))
# index_data_tables <- data_tables %>% group_keys()
# data_tables <- data_tables %>% group_split()
# index_data_tables <- index_data_tables %>%
#   left_join(gi_meta %>% select(variable, description, source, link)) %>%
#   left_join(gi_2020 %>% select(variable, domain))
#   

wb_data <- list(data.frame(),
                scores_2020 %>%
                  filter(domain %in% c("Total", "Work", "Money", "Time", "Knowledge", "Health", "Power"), ! score %in% NA) %>%
                  mutate(Score = NA,
                         men = ifelse(men < 1, men * 100, men),
                         women = ifelse(women < 1, women * 100, women),
                         overall = ifelse(overall < 1, overall * 100, overall)
                  ) %>%
                  select(Domain = domain, `Sub-Domain` = subdomain, Indicator = indicator, `Ref. Period` = ref_period, 
                         Type = reversed, Women = women, Men = men, Overall = overall, `Illustrative Score` = Score, `Gender Equality Score` = score) %>%
                  mutate(Type = case_when(Indicator %in% "Total" ~ "Total",
                                          #Overall %in% NA ~ "Absolute",
                                          Type %in% "complement" ~ "Reversed",
                                          TRUE ~ "Standard")) %>%
                  arrange(Domain != "Total", Domain, `Sub-Domain` != "Total", `Sub-Domain`, Indicator == "Total", Indicator)
                
     )
wb <- sg_tables(wb_data, c("Notes" = "Notes", "Gender Index" = "Gender Index"),
                number_fmt = list("Gender Index" = list(`Illustrative Score` = "0.00", `Gender Equality Score` = "0.00"))
                )

add_gi_formulae <- function(wb, sheet_name, ds) {
  for(r in seq_len(nrow(ds))) {
    row_type <- case_when(ds$Domain[r] == "Total" ~ "Total",
                          ds$`Sub-Domain`[r] == "Total" ~ "Domain",
                          ds$Indicator[r] == "Total" ~ "Sub-Domain",
                          TRUE ~ "Indicator")
    if(row_type == "Total") {
      total_rows <- which(ds$`Sub-Domain` == "Total" & ds$`Domain` != "Total")
      total_weights <- case_when(ds$`Domain`[total_rows] == "Work" ~ 0.21,
                                 ds$`Domain`[total_rows] == "Money" ~ 0.18,
                                 ds$`Domain`[total_rows] == "Knowledge" ~ 0.08,
                                 ds$`Domain`[total_rows] == "Time" ~ 0.27,
                                 ds$`Domain`[total_rows] == "Power" ~ 0.19,
                                 ds$`Domain`[total_rows] == "Health" ~ 0.07)
      
      writeFormula(wb, sheet_name, startCol = 9, startRow = r + 3, 
                   x = paste0("PRODUCT(",paste0("I", total_rows + 3, "^", total_weights, collapse = ","),")"))
      addStyle(wb, sheet_name, rows = r + 3, cols = grep("Score", names(ds), invert = TRUE), style = createStyle(textDecoration = "bold", border = c("bottom"))) #footer border
      addStyle(wb, sheet_name, rows = r + 3, cols = grep("Score", names(ds), invert = FALSE), style = createStyle(textDecoration = "bold", border = c("bottom"), numFmt = "0.00")) #footer border
    } else if(row_type == "Domain") {
      total_rows <- which(ds$`Indicator` == "Total" & ds$`Sub-Domain` != "Total" & ds$`Domain` == ds$Domain[r])
      writeFormula(wb, sheet_name, startCol = 9, startRow = r + 3, 
                   x = paste0("GEOMEAN(", paste0("I", total_rows + 3, collapse = ","), ")"))
      addStyle(wb, sheet_name, rows = r + 3, cols = grep("Score", names(ds), invert = TRUE), style = createStyle(textDecoration = "bold", border = c("bottom"))) #footer border
      addStyle(wb, sheet_name, rows = r + 3, cols = grep("Score", names(ds), invert = FALSE), style = createStyle(textDecoration = "bold", border = c("bottom"), numFmt = "0.00")) #footer border
    } else if(row_type == "Sub-Domain") {
      total_rows <- which(ds$`Indicator` != "Total" & ds$`Sub-Domain` == ds$`Sub-Domain`[r])
      writeFormula(wb, sheet_name, startCol = 9, startRow = r + 3, 
                   x = paste0("AVERAGE(", paste0("I", total_rows + 3, collapse = ","), ")"))
      addStyle(wb, sheet_name, rows = r + 3, cols = grep("Score", names(ds), invert = TRUE), style = createStyle(textDecoration = "bold", border = c("bottom"))) #footer border
      addStyle(wb, sheet_name, rows = r + 3, cols = grep("Score", names(ds), invert = FALSE), style = createStyle(textDecoration = "bold", border = c("bottom"), numFmt = "0.00")) #footer border
    } else if(ds$Type[r] == "Standard") {
      writeFormula(wb, sheet_name, startCol = 9, startRow = r + 3, 
                   x = paste0("1+99*(1-ABS((G", r+3, "-F", r+3, ")/(G", r+3, "+F", r+3, ")))"))
      #                   x = paste0("1+99*(1-ABS(1-F", r+3, "/H", r+3, "))"))
    } else if(ds$Type[r] == "Reversed") {
      writeFormula(wb, sheet_name, startCol = 9, startRow = r + 3, 
                   x = paste0("1+99*(1-ABS(((100-G", r+3, ")-(100-F", r+3, "))/((100-G", r+3, ")+(100-F", r+3, "))))"))
      #x = paste0("1+99*(1-ABS(1-(100-F", r+3, ")/(100-H", r+3, ")))"))
    } else if(ds$Type[r] == "Absolute") {
      writeFormula(wb, sheet_name, startCol = 9, startRow = r + 3, 
                   x = paste0("1+99*(1-ABS((G", r+3, "-F", r+3, ")/(G", r+3, "+F", r+3, ")))"))
    }
  }

  ds %>%
    mutate(r = row_number()) %>%
    group_by(Domain, `Sub-Domain`) %>%
    group_walk(~ {
      mergeCells(wb, sheet_name, cols = which(names(ds) == "Domain"), rows = range(.x$r) + 3)
      mergeCells(wb, sheet_name, cols = which(names(ds) == "Sub-Domain"), rows = range(.x$r) + 3)
    })
  addStyle(wb, sheet_name, rows = seq_len(nrow(ds)) + 3, cols = which(names(ds) %in% c("Domain")), style = createStyle(valign = "top", textDecoration = "bold", border = c("bottom")))
  addStyle(wb, sheet_name, rows = seq_len(nrow(ds)) + 3, cols = which(names(ds) %in% c("Sub-Domain")), style = createStyle(valign = "top", border = c("bottom"), textDecoration = "bold"))
  return(wb)
}
wb <- add_gi_formulae(wb, "Gender Index", wb_data[[2]])

saveWorkbook(wb, "Gender-Index-2020-Tables.xlsx", overwrite = TRUE)
