echarts4r::e_common(font_family = "Poppins")

echart_column_chart <- function(df, x, y, tog, title, dec, esttype, color) {
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)

  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # Create the most basic chart
  c <- df %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::group_by(.data[[tog]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE) %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  if (color == "blues") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#BFE9E7', '#73CFCB', '#40BDB8', '#00A7A0', '#00716c', '#005753'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "greens") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#E2F1CF', '#C0E095', '#A9D46E', '#8CC63E', '#588527', '#3f6618'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "oranges") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#FBD6C9', '#F7A489', '#F4835E', '#F05A28', '#9f3913', '#7a2700'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "purples") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#E3C9E3', '#C388C2', '#AD5CAB', '#91268F', '#630460', '#4a0048'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "jewel") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#91268F', '#F05A28', '#8CC63E', '#00A7A0', '#4C4C4C', '#630460', '#9f3913', '#588527', '#00716c', '#3e4040'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  c <- c %>% 
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_legend(show = FALSE)
  
  # Add in the Timeseries Selector
  c <- c %>%
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = 200,
                               left = 200,
                               #currentIndex = 2,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))

  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec)) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD")) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return('<strong>' + params.seriesName '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  return(c)
  
}

echart_bar_chart <- function(df, x, y, tog, title, dec, esttype, color) {
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # Create the most basic chart
  c <- df %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::group_by(.data[[tog]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE) %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  if (color == "blues") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#BFE9E7', '#73CFCB', '#40BDB8', '#00A7A0', '#00716c', '#005753'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "greens") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#E2F1CF', '#C0E095', '#A9D46E', '#8CC63E', '#588527', '#3f6618'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "oranges") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#FBD6C9', '#F7A489', '#F4835E', '#F05A28', '#9f3913', '#7a2700'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "purples") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#E3C9E3', '#C388C2', '#AD5CAB', '#91268F', '#630460', '#4a0048'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "jewel") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#91268F', '#F05A28', '#8CC63E', '#00A7A0', '#4C4C4C', '#630460', '#9f3913', '#588527', '#00716c', '#3e4040'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  c <- c %>% 
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_legend(show = FALSE)
  
  # Add in the Timeseries Selector
  c <- c %>%
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = 200,
                               left = 200,
                               #currentIndex = 2,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec)) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD")) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.marker + ' ' +\n
      params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  c <- c %>%
    e_flip_coords()
  
  return(c)
  
}

create_rgc_summary_table <- function(center_name, yr) {
  
  # Area
  r1 <- rgc_shape %>%
    st_drop_geometry() %>%
    filter(name == center_name) %>% 
    select("name", "acres") %>% 
    mutate(grouping="Land Area (acres)") %>% 
    mutate(estimate=as.character(round(acres,0))) %>%
    mutate(pic = as.character(icon("layer-group", lib = "font-awesome"))) %>%
    select(-"acres")
  
  # Designation Year
  r2 <- centers_info %>% 
    filter(name == center_name & rgc_mic == "Regional Growth Center") %>% 
    select("name", "designation_year") %>% 
    mutate(grouping="Designation Year") %>% 
    mutate(estimate=as.character(designation_year)) %>%
    mutate(pic = as.character(icon("calendar-check", lib = "font-awesome"))) %>%
    select(-"designation_year")
  
  # Center Type
  r3 <- centers_info %>% 
    filter(name == center_name & rgc_mic == "Regional Growth Center") %>% 
    select("name", "center_type") %>% 
    mutate(grouping="Center Type") %>% 
    mutate(estimate=as.character(center_type)) %>%
    mutate(pic = as.character(icon("city", lib = "font-awesome"))) %>%
    select(-"center_type")
  
  # Population
  r4 <- pop_hh_hu_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Population") %>% 
    select(name="geography", "grouping", "estimate") %>%
    mutate(estimate = format(round(estimate, -1), big.mark = ",")) %>%
    mutate(pic = as.character(icon("users", lib = "font-awesome")))
  
  # Housing Units
  r5 <- pop_hh_hu_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Housing Units") %>% 
    select(name="geography", "grouping", "estimate") %>%
    mutate(estimate = format(round(estimate, -1), big.mark = ",")) %>%
    mutate(pic = as.character(icon("building", lib = "font-awesome")))
  
  # Jobs
  j <- employment_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Total") %>% 
    mutate(grouping="Total Employment") %>% 
    select(name="geography", "grouping", "estimate") %>% 
    select("estimate") %>% 
    pull()
  
  if (j == "*") {
    
    r6 <- r3 %>% mutate(estimate = "*", grouping = "Total Employment", pic = as.character(icon("briefcase", lib = "font-awesome")))
    
  } else {
    
    r6 <- employment_data %>% 
      filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Total") %>% 
      mutate(grouping="Total Employment") %>% 
      select(name="geography", "grouping", "estimate") %>%
      mutate(estimate = format(round(as.integer(estimate), -1), big.mark = ",")) %>%
      mutate(pic = as.character(icon("briefcase", lib = "font-awesome")))
    
  }
  
  # Activity Units per Acre
  acres <- r1 %>% select("estimate") %>% pull() %>% as.integer()
  
  pop <- pop_hh_hu_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title) %>% 
    select(name="geography", "grouping", "estimate") %>%
    filter(grouping %in% c("Population")) %>% 
    select("estimate") %>% 
    pull() %>% 
    as.integer()
  
  j <- employment_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Total") %>% 
    mutate(grouping="Total Employment") %>% 
    select(name="geography", "grouping", "estimate") %>% 
    select("estimate") %>% 
    pull()
  
  if (j == "*") {
    
    jobs <-"*"
    
  } else {
    
    jobs <- as.integer(j)
    
  }
  
  if (j == "*") {
    
    au <- "*"
    
  } else {
    
    au <- round((pop+jobs)/acres,0)
    
  }
  
  r7 <- r6 %>% mutate(estimate = as.character(au), grouping = "Activity Units per Acre", pic = as.character(icon("people-group", lib = "font-awesome")))
  
  # Jobs / Pop Balance
  
  if (j == "*") {
    
    jobs <-"*"
    
  } else {
    
    jobs <- as.integer(j)
    
  }
  
  if (jobs == "*") {
    
    jpr <- "*"
    
  } else {
    
    jpr <- round((jobs)/pop,1)
    
  }
  
  r8 <- r7 %>% mutate(estimate = as.character(jpr), grouping = "Jobs per Person", pic = as.character(icon("person-shelter", lib = "font-awesome")))
  
  t <- bind_rows(r1, r2, r3, r4, r5, r6, r7, r8) %>% select("pic", "grouping", "estimate")
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 15,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(2), className = 'dt-right'),
                                                            list(targets = c(0), className = 'dt-center'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-bottom` = styleEqual(c("Center Type", "Total Employment", "Jobs per Person"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-top` = styleEqual(c("Land Area (acres)"), "solid 2px"))
  
  return(summary_tbl)
  
}

echart_multi_bar_chart <- function(df, x, y, fill, tog, dec, esttype, color) {
 
  c <- echart_multi_column_chart(df=df, x=x, y=y, fill=fill, tog=tog, dec=dec, esttype=esttype, color=color) 
  
  c <- c %>%
    e_flip_coords()
  
  return(c)
  
}


echart_multi_column_chart <- function(df, x, y, fill, tog, dec, esttype, color) {
  
  if (color == "blues") {chart_color <- psrcplot::psrc_colors$blues_inc}
  if (color == "greens") {chart_color <- psrcplot::psrc_colors$greens_inc}
  if (color == "oranges") {chart_color <- psrcplot::psrc_colors$oranges_inc}
  if (color == "purples") {chart_color <- psrcplot::psrc_colors$purples_inc}
  if (color == "jewel") {chart_color <- psrcplot::psrc_colors$pognbgy_5}
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Determine the number of Series to Plot
  bar_fill_values <- df %>% 
    select(all_of(fill)) %>% 
    dplyr::distinct() %>% 
    dplyr::pull() %>% 
    unique
  
  chart_fill <- as.character(bar_fill_values)
  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # Create the most basic chart
  chart_df <- df %>%
    dplyr::filter(.data[[fill]] %in% chart_fill) %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::select(tidyselect::all_of(fill), tidyselect::all_of(x), tidyselect::all_of(y), tidyselect::all_of(tog)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(fill), values_from = tidyselect::all_of(y))
  
  c <- chart_df %>%
    dplyr::group_by(.data[[tog]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE) %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  for(fill_items in chart_fill) {
    c <- c %>%
      echarts4r::e_bar_(fill_items, smooth = FALSE)
  }
  
  c <- c %>% 
    echarts4r::e_color(chart_color) %>%
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_legend(show = TRUE, bottom=0)
  
  # Add in the Timeseries Selector
  c <- c %>%
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = 200,
                               left = 200,
                               #currentIndex = 2,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec)) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD")) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return('<strong>' + params.seriesName '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  return(c)
  
}


echart_line_chart <- function(df, x, y, fill, tog, dec, esttype, color) {
  
  if (color == "blues") {chart_color <- psrcplot::psrc_colors$blues_inc}
  if (color == "greens") {chart_color <- psrcplot::psrc_colors$greens_inc}
  if (color == "oranges") {chart_color <- psrcplot::psrc_colors$oranges_inc}
  if (color == "purples") {chart_color <- psrcplot::psrc_colors$purples_inc}
  if (color == "jewel") {chart_color <- psrcplot::psrc_colors$pognbgy_5}
  
  # Determine the number of Series to Plot
  bar_fill_values <- df %>% 
    select(all_of(fill)) %>% 
    dplyr::distinct() %>% 
    dplyr::pull() %>% 
    unique
  
  chart_fill <- as.character(bar_fill_values)
  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Create the most basic chart
  chart_df <- df %>%
    dplyr::filter(.data[[fill]] %in% chart_fill) %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::select(tidyselect::all_of(fill), tidyselect::all_of(x), tidyselect::all_of(y), tidyselect::all_of(tog)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(fill), values_from = tidyselect::all_of(y))
  
  c <- chart_df %>%
    dplyr::group_by(.data[[tog]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE) %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  for(fill_items in chart_fill) {
    c <- c %>%
      echarts4r::e_line_(fill_items, smooth = FALSE)
  }
  
  c <- c %>% 
    echarts4r::e_color(chart_color) %>%
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_legend(show = TRUE, bottom=0)
  
  # Add in the Timeseries Selector
  c <- c %>%
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = 200,
                               left = 200,
                               #currentIndex = 2,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec)) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD")) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return('<strong>' + params.seriesName '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  return(c)
  
}

create_multi_year_table <- function(df, rgc_name, data_yrs, dec=0) {
  
  num_years <- length(data_yrs)
  
  # Define the Container for the Summary Data by Years
  if (num_years == 1) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, data_yrs[[1]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_years), th)
        )
      )
    ))
    
  }
  
  if (num_years == 2) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, data_yrs[[1]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[2]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_years), th)
        )
      )
    ))
    
  }
  
  if (num_years == 3) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, data_yrs[[1]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[2]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[3]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_years), th)
        )
      )
    ))
    
  }

  if (num_years == 4) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, data_yrs[[1]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[2]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[3]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[4]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_years), th)
        )
      )
    ))
    
  }

  if (num_years == 5) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, data_yrs[[1]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[2]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[3]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[4]]),
          th(class = 'dt-center', colspan = 2, data_yrs[[5]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_years), th)
        )
      )
    ))
    
  }  
  
  # Get All possible categories
  cat <- df %>% select("grouping") %>% distinct() %>% pull()
  tbl_full <- NULL
  for (y in data_yrs) {
    
    d <- data.frame(grouping = cat, year = y)
    ifelse(is.null(tbl_full), tbl_full<-d, tbl_full<-bind_rows(tbl_full,d))
    
  }
  
  # Filter Data
  tbl <- df %>% 
    filter(geography_type %in% c(rgc_title) & geography %in% c(rgc_name)) %>%
    select("grouping", "estimate", "share", "year") 
  
  tbl_full <- left_join(tbl_full, tbl, by=c("grouping", "year")) %>%
    mutate(estimate = replace_na(estimate, 0), share = replace_na(share, 0)) %>%
    pivot_longer(cols = !c(grouping, year)) %>%
    pivot_wider(names_from = c(year, name), values_from = "value")
  
  final_tbl <- datatable(tbl_full,
                         container = summary_container,
                         colnames = c('Group', rep(c('Estimate', 'Share'), num_years)),
                         options = list(pageLength = 15,
                                        dom = 'rtB',
                                        buttons = c('csv', 'excel'),
                                        columnDefs = list(list(className = 'dt-center', targets=1:(num_years*2)))
                         ),
                         extensions = 'Buttons',
                         filter = 'none',
                         rownames = FALSE) %>%
    formatPercentage(paste0(data_yrs,"_share"), dec) %>%
    formatCurrency(paste0(data_yrs,"_estimate"), "", digits = 0)
  
  return(final_tbl)
  
}

create_single_group_table <- function(df, rgc_name, data_yrs, group, dec=0) {
  
  df <- df %>% filter(geography_type == rgc_title, geography == rgc_name & grouping == group & year %in% data_yrs)
  
  # Get All possible categories
  tbl_full <- NULL
  for (y in data_yrs) {
    
    d <- data.frame(year = y)
    ifelse(is.null(tbl_full), tbl_full<-d, tbl_full<-bind_rows(tbl_full,d))
    
  }
  
  # Filter Data
  tbl <- df %>% 
    select("year", "estimate") 
  
  tbl_full <- left_join(tbl_full, tbl, by=c("year")) %>%
    mutate(estimate = replace_na(estimate, 0))
  
  final_tbl <- datatable(tbl_full,
                         colnames = c('Year', group),
                         options = list(pageLength = length(data_yrs),
                                        dom = 'rtB',
                                        buttons = c('csv', 'excel'),
                                        columnDefs = list(list(className = 'dt-center', targets=0:1))
                         ),
                         extensions = 'Buttons',
                         filter = 'none',
                         rownames = FALSE) %>%
    formatCurrency("estimate", "", digits = 0)
  
  return(final_tbl)
  
}

create_change_table <- function(df, yr, val, nm) {
  
  # Filter Data
  data_yrs <- df %>% select(all_of(yr)) %>% pull() %>% length
  tbl <- df %>% select(all_of(yr), all_of(val)) 

  final_tbl <- datatable(tbl,
                         colnames = c('Year', nm),
                         options = list(dom = 'rtB',
                                        buttons = c('csv', 'excel'),
                                        columnDefs = list(list(className = 'dt-center', targets=0:1))
                         ),
                         extensions = 'Buttons',
                         filter = 'none',
                         rownames = FALSE) %>%
    formatCurrency(val, "", digits = 0)
  
  return(final_tbl)
  
}

create_rgc_urban_form_table <- function(center_name) {
  
  # Intersections
  r1 <- intersection_density %>% 
    filter(name == center_name) %>% 
    select("name", estimate="intersection_count") %>% 
    mutate(grouping="# of Intersections")
  
  # Intersection Density
  r2 <- intersection_density %>% 
    filter(name == center_name) %>% 
    select("name", estimate="intersection_density") %>% 
    mutate(grouping="Intersection Density (per acre)")
  
  r3 <- intersection_density %>% 
    filter(name == "All Centers") %>% 
    select("name", estimate="intersection_density") %>% 
    mutate(grouping="Intersection Density (per acre) - All Centers")
  
  t <- bind_rows(r1, r2, r3) %>% select("grouping", "estimate")
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 15,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(1), className = 'dt-right'),
                                                            list(targets = c(0), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-bottom` = styleEqual(c("Intersection Density (per acre) - All Centers"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-top` = styleEqual(c("# of Intersections"), "solid 2px"))
  
  return(summary_tbl)
  
}

create_rgc_transit_stop_table <- function(center_name) {
  
  data <- transit_stop_data |> st_drop_geometry()
  
  # All Stops
  r1 <- data |>
    filter(rgc == center_name) |> 
    select(mode="rgc", "stop_id") |>
    mutate(estimate=1) |>
    distinct() |>
    group_by(mode) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(mode  = "All Transit Stops")
  
  # Light Rail Stops
  r2 <- data |>
    filter(rgc == center_name) |> 
    select("rgc", mode="lrt", "stop_id") |>
    drop_na() |>
    mutate(estimate=1) |>
    group_by(mode) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  # Commuter Rail Stops
  r3 <- data |>
    filter(rgc == center_name) |> 
    select("rgc", mode="crt", "stop_id") |>
    drop_na() |>
    mutate(estimate=1) |>
    group_by(mode) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  # Ferry Stops
  r4 <- data |>
    filter(rgc == center_name) |> 
    select("rgc", mode="ferry", "stop_id") |>
    drop_na() |>
    mutate(estimate=1) |>
    group_by(mode) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  # BRT Stops
  r5 <- data |>
    filter(rgc == center_name) |> 
    select("rgc", mode="brt", "stop_id") |>
    drop_na() |>
    mutate(estimate=1) |>
    group_by(mode) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  # Bus Stops
  r6 <- data |>
    filter(rgc == center_name) |> 
    select("rgc", mode="bus", "stop_id") |>
    drop_na() |>
    mutate(estimate=1) |>
    group_by(mode) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  t <- bind_rows(r1, r2, r3, r4, r5, r6) %>% select("mode", "estimate")
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 15,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(1), className = 'dt-right'),
                                                            list(targets = c(0), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "mode",
                `border-bottom` = styleEqual(c("Bus"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "mode",
                `border-top` = styleEqual(c("All Transit Stops"), "solid 2px"))
  
  return(summary_tbl)
  
}

create_rgc_transit_map <- function(center_name) {
  
  transit_pal <- colorFactor(
    palette = c("#BCBEC0", "#8CC63E", "#91268F", "#00A7A0", "#F05A28"),
    levels = c("Bus", "BRT", "Commuter Rail", "Ferry", "Light Rail or Streetcar"))
  
  center_shp <- rgc_shape |> filter(name %in% center_name)
  
  lrt_stops <- transit_stop_data |>
    filter(rgc %in% center_name) |>
    select(Stop="stop_id", Mode="lrt") |>
    drop_na()
  
  brt_stops <- transit_stop_data |>
    filter(rgc %in% center_name) |>
    select(Stop="stop_id", Mode="brt") |>
    drop_na()
  
  crt_stops <- transit_stop_data |>
    filter(rgc %in% center_name) |>
    select(Stop="stop_id", Mode="crt") |>
    drop_na()
  
  ferry_stops <- transit_stop_data |>
    filter(rgc %in% center_name) |>
    select(Stop="stop_id", Mode="ferry") |>
    drop_na()
  
  bus_stops <- transit_stop_data |>
    filter(rgc %in% center_name) |>
    select(Stop="stop_id", Mode="bus") |>
    drop_na()
  
  m <- leaflet() |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Bus",
                                       "BRT", 
                                       "Commuter Rail",
                                       "Ferry",
                                       "Light Rail or Streetcar", 
                                       "Center"),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addPolygons(data = center_shp,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                fillOpacity = 0.0,
                group="Center") |>
    
    addCircles(data=bus_stops, 
               group="Bus",
               color = "#BCBEC0",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=brt_stops, 
               group="BRT",
               color = "#8CC63E",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=crt_stops, 
               group="Commuter Rail",
               color = "#91268F",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=ferry_stops, 
               group="Ferry",
               color = "#00A7A0",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=lrt_stops, 
               group="Light Rail or Streetcar",
               color = "#F05A28",
               opacity = 1.0,
               fillOpacity = 1.0)
  
  return(m)
  
  
}

create_multi_group_table <- function(df, rgc_name, grp, dec=0) {
  
  num_grps <- df |> select(all_of(grp)) |> distinct() |> pull() |> length()
  grps <- df |> select(all_of(grp)) |> distinct() |> pull()
  
  # Define the Container for the Summary Data by Years
  if (num_grps == 1) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, grps[[1]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_grps), th)
        )
      )
    ))
    
  }
  
  if (num_grps == 2) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, grps[[1]]),
          th(class = 'dt-center', colspan = 2, grps[[2]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_grps), th)
        )
      )
    ))
    
  }
  
  if (num_grps == 3) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, grps[[1]]),
          th(class = 'dt-center', colspan = 2, grps[[2]]),
          th(class = 'dt-center', colspan = 2, grps[[3]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_grps), th)
        )
      )
    ))
    
  }
  
  if (num_grps == 4) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, grps[[1]]),
          th(class = 'dt-center', colspan = 2, grps[[2]]),
          th(class = 'dt-center', colspan = 2, grps[[3]]),
          th(class = 'dt-center', colspan = 2, grps[[4]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_grps), th)
        )
      )
    ))
    
  }
  
  if (num_grps == 5) {
    
    summary_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(class = 'dt-center', colspan = 2, grps[[1]]),
          th(class = 'dt-center', colspan = 2, grps[[2]]),
          th(class = 'dt-center', colspan = 2, grps[[3]]),
          th(class = 'dt-center', colspan = 2, grps[[4]]),
          th(class = 'dt-center', colspan = 2, grps[[5]])
        ),
        tr(
          lapply(rep(c('Estimate', 'Share'), num_grps), th)
        )
      )
    ))
    
  }  
  
  # Get All possible categories
  cat <- df %>% select("grouping") %>% distinct() %>% pull()
  tbl_full <- NULL
  for (g in grps) {
    
    d <- data.frame(grouping = cat, concept = g)
    ifelse(is.null(tbl_full), tbl_full<-d, tbl_full<-bind_rows(tbl_full,d))
    
  }
  
  # Filter Data
  tbl <- df %>% 
    filter(geography_type %in% c(rgc_title) & geography %in% c(rgc_name)) %>%
    select("grouping", "estimate", "share", "concept") 
  
  tbl_full <- left_join(tbl_full, tbl, by=c("grouping", "concept")) %>%
    mutate(estimate = replace_na(estimate, 0), share = replace_na(share, 0)) %>%
    pivot_longer(cols = !c(grouping, concept)) %>%
    pivot_wider(names_from = c(concept, name), values_from = "value")
  
  final_tbl <- datatable(tbl_full,
                         container = summary_container,
                         colnames = c('Group', rep(c('Estimate', 'Share'), num_grps)),
                         options = list(pageLength = 15,
                                        dom = 'rtB',
                                        buttons = c('csv', 'excel'),
                                        columnDefs = list(list(className = 'dt-center', targets=1:(num_grps*2)))
                         ),
                         extensions = 'Buttons',
                         filter = 'none',
                         rownames = FALSE) %>%
    formatPercentage(paste0(grps,"_share"), dec) %>%
    formatCurrency(paste0(grps,"_estimate"), "", digits = 0)
  
  return(final_tbl)
  
}

