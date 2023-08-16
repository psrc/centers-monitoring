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
    select(-"acres")
  
  # Designation Year
  r2 <- centers_info %>% 
    filter(name == center_name & rgc_mic == "Regional Growth Center") %>% 
    select("name", "designation_year") %>% 
    mutate(grouping="Designation Year") %>% 
    mutate(estimate=as.character(designation_year)) %>%
    select(-"designation_year")
  
  # Center Type
  r3 <- centers_info %>% 
    filter(name == center_name & rgc_mic == "Regional Growth Center") %>% 
    select("name", "center_type") %>% 
    mutate(grouping="Center Type") %>% 
    mutate(estimate=as.character(center_type)) %>%
    select(-"center_type")
  
  # Population, Households and Housing Units
  r4 <- pop_hh_hu_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title) %>% 
    select(name="geography", "grouping", "estimate") %>%
    mutate(estimate = format(round(estimate, -1), big.mark = ","))
  
  # Jobs
  j <- employment_data %>% 
    filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Total") %>% 
    mutate(grouping="Total Employment") %>% 
    select(name="geography", "grouping", "estimate") %>% 
    select("estimate") %>% 
    pull()
  
  if (j == "*") {
    
    r5 <- r3 %>% mutate(estimate = "*", grouping = "Total Employment")
    
  } else {
    
    r5 <- employment_data %>% 
      filter(year == yr & geography == center_name & geography_type == rgc_title & grouping == "Total") %>% 
      mutate(grouping="Total Employment") %>% 
      select(name="geography", "grouping", "estimate") %>%
      mutate(estimate = format(round(as.integer(estimate), -1), big.mark = ","))
    
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
  
  r6 <- r5 %>% mutate(estimate = as.character(au), grouping = "Activity Units per Acre")
  
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
  
  r7 <- r5 %>% mutate(estimate = as.character(jpr), grouping = "Jobs per Person")
  
  t <- bind_rows(r1, r2, r3, r4, r5, r6, r7) %>% select(-"name")
  
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
                                          columnDefs = list(list(targets = c(1), className = 'dt-right'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE               
  )
  
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


