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
