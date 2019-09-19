library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()

df <- read.csv('indicators.csv', header = TRUE, sep = ",")
available_indicators <- unique(df$Indicator_Name)
option_indicator <- lapply(available_indicators, function(x) list(label = x, value = x))

# First: a simple layout, no callbacks, no figures -- only controls, so we can see how
# to get started.
#
# We'll build the dropdown to select the x-axis, and the radio button to select the
# scale (linear or logarithmic).
app$layout(
  htmlDiv(list(
      dccDropdown(
        id = 'crossfilter-xaxis-column',
        options = option_indicator,
        value = 'Fertility rate, total (births per woman)'
        ),
      dccRadioItems(
        id = 'crossfilter-xaxis-type',
        options = list(list(label = 'Linear', value = 'linear'),
                       list(label = 'Log', value = 'log')),
        value = 'linear',
        labelStyle = list(display = 'inline-block')
        )
      ), 
      style = list(width = '49%', display = 'inline-block'))
)


app$run_server(host = "127.0.0.1", 
               port = 8050)

# Pretty easy! Now we'll add in the y-axis controls ...
#
# We can set some basic CSS styling by using the style property;
# watch as we add a line below the two dropdowns and radio
# button pairs:
app$layout(
  htmlDiv(list(
    htmlDiv(list(
      htmlDiv(list(
        dccDropdown(
          id = 'crossfilter-xaxis-column',
          options = option_indicator,
          value = 'Fertility rate, total (births per woman)'
        ),
        dccRadioItems(
          id = 'crossfilter-xaxis-type',
          options = list(list(label = 'Linear', value = 'linear'),
                         list(label = 'Log', value = 'log')),
          value = 'linear',
          labelStyle = list(display = 'inline-block')
        )
      ), style = list(width = '49%', display = 'inline-block')),
      
      htmlDiv(list(
        dccDropdown(
          id = 'crossfilter-yaxis-column',
          options = option_indicator,
          value = 'Life expectancy at birth, total (years)'
        ),
        dccRadioItems(
          id = 'crossfilter-yaxis-type',
          options = list(list(label = 'Linear', value = 'linear'),
                         list(label = 'Log', value = 'log')),
          value = 'linear',
          labelStyle = list(display = 'inline-block')
        )
      ), style = list(width = '49%', flaot = 'display', display = 'inline-block'))
    ), style = list(
      borderBottom = 'thin lightgrey solid',
      backgroundColor = 'rgb(250, 250, 250)',
      padding = '10px 5px')
    )
  )
  )
)

app$run_server(host = "127.0.0.1", 
               port = 8050)
    

# Now we'll add in our first of three figures using the Graph component,
# which allows us to easily embed interactive figures which are drawn
# natively using Plotly.js!
app$layout(
  htmlDiv(list(
    htmlDiv(list(
      htmlDiv(list(
        dccDropdown(
          id = 'crossfilter-xaxis-column',
          options = option_indicator,
          value = 'Fertility rate, total (births per woman)'
        ),
        dccRadioItems(
          id = 'crossfilter-xaxis-type',
          options = list(list(label = 'Linear', value = 'linear'),
                         list(label = 'Log', value = 'log')),
          value = 'linear',
          labelStyle = list(display = 'inline-block')
        )
      ), style = list(width = '49%', display = 'inline-block')),
      
      htmlDiv(list(
        dccDropdown(
          id = 'crossfilter-yaxis-column',
          options = option_indicator,
          value = 'Life expectancy at birth, total (years)'
        ),
        dccRadioItems(
          id = 'crossfilter-yaxis-type',
          options = list(list(label = 'Linear', value = 'linear'),
                         list(label = 'Log', value = 'log')),
          value = 'linear',
          labelStyle = list(display = 'inline-block')
        )
      ), style = list(width = '49%', flaot = 'display', display = 'inline-block'))
    ), style = list(
      borderBottom = 'thin lightgrey solid',
      backgroundColor = 'rgb(250, 250, 250)',
      padding = '10px 5px')
    ),
    
    htmlDiv(list(
      dccGraph(
        id = 'crossfilter-indicator-scatter',
        hoverData = list(points = list(list(customdata = 'Japan')))
      )), style = list(
        width ='49%',
        display = 'inline-block',
        padding = '0 20')
    ),
    
    htmlDiv(list(
      dccSlider(
        id = 'crossfilter-year--slider',
        min = 0,
        max = length(unique(df$Year))-1,
        marks = unique(df$Year),
        value = length(unique(df$Year))-1 
      )
    ), style = list(width = '49%', padding = '0px 20px 20px 20px'))
  ))
)

app$run_server(host = "127.0.0.1", 
               port = 8050)

# OK, now that we've got our first graph in place, we need a callback
# to supply it with some data.
#
# Don't worry too much about the subsetting R code within the callback;
# the important thing is to notice the structure of the callback handler
# and how it accepts inputs and returns outputs.
app$callback(
  output = list(id='crossfilter-indicator-scatter', property='figure'),
  params = list(input(id='crossfilter-xaxis-column', property='value'),
                input(id='crossfilter-yaxis-column', property='value'),
                input(id='crossfilter-xaxis-type', property='value'),
                input(id='crossfilter-yaxis-type', property='value'),
                input(id='crossfilter-year--slider', property='value')),
  function(xaxis_column_name, yaxis_column_name, xaxis_type, yaxis_type, year_value) {
    selected_year <- unique(df$Year)[year_value]
    dff <- selected_year
    traces <- list()
    if (selected_year %in% unique(df$Year)){
      filtered_df <- df[df[["Year"]] %in% selected_year, ]
      traces[[1]] <- list(
        x = filtered_df[filtered_df$Indicator_Name %in% xaxis_column_name, "Value"],
        y = filtered_df[filtered_df$Indicator_Name %in% yaxis_column_name, "Value"],
        opacity=0.7,
        text = filtered_df[filtered_df$Indicator_Name %in% yaxis_column_name, "Country_Name"],
        customdata = filtered_df[filtered_df$Indicator_Name %in% yaxis_column_name, "Country_Name"],
        mode = 'markers',
        marker = list(
          'size'= 15,
          'opacity' = 0.5,
          'line' = list('width' = 0.5, 'color' = 'white')
        )
      )
      
      return (list(
        data = traces,
        layout = list(
          xaxis = list('title' = xaxis_column_name, 'type' = xaxis_type),
          yaxis = list('title' = yaxis_column_name, 'type' = yaxis_type),
          margin = list('l' = 40, 'b' = 30, 't' = 10, 'r' = 0),
          height = 450,
          hovermode = 'closest'
        )
      ))
    }
  }
)

app$run_server(host = "127.0.0.1", 
               port = 8050)

# Now we're getting somewhere! Since this figure is drawn using Plotly.js, we
# get a few nice things right out of the box:
# 
# - the Plotly "modebar", which gives us some handy subsetting/selection tools
# - the ability to reference selected data points for use in other callbacks ("brushing")
# - the ability to present metadata or contextual information for individual points
#   using hoverdata
# - the ability to save the plot as a PNG directly within the figure itself
#
# Alright, let's add some additional graphs and interface elements for them!
#
# There's a bit of a gap above the left figure because we haven't finished sizing
# the right-side figures yet, but we'll do that in a moment.
app$layout(
  htmlDiv(list(
    htmlDiv(list(
      htmlDiv(list(
        dccDropdown(
          id = 'crossfilter-xaxis-column',
          options = option_indicator,
          value = 'Fertility rate, total (births per woman)'
        ),
        dccRadioItems(
          id = 'crossfilter-xaxis-type',
          options = list(list(label = 'Linear', value = 'linear'),
                         list(label = 'Log', value = 'log')),
          value = 'linear',
          labelStyle = list(display = 'inline-block')
        )
      ), style = list(width = '49%', display = 'inline-block')),
      
      htmlDiv(list(
        dccDropdown(
          id = 'crossfilter-yaxis-column',
          options = option_indicator,
          value = 'Life expectancy at birth, total (years)'
        ),
        dccRadioItems(
          id = 'crossfilter-yaxis-type',
          options = list(list(label = 'Linear', value = 'linear'),
                         list(label = 'Log', value = 'log')),
          value = 'linear',
          labelStyle = list(display = 'inline-block')
        )
      ), style = list(width = '49%', flaot = 'display', display = 'inline-block'))
    ), style = list(
      borderBottom = 'thin lightgrey solid',
      backgroundColor = 'rgb(250, 250, 250)',
      padding = '10px 5px')
    ),
    
    htmlDiv(list(
      dccGraph(
        id = 'crossfilter-indicator-scatter',
        hoverData = list(points = list(list(customdata = 'Japan')))
      )), style = list(
        width ='49%',
        display = 'inline-block',
        padding = '0 20')
    ),
    
    htmlDiv(list(
      dccGraph(id='x-time-series'),
      dccGraph(id='y-time-series')
    ), style = list(display = 'inline-block', width = '49%')),
    
    htmlDiv(list(
      dccSlider(
        id = 'crossfilter-year--slider',
        min = 0,
        max = length(unique(df$Year))-1,
        marks = unique(df$Year),
        value = length(unique(df$Year))-1 
      )
    ), style = list(width = '49%', padding = '0px 20px 20px 20px'))
  ))
)

app$run_server(host = "127.0.0.1", 
               port = 8050)


# We're going to add a subsetting function for the right-side plots,
# which will display time series data.
create_time_series <- function(dff, axis_type, title){
  return(list(
    'data' = list(list(
      x = dff[['Year']],
      y = dff[['Value']],
      mode = 'lines+markers'
    )),
    'layout' = list(
      height = 225,
      margin = list('l' = 20, 'b' = 30, 'r' = 10, 't' = 10),
      'annotations' = list(list(
        x = 0, 'y' = 0.85, xanchor = 'left', yanchor = 'bottom',
        xref = 'paper', yref = 'paper', showarrow = FALSE,
        align = 'left', bgcolor = 'rgba(255, 255, 255, 0.5)',
        text = title[1]
      )),
      yaxis = list(type = axis_type),
      xaxis = list(showgrid = FALSE)
    )
  ))
}

# And now, a callback for the x-time-series figure.
#
# Here's where things start to get interesting!
# Let's tie these figures together, so that the graphical
# displays we're creating become interface elements themselves.
#
# We'll add in some hoverdata, which is sent from the figure
# at left on mouseover.
#
# We'll also set the xaxis column name and type based on the
# dropdown and radio button inputs above, which we declared earlier.
app$callback(
  output = list(id='x-time-series', property='figure'),
  params = list(input(id='crossfilter-indicator-scatter', property='hoverData'),
                input(id='crossfilter-xaxis-column', property='value'),
                input(id='crossfilter-xaxis-type', property='value')),
  function(hoverData, xaxis_column_name, axis_type) {
    country_name = hoverData$points[[1]]$customdata
    dff <- df[df[["Country_Name"]] %in% country_name, ]
    dff <- dff[dff[["Indicator_Name"]] %in% xaxis_column_name, ]
    title = paste(c(country_name, xaxis_column_name), sep = '<br>')
    return(create_time_series(dff, axis_type, title))
  }
)

app$run_server(host = "127.0.0.1", 
               port = 8050)

# Great! Now we can investigate trends (or the absence thereof) in
# the x-axis data by mousing over the points in the scatter plot on
# the left side.
#
# We want to provide the same capability to examine trends in the
# y-axis data too, so let's add one more callback that will update
# the figure at the lower right.
app$callback(
  output = list(id='y-time-series', property='figure'),
  params = list(input(id='crossfilter-indicator-scatter', property='hoverData'),
                input(id='crossfilter-yaxis-column', property='value'),
                input(id='crossfilter-yaxis-type', property='value')),
  function(hoverData, yaxis_column_name, axis_type) {
    dff <- df[df[["Country_Name"]] %in% hoverData$points[[1]]$customdata, ]
    dff <- dff[dff[["Indicator_Name"]] %in% yaxis_column_name, ]
    return(create_time_series(dff, axis_type, yaxis_column_name))
  }
)

# Done! Let's see the finished product.
app$run_server(host = "127.0.0.1", 
               port = 8050)
