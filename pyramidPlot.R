library(devtools)
install_github('ramnathv/rCharts@dev')
library(rCharts)

## Plot pyramid plots usinf NDV3 javascript library
pyramidPlot <- function(data.df,colors=NULL){
  library(rCharts)
  n1 <- nPlot(
    y = 'Count',
    x = 'reaction',
    group = 'drug',
    type = 'multiBarHorizontalChart',
    data = data.df
    )
  n1$chart(stacked = TRUE)
  n1$chart(tooltipContent = "#! function(key, x, y){
   var format = d3.format('0,000');
   return '<h3>' + key + ', Adverse reaction : ' + x + '</h3>' +
   '<p>' + 'Counts : ' + y + '</p>'
   } !#")
  if (max(new.df$count >= 20)) {
    n1$yAxis(axisLabel = "Count",
             tickFormat = "#! function(d) {
            return d3.format(',.1f')(Math.abs(d) / 1)
             } !#")
    } else {
      n1$yAxis(axisLabel = "Count",
               tickFormat = "#! function(d) {
               return d3.format(',.0f')(Math.abs(d) / 1)
               } !#")
    }
   if (!is.null(colors)) {
     n1$chart(color = colors)
     }
   n1$chart(stacked = TRUE)
   n1$set(height=800,width=650)
   n1$chart(margin = list(left=200))
   n1
}
