#' Violin plots or histograms
#'
#' @param coin The coin
#' @param dset Name of data set
#' @param iCode Indicator code
#' @param Level Index level to target - see [COINr::get_data()]
#' @param ptype Either `"Violin"` or `"Histogram"`.
#' @param colour Optional to specify colour of plot
#'
#' @return Plotly plot
#' @export
#'
#' @importFrom rlang abort
#'
iplot_dist <- function(coin, dset = "Raw", iCode = NULL, Level = NULL, ptype = "Violin", colour = NULL){

  iData <- COINr::get_data(coin, dset = dset, iCodes = iCode, Level = Level, also_get = "none")
  iName <- COINr::icodes_to_inames(coin, names(iData))
  iCode <- names(iData)

  if(is.null(colour)){
    colour <- "#8dd3c7"
  }

  if(length(iName) > 1){
    abort("This function only supports plotting single indicators. Ensure you are not selecting multiple indicators with 'Level' argument.")
  }

  if (ptype == "Violin"){

    fig <- plotly::plot_ly(data = iData, y = ~get(iCode), type = 'violin',
                           box = list(visible = T),
                           meanline = list(visible = T),
                           x0 = iName,
                           points = 'all',
                           pointpos = -1.5,
                           jitter = 0.1,
                           hoveron = "points+kde",
                           color = I(colour),
                           marker = list(
                             line = list(
                               width = 2,
                               color = colour
                             ),
                             symbol = 'line-ew'
                           )
    )

    # scale axes (for matching with another plot, for example).
    # Not scaled if axlims not specified (range = NULL does nothing)
    fig <- plotly::layout(fig,
                          yaxis = list(title = "", zeroline = F),
                          xaxis = list(title = NULL))

  } else if (ptype == "Histogram"){

    fig <- plotly::plot_ly(data = iData, x = ~get(iCode), type = "histogram")

    # scale axes (for matching with another plot, for example).
    # Not scaled if axlims not specified (range = NULL does nothing)
    fig <- plotly::layout(fig, bargap=0.1,
                          xaxis = list(title = iName),
                          yaxis = list(title = "Count"))

  }

  fig

}
