#' Scatter plot
#'
#' Plots two indicators against each other. Each indicator is taken from the
#' entries in `dsets`, `iCodes` and `Levels`. For any of these with only one entry
#' it is repeated for both.
#'
#' @param coin A coin
#' @param dsets Character vector with names of one or two data sets
#' @param iCodes Character vector with codes of one or two indicators
#' @param Levels Numeric vector with one or two levels
#' @param axes_label Either `"iName"` or `"iCode"`
#'
#' @return Plotly object
#' @export
iplot_scatter <- function(coin, dsets, iCodes, Levels, axes_label = "iName"){

  stopifnot(COINr::is.coin(coin),
            length(dsets) %in% 1:2,
            length(iCodes) %in% 1:2)

  # If only one dset specified, use this for both indicators
  if(length(dsets)==1){dsets <- rep(dsets, 2)}

  # if only one ind specified, use for both
  if(length(iCodes)==1){iCodes <- rep(iCodes, 2)}

  # if only one aglev specified, use for both
  if(length(Levels)==1){Levels <- rep(Levels, 2)}

  # get data
  iData1 <- COINr::get_data(coin, dset = dsets[1], iCodes = iCodes[1], Level = Levels[1], also_get = "uName")
  iData2 <- COINr::get_data(coin, dset = dsets[2], iCodes = iCodes[2], Level = Levels[2], also_get = "uName")

  # merge data
  iData <- merge(iData1, iData2, all = FALSE)

  xlab <- iCodes[1]
  ylab <- iCodes[2]

  if(axes_label == "iName"){
    xlab <- COINr::icodes_to_inames(coin, xlab)
    ylab <- COINr::icodes_to_inames(coin, ylab)
  }

  fig <- plotly::plot_ly(data = iData, type = 'scatter', mode = 'markers') |>
    plotly::add_trace(
      x = ~get(iCodes[1]),
      y = ~get(iCodes[2]),
      text = ~uName,
      hoverinfo = 'text',
      marker = list(size = 15),
      showlegend = F
    ) |>
    plotly::layout(xaxis = list(title = xlab),
                   yaxis = list(title = ylab))

  fig

}
