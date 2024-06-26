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
#' @param axes_label Either `"iName"` or `"iCode"`, optionally appended with `"+dset"` to additionally
#' write the data set name on the axis labels.
#' @param log_axes 2-length logical vector specifying log (TRUE) for x and y axes respectively
#' @param trendline Logical: if `TRUE` adds a trendline which is calculated based on the specifications of
#' `log_axes`. E.g. if the x axis is log-transformed, will regress y on log(x).
#' @param show_details Logical: if `TRUE` shows correlation and p-value as overlaid text on the plot.
#' @param marker_colour Optional to specify the colour of the points.
#'
#' @return Plotly object
#' @export
iplot_scatter <- function(coin, dsets, iCodes, Levels, axes_label = "iName", log_axes = c(FALSE, FALSE),
                          trendline = FALSE, show_details = FALSE, marker_colour = NULL){

  stopifnot(COINr::is.coin(coin),
            length(dsets) %in% 1:2,
            length(iCodes) %in% 1:2,
            is.logical(log_axes),
            length(log_axes) == 2)

  # If only one dset specified, use this for both indicators
  if(length(dsets)==1){dsets <- rep(dsets, 2)}

  # if only one ind specified, use for both
  if(length(iCodes)==1){iCodes <- rep(iCodes, 2)}

  # if only one aglev specified, use for both
  if(length(Levels)==1){Levels <- rep(Levels, 2)}

  if(is.null(marker_colour)){
    marker_colour <- "rgba(10,77,104,0.5)"
  }

  # axis types
  xaxis_type <- if(log_axes[1]) "log" else "linear"
  yaxis_type <- if(log_axes[2]) "log" else "linear"

  # how to show axis labels
  use_dset_label <- grepl("\\+", axes_label)
  if(use_dset_label){
    l_label <- strsplit(axes_label, split = "\\+")
    axes_label <- l_label[[1]][1]
  }

  # get data
  iData1 <- COINr::get_data(coin, dset = dsets[1], iCodes = iCodes[1], Level = Levels[1], also_get = "uName")
  iData2 <- COINr::get_data(coin, dset = dsets[2], iCodes = iCodes[2], Level = Levels[2], also_get = "uName")

  # merge data
  iData <- merge(iData1, iData2, by = c("uCode", "uName"))

  # remove any rows with NAs, which we can't plot anyway
  iData <- stats::na.omit(iData)

  xlab <- iCodes[1]
  ylab <- iCodes[2]

  if(axes_label == "iName"){
    xlab <- COINr::icodes_to_inames(coin, xlab)
    ylab <- COINr::icodes_to_inames(coin, ylab)
  }

  if (use_dset_label) {
    xlab <- add_dset_name(xlab, dsets[1])
    ylab <- add_dset_name(ylab, dsets[2])
  }

  htext <- paste0("<b>", iData$uName, "</b><br>",
                  iCodes[1], ": ", signif(iData[[3]], 5), "<br>",
                  iCodes[2], ": ", signif(iData[[4]], 5))

  fig <- plotly::plot_ly(data = iData, type = 'scatter', mode = 'markers') |>
    plotly::add_trace(
      x = ~get(names(iData)[3]),
      y = ~get(names(iData)[4]),
      text = htext,
      hoverinfo = 'text',
      marker = list(size = 12, color = marker_colour),
      showlegend = F
    )

  if(nrow(iData) < 2){
    trendline <- FALSE
    warning("Cannot plot trend line because less than two data points.")
  }

  # TODO remove zeros and negative values for log axes!

  if (trendline) {

    # remove zeros for log - replace with NA then remove rows
    if(log_axes[1]){
      iData[iData[[3]] <= 0, 3] <- NA
    }
    if(log_axes[2]){
      iData[iData[[4]] <= 0, 4] <- NA
    }
    iData <- stats::na.omit(iData)

    y <- iData[[4]]
    x <- iData[[3]]

    logy <- FALSE
    if(all(log_axes)){
      y <- log(y)
      logy <- TRUE
      lfit <- stats::lm(y ~ log(x))
    } else if (all(!log_axes)){
      lfit <- stats::lm(y ~ x)
    } else if (log_axes[1]){
      lfit <- stats::lm(y ~ log(x))
    } else {
      y <- log(y)
      logy <- TRUE
      lfit <- stats::lm(y ~ x)
    }

    if(logy){
      fig <- plotly::add_lines(fig, x = iData[[3]], y = exp(stats::predict(lfit)), name = "Trend")
    } else {
      fig <- plotly::add_lines(fig, x = iData[[3]], y = stats::predict(lfit), name = "Trend")
    }


  }


  fig <- plotly::layout(
    fig,
    showlegend = FALSE,
    xaxis = list(title = xlab, type = xaxis_type),
    yaxis = list(title = ylab, type = yaxis_type))


  # add some details
  if(show_details){

    x <- iData[[3]]
    y <- iData[[4]]

    c_p <- stats::cor.test(x, y, method = "pearson")
    c_sp <- stats::cor.test(x, y, method = "spearman")

    round_to <- 3

    corr_p <- c_p$estimate |> round(round_to)
    corr_sp <- c_sp$estimate |> round(round_to)
    pv_p <- c_p$p.value |> round(round_to)
    pv_sp <- c_sp$p.value |> round(round_to)

    corr_string <- paste0("Corr: ", corr_p, " (p-val: ", pv_p, ") | Rank corr: ", corr_sp, " (p-val: ", pv_sp, ")")

    fig <- plotly::layout(
      fig,
      title = list(
        text = corr_string,
        x = 1,
        xanchor = 'right',
        font = list(
          color = "#696969",
          size = 11)

      )
    )
  }


  fig

}
