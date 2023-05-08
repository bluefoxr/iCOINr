#' Radar chart
#'
#' Generates an interactive radar chart for a specified unit or set of units.
#'
#' This function uses **plotly** to generate a radar chart for showing one or more units, compared using a specified set of indicators.
#' Optionally, you can add mean/median as an extra trace. The point being to show how a particular unit compares to
#' its peers.
#'
#' @param coin The coin object
#' @param dset The data set to use in the table
#' @param usel Character vector of unit code(s) to plot data from
#' @param iCodes The indicator or aggregation code(s) to plot
#' @param Level The selected aggregation level to take indicator data from,
#' where 1 is the base indicator level, and 2, 3 etc are higher aggregation levels
#' @param addstat Adds the statistic of the scores in each dimension as a separate trace. If `"mean"` adds the overall
#' mean for each dimension/indicator. If `"median"` adds the overall median.
#'
#' @examples
#' #
#'
#' @return Interactive radar chart generated using plotly.
#'
#' @export

iplot_radar <- function(coin, dset = "Raw", usel = NULL, Level = NULL, iCodes = NULL, addstat = "none"){

  if(is.null(usel)){
    stop("You need to select a unit using usel.")
  }

  # get indicator data
  iData <- COINr::get_data(coin, dset = dset, iCodes = iCodes,
                           Level = Level, also_get = "uName")

  # data to plot on radar chart (vals of each indicator/aggregate)
  iData_usel <- iData[iData$uCode %in% usel, ]
  uNames <- iData_usel$uName

  iData_usel_numeric <- iData_usel[!(names(iData_usel) %in% c("uCode", "uName"))]
  iData_numeric <- iData[!(names(iData) %in% c("uCode", "uName"))]

  if(ncol(iData_usel_numeric) < 3){
    stop("Cannot plot (meaningful) radar chart for less than three variables.")
  }

  if(addstat == "mean"){

    iData_usel_numeric <- rbind(colMeans(iData_numeric, na.rm = TRUE),
                  iData_usel_numeric)
    uNames <- c("<b>Mean<b>", uNames)

  } else if (addstat == "median"){

    iData_usel_numeric <- rbind(apply(iData_numeric, 2, stats::median, na.rm = T),
                  iData_usel_numeric)
    uNames <- c("<b>Median<b>", uNames)

  }

  # build plot
  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    mode = "markers",
    fill = 'toself'
  )

  # Add each unit trace by looping over units
  for (ii in 1:length(uNames)){
    fig <- plotly::add_trace(
        fig,
        r = as.numeric(iData_usel_numeric[ii,]),
        theta = colnames(iData_usel_numeric),
        name = uNames[ii]
      )
  }

  # add title
  fig <- plotly::layout(fig, title = list(text = iCodes,
                                             y = 0.95, x = 0.03)) # title position

  fig
}
