#' Bar chart
#'
#' Generates an interactive bar chart.
#'
#' @param coin The coin object
#' @param dset The data set to plot.
#' @param iCode The selected indicator code or aggregate (does not support multiple indicators)
#' @param usel A character vector of unit codes to highlight on the bar chart (optional)
#' @param stack_children If `TRUE`, produces a stacked bar chart with any children of `iCode.`
#' This only works for aggregates.
#' @param ulabs Tick labels to display on x axis: either `"uCode"` or `"uName"`.
#' @param ilabs if `"iName"`, displays axis and legend using indicator names, else if `"iCode"`, uses codes.
#' @param orientation Controls the orientation of the plot: either `"horizontal"` or `"vertical"`.
#' @param plot_subset Optional number to plot top or bottom-scoring subset of units. Set to a positive integer (e.g. 10)
#' to plot the top 10, or a negative integer to plot the bottom (e.g. -20 plots the bottom 20).
#' @param trunc_ulabs Logical: if `TRUE` truncates unit labels using [truncate_strings()].
#' @param colour_pal Optional vector of colour codes to use for bar fill
#' @param opacity Optional opacity to apply to all bars (value between 0 and 1)
#'
#' @examples
#' #
#'
#' @return Interactive bar chart generated by plotly.
#'
#' @export

iplot_bar <- function(coin, dset = "Raw", iCode = NULL, usel = NULL,
                     stack_children = FALSE, ulabs = "uCode", ilabs = "iCode",
                     orientation = "horizontal", plot_subset = NULL, trunc_ulabs = FALSE,
                     colour_pal = NULL, opacity = 1){

  # PREP ----

  stopifnot(ulabs %in% c("uCode", "uName"))

  iData <- COINr::get_data(coin, dset = dset, iCodes = iCode, also_get = "uName")

  multiple_indicators <- ncol(iData) > 3

  if(multiple_indicators){
    abort("You have tried to plot multiple indicators. The bar chart only supports plotting a single indicator at a time.")
  }

  if(ilabs == "iCode"){
    axtitle <- iCode
  } else if (ilabs == "iName"){
    axtitle <- COINr::icodes_to_inames(coin, iCode)
  } else {
    abort("ilabs must be one of 'iCode' or 'iName'")
  }

  if(is.null(colour_pal)){
    colour_pal <- c("#6929c4", "#1192e8", "#005d5d", "#9f1853", "#fa4d56",
                    "#570408", "#198038", "#002d9c", "#ee538b", "#b28600",
                    "#009d9a", "#012749", "#8a3800", "#a56eff")
  }

  if(!is.null(plot_subset)){
    if(abs(plot_subset) > nrow(iData)){
      plot_subset <- NULL
    }
  }

  # setup some parameters based on hor/vert
  if(orientation == "vertical"){
    decreasing <- FALSE
    xtitle <- axtitle
    ytitle <- ""
    if(!is.null(plot_subset)){
      plot_subset <- -1*plot_subset # has to be flipped due to vertical ordering
    }
  } else if (orientation == "horizontal"){
    decreasing <- TRUE
    ytitle <- axtitle
    xtitle <- ""
  } else {
    abort("'orientation' must be either 'horizontal' or 'vertical'")
  }

  # this is for highlighting selected units and will be modified later
  # if usel is not NULL
  #opacity <- 1

  # Stacked plot ------------------------------------------------------------

  if(stack_children){

    # get iMeta
    iMeta <- coin$Meta$Ind
    # get child codes
    iCodes_ch <- iMeta$iCode[iMeta$Parent == iCode]
    # remove NAs
    iCodes_ch <- iCodes_ch[!is.na(iCodes_ch)]
    # check
    if(length(iCodes_ch) == 0){
      abort("No child codes found for selected iCode: if stack_children = TRUE, you must select an iCode in Level 2 or above (it must be an aggregate).")
    }

    # get data
    iData_ch <- COINr::get_data(coin, dset = dset, iCodes = iCodes_ch)

    # merge onto iData
    iData <- merge(iData, iData_ch, by = "uCode")

    # sort
    iData <- iData[order(iData[[iCode]], decreasing = decreasing),]

    # filter
    if(!is.null(plot_subset)){

      stopifnot(is.numeric(plot_subset),
                length(plot_subset) == 1)

      if(plot_subset > 0){
        iData <- iData[1:plot_subset, ]
      } else {
        # negative value mean bottom N
        plot_subset <- -1*plot_subset
        iData <- iData[(nrow(iData) - plot_subset + 1):nrow(iData), ]
      }
    }

    # copy for later (hover text)
    iData0 <- iData

    # scale children to add up to parent score
    iData$scale_fac <- iData[[iCode]]/rowSums(iData[iCodes_ch])
    iData[iCodes_ch] <- sapply(iData[iCodes_ch], `*`, iData$scale_fac)

    # get child names
    if(ilabs == "iName"){
      iNames_ch <- COINr::icodes_to_inames(coin, iCodes_ch)
    } else {
      iNames_ch <- iCodes_ch
    }

    # text for hover: make one character vector per trace (child)
    text_template <- lapply(1:length(iCodes_ch), function(ii){
      paste0("<b>", iData$uName, "</b><br>", iNames_ch[ii], "<br>", round(iData0[[iCodes_ch[ii]]],1))
    })

    if(!is.null(usel)){
      opacity <- rep(0.5, nrow(iData))
      opacity[iData$uCode %in% usel] <- 1
    }

    # name truncation
    if(trunc_ulabs){
      iData[[ulabs]] <- truncate_strings(iData[[ulabs]])
    }

    if(orientation == "vertical"){

      # plot first trace, then loop-plot remaining traces
      fig <- plotly::plot_ly(
        data = iData,
        y = ~get(ulabs),
        x = ~get(iCodes_ch[1]),
        source = 'bar_chart',
        key = ~uCode,
        type = "bar",
        name = iNames_ch[1],
        hovertext = text_template[[1]],
        hoverinfo = 'text',
        orientation = "h",
        marker = list(opacity = opacity,
                      color = colour_pal[1])
      )

      for (ii in 2:length(iCodes_ch)){
        fig <- plotly::add_trace(
          fig,
          x = iData[[iCodes_ch[ii]]],
          name = iNames_ch[ii],
          hovertext = text_template[[ii]],
          marker = list(color = colour_pal[ii])
        )
      }

      fig <- plotly::layout(
        fig,
        title = list(text = NULL, y = 0.95),
        xaxis = list(
          title = xtitle
        ),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = iData[iCode],
          tickangle = 0),
        barmode = "stack",
        legend = list(x = 0.7, y = 0.05)
        #autorange="reversed"
      )

    } else if (orientation == "horizontal"){

      # plot first trace, then loop-plot remaining traces
      fig <- plotly::plot_ly(
        data = iData,
        x = ~get(ulabs),
        y = ~get(iCodes_ch[1]),
        source = 'bar_chart',
        key = ~uCode,
        type = "bar",
        name = iNames_ch[1],
        hovertext = text_template[[1]],
        hoverinfo = 'text',
        marker = list(opacity = opacity,
                      color = colour_pal[1])
      )

      for (ii in 2:length(iCodes_ch)){
        fig <- plotly::add_trace(
          fig,
          y = iData[[iCodes_ch[ii]]],
          name = iNames_ch[ii],
          hovertext = text_template[[ii]],
          marker = list(color = colour_pal[ii])
        )
      }

    }


    # Standard plot -----------------------------------------------------------

    } else { # standard bar plot

    # sort
    iData <- iData[order(iData[[iCode]], decreasing = decreasing),]

    # filter
    if(!is.null(plot_subset)){

      stopifnot(is.numeric(plot_subset),
                length(plot_subset) == 1)

      if(plot_subset > 0){
        iData <- iData[1:plot_subset, ]
      } else {
        # negative value mean bottom N
        plot_subset <- -1*plot_subset
        iData <- iData[(nrow(iData) - plot_subset + 1):nrow(iData), ]
      }
    }

    # highlighting
    if(!is.null(usel)){
      opacity <- rep(0.5, nrow(iData))
      opacity[iData$uCode %in% usel] <- 1
    }

    # get names
    if(ilabs == "iName"){
      iName <- COINr::icodes_to_inames(coin, iCode)
    } else {
      iName <- iCode
    }

    # name truncation
    if(trunc_ulabs){
      iData[[ulabs]] <- truncate_strings(iData[[ulabs]])
    }

    # hover text
    # text for hover: make one character vector per trace (child)
    text_template <- paste0("<b>", iData$uName, "</b><br>", iName, "<br>", round(iData[[iCode]],1))

    if (orientation == "vertical"){

      fig <- plotly::plot_ly(
        data = iData,
        y = ~get(ulabs),
        x = ~get(iCode),
        source = 'bar_chart',
        key = ~uCode,
        type = "bar",
        marker = list(opacity = opacity,
                      color = colour_pal[1]),
        name = iName,
        hovertext = text_template,
        hoverinfo = 'text',
        orientation = "h"
      )

    } else if (orientation == "horizontal"){

      fig <- plotly::plot_ly(
        data = iData,
        x = ~get(ulabs),
        y = ~get(iCode),
        source = 'bar_chart',
        key = ~uCode,
        type = "bar",
        marker = list(opacity = opacity,
                      color = colour_pal[1]),
        name = iName,
        hovertext = text_template,
        hoverinfo = 'text'
      )

    }

    }

  # Layout extra ------------------------------------------------------------

  if(orientation == "vertical"){

    fig <- plotly::layout(
      fig,
      title = list(text = NULL, y = 0.95),
      xaxis = list(
        title = xtitle
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = iData[iCode],
        tickangle = 0),
      barmode = "stack",
      legend = list(x = 0.7, y = 0.05)
      #autorange="reversed"
    )

  } else {

    fig <- plotly::layout(
      fig,
      title = list(text = NULL, y = 0.95),
      yaxis = list(
        title = COINr::icodes_to_inames(coin, iCode),
        showticklabels = TRUE
      ),
      xaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = iData[iCode],
        tickangle = -45),
      barmode = "stack",
      legend = list(x = 0.7, y = 0.95)
    )

  }

  fig
}
