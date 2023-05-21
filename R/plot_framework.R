#' Interactive sunburst plot of index structure
#'
#' Plots the structure of the index using a sunburst plot with **plotly**.
#'
#' @param coin The coin object
#' @param plotly_colorway Optional character vector of colour codes. This is passed to
#' plotly's "colorway" parameter, and specifies the main colours to use in the chart.
#'
#' @export
iplot_framework <- function(coin, plotly_colorway = NULL){

  # get iMeta
  iMeta <- coin$Meta$Ind

  # check if EffWeight present, if not, get
  if(is.null(iMeta$EffWeight)){
    coin <- COINr::get_eff_weights(coin, out2 = "coin")
    # get iMeta
    iMeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Parent), ]
  }

  fig <- plotly::plot_ly(
    labels = iMeta$iCode,
    parents = iMeta$Parent,
    values = iMeta$EffWeight,
    type = 'sunburst',
    branchvalues = 'total',
    text = iMeta$iName,
    hoverinfo = 'text',
    texttemplate = '%{label}'
  )

  if(!is.null(plotly_colorway)){

    stopifnot(is.character(plotly_colorway))

    fig <- plotly::layout(
      fig,
      colorway = plotly_colorway
    )

  }


  fig
}
