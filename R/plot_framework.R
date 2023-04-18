#' Interactive sunburst plot of index structure
#'
#' Plots the structure of the index using a sunburst plot with **plotly**.
#'
#' @param coin The coin object
#' @param seg_cols A character vector of colour codes, one for each segment in the plot. The length of this
#' vector must be equal to the number of segments, i.e. the sum of the number of indicators and aggregates
#' in each level.
#'
#' @export
iplot_framework <- function(coin, seg_cols = NULL){

  # get iMeta
  iMeta <- coin$Meta$Ind

  # check if EffWeight present, if not, get
  if(is.null(iMeta$EffWeight)){
    coin <- COINr::get_eff_weights(coin, out2 = "coin")
    # get iMeta
    iMeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Parent), ]
  }

  #iMeta$EffWeight <- round(iMeta$EffWeight, 2)

  if(is.null(seg_cols)){
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
  } else {

    warning("This feature is not active yet...")
    stopifnot(is.vector(seg_cols),
              is.character(seg_cols))
    # if(length(seg_cols) != length(outW$LabelsParents$Labels)){
    #   stop("seg_cols is the wrong length: it needs to be a character vector of colour codes that is
    #        the same length as the sum of all elements of the structure, in this case length should be ",
    #        length(outW$LabelsParents$Labels))
    # }
    # fig <- plotly::plot_ly(
    #   labels = iMeta$iCode,
    #   parents = iMeta$Parent,
    #   values = iMeta$EffWeight,
    #   type = 'sunburst',
    #   branchvalues = 'total',
    #   marker = list(colors = seg_cols),
    #   text = iMeta$iName,
    #   hoverinfo = 'text',
    #   texttemplate = '%{label}'
    # )
  }

  fig
}
