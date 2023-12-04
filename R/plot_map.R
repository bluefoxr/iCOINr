
# NOT CURRENTLY WORKING - TO FINISH, maybe change to Plotly...?

# plot_choropleth_map <- function(coin, dset, iCode){
#
#   # extract data from coin
#   iData <- get_data(coin, dset = dset, iCodes = iCode)
#
#   # merge data into shape df
#   SHP_0 <- base::merge(world_borders, iData, by.x = "ISO_A3_EH", by.y = "uCode")
#
#   # Create a color palette for the map:
#   mypalette <- leaflet::colorNumeric(palette = "Blues", domain = SHP_0[[iCode]], na.color = "transparent")
#
#   # now plot
#   leaflet::leaflet(SHP_0) |>
#     leaflet::addProviderTiles("CartoDB.Positron") |>
#     leaflet::addPolygons(layerId = ~ISO_A3,
#                          fillColor = ~mypalette(get(iCode)),
#                          weight = 2,
#                          opacity = 1,
#                          color = "white",
#                          dashArray = "3",
#                          fillOpacity = 0.7,
#                          highlightOptions = leaflet::highlightOptions(
#                            weight = 5,
#                            color = "#666",
#                            dashArray = "",
#                            fillOpacity = 0.7,
#                            bringToFront = TRUE)) |>
#     leaflet::addLegend(pal = mypalette, values = ~get(iCode), opacity = 0.7, title = NULL,
#                        position = "bottomright")
#
# }
