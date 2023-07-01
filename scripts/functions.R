plot_range <- function(df, fill_col) {
  
  decade <- unique(df$decade)
  
  ggplot() +
    geom_sf(data = ozmaps::ozmap_country, fill = NA) +
    geom_sf(data = df, fill = fill_col, colour = NA) +
    annotate("text", x = 133, y = -45.5, 
             label = str_glue("Decade: {decade}s"), 
             size = 28/.pt, 
             colour = "#273b50",
             face = "bold") +
    coord_sf(xlim = c(110, 155), ylim = c(-45,-10)) +
    theme_void() +
    theme(text = element_text(family = "poppins", size = 28))
}

plot_heatmap <- function(df, legend_title, pal) {
  
  ggplot(df) +
    geom_tile(aes(x = ibraRegionAbbr,
                  y = timePeriod,
                  fill = propInvInt)) +
    scale_x_discrete(position = "top") +
    scale_fill_distiller(name = legend_title,
                         type = "seq",
                         palette = pal,
                         na.value = NA,
                         direction = 1,
                         trans = "log",
                         breaks = scales::breaks_log(n = 5),
                         guide = guide_colorbar(direction = "horizontal",
                                                label.position = "bottom",
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                ticks = FALSE,
                                                barwidth = 25, 
                                                barheight = 2)) +
    coord_equal() +
    theme_classic() +
    theme(text = element_text(family = "poppins"),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 20, colour = "#444444"),
          legend.title.align = 0.5,
          legend.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA))
}

plot_textline <- function(df, line_col) {
  
  df |> 
    filter(type = types_of_records) |> 
    ggplot() +
    geom_textpath(aes(x = year,
                      y = cumul_count,
                      label = type, 
                      colour = type,
                      hjust = 0.9), 
                  lwd = 1.2, 
                  #colour = line_col, 
                  size = 6) +
    scale_y_continuous(labels = scales::comma, 
                       limits = c(0, 100000000)) +
    # scale_x_discrete(limits = c(1900, 2000),
    #                  labels = c(1900, 1925, 1950, 1975, 2000)) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          text = element_text(family = "poppins"),
          legend.position = "none", 
          panel.grid = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA))
}



theme_textline <- function(){ 
  
  font <- "poppins"  
  
  theme_minimal() %+replace%
    theme(axis.title = element_blank(),
          text = element_text(family = font),
          legend.position = "none",
          panel.grid = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background =  element_rect(fill = NA, colour = NA))
}

plot_choropleth <- function(df) {
  
  each_year <- unique(df$year)
  
  ggplot() +
    geom_sf(data = ibra_sf, colour = "#232F0B", fill = NA) +
    geom_sf(data = df,
            aes(fill = cutCounts),
            colour = NA) +
    scale_fill_manual(name = NULL, 
                      drop = FALSE,
                      labels = c("0.2", "0.4", "0.6", "0.8"),
                      # 5-class YlOrBr
                      values = c("#B9CA98", "#92AC5D", "#6B8E23", "#4E6819", "#314210"),
                      na.value = "#efefef",
                      guide = guide_colorsteps(
                        direction = "horizontal",
                        label.position = "bottom",
                        title.position = "left",
                        title.vjust = 0.8, 
                        title.hjust = 0.8)) +
    annotate("text", 
             x = 133,
             y = -45.5,
             label = str_glue("Biological monitoring events: {each_year}"),
             size = 8, family = "poppins", colour = "#273b50") +
    coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.key.width = unit(25, 'mm'),
          legend.text = element_text(size = 6),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA)) 
}

gt_theme <- function(data, ...){
  data |> 
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts())) |> 
    opt_row_striping() |> 
    tab_options(
      table.font.size = 24,
      #table.font.weight = "lighter",
      column_labels.font.size = 28,
      column_labels.font.weight = "lighter",
      row.striping.background_color = "#a4782310",
      table_body.hlines.color = "transparent",
      table.font.color = "#273b50",
      data_row.padding = 3,
      column_labels.background.color = "#a47823",
      ...
    ) 
}