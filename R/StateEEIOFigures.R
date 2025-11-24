# StateEEIOFigures.R
library(ggplot2)
library(dplyr)


#' Plot specified matrix coefficients as box and whisker plot
#' @param model_list List of completed EEIO models of all 50 states
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param indicator Row name in the specified matrix
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @param xlim optional upper limit for X-axis.
#' @param scale, int, number of digits to remove from x-axis
#' @export
plotTwoRegionBoxWhisker <- function(model_list, matrix_name, indicator, sector_to_remove="", y_title,
                                    xlim=NA, scale=0) {
  
  df <- prepareDFforFigure(model_list=model_list, matrix_name=matrix_name, 
                           perspective=NULL, indicator=indicator,
                           sector_to_remove=sector_to_remove, y_title=y_title)
  # drop the RoUS coefficient
  df <- subset(df, df$region != "RoUS")
  df_wide <- reshape2::dcast(df, Indicator + Sector + color + GroupName + SectorName ~ modelname, value.var = "Value")
  df <- reshape2::melt(df_wide, id.vars = c("Indicator", "Sector", "color", "GroupName", "SectorName"),
                       variable.name = "modelname", value.name = "Value")
  df <- df[order(df$GroupName), ]
  label_colors <- rev(unique(df[, c("SectorName", "color")])[, "color"])
  df$x <- df$SectorName
  
  df <- df[complete.cases(df), ]
  df$Value = df$Value / 10^scale
  
  # plot
  # https://ggplot2.tidyverse.org/reference/geom_boxplot.html
  
  p <- ggplot(df, aes(x = Value,
                      y = factor(x, levels = rev(unique(x))),
                      fill = GroupName,
  ))
  
  p <- p + geom_boxplot(outlier.size = 1) +
    scale_fill_manual(values = unique(rev(label_colors))) +
    labs(x = y_title,
         y = element_blank()) +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.text.y = element_text(size = 10, color = label_colors),
          axis.title.x = element_text(size = 10), legend.title = element_blank(),
          legend.justification = c(1, 1), axis.ticks = element_blank(),
          panel.grid.minor.y = element_blank(), plot.margin = margin(c(5, 20, 5, 5)))
  if(!is.na(xlim)){
    p <- p + xlim(0, xlim)
  }
  
  return(p)
}

#' Plot results for specified sector on map
#' @param model_list List of completed EEIO models of all 50 states
#' @param indicator Row name in the specified matrix for the line plot
#' @param sector The selected sector to highlight in the map, NULL to return all sectors and subset later.
#' @param demand, e.g., "Consumption" or "Production"
#' @param matrix_name, e.g., "N", use NULL if running a demand vector
plotMapResults <- function(model_list, indicator, sector=NULL, demand="Consumption", matrix_name=NULL) {
  df <- prepareDFforFigure(model_list=model_list, matrix_name=matrix_name, perspective="DIRECT",
                           indicator=indicator, sector_to_remove="", demand=demand,
                           combine_SoIRoUS=FALSE)
  if(!is.null(sector)){
    df <- subset(df, df$Sector == sector)    
  }
  if(is.null(matrix_name)) {
    df['perspective'] = 'DIRECT'
    df['demand_type'] = demand
    df2 <- prepareDFforFigure(model_list=model_list, matrix_name=matrix_name, perspective="FINAL",
                              indicator=indicator, sector_to_remove="", demand=demand,
                              combine_SoIRoUS=FALSE)
    df2['perspective'] = 'FINAL'
    df2['demand_type'] = demand
    if(!is.null(sector)) {
      df2 <- subset(df2, df2$Sector == sector)
    }
    df_combined <- rbind(df, df2)
  } else {
    df_combined <- df
  }
  
  df_combined$state <- gsub("US-", "", df_combined$modelname)
  return(df_combined)
  
  # https://www.storybench.org/plot-state-state-data-map-u-s-r/
  # states <- read.csv(file.path("../../data/state_lat_long.csv"), header=TRUE, stringsAsFactors=FALSE)
  # # states <- read.csv(file.path("data/state_lat_long.csv"), header=TRUE, stringsAsFactors=FALSE)
  # states["state"] <- paste0("US-", states$state)
  # df2 <- merge(df2, states, by.x = 'modelname', by.y = 'state')
  # 
  # # plot
  # devtools::install_github("wmurphyrd/fiftystater")
  # data("fifty_states")
  # p <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92") + 
  #       geom_point(data=df2, aes(x=lon, y=lat, size = Value), color="black") + 
  #       scale_size(name="") + 
  #       guides(size=guide_legend(paste(y_title, sector, sep="-"))) +
  #       theme_void()
  # 
  # return(p)
}

#' Stacked bar chart (e.g., for showing location of impact as SoI or RoUS or RoW)
#' @param df, must include "Sector", "Value" and "ID" columns
stackedBarChartResultFigure <- function(df, model, grouping="Sector") {
  mapping <- useeior:::getBEASectorColorMapping(model)
  short_names <- read.csv("../data/names_short.csv")[,c("Code","Name_short")]

   mapping <- rbind(mapping,
                    data.frame(Sector = c("F010-Mobile", "F010-Stationary"),
                               SummaryCode = c("F010-Mobile", "F010-Stationary"),
                               color = mapping$color[mapping$Sector=="F010"], 
                               SectorName = c("Households - Mobile", "Households - Stationary"))
  )
  
  if (grouping == "Summary") {
    mapping <- merge(mapping,short_names,by.x = c("SummaryCode"), by.y = c("Code"), all.x = TRUE)
    df <- merge(df, mapping[, c("SummaryCode", "color", "Name_short")], by.x = "Sector", by.y = "SummaryCode", all.x = TRUE)  
    names(df)[names(df) == 'Name_short'] <- 'SectorName'
    df$SectorName <- as.factor(df$SectorName)
  } else {
    mapping <- unique(mapping[, c("Sector", "color")])
    mapping <- merge(mapping,short_names,by.x = c("Sector"), by.y = c("Code"), all.x = TRUE)
    df <- merge(df, unique(mapping[, c("Sector", "color", "Name_short")]), by = "Sector")  
    names(df)[names(df) == 'Name_short'] <- 'SectorName'
    df$SectorName <- as.factor(df$SectorName)
  }

  # Extract primary code in order to set figure stack alignment
  state <- unique(df$ID)[!(unique(df$ID) %in% c("RoUS", "RoW"))]
  df$ID <- factor(df$ID, levels=c("RoW", "RoUS", state))
  
  df <- df[order(df$SectorName),]
  label_colors <- rev(unique(df[, c("Sector", "color")])[, "color"])
  p <- ggplot(df, aes(x = Value, fill = ID, y = SectorName)) +
          geom_col() + 
          guides(fill = guide_legend(reverse = TRUE)) + # Swap legend order
          scale_y_discrete(limits=rev) + # Reverse Y-axis
          scale_x_continuous(expand = c(0, 0)) +
       theme_bw() +
        theme(
                axis.text = element_text(color = "black", size = 12),
                axis.text.y = element_text(size = 10, color = label_colors),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_blank(),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                )

  return(p)
}


#Plots a line chart for a StateResult where x is time in years and y is variable
# ylabel
lineChartFigure <- function(result,ylabel) {
  df_figure <- reformatWidetoLong(result)
  p <- ggplot(df_figure, aes(x = Year, y = value, group=variable, color=variable))+
    geom_line(linewidth=1) +
    ylab(ylabel) +
    xlab("Year") +
    scale_colour_hue() +
    theme_bw() +
    theme(text = element_text(size=20), legend.title=element_blank())
  return(p)
}

#' Plot time series for two region models
#' @param df
#' @param plottype, str, "line" or "bar"
#' @param legend_ncol
twoRegionTimeSeriesPlot <- function(df,
                                    model,
                                    plottype, # bar or line
                                    legend_ncol = 2) {
  df_figure <- reformatWidetoLong(df)
  mapping <- useeior:::getBEASectorColorMapping(model)
  mapping <- rbind(mapping,
                   data.frame(Sector=c("F010-Mobile", "F010-Stationary"),
                              SummaryCode=c("F010-Mobile", "F010-Stationary"),
                              color=c("#818589", "#899499"),
                              SectorName=c("Households - Mobile", "Households - Stationary")))
  
  df_figure <- merge(df_figure, unique(mapping[, c("Sector", "color", "SectorName")]),
                     by.x = "variable", by.y = "Sector")

  # Load visualization elements
  vizElements <- loadVisualizationElementsForTimeSeriesPlot()
  barplot_theme <- vizElements$barplot_theme
  
  df_figure <- subset(df_figure, df_figure$value >= 0)
  # Plot
  label_colors <- unique(df_figure[, c("variable", "SectorName", "color")])
  label_colors <- na.omit(label_colors[match(levels(df_figure$SectorName),
                                             label_colors$SectorName),][, "color"])

  if (plottype == "bar") {
    p <- ggplot(df_figure, aes(x = Year, y = value, fill = SectorName))
    p <- p + geom_bar(stat = "identity", width = 0.8, color = "white") +
      scale_fill_manual(name = "", values = label_colors) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(ncol = legend_ncol))

  } else if (plottype == "line") {
    p <- ggplot(df_figure, aes(x = Year, y = value, group = SectorName, color = SectorName)) +
      geom_line(stat = "identity", linewidth=1) +
      scale_color_manual(name = "", values = label_colors) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(color = guide_legend(ncol = legend_ncol))

  } else {
    print('Error not an available plottype')
    return()
  }
  return(p)
}


contributionToImpactBySectorChart <- function(model, sector, indicator, state) {
  df0 <- useeior::disaggregateTotalToDirectAndTier1(model, indicator)
  
  sector_codes <- c(paste0(sector, "/US-", state), paste0(sector, "/RoUS"))
  df <- subset(df0, df0$sector_code %in% sector_codes)
  
  # Reaggregate ignoring location of purchased commodity
  df_agg <- df %>%
    group_by(purchased_commodity) %>%
    summarize(
      impact_per_purchase = sum(impact_per_purchase),
      .groups = 'drop'
    )
  
  # Identify the top 5 sectors being purchases
  top5 <- df_agg %>% top_n(5)
  if("Direct" %in% top5$purchased_commodity) {
    top5 <- df_agg %>%
      top_n(6) %>%
      filter(purchased_commodity!='Direct') %>%
      arrange(impact_per_purchase)
  }
  sectors_to_show <- top5[["purchased_commodity"]]
  sectors_to_show <- c(sectors_to_show, "Imports", "Direct")
  
  df <- df %>% 
    mutate(
      tag = case_when(
        purchased_commodity %in% sectors_to_show ~ 1,
        !(purchased_commodity %in% sectors_to_show) ~ 0
      )
    )
  ## Aggregate all other sectors and recombine
  others <- subset(df, df$tag == 0)
  others <- aggregate(impact_per_purchase ~ sector_code, data = others, FUN=sum)
  others$purchased_commodity <- "Other"
  others$purchased_commodity_code <- "Other"
  df <- subset(df, df$tag == 1)
  df[is.na(df)] <- "Direct"
  common_cols <- intersect(colnames(df), colnames(others))
  df <- rbind(df[, common_cols], others[, common_cols])
  
  # Reaggregate ignoring location of purchased commodity
  df <- df %>%
    group_by(sector_code, purchased_commodity) %>%
    summarize(
      impact_per_purchase = sum(impact_per_purchase),
      .groups = 'drop'
    )
  
  import_row <- data.frame(sector_code = "RoW", purchased_commodity = "Imports",
                           impact_per_purchase = model$N_m[indicator, paste0(sector, "/US-", state)])
  df <- rbind(df, import_row)    

  name <- model$Commodities[model$Commodities$Code == sector, "Name"][1]
  # Rename sectors to states
  df['sector_code'] <- data.frame(lapply(df['sector_code'], function(x) {
    gsub(sector_codes[1], state, x)}))
  df['sector_code'] <- data.frame(lapply(df['sector_code'], function(x) {
    gsub(sector_codes[2], "RoUS", x)}))
  
  ## Order sectors for figure
  df$sector_code <- factor(df$sector_code, levels=c(state, "RoUS", "RoW"))
  df$purchased_commodity <- factor(df$purchased_commodity, levels=c("Other", sectors_to_show))
  levels(df$purchased_commodity) <- str_wrap(levels(df$purchased_commodity), 40)
  p <- ggplot(df, aes(fill=purchased_commodity, x = sector_code, y=impact_per_purchase)) +
    geom_bar(position="stack", stat = "identity") +
    xlab(element_blank()) +
    ylab("kg CO2e / $ produced") + 
    theme_bw() + 
    labs(fill="Source") +
    theme(text = element_text(size=18)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(p)
}

# extract a legend, for use in multi-pane figures
# see https://stackoverflow.com/a/13650878
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Taken from VisualizeStateIOresults.R global objects
loadVisualizationElementsForTimeSeriesPlot <- function(){
  vizElements <- list()

  configfile <- system.file("extdata", "VisualizationEssentials.yml", package = "useeior")
  vizElements$VisualizationEssentials <- configr::read.config(configfile)
  vizElements$ColorLabelMapping <- as.data.frame(t(cbind.data.frame(vizElements$VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  vizElements$ColorLabelMapping$color <- rownames(vizElements$ColorLabelMapping)

  # Define bar plot common theme
  vizElements$barplot_theme <- theme_linedraw() +
    theme(
      # axis
      axis.text = element_text(color = "black", size = 14),
      axis.title = element_text(size = 16),
      axis.ticks = element_blank(),
      # legend
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.6, "cm"),
      legend.position = "bottom",
      # panel grid
      panel.spacing = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      # facet strip
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(colour = "black", size = 30)
    )
  
  return(vizElements)
}
