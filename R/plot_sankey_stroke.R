#' Plot Sankey diagram for stroke risk contributors
#'
#' @param df Dataset containing stroke PAR values. Can be:
#'   1) wide raw Excel-style data,
#'   2) wide cleaned data with RiskFactor + year columns,
#'   3) long-format data with RiskFactor, Year, PAR.
#' @param plot_title Title of the plot
#' @param vertical_gap Vertical whitespace between ribbons
#' @param horizontal_gap Horizontal whitespace between year blocks
#' @param block_width Width of the year blocks
#' @param min_h Minimum visual ribbon height
#' @param smooth_pts Number of interpolation points
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @export
plot_sankey_stroke <- function(df,
                               plot_title = "Contributors to Stroke in Blacks",
                               vertical_gap = 0.02,
                               horizontal_gap = 0.00,
                               block_width = 0.70,
                               min_h = 0.05,
                               smooth_pts = 40) {

  valid_risk_factors <- c(
    "Hypertension",
    "Diabetes",
    "Smoking",
    "Hypercholesterolemia",
    "Obesity"
  )

  # ---------------------------------------------------------
  # 1. Standardize input data to long format
  # ---------------------------------------------------------

  # Case A: already long format
  if (all(c("RiskFactor", "Year", "PAR") %in% names(df))) {

    if (!("x_pos" %in% names(df))) {
      df$x_pos <- match(as.character(df$Year), c("1990", "1995", "2000", "2005", "2010"))
    }

    if (!("rf_order" %in% names(df))) {
      df$rf_order <- factor(df$RiskFactor, levels = valid_risk_factors)
    }

    df_long <- df
  } else {

    # Case B: wide format
    df_wide <- df

    # Handle raw Excel column name if present
    if ("Risk Factors for Stroke in Blacks" %in% names(df_wide)) {
      names(df_wide)[names(df_wide) == "Risk Factors for Stroke in Blacks"] <- "RiskFactor"
    }

    if (!("RiskFactor" %in% names(df_wide))) {
      stop("Input dataset must contain either 'RiskFactor' or 'Risk Factors for Stroke in Blacks'.")
    }

    # Keep only RiskFactor + year columns that exist
    year_cols <- intersect(c("1990", "1995", "2000", "2005", "2010"), names(df_wide))

    if (length(year_cols) == 0) {
      stop("Wide-format dataset must contain year columns: 1990, 1995, 2000, 2005, 2010.")
    }

    df_wide <- dplyr::select(df_wide, RiskFactor, dplyr::all_of(year_cols))
    df_wide <- dplyr::filter(df_wide, RiskFactor %in% valid_risk_factors)

    # convert year columns to character first to avoid pivot type conflict
    df_wide <- dplyr::mutate(
      df_wide,
      dplyr::across(-RiskFactor, as.character)
    )

    df_long <- tidyr::pivot_longer(
      df_wide,
      cols = -RiskFactor,
      names_to = "Year",
      values_to = "PAR"
    )

    # convert PAR to numeric after pivot
    df_long$PAR <- as.numeric(df_long$PAR)

    df_long <- dplyr::mutate(
      df_long,
      Year = as.character(Year),
      PAR = as.numeric(PAR),
      x_pos = match(Year, c("1990", "1995", "2000", "2005", "2010")),
      rf_order = factor(RiskFactor, levels = valid_risk_factors)
    )
  }

  # Final safety checks
  if (!all(c("RiskFactor", "Year", "PAR", "x_pos", "rf_order") %in% names(df_long))) {
    stop("Data could not be standardized to the required long format.")
  }

  # Remove rows with missing PAR or x position
  df_long <- dplyr::filter(df_long, !is.na(PAR), !is.na(x_pos))

  # ---------------------------------------------------------
  # 2. Internal geometry builder
  # ---------------------------------------------------------
  build_geometry <- function(dat,
                             vertical_gap,
                             horizontal_gap,
                             block_width,
                             min_h,
                             smooth_pts) {

    rects <- dplyr::group_by(dat, Year)
    rects <- dplyr::arrange(rects, dplyr::desc(PAR), rf_order, .by_group = TRUE)

    rects <- dplyr::mutate(
      rects,
      visual_h = pmax(PAR, min_h),
      ymax = rev(cumsum(rev(visual_h + vertical_gap))) - vertical_gap,
      ymin = ymax - visual_h,
      ymid = ymin + visual_h / 2,
      xmin = x_pos - block_width / 2,
      xmax = x_pos + block_width / 2
    )

    rects <- dplyr::ungroup(rects)

    connectors <- data.frame()
    years <- sort(unique(rects$x_pos))

    for (i in 1:(length(years) - 1)) {
      for (rf in unique(rects$RiskFactor)) {

        L <- dplyr::filter(rects, x_pos == years[i], RiskFactor == rf)
        R <- dplyr::filter(rects, x_pos == years[i + 1], RiskFactor == rf)

        if (nrow(L) == 0 || nrow(R) == 0) next

        x_start <- L$xmax + horizontal_gap
        x_end   <- R$xmin - horizontal_gap

        if (x_start >= x_end) {
          x_start <- L$xmax
          x_end   <- R$xmin
        }

        x_vals <- seq(x_start, x_end, length.out = smooth_pts)
        y_top  <- seq(L$ymax, R$ymax, length.out = smooth_pts)
        y_bot  <- seq(L$ymin, R$ymin, length.out = smooth_pts)

        ribbon <- data.frame(
          RiskFactor = rf,
          group_id = paste(rf, i, sep = "_"),
          x = c(x_vals, rev(x_vals)),
          y = c(y_top, rev(y_bot))
        )

        connectors <- rbind(connectors, ribbon)
      }
    }

    list(rects = rects, connectors = connectors)
  }

  geo <- build_geometry(
    dat = df_long,
    vertical_gap = vertical_gap,
    horizontal_gap = horizontal_gap,
    block_width = block_width,
    min_h = min_h,
    smooth_pts = smooth_pts
  )

  # ---------------------------------------------------------
  # 3. Plot
  # ---------------------------------------------------------
  risk_colors <- c(
    "Hypertension" = "#86B487",
    "Diabetes" = "#D96549",
    "Hypercholesterolemia" = "#D58C49",
    "Smoking" = "#7AA6C2",
    "Obesity" = "#4F81BD"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = geo$connectors,
      ggplot2::aes(x = x, y = y, group = group_id, fill = RiskFactor),
      alpha = 0.80,
      color = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_rect(
      data = geo$rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = RiskFactor),
      color = "white",
      linewidth = 0.70
    ) +
    ggplot2::geom_text(
      data = geo$rects,
      ggplot2::aes(x = x_pos, y = ymid, label = sprintf("%.2f", PAR)),
      color = "white",
      fontface = "bold",
      size = 3
    ) +
    ggplot2::scale_fill_manual(values = risk_colors) +
    ggplot2::scale_x_continuous(
      breaks = 1:5,
      labels = c("1990", "1995", "2000", "2005", "2010")
    ) +
    ggplot2::coord_cartesian(xlim = c(0.5, 5.5), clip = "off") +
    ggplot2::labs(
      title = plot_title,
      x = "Year",
      y = "Population Attributable Risk"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 18),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )

  return(p)
}
