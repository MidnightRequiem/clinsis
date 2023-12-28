#' Analyze Temperature Data
#'
#' This function analyzes temperature data for a given country.
#' @param country The name of the country to analyze.
#' @return A plot of the temperature data.
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_color_gradient ggtitle
#' @importFrom dplyr filter group_by summarise %>%
#' @importFrom tidyr separate
#' @importFrom stats aov shapiro.test model.tables oneway.test residuals dt
#' @importFrom car leveneTest
#' @importFrom utils head tail read.csv
#' @importFrom rlang .data
#' @export
#' @examples
#' analyzeTemperatureData("Japan")

analyzeTemperatureData <- function(country) {
  # Load the data
  url <- "https://raw.githubusercontent.com/gindeleo/climate/master/GlobalLandTemperaturesByCountry.csv"
  data <- utils::read.csv(url, stringsAsFactors = FALSE)

  # Use dplyr and tidyr for data manipulation
  data_filtered <- data %>%
    dplyr::filter(.data$Country == country, !is.na(.data$AverageTemperature)) %>%
    tidyr::separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
    dplyr::filter(.data$Year > 1752) %>%
    dplyr::group_by(.data$Year) %>%
    dplyr::summarise(Temp = mean(.data$AverageTemperature, na.rm = TRUE))

  # Plotting with ggplot2
  p1 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = .data$Year, y = .data$Temp)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$Temp)) +
    ggplot2::geom_smooth() +
    ggplot2::scale_color_gradient(low = "blue", high = "red") +
    ggplot2::ggtitle(paste(country, "'s Average Temperature 1753-2013")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  print(p1)

  # Create intervals and plot boxplot
  min_year <- min(data_filtered$Year)
  max_year <- max(data_filtered$Year)
  total_years <- max_year - min_year + 1
  num_intervals <- 6
  interval_length <- floor(total_years / num_intervals)

  year_breaks <- c(min_year,
                   sapply(1:(num_intervals - 1), function(i) min_year + i * interval_length),
                   max_year + 1)

  interval_labels <- paste(head(year_breaks, -1), tail(year_breaks, -1) - 1, sep = "-")
  data_filtered$Interval <- cut(data_filtered$Year, breaks = year_breaks, labels = interval_labels, include.lowest = TRUE)

  p2 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = .data$Interval, y = .data$Temp)) +
    ggplot2::geom_boxplot(fill = "blue1") +
    ggplot2::ggtitle("Temperature Distribution Across Intervals") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), plot.title = ggplot2::element_text(hjust = 0.5))

  print(p2)

  # Conduct ANOVA
  anova_result <- stats::aov(Temp ~ Interval, data = data_filtered)
  print(summary(anova_result))
  print(stats::model.tables(anova_result, "means"), digits = 3)

  # Check ANOVA assumptions
  shapiro_res <- stats::shapiro.test(stats::residuals(anova_result))
  levene_res <- car::leveneTest(Temp ~ Interval, data = data_filtered)

  # Print results of assumption tests
  print(shapiro_res)
  print(levene_res)

  # If assumptions are violated, perform Welch's ANOVA and Games-Howell test
  if (shapiro_res$p.value < 0.05 || levene_res$'Pr(>F)'[1] < 0.05) {
    print("ANOVA assumptions violated. Proceeding with Welch's ANOVA and Games-Howell test.")
    welch_res <- stats::oneway.test(Temp ~ Interval, data = data_filtered)
    print(welch_res)

    # Check for PMCMRplus package and load it
    if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
      stop("Package 'PMCMRplus' is required but not installed. Please install it to use this function.")
    }

    # Later in your function when using a function from PMCMRplus
    gh_result <- PMCMRplus::gamesHowellTest(Temp ~ Interval, data = data_filtered)

    print(gh_result)
  } else {
    # Tukey HSD test if ANOVA assumptions are not violated
    tukey_res <- stats::TukeyHSD(anova_result)
    print(tukey_res)
  }
}
