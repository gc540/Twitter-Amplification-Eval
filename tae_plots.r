#######################################################################################################################
   # Set up environment #
#######################################################################################################################
  options(scipen = 999)
  library(tidyverse)
  library(purrr)
  library(data.table)
  library(jsonlite)
  library(scales)
  library(gridExtra)

  covid_tweets_flat = read.csv('covid_tweets_flat.csv')
  climate_tweets_flat  = read.csv('climate_tweets_flat.csv')
  climate_tweets_lowtrust  = read.csv('climate_lowtrust_tweets.csv')
  covid_tweets_lowtrust  = read.csv('covid_lowtrust_tweets.csv')
  user_timelines_covid  = read.csv('user_timelines_covid.csv')
  user_timelines_climate  = read.csv('user_timelines_climate.csv')

#######################################################################################################################
  # Plot proportion of low-trust tweets to full dataset #
#######################################################################################################################

plot_proportion_of_lowtrust_tweets <- function(df1, df2, main_title, low_color, high_color){
  
  custom_theme <- theme_bw(base_size = 16) +
    theme(panel.border = element_blank(),
          panel.grid.major.y = element_line(color = "gray"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          axis.ticks = element_line(size = 1),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 12, face = "bold"),
          legend.position = "none",
          legend.box = "none", 
          plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))
  
  df1 <- df1 %>% 
    mutate(day = as.Date(created_at)) %>%
    select(day, everything())
  df2 <- df2 %>% 
    mutate(day = as.Date(created_at)) %>%
    select(day, everything())
  
  df1_counts <- aggregate(df1$day, by=list(df1$day), FUN=length)
  colnames(df1_counts) <- c("Day", "Count_df1")
  df2_counts <- aggregate(df2$day, by=list(df2$day), FUN=length)
  colnames(df2_counts) <- c("Day", "Count_df2")
  
  # Join the two data frames on the day column
  df_join <- merge(df1_counts, df2_counts, by="Day")
  
  # Calculate the proportion of df_1 entries per day compared to df_2 entries per day
  df_join$prop <- df_join$Count_df1 / df_join$Count_df2
  
  # Plot the bar chart
  ggplot(df_join, aes(x=Day, y=prop, fill=prop)) +
    geom_bar(stat="identity", width=0.7) +
    scale_fill_gradient(low=low_color, high=high_color) +
    scale_x_date(limits=c(min(df_join$Day) - 1, max(df_join$Day) + 1), date_labels = "%b %d", breaks = "1 day") +
    labs(title=main_title, x="Day", y="Proportion of Tweets\nContaining Low-Trust URLs (%)") +
    scale_y_continuous(labels = percent_format()) +
    ggtitle(main_title) +
    custom_theme +
    theme(panel.grid.major.x = element_blank())
}

  covid_proportion_plot <- plot_proportion_of_lowtrust_tweets(covid_tweets_lowtrust, covid_tweets_flat, "COVID-19 Data",  "#e4e470f6","#D4AF37")
  climate_proportion_plot <- plot_proportion_of_lowtrust_tweets(climate_tweets_lowtrust, climate_tweets_flat, "Climate Change Data",  "#CCE5FF", "#0077B6")

  proportion_full = grid.arrange(covid_proportion_plot, climate_proportion_plot)

  ggsave("prop_full.png", plot = proportion_full, width = 14, height = 8, dpi = 300)

#######################################################################################################################
  # Plot the most common low-trust domains for each dataset #
#######################################################################################################################

  plot_top_domains <- function(data, title, fill_color) {
    data %>%
      count(domains, sort = TRUE) %>%
      slice_max(n = 15, n) %>%
      ggplot(aes(x = n, y = fct_reorder(domains, n))) +
      geom_col(fill = fill_color, color = NA,alpha=0.7) +
      scale_fill_continuous(palette = fill_color) +
      theme_minimal() +
      theme(plot.margin = unit(c(1, 5, 1, 1), "cm")) +
      theme(legend.position = "none",
            plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
            axis.title.x = element_text(size = 28),
            axis.title.y = element_text(size = 28),
            axis.text.x = element_text(size = 26),
            axis.text.y = element_text(size = 26)) +
      labs(x = "Frequency", y = "Domain", title = title)
}

  covid_top_domains <- plot_top_domains(covid_tweets_lowtrust, "Top 10 Domains in Low-Trust Covid Tweets", "#D4AF37")
  climate_top_domains <- plot_top_domains(climate_tweets_lowtrust, "Top 10 Domains in Low-Trust Climate Tweets", "#5B6B8F")

  full_domains = grid.arrange(covid_top_domains,climate_top_domains,nrow=1)
  ggsave("fulldomains.png", plot = full_domains, width = 28, height = 12, dpi = 300)

#####################################################################################################################
  # Plot Full Distribution with Violin Plot #
#####################################################################################################################

  climate_final_results = read.csv('climate_results.csv')
  covid_final_results = read.csv('covid_results.csv')

  violin_plots <- function(climate_final_results, covid_final_results) {

  custom_colors <- c("#5B6B8F", "#D4AF37")

  custom_theme <- theme_bw(base_size = 16) +
      theme(panel.border = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.line.x = element_line(color = "black", size = 0.2),
            axis.line.y = element_line(color = "black", size = 0.2),
            axis.ticks = element_line(size = 1),
            axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 18),
            axis.title.x = element_text(size = 20, face = "bold"),
            axis.title.y = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
            plot.caption = element_text(size = 12, face = "bold"),
            legend.position = "none",
            legend.box = "none",
            plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))
  
  full_data <- rbind(
    mutate(climate_final_results, category = "Climate Change"),
    mutate(covid_final_results, category = "COVID-19")
  )

    ggplot(full_data, aes(x = category, y = impressions_performance, fill = category)) +
      geom_violin(
        width = 0.8,
        outlier.size = 0.2,
        outlier.stroke = 0.2,
        fatten = 1,
        alpha = 0.65,
        adjust = 2,
        trim = FALSE
      ) +
      geom_boxplot(
        width = 0.05,
      outlier.size = 0.2,
      outlier.stroke = 0.1,
        alpha = 0.1
      ) +
      stat_summary(fun = mean, geom = "point", aes(group = category), shape = 21, size = 2) +
      scale_fill_manual(values = custom_colors, guide = "none") +
      custom_theme +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
      labs(x = "Dataset", y = "Impressions Performance") +
      coord_cartesian(ylim = c(-100, 2000))

}

  violins = violin_plots(climate_final_results, covid_final_results)
  ggsave("violins.png", plot = violins, width = 7, height = 8, dpi = 300)

#####################################################################################################################
  # Plot Error Bars with Bootstrapping #
#####################################################################################################################

  plot_errorbars <- function(data,ylimits,title) {
    library(scales)
    library(gridExtra)
    library(grid)

  custom_theme <- theme_bw(base_size = 16) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_line(color = "grey80"),
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          axis.ticks = element_line(size = 1),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16,face = "bold"),
          axis.title.y = element_text(size = 16,face = "bold"),
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5,face = "bold"),
          plot.caption = element_text(size = 12,face = "bold"),
          legend.position = "none",
          legend.box = "none", 
          plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

    data$engagement_level <- ifelse(data$engagement_level == "low_engagement", "Low", 
                                       ifelse(data$engagement_level == "mid_engagement", "Mid", 
                                              ifelse(data$engagement_level == "high_engagement", "High", 
                                                     data$engagement_level)))

    data$sentiment_label <- ifelse(data$sentiment_label == "negative", "Negative", 
                                       ifelse(data$sentiment_label == "neutral", "Neutral", 
                                              ifelse(data$sentiment_label == "positive", "Positive", 
                                                     data$sentiment_label)))                                                      
  # Reshape data
    data <- data %>%
      mutate(engagement_label = factor(engagement_level, levels = unique(engagement_level), labels = paste("Engagement - ", unique(engagement_level), sep = " "))) %>%
      mutate(sentiment_label = factor(sentiment_label, levels = unique(sentiment_label), labels = paste("Sentiment - ", unique(sentiment_label), sep = " "))) %>%
      select(engagement_label, sentiment_label, impressions_performance) %>%
      gather(key = "category", value = "label", engagement_label, sentiment_label) %>%
      filter(!is.na(label))
  
    # Generate bootstrap samples
    n_bootstraps <- 1000
    bootstrap_means <- data %>%
      group_by(category, label) %>%
      do(data.frame(t(replicate(n_bootstraps, mean(sample(.$impressions_performance, replace = TRUE)))))) %>%
      gather(bootstrap_id, value, -category, -label)
    
    # Calculate mean and standard error for each group
    mean_and_error <- bootstrap_means %>%
      group_by(category, label) %>%
      summarise(mean = mean(value), se = sd(value))
    
    # Create plot
  ggplot(mean_and_error, aes(x = label, y = mean, ymin = mean - se, ymax = mean + se, group = category)) +
    geom_linerange(size = 2, aes(color = category), alpha = 0.8) +
    geom_point(size = 5, aes(color = category), alpha = 0.8, shape = 19) +
    xlab("Stratum Feature") +
    ylab("Impressions Performance") +
    ggtitle(title) +
    theme_minimal() +
    custom_theme +
    coord_flip() +
    scale_color_manual(values = c("#5B6B8F", "#D4AF37")) +
    ylim(ylimits) +
    theme(panel.grid.major = element_blank())
        }

  covid_error_plots <- plot_errorbars(covid_final_results, ylimits = c(200, 450), 'COVID-19 Data')
  climate_error_plots <- plot_errorbars(climate_final_results,ylimits = c(100, 250),'Climate Change Data')

  error_bars = grid.arrange(covid_error_plots, climate_error_plots, nrow=2)

  ggsave("errbars.png", plot = error_bars, width = 9, height = 10, dpi = 300)

#####################################################################################################################
  # Plot CDF Distribution #
#####################################################################################################################

  cdf_plot <- function(data, title) {

   custom_theme <- theme_bw(base_size = 16) +
    theme(panel.grid.major = element_line(color = "grey", size = 0.5, linetype = "solid"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          axis.ticks = element_line(size = 1),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
          axis.title.x = element_text(size = 19, face = "bold"),
          axis.title.y = element_text(size = 19, face = "bold"),
          plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 12, face = "bold"),
          plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))
  
  ggplot(data, aes(x = impressions_performance, color = factor(stratum))) +
    stat_ecdf(geom = "line") +
    labs(x = "Impressions Performance",
         y = "Cumulative Probability",
         color = "Stratum") +
    theme_minimal() +
    scale_color_discrete(name = "Stratum") +
    ggtitle(title) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    custom_theme
}

  covid_cdf = cdf_plot(covid_final_results, 'COVID-19 Data')
  climate_cdf = cdf_plot(climate_final_results, 'Climate Change Data')
  full_cdf = grid.arrange(covid_cdf, climate_cdf, nrow=2)

  ggsave("cdf.png", plot = full_cdf, width = 10, height = 12, dpi = 300)

#######################################################################################################################
  # Plot boxplot with performance by political bias  #
#######################################################################################################################

  climate_bias = read.csv('climate_bias.csv')
  covid_bias = read.csv('covid_bias.csv')

  climate_bias %>% count(bias_rating,sort=TRUE)
  climate_bias$bi

  bias_preprocess <- function(data) {
  filtered_data <- data %>% filter(!bias_rating %in% c("Bias Rating Not Found"))

  bias_rating_counts <- filtered_data %>% group_by(bias_rating) %>%
  summarise(count = n()) %>% mutate(percentage = count / sum(count) * 100)
  relevant_bias_ratings <- bias_rating_counts %>% filter(percentage >= 3) %>% select(bias_rating)
  filtered_data <- filtered_data %>% filter(bias_rating %in% relevant_bias_ratings$bias_rating)
  filtered_data$bias_rating <- ifelse(filtered_data$bias_rating == "FAR RIGHT", "Far Right",
  ifelse(filtered_data$bias_rating == "RIGHT", "Right", filtered_data$bias_rating))

  return(filtered_data)
  }

climate_bias = bias_preprocess(climate_bias)
covid_bias = bias_preprocess(covid_bias)

  plot_bias <- function(data1, data2) {
    library(scales)
    library(gridExtra)
    library(grid)
    library(dplyr)
    library(ggplot2)

    custom_theme <- theme_bw(base_size = 16) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_line(color = "grey80"),
            axis.line.x = element_line(color = "black", size = 0.2),
            axis.line.y = element_line(color = "black", size = 0.2),
            axis.ticks = element_line(size = 1),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 15, face = "bold",vjust = -1),
            axis.title.y = element_text(size = 15, face = "bold", angle = 90, vjust = 1),
            plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
            plot.caption = element_text(size = 12, face = "bold"),
            legend.position = "none",
            legend.box = "none", 
            plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

  process_data <- function(data, dataset_name) {
    # Reshape data
    data <- data %>%
      select(bias_rating, impressions_performance) %>%
      filter(!is.na(bias_rating)) %>%
      mutate(dataset = dataset_name)

    # Generate bootstrap samples
    n_bootstraps <- 1000
    bootstrap_means <- data %>%
      group_by(bias_rating, dataset) %>%
      do(data.frame(t(replicate(n_bootstraps, mean(sample(.$impressions_performance, replace = TRUE)))))) %>%
      gather(bootstrap_id, value, -bias_rating, -dataset)

    # Calculate mean and standard error for each group
  mean_and_error <- bootstrap_means %>%
  group_by(bias_rating, dataset) %>%
  summarise(mean = mean(value), se = sd(value), .groups = "drop")

    return(mean_and_error)
  }

  mean_and_error1 <- process_data(data1, "COVID-19")
  mean_and_error2 <- process_data(data2, "Climate Change")
  mean_and_error_combined <- rbind(mean_and_error1, mean_and_error2)

  # Create plot
  ggplot(mean_and_error_combined, aes(x = bias_rating, y = mean, ymin = mean - se, ymax = mean + se, color = dataset)) +
  geom_linerange(size = 2, alpha = 0.8, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, alpha = 0.8, shape = 19, position = position_dodge2(width = 0.5)) +
  xlab("Bias Rating") +
  ylab("Impressions Performance") +
  scale_color_manual(values = c("#5B6B8F", "#D4AF37")) +
  theme_minimal() +
  custom_theme +
  theme(legend.position = "right") +
  labs(color = "Data") +
  theme(panel.grid.major = element_blank())
}

  bias_plot= plot_bias(covid_bias, climate_bias)
  ggsave("bias.png", plot = bias_plot, width = 5, height = 7, dpi = 300)

#######################################################################################################################
  # Plot boxplot with performance by verified status  #
#######################################################################################################################

  plot_verified <- function(data1, data2) {
    library(scales)
    library(gridExtra)
    library(grid)
    library(dplyr)
    library(ggplot2)

  custom_theme <- theme_bw(base_size = 16) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_line(color = "grey80"),
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          axis.ticks = element_line(size = 1),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 15, face = "bold",vjust = -1),
          axis.title.y = element_text(size = 15, face = "bold", angle = 90, vjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 12, face = "bold"),
          legend.position = "none",
          legend.box = "none", 
          plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

  process_data <- function(data, dataset_name) {
    # Reshape data
    data <- data %>%
      select(verified, impressions_performance) %>%
      filter(!is.na(verified)) %>%
      mutate(dataset = dataset_name)

    # Generate bootstrap samples
    n_bootstraps <- 1000
    bootstrap_means <- data %>%
      group_by(verified, dataset) %>%
      do(data.frame(t(replicate(n_bootstraps, mean(sample(.$impressions_performance, replace = TRUE)))))) %>%
      gather(bootstrap_id, value, -verified, -dataset)

    # Calculate mean and standard error for each group
  mean_and_error <- bootstrap_means %>%
  group_by(verified, dataset) %>%
  summarise(mean = mean(value), se = sd(value), .groups = "drop")

    return(mean_and_error)
  }

  mean_and_error1 <- process_data(data1, "COVID-19")
  mean_and_error2 <- process_data(data2, "Climate Change")
  mean_and_error_combined <- rbind(mean_and_error1, mean_and_error2)

  # Create plot
  ggplot(mean_and_error_combined, aes(x = verified, y = mean, ymin = mean - se, ymax = mean + se, color = dataset)) +
  geom_linerange(size = 2, alpha = 0.8, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, alpha = 0.8, shape = 19, position = position_dodge2(width = 0.5)) +
  xlab("Verified Status") +
  ylab("Impressions Performance") +
  scale_color_manual(values = c("#5B6B8F", "#D4AF37")) +
  theme_minimal() +
  custom_theme +
  theme(legend.position = "right") +
  labs(color = "Data") +
  theme(panel.grid.major = element_blank()) +
  ylim(c(10,400))
}

  verified_plot = plot_verified(covid_final_results, climate_final_results)
  ggsave("verified.png", plot = verified_plot, width = 5, height = 7, dpi = 300)

#######################################################################################################################
  # Sentiment Plot #
#######################################################################################################################

  plot_sentiment <- function(covid_lowtrust, climate_lowtrust, covid_timelines, climate_timelines) {
    library(scales)
    library(gridExtra)
    library(grid)
    library(dplyr)
    library(ggplot2)

  custom_theme <- theme_bw(base_size = 16) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          axis.ticks = element_line(size = 1),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 12, face = "bold"),
          plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

  climate_lowtrust$dataset = 'Climate Low-Trust'
  covid_lowtrust$dataset = 'COVID Low-Trust'
  climate_timelines$dataset = 'Climate Timelines'
  covid_timelines$dataset = 'COVID Timelines'

  combined_data <- rbind(climate_lowtrust, covid_lowtrust,climate_timelines,covid_timelines)

  summary_data <- combined_data %>% 
    group_by(dataset, sentiment_label) %>% 
    summarize(n = n())

  summary_data <- summary_data %>% 
    group_by(dataset) %>% 
    mutate(percentage = n / sum(n) * 100,
           sentiment_label = reorder(sentiment_label, -percentage))

  # Create the plot
  ggplot(summary_data, aes(x = dataset, y = percentage, fill = sentiment_label)) +
    geom_bar(stat = "identity",alpha=0.8) +
    geom_text(aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 4, color = "white") +
    labs(x = "Dataset", y = "Percentage", fill = "Sentiment Label",
    title = "Sentiment Label Percentage by Dataset") +
    scale_fill_manual(values = c("#5B6B8F", "#D4AF37", "#3B7861")) +
    custom_theme
    }

  sentiment = plot_sentiment(covid_tweets_lowtrust,climate_tweets_lowtrust,user_timelines_covid,user_timelines_climate)

  ggsave("sentiment_plot.png", plot = sentiment, width = 11, height = 7, dpi = 300)

#######################################################################################################################
  # Engagement Plot #
#######################################################################################################################

  plot_engagement <- function(covid_lowtrust, climate_lowtrust, covid_timelines, climate_timelines) {
    library(scales)
    library(gridExtra)
    library(grid)
    library(dplyr)
    library(ggplot2)

  custom_theme <- theme_bw(base_size = 16) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          axis.ticks = element_line(size = 1),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 12, face = "bold"),
          plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

    climate_lowtrust$dataset = 'Climate Low-Trust'
    covid_lowtrust$dataset = 'COVID Low-Trust'
    climate_timelines$dataset = 'Climate Timelines'
    covid_timelines$dataset = 'COVID Timelines'

    combined_data <- rbind(climate_lowtrust, covid_lowtrust,climate_timelines,covid_timelines)

    summary_data <- combined_data %>% 
      group_by(dataset, engagement_level) %>% 
      summarize(n = n())

    summary_data <- summary_data %>% 
      group_by(dataset) %>% 
      mutate(percentage = n / sum(n) * 100,
            engagement_level = reorder(engagement_level, -percentage))

    # Create the plot
    ggplot(summary_data, aes(x = dataset, y = percentage, fill = engagement_level)) +
      geom_bar(stat = "identity",alpha=0.8) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
      position = position_stack(vjust = 0.5),
      size = 4, color = "white") +
      labs(x = "Dataset", y = "Percentage", fill = "Engagement Label",
      title = "Engagement Level Percentage by Dataset") +
      scale_fill_manual(values = c("#5B6B8F", "#D4AF37", "#3B7861")) +
      custom_theme
  }


  engagement_plot = plot_engagement(covid_tweets_lowtrust,climate_tweets_lowtrust,user_timelines_covid,user_timelines_climate)

  ggsave("engagement_plot.png", plot = engagement_plot, width = 11, height = 7, dpi = 300)

#######################################################################################################################
  # </end> #
#######################################################################################################################