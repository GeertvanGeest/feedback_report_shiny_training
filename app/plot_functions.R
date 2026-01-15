# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggthemes)

# Helper function to find a question in metadata by question_text
find_question_in_metadata <- function(question_metadata, variable) {
  for (i in seq_along(question_metadata$sections$title)) {
    section_questions <- question_metadata$sections$questions[[i]]
    for (j in seq_len(nrow(section_questions))) {
      if (section_questions$question_text[j] == variable) {
        return(list(
          question_text = section_questions$question_text[j],
          question_type = section_questions$question_type[j],
          is_ordinal = section_questions$is_ordinal[j],
          possible_answers = section_questions$possible_answers[[j]]
        ))
      }
    }
  }
  return(NULL)
}

# Create a pie chart for multiple choice questions
pie_chart <- function(feedback_results, variable, question_metadata, metadata_text = NULL) {
  
  # Find the matching question in metadata (use metadata_text if provided)
  lookup_text <- if (!is.null(metadata_text)) metadata_text else variable
  question <- find_question_in_metadata(question_metadata, lookup_text)
  
  possible_answers <- if (!is.null(question) && !is.null(question$possible_answers)) {
    question$possible_answers
  } else {
    NULL
  }
  
  # Calculate counts and percentages
  plot_data <- feedback_results %>%
    count(.data[[variable]]) %>%
    filter(!is.na(.data[[variable]])) %>%
    mutate(
      percentage = n / sum(n) * 100,
      label = paste0(.data[[variable]], "\n(", round(percentage, 1), "%)")
    )
  
  # If possible_answers provided, ensure all answers are included (even with 0 count)
  if (!is.null(possible_answers)) {
    # Create a complete data frame with all possible answers
    complete_data <- data.frame(
      answer = possible_answers,
      stringsAsFactors = FALSE
    )
    names(complete_data)[1] <- variable
    
    # Left join to include all possible answers
    plot_data <- complete_data %>%
      left_join(plot_data, by = variable) %>%
      mutate(
        n = replace_na(n, 0),
        percentage = replace_na(percentage, 0)
      ) %>%
      mutate(!!variable := factor(.data[[variable]], levels = possible_answers))
  }
  
  # Filter out zero counts for pie chart display (but keep all in data for legend)
  plot_data_display <- plot_data %>% filter(n > 0)
  
  # Create pie chart with appealing colors
  # Use full plot_data for legend, but only non-zero for actual pie
  p <- ggplot(plot_data, aes(x = "", y = n, fill = .data[[variable]])) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
    coord_polar("y", start = 0) +
    geom_text(data = plot_data_display,
              aes(label = paste0(round(percentage, 1), "%")),
              position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
    labs(fill = "") +
    theme_void() +
    theme(legend.position = "right",
          legend.text = element_text(size = 12)) +
    scale_fill_tableau(palette = "Nuriel Stone", drop = FALSE)
  
  p
}

# Create a horizontal bar chart for multiple choice and multiple select questions
bar_chart <- function(feedback_results, variable, question_metadata, metadata_text = NULL) {
  
  # Find the matching question in metadata (use metadata_text if provided)
  lookup_text <- if (!is.null(metadata_text)) metadata_text else variable
  question <- find_question_in_metadata(question_metadata, lookup_text)
  
  possible_answers <- if (!is.null(question) && !is.null(question$possible_answers)) {
    question$possible_answers
  } else {
    NULL
  }
  
  # Check if this is a multiple_select question
  is_multiple_select <- !is.null(question) && question$question_type == "multiple_select"
  
  if (is_multiple_select) {
    # Split semicolon-separated values and count each answer
    plot_data <- feedback_results %>%
      select(all_of(variable)) %>%
      filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
      mutate(answers = strsplit(as.character(.data[[variable]]), ";")) %>%
      unnest(answers) %>%
      mutate(answers = trimws(answers)) %>%
      filter(answers != "") %>%
      count(answers, name = "n")
    
    # If possible_answers provided, ensure all answers are included (even with 0 count)
    if (!is.null(possible_answers)) {
      complete_data <- data.frame(
        answers = possible_answers,
        stringsAsFactors = FALSE
      )

      plot_data <- complete_data %>%
        left_join(plot_data, by = "answers") %>%
        mutate(n = replace_na(n, 0)) %>%
        mutate(answers = factor(answers, levels = rev(possible_answers)))
      
      y_var <- plot_data$answers
    } else {
      y_var <- reorder(plot_data$answers, plot_data$n)
    }
    
    # Create horizontal bar chart
    ggplot(plot_data, aes(x = n, y = y_var)) +
      geom_bar(stat = "identity", fill = "#4E79A7", width = 0.7) +
      geom_text(aes(label = n), hjust = -0.3, size = 4) +
      labs(x = "Frequency", y = "") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  } else {
    # Regular multiple choice
    plot_data <- feedback_results %>%
      count(.data[[variable]]) %>%
      filter(!is.na(.data[[variable]]))
    
    # If possible_answers provided, ensure all answers are included (even with 0 count)
    if (!is.null(possible_answers)) {
      complete_data <- data.frame(
        answer = possible_answers,
        stringsAsFactors = FALSE
      )
      names(complete_data)[1] <- variable
      
      plot_data <- complete_data %>%
        left_join(plot_data, by = variable) %>%
        mutate(n = replace_na(n, 0)) %>%
        mutate(!!variable := factor(.data[[variable]], levels = rev(possible_answers)))
    } else {
      # No ordering specified, order by frequency
      plot_data <- plot_data %>%
        mutate(!!variable := reorder(.data[[variable]], n))
    }
    
    # Create horizontal bar chart
    ggplot(plot_data, aes(x = n, y = .data[[variable]])) +
      geom_bar(stat = "identity", fill = "#4E79A7", width = 0.7) +
      geom_text(aes(label = n), hjust = -0.3, size = 4) +
      labs(x = "Count", y = "") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  }
}


