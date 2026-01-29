# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggthemes)
library(stringr)
library(wordcloud)
library(RColorBrewer)

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

# Create a word cloud visualization for open text questions
word_cloud <- function(feedback_results, variable, max_words = 100, min_freq = 1) {
  
  # Common English stop words (expanded list)
  stop_words <- c(
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours",
    "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers",
    "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves",
    "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are",
    "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does",
    "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until",
    "while", "of", "at", "by", "for", "with", "about", "against", "between", "into",
    "through", "during", "before", "after", "above", "below", "to", "from", "up", "down",
    "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here",
    "there", "when", "where", "why", "how", "all", "both", "each", "few", "more", "most",
    "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than",
    "too", "very", "s", "t", "can", "will", "just", "don", "should", "now", "would",
    "could", "also", "one", "two", "get", "make", "like", "well", "much", "many", "way",
    "use", "used", "etc", "e.g", "quite", "really", "think", "see", "go", "come", "want",
    "know", "take", "give", "find", "tell", "ask", "work", "seem", "feel", "try", "leave",
    "call", "may", "might", "must", "shall", "let", "say", "said", "even", "back", "good",
    "new", "first", "last", "long", "great", "little", "own", "old", "right", "big", "high",
    "small", "large", "next", "course"
  )
  
  # Collect and process text
  text_data <- feedback_results %>%
    select(all_of(variable)) %>%
    filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
    pull(.data[[variable]])
  
  if (length(text_data) == 0) {
    # Return empty plot with message
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No text data available", size = 6) +
        theme_void()
    )
  }
  
  # Tokenize and count words
  words <- text_data %>%
    paste(collapse = " ") %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("[[:digit:]]", " ") %>%
    str_split("\\s+") %>%
    unlist() %>%
    .[nchar(.) > 2] %>%  # Remove very short words
    .[!(. %in% stop_words)]
  
  if (length(words) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No meaningful words found", size = 6) +
        theme_void()
    )
  }
  
  # Count word frequencies
  word_freq <- table(words)
  word_freq <- sort(word_freq, decreasing = TRUE)
  
  # Filter by minimum frequency and limit to max words
  word_freq <- word_freq[word_freq >= min_freq]
  if (length(word_freq) > max_words) {
    word_freq <- word_freq[1:max_words]
  }
  
  if (length(word_freq) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No frequent words found", size = 6) +
        theme_void()
    )
  }
  
  # Create a plotting function that captures the wordcloud output
  # We need to use a custom plotting device since wordcloud doesn't return a ggplot
  plot_function <- function() {
    # Set up color palette
    colors <- brewer.pal(8, "Dark2")
    
    # Create word cloud
    wordcloud::wordcloud(
      words = names(word_freq),
      freq = as.numeric(word_freq),
      min.freq = min_freq,
      max.words = max_words,
      random.order = FALSE,
      rot.per = 0.35,
      colors = colors,
      scale = c(4, 0.5),
      random.color = FALSE
    )
  }
  
  # Return the plotting function wrapped so it can be called by ggplot_to_base64
  return(plot_function)
}
