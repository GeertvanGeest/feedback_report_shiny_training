# Shared metadata resolver utilities.
# Supports a single base question catalog and lightweight profile overrides.

# Resolve a metadata path relative to the profile JSON file.
resolve_metadata_path <- function(profile_path, metadata_path) {
  if (is.null(metadata_path) || !nzchar(metadata_path)) {
    stop("metadata_path must be a non-empty string")
  }

  if (grepl("^(/|[A-Za-z]:[/\\\\])", metadata_path)) {
    return(metadata_path)
  }

  candidate_relative <- file.path(dirname(profile_path), metadata_path)
  if (file.exists(candidate_relative)) {
    return(candidate_relative)
  }

  if (file.exists(metadata_path)) {
    return(metadata_path)
  }

  candidate_root <- file.path(getwd(), metadata_path)
  if (file.exists(candidate_root)) {
    return(candidate_root)
  }

  # Return best-effort candidate for clearer error messages upstream.
  candidate_relative
}

merge_named_list <- function(base_item, overrides) {
  if (is.null(overrides) || !is.list(overrides)) {
    return(base_item)
  }

  for (nm in names(overrides)) {
    base_item[[nm]] <- overrides[[nm]]
  }

  base_item
}

normalize_question_overrides <- function(question_overrides) {
  if (is.null(question_overrides)) {
    return(list())
  }

  # Named list style: { "variable_name": { ...fields... } }
  if (!is.null(names(question_overrides)) && all(nzchar(names(question_overrides)))) {
    return(question_overrides)
  }

  # Array style: [{"variable_name": "x", ...fields...}, ...]
  normalized <- list()
  for (entry in question_overrides) {
    if (is.null(entry$variable_name) || !nzchar(entry$variable_name)) {
      next
    }

    variable_name <- entry$variable_name
    entry$variable_name <- NULL
    normalized[[variable_name]] <- entry
  }

  normalized
}

apply_section_overrides <- function(sections, section_overrides) {
  if (is.null(section_overrides) || length(sections) == 0) {
    return(sections)
  }

  # Named list style: { "Section title": { ...fields... } }
  named_style <- !is.null(names(section_overrides)) && all(nzchar(names(section_overrides)))

  for (idx in seq_along(sections)) {
    section <- sections[[idx]]
    override <- NULL

    if (named_style && !is.null(section$title) && section$title %in% names(section_overrides)) {
      override <- section_overrides[[section$title]]
    } else if (!named_style) {
      for (candidate in section_overrides) {
        if (!is.null(candidate$title) && identical(candidate$title, section$title)) {
          candidate$title <- NULL
          override <- candidate
          break
        }
      }
    }

    if (!is.null(override)) {
      sections[[idx]] <- merge_named_list(section, override)
    }
  }

  sections
}

apply_question_overrides <- function(sections, question_overrides) {
  override_map <- normalize_question_overrides(question_overrides)
  if (length(override_map) == 0 || length(sections) == 0) {
    return(sections)
  }

  for (section_idx in seq_along(sections)) {
    questions <- sections[[section_idx]]$questions
    if (is.null(questions) || length(questions) == 0) {
      next
    }

    for (q_idx in seq_along(questions)) {
      variable_name <- questions[[q_idx]]$variable_name
      if (is.null(variable_name) || !variable_name %in% names(override_map)) {
        next
      }

      sections[[section_idx]]$questions[[q_idx]] <- merge_named_list(
        questions[[q_idx]],
        override_map[[variable_name]]
      )
    }
  }

  sections
}

apply_section_order <- function(sections, section_order) {
  if (length(sections) == 0 || is.null(section_order) || length(section_order) == 0) {
    return(sections)
  }

  current_titles <- vapply(sections, function(s) {
    if (is.null(s$title)) "" else as.character(s$title)
  }, character(1))

  ordered_indices <- integer(0)
  for (title in section_order) {
    matched <- which(current_titles == title)
    if (length(matched) > 0) {
      ordered_indices <- c(ordered_indices, matched[1])
    }
  }

  remaining <- setdiff(seq_along(sections), ordered_indices)
  sections[c(ordered_indices, remaining)]
}

apply_visibility_rules <- function(sections, visibility) {
  if (is.null(visibility) || length(sections) == 0) {
    return(sections)
  }

  section_titles <- vapply(sections, function(s) {
    if (is.null(s$title)) "" else as.character(s$title)
  }, character(1))

  include_sections <- visibility$include_sections
  exclude_sections <- visibility$exclude_sections
  include_questions <- visibility$include_questions
  exclude_questions <- visibility$exclude_questions
  drop_empty_sections <- visibility$drop_empty_sections
  if (is.null(drop_empty_sections)) {
    drop_empty_sections <- TRUE
  }

  keep_sections <- rep(TRUE, length(sections))

  if (!is.null(include_sections) && length(include_sections) > 0) {
    keep_sections <- keep_sections & section_titles %in% include_sections
  }

  if (!is.null(exclude_sections) && length(exclude_sections) > 0) {
    keep_sections <- keep_sections & !(section_titles %in% exclude_sections)
  }

  sections <- sections[keep_sections]

  if (length(sections) == 0) {
    return(sections)
  }

  for (idx in seq_along(sections)) {
    questions <- sections[[idx]]$questions
    if (is.null(questions) || length(questions) == 0) {
      next
    }

    question_names <- vapply(questions, function(q) {
      if (is.null(q$variable_name)) "" else as.character(q$variable_name)
    }, character(1))

    keep_questions <- rep(TRUE, length(questions))

    if (!is.null(include_questions) && length(include_questions) > 0) {
      keep_questions <- keep_questions & question_names %in% include_questions
    }

    if (!is.null(exclude_questions) && length(exclude_questions) > 0) {
      keep_questions <- keep_questions & !(question_names %in% exclude_questions)
    }

    sections[[idx]]$questions <- questions[keep_questions]
  }

  if (isTRUE(drop_empty_sections)) {
    sections <- Filter(function(s) {
      !is.null(s$questions) && length(s$questions) > 0
    }, sections)
  }

  sections
}

# Resolve question metadata into the original data-frame-oriented structure
# expected by report/plot processing code.
resolve_question_metadata <- function(metadata_file) {
  profile <- jsonlite::fromJSON(metadata_file, simplifyVector = FALSE)

  if (is.null(profile$base_metadata) || !nzchar(profile$base_metadata)) {
    return(jsonlite::fromJSON(metadata_file))
  }

  base_path <- resolve_metadata_path(metadata_file, profile$base_metadata)
  if (!file.exists(base_path)) {
    stop("Base metadata file not found: ", base_path)
  }

  base <- jsonlite::fromJSON(base_path, simplifyVector = FALSE)
  resolved <- base

  control_keys <- c(
    "base_metadata",
    "visibility",
    "section_overrides",
    "question_overrides",
    "section_order"
  )

  for (nm in names(profile)) {
    if (nm %in% control_keys || nm == "sections") {
      next
    }
    resolved[[nm]] <- profile[[nm]]
  }

  if (!is.null(profile$sections)) {
    # Full replacement escape hatch.
    resolved$sections <- profile$sections
  }

  if (is.null(resolved$sections) || !is.list(resolved$sections)) {
    stop("Resolved metadata must contain a 'sections' list")
  }

  resolved$sections <- apply_section_overrides(resolved$sections, profile$section_overrides)
  resolved$sections <- apply_question_overrides(resolved$sections, profile$question_overrides)
  resolved$sections <- apply_visibility_rules(resolved$sections, profile$visibility)
  resolved$sections <- apply_section_order(resolved$sections, profile$section_order)

  # Convert back to the same simplified structure produced by jsonlite::fromJSON,
  # so existing code can keep using section data frames without changes.
  serialized <- jsonlite::toJSON(
    resolved,
    auto_unbox = TRUE,
    null = "null",
    pretty = FALSE
  )

  jsonlite::fromJSON(serialized)
}
