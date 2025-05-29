# --------------------------------------------------------------------------
# Script to copy the LOWEST AVAILABLE NUMBERED RUN (e.g., '1-', '2-', etc.)
# of SDM outputs and rename it to 'mean_'.
# Assumes the R working directory is 'anemonefish_post_analysis/'
# --------------------------------------------------------------------------

# No special packages needed beyond base R for file operations

# 1. Define base directories (relative to the project root 'anemonefish_post_analysis/')
relative_input_base_dir <- "data/output/predictions"
# Using a distinct name to avoid conflict if you run both scripts.
# Change to "data/output/mean_predictions" if you want to overwrite or use the same folder.
relative_output_base_dir <- "data/output/mean_predictions_from_lowest_run"

# Create the main output directory if it doesn't exist
if (!dir.exists(relative_output_base_dir)) {
  dir.create(relative_output_base_dir, recursive = TRUE)
  message("Created base output directory: ", normalizePath(relative_output_base_dir))
} else {
  message("Base output directory already exists: ", normalizePath(relative_output_base_dir))
}

# Normalize input base directory path for internal use.
absolute_input_base_dir <- normalizePath(relative_input_base_dir, mustWork = TRUE)
if (!endsWith(absolute_input_base_dir, .Platform$file.sep)) {
  absolute_input_base_dir <- paste0(absolute_input_base_dir, .Platform$file.sep)
}
message("Using absolute input base directory: ", absolute_input_base_dir)


# 2. Find all prediction .tif files recursively
all_pred_files <- list.files(
  path = relative_input_base_dir,
  pattern = "^\\d+-pred_.*\\.tif$", # Starts with number-pred_ and ends with .tif
  recursive = TRUE,
  full.names = TRUE
)

if (length(all_pred_files) == 0) {
  stop("No prediction files found matching the pattern in: ", absolute_input_base_dir)
}
message("Found ", length(all_pred_files), " potential prediction files to process.")

# 3. Group files by their unique identifier and original directory
file_info <- data.frame(
  full_path = all_pred_files,
  stringsAsFactors = FALSE
)
file_info$path_suffix <- sub(pattern = fixed(absolute_input_base_dir), replacement = "", x = file_info$full_path)
file_info$original_relative_dir <- dirname(file_info$path_suffix)
file_info$original_relative_dir[file_info$original_relative_dir == ""] <- "."
file_info$filename <- basename(file_info$full_path)
file_info$identifier_stem <- sub("^\\d+-", "", file_info$filename)
file_info$group_key <- paste(file_info$original_relative_dir, file_info$identifier_stem, sep = "///")
grouped_files_list <- split(file_info$full_path, file_info$group_key)

message("Identified ", length(grouped_files_list), " unique groups of predictions.")

# 4. Process each group to copy the lowest available numbered run file
processed_groups_count <- 0
copied_files_count <- 0
skipped_groups_no_valid_runs <- 0
problematic_group_keys <- 0 # For groups that are unexpectedly empty

for (group_key in names(grouped_files_list)) {
  files_in_group <- grouped_files_list[[group_key]]
  
  key_parts <- strsplit(group_key, "///", fixed = TRUE)[[1]]
  group_original_relative_dir <- key_parts[1]
  group_identifier_stem <- key_parts[2]
  
  if (length(files_in_group) == 0) {
    warning(paste0("CRITICAL-LOGIC-ERROR: Group key '", group_key, "' exists but has no files associated. Skipping."))
    problematic_group_keys <- problematic_group_keys + 1
    next
  }
  
  # Create a data frame of (path, run_number) for valid files in this group
  candidate_files_data <- data.frame(
    path = character(),
    run = integer(),
    stringsAsFactors = FALSE
  )
  
  for (f_path in files_in_group) {
    fname <- basename(f_path)
    
    # Regex to match "NUMBER-" prefix
    run_num_prefix_match <- regexpr("^(\\d+)-", fname) 
    
    if (run_num_prefix_match != -1 && attr(run_num_prefix_match, "match.length") > 1) {
      # Extract run number string (e.g., "1", "23")
      run_num_str <- substr(fname, 1, attr(run_num_prefix_match, "match.length") - 1)
      run_num <- as.integer(run_num_str)
      
      # Extract the suffix part of the filename after "NUMBER-"
      actual_suffix_from_file <- substr(fname, attr(run_num_prefix_match, "match.length") + 1, nchar(fname))
      
      # Check if this suffix matches the group's identifier_stem
      if (actual_suffix_from_file == group_identifier_stem) {
        candidate_files_data <- rbind(candidate_files_data, data.frame(path = f_path, run = run_num))
      } else {
        # This case implies a file like "1-pred_A_extra.tif" might be in a group for "pred_A.tif"
        # This should ideally be caught by the grouping logic, but acts as a safeguard.
        warning(paste0("File '", fname, "' in group for stem '", group_identifier_stem, 
                       "' has a run number prefix, but its suffix '", actual_suffix_from_file, 
                       "' does not match. Skipping this specific file from consideration for the group."))
      }
    } else {
      # This file was in `all_pred_files` (so matched `^\d+-pred_.*\.tif$`)
      # but failed the more specific `^(\d+)-` parsing here.
      # This could happen if the filename is like "1pred_..." (no hyphen).
      # The initial pattern `^\d+-pred_` should make this rare.
      message(paste0("DEBUG: File '", fname, "' (path: ", f_path, ") from group '", group_identifier_stem, 
                     "' did not fully match 'NUMBER-identifier_stem' pattern during detailed parsing. ",
                     "It might have been caught by the global pattern but failed specific structure check."))
    }
  }
  
  if (nrow(candidate_files_data) == 0) {
    message(paste0("INFO: No valid numbered run files (e.g., '1-", group_identifier_stem, "', '2-", group_identifier_stem, "', etc.) ",
                   "were confirmed for group defined by stem: '", group_identifier_stem, 
                   "' in directory '", group_original_relative_dir, "'. This group will be skipped."))
    skipped_groups_no_valid_runs <- skipped_groups_no_valid_runs + 1
    next 
  }
  
  # Sort candidates by run number and pick the first one (lowest run number)
  candidate_files_data <- candidate_files_data[order(candidate_files_data$run), ]
  
  source_file_to_copy <- candidate_files_data$path[1]
  lowest_run_number_used <- candidate_files_data$run[1]
  
  processed_groups_count <- processed_groups_count + 1
  
  # Construct output directory and filename
  current_output_dir <- file.path(relative_output_base_dir, group_original_relative_dir)
  if (!dir.exists(current_output_dir)) {
    dir.create(current_output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  output_filename_mean <- paste0("mean_", group_identifier_stem)
  output_filepath <- file.path(current_output_dir, output_filename_mean)
  
  message(paste0("\nProcessing group (identifier: ", group_identifier_stem, ")"))
  message(paste0("   Lowest available run is '", lowest_run_number_used, "'. Source file: ", source_file_to_copy))
  message(paste0("   Target output file: ", normalizePath(output_filepath, mustWork = FALSE)))
  
  tryCatch({
    if (!file.exists(source_file_to_copy)) {
      stop(paste("CRITICAL: Source file '", source_file_to_copy, "' identified from run ", lowest_run_number_used, " was not found on disk."))
    }
    copied <- file.copy(source_file_to_copy, output_filepath, overwrite = TRUE)
    if (copied) {
      message(paste0("   Successfully copied '", basename(source_file_to_copy), "' and renamed to '", output_filename_mean, "'"))
      copied_files_count <- copied_files_count + 1
    } else {
      warning(paste0("   Failed to copy file: ", source_file_to_copy, " to ", output_filepath,
                     ". Check permissions, disk space, or if the source file is accessible."))
    }
  }, error = function(e) {
    warning(paste0("Error copying file '", basename(source_file_to_copy), 
                   "' (run ", lowest_run_number_used, ") for group '", group_identifier_stem, "': ", e$message))
  })
}

# Final Summary
message(paste0("\n--- Script Summary ---"))
message(paste0("Total unique prediction groups initially identified: ", length(grouped_files_list)))
if (problematic_group_keys > 0) {
  message(paste0("Number of group keys that were unexpectedly empty (logic error): ", problematic_group_keys))
}
message(paste0("Groups for which a lowest-numbered run file was identified for processing: ", processed_groups_count))
message(paste0("Successfully copied ", copied_files_count, " files."))
if (skipped_groups_no_valid_runs > 0) {
  message(paste0(skipped_groups_no_valid_runs, 
                 " groups did not have any valid numbered run files matching the expected 'N-identifier_stem' structure and were skipped."))
}
message("Copied lowest available run predictions (renamed to 'mean_') saved in: ", normalizePath(relative_output_base_dir))