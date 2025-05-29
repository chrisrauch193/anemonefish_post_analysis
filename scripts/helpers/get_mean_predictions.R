# --------------------------------------------------------------------------
# Script to calculate mean predictions from multiple runs of SDM outputs
# Assumes the R working directory is 'anemonefish_post_analysis/'
# --------------------------------------------------------------------------

# 1. Load necessary libraries
# install.packages("terra") # Run once if not installed
library(terra)

# 2. Define base directories (relative to the project root 'anemonefish_post_analysis/')
# This script assumes your R working directory is set to:
# ~/a0236995/anemonefish_post_analysis/
# If not, these paths need to be adjusted or made absolute.

relative_input_base_dir <- "data/output/predictions"
relative_output_base_dir <- "data/output/mean_predictions"

# Create the main output directory if it doesn't exist
# dir.create uses paths relative to the current working directory if they are not absolute
if (!dir.exists(relative_output_base_dir)) {
  dir.create(relative_output_base_dir, recursive = TRUE)
  message("Created base output directory: ", normalizePath(relative_output_base_dir))
} else {
  message("Base output directory already exists: ", normalizePath(relative_output_base_dir))
}

# Normalize input base directory path for internal use.
# This makes it an absolute path, which is robust for string manipulations later.
# It also checks if the input directory actually exists.
absolute_input_base_dir <- normalizePath(relative_input_base_dir, mustWork = TRUE)
if (!endsWith(absolute_input_base_dir, .Platform$file.sep)) {
  absolute_input_base_dir <- paste0(absolute_input_base_dir, .Platform$file.sep)
}
message("Using absolute input base directory: ", absolute_input_base_dir)


# 3. Find all prediction .tif files recursively
# list.files path is relative to CWD, but full.names = TRUE makes returned paths absolute.
all_pred_files <- list.files(
  path = relative_input_base_dir, # Use relative path here for list.files
  pattern = "^\\d+-pred_.*\\.tif$", # Starts with number-pred_ and ends with .tif
  recursive = TRUE,
  full.names = TRUE # Important: ensures paths are absolute for robust processing
)

if (length(all_pred_files) == 0) {
  stop("No prediction files found matching the pattern in: ", absolute_input_base_dir)
}

message("Found ", length(all_pred_files), " prediction files to process.")

# 4. Group files by their unique identifier (species, scenario, method, etc.) and original directory
file_info <- data.frame(
  full_path = all_pred_files,
  stringsAsFactors = FALSE
)

# Extract the part of the path relative to absolute_input_base_dir
# This will be used to reconstruct the output directory structure
# The `sub` pattern `absolute_input_base_dir` needs to be treated as a fixed string.
file_info$path_suffix <- sub(pattern = fixed(absolute_input_base_dir), replacement = "", x = file_info$full_path)

# Extract the original directory (relative to absolute_input_base_dir)
file_info$original_relative_dir <- dirname(file_info$path_suffix)

# Extract the base filename
file_info$filename <- basename(file_info$full_path)

# Extract the identifier stem (e.g., "pred_Entacmaea_quadricolor_pca.tif")
file_info$identifier_stem <- sub("^\\d+-", "", file_info$filename)

# Create a unique group key based on the original directory (relative to input) and the identifier stem
file_info$group_key <- paste(file_info$original_relative_dir, file_info$identifier_stem, sep = "///")

# Split files into groups based on this key
grouped_files_list <- split(file_info$full_path, file_info$group_key)

message("Processing ", length(grouped_files_list), " unique groups of predictions.")

# 5. Process each group
for (group_key in names(grouped_files_list)) {
  files_in_group <- grouped_files_list[[group_key]]
  
  if (length(files_in_group) == 0) {
    warning("Empty group for key: ", group_key, ". Skipping.")
    next
  }
  
  # Get information from the first file (all files in group share this)
  first_file_in_group <- files_in_group[1]
  
  # Determine the relative output subdirectory structure from the group key
  # The group_key contains "original_relative_dir///identifier_stem"
  # We need original_relative_dir from the first file's info
  # (More robust to re-extract from file_info than parse group_key, though parsing would work)
  current_file_info <- file_info[file_info$full_path == first_file_in_group, ]
  relative_output_subdir_structure <- current_file_info$original_relative_dir
  identifier_stem_for_output <- current_file_info$identifier_stem
  
  # Construct output directory and filename
  # current_output_dir will be relative to CWD unless relative_output_base_dir was absolute
  current_output_dir <- file.path(relative_output_base_dir, relative_output_subdir_structure)
  if (!dir.exists(current_output_dir)) {
    dir.create(current_output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  output_filename <- paste0("mean_", identifier_stem_for_output)
  output_filepath <- file.path(current_output_dir, output_filename) # Path relative to CWD
  
  message(paste0("\nProcessing group: ", identifier_stem_for_output, " (", length(files_in_group), " files)"))
  message(paste0("   Input files from: ", dirname(first_file_in_group))) # Shows absolute path of input dir
  message(paste0("   Outputting to: ", normalizePath(output_filepath, mustWork = FALSE))) # Shows absolute path of output file
  
  tryCatch({
    # Read all rasters in the group
    raster_list <- lapply(files_in_group, terra::rast)
    
    # Check if all rasters can be stacked (same extent, resolution, crs)
    if(length(raster_list) > 1) {
      ext_check <- all(sapply(raster_list[-1], function(r) ext(r) == ext(raster_list[[1]])))
      res_check <- all(sapply(raster_list[-1], function(r) all(res(r) == res(raster_list[[1]]))))
      crs_check <- all(sapply(raster_list[-1], function(r) crs(r, proj=TRUE) == crs(raster_list[[1]], proj=TRUE)))
      if(!all(ext_check, res_check, crs_check)){
        # Print extent, res, crs of the first two differing rasters for easier debugging
        first_r <- raster_list[[1]]
        offending_r_idx <- NA
        if (!ext_check) offending_r_idx <- which(!sapply(raster_list[-1], function(r) ext(r) == ext(first_r)))[1] + 1
        else if (!res_check) offending_r_idx <- which(!sapply(raster_list[-1], function(r) all(res(r) == res(first_r))))[1] + 1
        else if (!crs_check) offending_r_idx <- which(!sapply(raster_list[-1], function(r) crs(r, proj=TRUE) == crs(first_r, proj=TRUE)))[1] + 1
        
        warning_msg <- paste0("Rasters in group ", identifier_stem_for_output, " do not have matching extent/resolution/CRS. Attempting to stack anyway but CHECK RESULTS CAREFULLY.")
        if (!is.na(offending_r_idx)) {
          second_r <- raster_list[[offending_r_idx]]
          warning_msg <- paste0(warning_msg,
                                "\n   File 1 (", basename(files_in_group[1]), "): ",
                                "Ext: ", paste(round(as.vector(ext(first_r)),3), collapse=","),
                                "; Res: ", paste(round(res(first_r),5), collapse=","),
                                "; CRS: ", crs(first_r, proj=TRUE),
                                "\n   File ", offending_r_idx, " (", basename(files_in_group[offending_r_idx]), "): ",
                                "Ext: ", paste(round(as.vector(ext(second_r)),3), collapse=","),
                                "; Res: ", paste(round(res(second_r),5), collapse=","),
                                "; CRS: ", crs(second_r, proj=TRUE))
        }
        warning(warning_msg)
      }
    }
    
    # Stack the rasters
    stacked_rasters <- terra::rast(raster_list)
    
    # Calculate the mean
    mean_raster <- terra::mean(stacked_rasters, na.rm = TRUE)
    
    # Write the mean raster
    terra::writeRaster(mean_raster, output_filepath, overwrite = TRUE, datatype="FLT4S")
    message(paste0("   Successfully created: ", normalizePath(output_filepath, mustWork = FALSE)))
    
  }, error = function(e) {
    warning(paste0("Error processing group for ", identifier_stem_for_output, ": ", e$message))
    warning(paste0("   Files in problematic group: ", paste(basename(files_in_group), collapse=", ")))
  })
}

message("\nProcessing complete.")
message("Mean predictions saved in: ", normalizePath(relative_output_base_dir))

# 6. Optional: Clean up terra's temporary files
# terra::tmpFiles(remove=TRUE) # Use with caution