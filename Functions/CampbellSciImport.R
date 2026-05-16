################################################################
# R function to process and plot Campbell Sci soil sensor data # 
# Megan Duffy - Adair Lab, UVM #################################
# last updated 2026-05-15 ######################################
################################################################

library(readr)
library(tools)
library(tidyverse)
library(patchwork)

# --- 1. Core Campbell Sci import function ---
# This is from the Campbell website, but added a check for the TIMESTAMP format)
importCSdata <- function(filename, RetOpt="data"){
  if(RetOpt=="info"){
    stn.info <- scan(file=filename, nlines=4, what=character(), sep="\r")
    return(stn.info)
  } else {
    header <- scan(file=filename, skip=1, nlines=1, what=character(), sep=",")
    stn.data <- read.table(file=filename, skip=4, header=FALSE, na.strings=c("NAN"), sep=",")
    names(stn.data) <- header
    # Standardizing to POSIXct for easier plotting later
    stn.data$TIMESTAMP <- as.POSIXct(stn.data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
    return(stn.data)
  }
}

# --- 2. Batch processing function ---
process_soil_sensors <- function(input_dir, output_dir) {
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # List all .dat files
  dat_files <- list.files(input_dir, pattern = "\\.dat$", full.names = TRUE)
  
  if (length(dat_files) == 0) {
    stop("No .dat files found in the input directory!")
  }
  
  for (file in dat_files) {
    # 1. Generate a "Clean Name" for the dataframe and CSV
    # This takes "HD1_79113_Table1_2025-09-12..." and returns "HD1_79113_Table1"
    base_name <- basename(file)
    clean_name <- sub("^(.*_Table[1-2]).*", "\\1", base_name)
    
    # 2. Import
    dat <- importCSdata(file)
    
    # 3. Save to CSV
    out_path <- file.path(output_dir, paste0(clean_name, ".csv"))
    write_csv(dat, out_path)
    
    # 4. Assign to global environment
    # This creates the dataframe variable (e.g., HD1_79113_Table1) in your Workspace
    assign(clean_name, dat, envir = .GlobalEnv)
    
    message("Processed and assigned: ", clean_name)
  }
}

# ---3. Plot Campbell Sci data timeseroes ---
plot_soil_sensors <- function(df, df_name, sensor_types, time_range, output_dir = NULL) {
  
  # 1. Filter by date range
  df_sub <- df %>%
    filter(TIMESTAMP >= time_range[1] & TIMESTAMP <= time_range[2])
  
  if (nrow(df_sub) == 0) stop("No data found in the specified time range.")

  # 2. Map depths to actual measurement heights
  depth_labels <- c("Depth 1" = "15 cm", "Depth 2" = "30 cm", "Depth 3" = "45 cm")

  # 3. Generate a list of plots
  plot_list <- sensor_types %>%
    map(function(sensor) {
      
      target_cols <- names(df_sub)[grepl(paste0("^", sensor, "\\("), names(df_sub))]
      
      if (length(target_cols) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label=paste("Missing:", sensor)) + theme_void())
      }
      
      df_long <- df_sub %>%
        select(TIMESTAMP, all_of(target_cols)) %>%
        pivot_longer(cols = -TIMESTAMP, names_to = "Sensor_Index", values_to = "Value") %>%
        mutate(Sensor_Index = gsub(paste0(sensor, "\\((.*)\\)"), "Depth \\1", Sensor_Index)) %>%
        # Rename depths to actual cm measurements
        mutate(Depth = recode(Sensor_Index, !!!depth_labels))
      
      ggplot(df_long, aes(x = TIMESTAMP, y = Value, color = Depth)) +
        geom_line(linewidth = 0.8, alpha = 0.8) +
        theme_bw() +
        labs(y = sensor, color = "Soil depth") +
        theme(axis.title.x = element_blank()) +
        theme(text = element_text(size = 16)) +
        scale_color_viridis_d(option = "mako", end = 0.8)
    })
  
  # 4. Stack the plots
  final_stack <- wrap_plots(plot_list, ncol = 1) + 
    plot_layout(guides = "collect") +
    plot_annotation(title = paste("Soil sensors:", df_name)) & 
    theme(legend.position = "right")
  
  # 5. Save out the plot if a directory is provided
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    # Create a filename based on the dataframe name and sensors
    file_tag <- paste(sensor_types, collapse = "_")
    file_name <- paste0(df_name, "_", file_tag, ".jpg")
    
    ggsave(filename = file.path(output_dir, file_name), 
           plot = final_stack, 
           width = 10, height = 4 * length(sensor_types), 
           dpi = 300)
    
    message("Saved: ", file_name, " to ", output_dir)
  }
  
  return(final_stack)
}