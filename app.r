# Updated Sep 2023.
# Makes use of PNW-Cnet version 4 (51 classes) or version 5 (135 classes). See 
# ./Target_Classes.csv for descriptions of each class.

library(lubridate)
library(parallel)
library(reticulate)
library(shinyjs)
library(tidyverse)
library(tuneR)

if(!file.exists("./settings")) { 
  cat('custom_output_dir=""\n' , file="./settings")
  }

source("./settings")
source("./functions.r")
source_python("./scripts/PNW-Cnet_predict.py")

ncores_logical <- detectCores()
ncores_physical <- detectCores(logical = FALSE)

# Change the following line if you installed SoX in a different location.
sox_dir <- "C:/Program Files (x86)/sox-14-4-2"
sox_path <- file.path(sox_dir, "sox")

# Setting up to run with either version of PNW-Cnet (v4 or v5)
v4_model_path <- "./PNW-Cnet_v4_TF.h5"
v5_model_path <- "./PNW-Cnet_v5_TF.h5"

class_desc <- read_csv("./Target_Classes.csv", show_col_types = FALSE)

class_cats <- class_desc %>% 
  distinct(Subcategory) %>% 
  arrange(Subcategory) %>% 
  pull(Subcategory)

nso_classes <- c("STOC", "STOC_IRREG", "STOC_4Note", "STOC_Series")

default_review_settings <- class_desc %>% 
  mutate(Threshold = ifelse(Class %in% nso_classes, 0.25, 0.95)) %>% 
  arrange(Version, desc(Class %in% nso_classes), Class) %>% 
  select(Version, Class, Sound, Threshold)

ui <- fluidPage(
	useShinyjs(),
	
	titlePanel("Shiny_PNW-Cnet: Neural net processing for audio data"),

	# Tabset panel allows switching between several modes: Input, where the user
	# can designate a target directory and carry out processing tasks; Explore,
	# where the user can examine detections from a CNN prediction file plotted
	# by class, day/week, station, and detection threshold; and Settings, where
	# the user can customize various aspects of the app's behavior.
	sidebarLayout(
		sidebarPanel(
		  tabsetPanel(
		    id = "controls",
		    type = "hidden",
		    
		    ##### INPUT CONTROL PANEL #####
		    tabPanelBody("inputControlPanel",
      		textInput("topdir", h5("Copy and paste (recommended) or type target directory path into box and click Check Directory:"),
      		          placeholder = "F:\\Path\\to\\directory"),
      		actionButton("checkdirbutton", label="Check Directory"),
      		br(),
      		h5("Click Process Files to generate spectrograms and use PNW-Cnet to predict class scores. This may take several hours. Please make sure all inputs are correct and that your computer will not go to sleep, shut down or restart during this process."),
      		actionButton("processbutton", label = "Process Files", disabled = TRUE),
      		br(),
      		h5("Click Create Review File to export apparent detections to a separate file for manual review."),
      		actionButton("revExtButton", label = "Create Review File", disabled = TRUE),
      		br(),
      		h5("Click Extract Review Clips to extract apparent detections as 12-second audio clips for further review or archival."),
      		actionButton("extractWavsButton", label = "Extract Review Clips", disabled = TRUE),
      		br(),
      		h5("Click Explore Detections to view apparent detections plotted over time by class and recording station."),
      		actionButton("exploreDetsButton", label = "Explore Detections", disabled = TRUE),
      		br(),
      		h5("Click Settings and Utilities to configure the app's behavior and find tools to assist in audio processing."),
      		actionButton("settingsButton", "Settings and Utilities")
		    ),

		    ##### EXPLORE CONTROL PANEL #####
		    tabPanelBody("exploreControlPanel",
		      textOutput({"exploreTitle"}),
		      h5(" Select a target class and a detection threshold to see apparent detections plotted by station over time."),
		      uiOutput("selectPlotClass"),
		      
		      uiOutput("selectClassCategories"),

		      selectInput("plot_threshold", label = "Detection threshold",
		                  choices = c("0.99", "0.98", "0.95", "0.90", "0.85", "0.80",
		                              "0.75", "0.70", "0.65", "0.60", "0.55", "0.50",
		                              "0.45", "0.40", "0.35", "0.30", "0.25", "0.20",
		                              "0.15", "0.10", "0.05"), selected = "0.95"),

		      selectInput("plot_timescale", label = "Summarize detections...",
		                  choices = c("Weekly", "Daily")),

		      h5("Click Create single-class review file to generate a file listing apparent detections for the chosen class at the selected threshold."),
		      
		      actionButton("singleClassReviewButton", "Create single-class review file"),
		      br(),
		      actionButton("backButton", label = "Back to inputs"),
		    ),
		    
		    ##### SETTINGS CONTROL PANEL #####
		    tabPanelBody("settingsControlPanel",
                     h3("Settings and Utilities"),
		                 h5("_______________________________"),
		                 h4("Neural net version"),
		                 radioButtons(inputId = "pnw_cnet_version",
		                              label = h5("Choose which version of PNW-Cnet to use to generate class scores:"),
		                              choices = list("Use PNW-Cnet v5 (135 target classes)" = "v5",
		                                             "Use PNW-Cnet v4 (51 target classes)" = "v4"),
		                              selected = "v5"),
		                 h5("_______________________________"),
		                 h4("Image directory"),
		                 textInput("customOutputDir",
		                           h5("Choose the directory where spectrogram image files will be saved. This folder will be created if it does not already exist. If left blank, spectrograms will be created in a folder called Temp in the directory containing your audio data."),
		                           placeholder = "F:\\Path\\to\\directory",
		                           value=custom_output_dir),
		                 actionButton("saveOutputDirButton",
		                              label = "Save"),
		                 actionButton("clearOutputDirButton",
		                              label = "Clear"),
		                 checkboxInput("splitSpectrogramGeneration", label="Split image directory into subfolders for large datasets (recommended)", value=TRUE),
		                 checkboxInput("deleteSpectrogramsWhenDone", label="Delete spectrograms when processing is complete", value=TRUE),
		                 
		                 h5("_______________________________"),
		                 
		                 h4("Review file"),
		                 
		                 selectInput("useReviewSettings",
		                             label = h5("Select which settings (target classes and detection thresholds) to use when generating review files. To use custom settings, the target directory must contain a file named Review_Settings.csv with one column named \"Class\" listing the abbreviated names of the classes you want to include and another column named \"Threshold\" listing the score threshold to be used for each class."),
		                             choices = list("Use default settings" = "default",
		                                            "Use custom settings" = "custom")),
		                 actionButton("showReviewSettingsButton", 
		                              label="Show settings"),
		                 actionButton("hideReviewSettingsButton",
		                              label="Hide settings"),
		                 selectInput("reviewTimescale",
		                             label = h5("Review file should group apparent detections..."),
		                             choices = list("Weekly", "Daily"),
		                             selected = "Weekly"),
		                 
		                 h5("_______________________________"),
		                 
		                 h4("Multiprocessing"),
		                 
		                 uiOutput("use_nCores"),
		                 
						         checkboxInput("useLogicalCores", label = "Count logical cores (enables more nodes)", value = TRUE),
						         h5("Note: Using more nodes is generally faster, but processing speed may be limited by other factors such as disk read/write speeds."),
						         
						         h5("_______________________________"),
						         h4("Standardize filenames"),
						         h5("Rename files to match the filename format expected by the app. Click Preview Changes to show how filenames will be changed, then click Rename Files to execute the changes. Changes will be recorded in a file in the target directory called Rename_Log. This file can also be used to reverse the changes."),
						         actionButton("showPreviewButton", label="Preview Changes",
						                      disabled=TRUE),
						         actionButton("hidePreviewButton", label="Hide Preview",
						                      disabled=TRUE),
						         uiOutput("renameButton"),
						         br(),
						         
						         actionButton("settingsBackButton", label="Back to inputs"))
		  )
  	),

  	mainPanel(
  	  tabsetPanel(
  	    id = "mainTabs",
  	    type = "hidden",
  	    tabPanelBody("inputMainPanel",
      		h4(textOutput({"predsFound"})),
      		br(),
      		h4(textOutput({"revFilesFound"})),
      		br(),
      		h4(tagAppendAttributes(textOutput("wavsFound"), style = "white-space:pre-wrap;")),
      		fluidRow(column(12, div(tableOutput( {"showFiles"} ),
      		                        style="height:500px;overflow-y:scroll"))),
  	    ),
  	    tabPanelBody("exploreMainPanel",
  	                 plotOutput({ "detectionPlot" }),
  	    ),
  	    tabPanelBody("settingsMainPanel",
  	                 h4(textOutput({"renamePreviewHeader"})),
  	                 h4(textOutput({"renamePreviewSummary"})),
  	                 fluidRow(column(12, div(tableOutput( {"renamePreviewTable"} ),
  	                                         style="height:500px;overflow-y:scroll"))),
  	                 fluidRow(column(12, div(tableOutput( {"reviewSettingsTable"} ),
  	                                         style="height:500px;overflow-y:scroll"))))

  	  )
  	)
	)
)

# Reactive functions or variables that respond to events within the interface go here.
server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$show_rename_preview_table <- FALSE
  rv$show_review_settings_table <- FALSE
  rv$files_renamed <- FALSE
  rv$use_cores <- detectCores()
  rv$visible_classes <- class_desc %>%
    mutate(Str_Class = paste0(Class, " - ", Sound)) %>%
    pull(Str_Class)
  
  # Change from Input tab to Explore tab.
  switchTabs1 <- observeEvent(input$exploreDetsButton, {
    req(getSummaryFile())
    updateTabsetPanel(inputId = "controls", selected = "exploreControlPanel")
    updateTabsetPanel(inputId = "mainTabs", selected = "exploreMainPanel")
  })
  
  # Change from Explore tab back to Input tab.
  switchTabs2 <- observeEvent(input$backButton, {
    updateTabsetPanel(inputId = "controls", selected = "inputControlPanel")
    updateTabsetPanel(inputId = "mainTabs", selected = "inputMainPanel")
  })
  
  switchTabs3 <- observeEvent(input$settingsButton, {
    updateTabsetPanel(inputId = "controls", selected = "settingsControlPanel")
    updateTabsetPanel(inputId = "mainTabs", selected = "settingsMainPanel")
  })
  
  # Change from Explore tab back to Input tab.
  switchTabs4 <- observeEvent(input$settingsBackButton, {
    updateTabsetPanel(inputId = "controls", selected = "inputControlPanel")
    updateTabsetPanel(inputId = "mainTabs", selected = "inputMainPanel")
  })
  
  # Checks if the input directory exists
	checkDir <- eventReactive(input$checkdirbutton, {
		topdir <- correctedDir()
		if(dir.exists(topdir)) { "valid" } else { "invalid" }
	})
  
	# Replaces backslashes with forward slashes in input path for neatness.
	correctedDir <- reactive({
		orig <- input$topdir
		corrected <- correctPath(orig)
	})
	
	correctedOutputDir <- reactive({
	  orig <- input$customOutputDir
	  corrected <- correctPath(orig)
	})
	
	clearCustomOutputDir <- observeEvent(input$clearOutputDirButton, {
	  updateTextInput(session, "customOutputDir", value="")
	})
	
	saveCustomOutputDir <- observeEvent(input$saveOutputDirButton, {
	  output_dir <- correctedOutputDir()
	  config_file_path <- "./settings"
	  cat(sprintf('custom_output_dir="%s"\n', output_dir), file=config_file_path)
	})
	
	# Set the state of various actionButtons when Check Directory is clicked, based
	# on files that were or were not found in the target directory.
	updateButtons <- observeEvent(input$checkdirbutton, {
	  toggleState(id = "checkdirbutton", condition = TRUE)
	  toggleState(id = "settingsButton", condition = TRUE)
	  
	  rv$show_review_settings_table <- FALSE
	  toggleState(id="hideReviewSettingsButton", condition=FALSE)
	  toggleState(id="showReviewSettingsButton", condition=TRUE)
	  toggleState(id="refreshReviewSettingsButton", condition=FALSE)
	  
	  wavs <- getWavs()
	  predfile <- getPredFile()
	  revfile <- getRevFile()
	  renamelogfile <- getRenameLogFile()
	  if(renamelogfile != "") {
	    rv$files_renamed <- TRUE
	  }
	  
	  if ( length(wavs) > 0 ) {
	    toggleState(id = "processbutton", condition = TRUE)
	    toggleState(id = "previewRenameButton", condition=TRUE)
	    toggleState(id = "showPreviewButton", condition=TRUE)
	  } else { toggleState(id = "processbutton", condition = FALSE) }
	  
	  if (predfile != "") {
	    toggleState(id = "processbutton", condition = FALSE)
	    toggleState(id = "exploreDetsButton", condition = TRUE)
	    if (revfile != ""){
	      toggleState(id = "revExtButton", condition = FALSE)
	    } else { toggleState(id = "revExtButton", condition = TRUE) 
	      }
	    } else {
	      toggleState(id = "revExtButton", condition = FALSE) 
	      toggleState(id = "exploreDetsButton", condition = FALSE)
	      }
	  
	  if (revfile != "") {
	    toggleState(id = "extractWavsButton", condition = TRUE)} else {
	      toggleState(id = "extractWavsButton", condition = FALSE) }
	})
	
	updateVisibleClasses <- observeEvent(input$class_categories, {
	  rv$visible_classes <- class_desc %>% 
	    filter(Subcategory %in% input$class_categories, 
	           Version == input$pnw_cnet_version) %>% 
	    mutate(Str_Class = paste0(Class, " - ", Sound)) %>%
	    pull(Str_Class)
	})
	
	showPreviewTable <- observeEvent(input$showPreviewButton, {
	  if(rv$show_review_settings_table) {
	    rv$show_review_settings_table <- FALSE
	  }
	  rv$show_rename_preview_table <- TRUE
	  toggleState(id="showPreviewButton", condition=FALSE)
	  toggleState(id="hidePreviewButton", condition=TRUE)
	  if(detectDuplicates()) {
	    showModal(modalDialog(title = "Cannot Rename Files",
	                          "Renaming would result in duplicate file paths.",
	                          size = "m",
	                          footer = modalButton("Dismiss"),
	                          easyClose = TRUE))  	  
	  } else {
	    if(nFilenameChanges() > 0) {
  	    toggleState(id="renameFilesButton", condition=TRUE)
      }
	   }
	})
	
	hidePreviewTable <- observeEvent(input$hidePreviewButton, {
	  rv$show_rename_preview_table <- FALSE
	  toggleState(id="hidePreviewButton", condition=FALSE)
	  toggleState(id="showPreviewButton", condition=TRUE)
	  toggleState(id="renameFilesButton", condition=FALSE)
	})
	
	showReviewSettings <- observeEvent(input$showReviewSettingsButton, {
	  if(rv$show_rename_preview_table) {
	    rv$show_rename_preview_table <- FALSE
	  }
	  rv$show_review_settings_table <- TRUE
	  # toggleState(id="showReviewSettingsButton", condition=FALSE)
	  toggleState(id="hideReviewSettingsButton", condition=TRUE)
	})
	
	hideReviewSettings <- observeEvent(input$hideReviewSettingsButton, {
	  rv$show_review_settings_table <- FALSE
	  toggleState(id="hideReviewSettingsButton", condition=FALSE)
	  toggleState(id="showReviewSettingsButton", condition=TRUE)
	  toggleState(id="refreshReviewSettingsButton", condition=FALSE)
	})

	getWavs <- eventReactive(input$checkdirbutton, {
	  in_dir <- correctedDir()
	  wavs_found <- data.frame(Filename = findFiles(in_dir, ".wav")) %>% 
	    filter(file.size(Filename) >= 500000,
	           !(str_detect(Filename, "_part_"))) %>% 
	    pull(Filename)
	})
	
	# Return a text summary with the number of wav files in the directory tree and
	# their total duration and size.
	wavSummary <- eventReactive(input$checkdirbutton, {
	  wavs <- getWavs()
	  n_wavs <- length(wavs)
	  if(n_wavs == 0) {
	    message <- "Directory tree contains no valid .wav files."
	    } else {
	    durs <- sapply(wavs, getDuration)
	    sizes <- sapply(wavs, file.size)
	    total_dur <- sum(durs) / 3600
	    total_size <- sum(sizes) / 1024^3
	    
	    message <- paste(sprintf("Directory tree contains %d valid .wav files.", n_wavs),
	                     sprintf(" - Total duration: %.1f hours", total_dur),
	                     sprintf(" - Total size: %.1f GB", total_size), 
	                     sep = "\n")
	    }
	  return( message )
	})
	
	# Starts processing wav files when user hits the button. Generates spectrograms
	# representing non-overlapping 12-s segments of audio in the frequency range
	# 0 - 4000 Hz, then uses the trained PNW-Cnet model to generate class scores for
	# 51 target classes for each image.
	processWavs <- observeEvent(input$processbutton, {
	  # Disable other controls while processing
	  toggleState(id = "processbutton", condition = FALSE)
	  toggleState(id = "checkdirbutton", condition = FALSE)
	  toggleState(id = "settingsButton", condition = FALSE)
	  
	  wavs <- getWavs()
	  
	  if(length(wavs) == 0) {
      cat("\nNo .wav files available to process.\n")
	    return()
	  }
    
	  total_duration <- sum(sapply(wavs, getDuration)) / 3600
		
	  if(input$pnw_cnet_version == "v5") {
      model_path <- v5_model_path
	  } else {
	    model_path <- v4_model_path
	  }
	  
		target_dir <- correctedDir()
    custom_output_dir <- correctedOutputDir()
    
    # Check if the user entered a custom output directory for the spectrograms,
	  # create it if necessary, and use this value for image_dir. On error, it will
	  # use the default version.
		if(custom_output_dir == "") {
  		image_dir = file.path(target_dir, "Temp", "images")
		} else { 
  		  image_dir = makeImageDir(target_dir, custom_output_dir) }
		
    if(!dir.exists(image_dir)) { dir.create(image_dir, recursive=TRUE) }
    
    # Try to split up spectrograms so there are no more than ~50,000 in a folder
    split_spectro_gen <- input$splitSpectrogramGeneration
    if( split_spectro_gen ) {
      n_images <- total_duration * 300
      n_parts <- floor(n_images / 50000)
      spectro_dirs <- makeSpectroDirList(wavs, image_dir, n_parts)
    } else { spectro_dirs <- rep(image_dir, length(wavs)) }
    
		outname <- getSiteName(target_dir)
		
		# Check if spectrograms have already been generated. If so, skip that step.
		pngs <- findFiles(image_dir, ".png")
		
		spectro_start_time <- Sys.time()
		
		if(length(pngs) > 0) {
			cat(sprintf("\nImage data already present in %s.\n", image_dir))
		  grams_generated <- FALSE
		  spectro_end_time <- Sys.time()
		} else {
		  grams_generated <- TRUE
		  use_cores <- rv$use_cores
		  cat(sprintf("\nSpectrograms will be created in %s.\n", image_dir))
		  cat(sprintf("\nGenerating spectrograms using %d nodes starting at %s... \n", 
		              use_cores, format(spectro_start_time, "%X")))
			
		  # Build the multiprocessing cluster
			cl = makeCluster(use_cores)
			clusterEvalQ(cl, c(library(stringr), library(tuneR)))
			clusterExport(cl = cl, varlist=c('makeImgs', 'image_dir', 'spectro_dirs',
			                                 'wavs', 'sox_path'), envir = environment())
			# Use the multiprocessing cluster to generate the spectrograms
			clusterMap(cl, wavs, spectro_dirs, fun = makeImgs, sox_path = sox_path)
			stopCluster(cl)
			spectro_end_time <- Sys.time()
			cat(sprintf("\nFinished at %s.\n", format(spectro_end_time, "%X")))
		}
		
		predict_start_time <- Sys.time()
		
		cat(sprintf("\nGenerating class scores using PNW-Cnet %s starting at %s...\n", 
		            input$pnw_cnet_version,
		            format(predict_start_time, "%X")))
		
		cat("\n(The following warnings from TensorFlow are expected and can be safely ignored.)\n")
		
		# Generate class scores and write them to the output file
		predictions <- makePredictions(image_dir, model_path, show_prog = TRUE)
		pred_classes <- names(predictions)[2:ncol(predictions)]
    
	  predictions <- predictions %>% 
		  mutate(across(all_of(pred_classes), function(x) round(x, digits=5)))
		
		output_file <- file.path(target_dir, sprintf("CNN_Predictions_%s.csv", outname))
		write_csv(predictions, output_file)
		
		predict_end_time <- Sys.time()
		
		cat(sprintf("\nClass scores written to %s in folder %s.\n", 
		            basename(output_file), target_dir))
		
		cat("\nSummarizing detections...\n")
		det_table <- buildDetTable(predictions)
		det_file_name <- sprintf("%s_detection_summary.csv", outname)
		det_file_path <- file.path(target_dir, det_file_name)
		write_csv(det_table, det_file_path)
		cat(sprintf("\nDetection summary written to file.\n"))
		
		run_time <- as.numeric(predict_end_time - spectro_start_time, units="hours")
		total_duration_pretty <- total_duration %>% 
		  round(digits=1) %>% 
		  format(big.mark=",")
		
		if(grams_generated) {
		  cat(sprintf("\nSpectrogram generation start: %s", format(spectro_start_time, "%X")))
		  cat(sprintf("\nSpectrogram generation end: %s", format(predict_start_time, "%X")))
		}
		cat(sprintf("\nPrediction start: %s", format(predict_start_time, "%X")))
		cat(sprintf("\nPrediction end: %s", format(predict_end_time, "%X")))
		
		cat(sprintf("\n\nSuccessfully processed %s hours of data in %.1f hours (ratio: %.1f).\n",
		            total_duration_pretty, run_time, total_duration / run_time))
		
		if(input$deleteSpectrogramsWhenDone) {
		  cat("\nDeleting spectrograms and removing temporary directories... ")
		  temp_dir <- dirname(image_dir)
		  unlink(temp_dir, recursive=TRUE)
		  cat("Done.\n")
		}
		
		showModal(modalDialog(title = "Processing complete",
		                      "Audio processing complete. Class scores written to file.",
		                      size = "l",
		                      footer = modalButton("Dismiss"),
		                      easyClose = TRUE))

		toggleState(id = "checkdirbutton", condition = TRUE)
		click(id = "checkdirbutton")
	})

	# Creates a review file based on the CNN results file in the folder specified.
	# Rewritten to use newer functions
	mkReviewFile <- observeEvent(input$revExtButton, {
	  in_dir <- correctedDir()
	  pred_file <- getPredFile()
	  review_timescale <- input$reviewTimescale
	
	  cat(sprintf("\nAtttempting to create review file based on %s.\n",
	              basename(pred_file)))
	  
	  if(input$useReviewSettings == "custom") {
	    rev_settings_file <- getReviewSettingsFile()
	    if(rev_settings_file == "") {
	      cat("Could not find Review_Settings.csv. Using default settings...\n")
	      review_settings <- default_review_settings
	      } else {
	        cat("Using custom review file settings...\n")
	        review_settings <- read_csv(rev_settings_file, show_col_types = FALSE)
	        }    
	    } else {
		    review_settings <- default_review_settings
	    }
	  
	  review_tables <- makeCustomKscopeTable(pred_file, review_settings, review_timescale)
	  
	  review_table <- review_tables[[1]]
	  kscope_table <- review_tables[[2]]

	  review_path <- file.path(in_dir, sprintf("%s_review.csv", getSiteName(in_dir)))
	  kscope_path <- str_replace(review_path, "review.csv", "review_kscope.csv")
		
	  nrevlines <- nrow(review_table)
    if(nrevlines == 0) {
      cat("\nPrediction file contains 0 potential detections. No review file created.\n")
    } else {
      review_table %>% write_csv(review_path)
      kscope_table %>% write_csv(kscope_path)
      cat(sprintf("\n%d potential detections written to review file.\n", nrevlines))
    }
		click(id = "checkdirbutton")
	})
	
	# Create a review file for a single class with a selected detection threshold.
	mkSingleClassReviewFile <- observeEvent(input$singleClassReviewButton, {
	  target_class <- str_split(input$class_selection, " - ")[[1]]
	  class_abbr <- target_class[1]
	  class_desc <- target_class[2]
	  thresh <- as.double(input$plot_threshold)
	  
	  rev_df <- data.frame(Class = class_abbr, Threshold = thresh)
	  
	  timescale <- input$plot_timescale
	  cat(sprintf("\nGenerating review file for %s (%s) with detection threshold %.2f, grouped %s...\n",
	              class_abbr,
	              class_desc,
	              thresh,
	              tolower(timescale)))
    pred_file <- getPredFile()
    preds <- read_csv(pred_file, show_col_types = FALSE)
    
    in_name <- basename(pred_file)
    
    det_lines <- makeCustomKscopeTable(pred_file, rev_df, timescale)[[2]]
    
    if(nrow(det_lines) == 0) {
      cat("Zero detections at this threshold. File not generated.\n")
    } else {
      out_name <- in_name %>% str_replace("CNN_Predictions_", "") %>%
        str_replace(".csv", sprintf("_review_kscope_%s_%.2f_%s.csv",
                                    class_abbr, thresh, timescale))
      
      det_lines %>% write_csv(file.path(correctedDir(), out_name))
      
      cat(sprintf("%d lines written to file.\n", nrow(det_lines)))
    }
	})

	# Get a table of class abbreviations and corresponding detection thresholds,
	# create a review file containing only the selected classes
	mkCustomReviewFile <- observeEvent(input$customReviewFileButton, {
	  rev_settings_file <- getReviewSettingsFile()
	  if(rev_settings_file == "") {
	    return()
	  } else {
	      pred_file <- getPredFile()
	      preds <- read_csv(pred_file, show_col_types=FALSE)
	      settings <- read_csv(rev_settings_file, show_col_types=FALSE)
	      target_classes <- settings %>% pull(Class)
	      for(i in seq_along(target_classes)) {
	        target_class <- target_classes[i]
	        det_threshold <- settings %>% filter(Class == target_class) %>% 
	          pull(Threshold)
	        new_dets <- makeCustomKscopeTable(preds, pred_file, target_class,
	                                          det_threshold)
	        if(i == 1) {
	          output_lines <- new_dets
	        } else {
	          output_lines <- bind_rows(output_lines, new_dets)
	        }
	      }
	    }
	})
	
	# Extracts wav files based on review file.
	extractWavFiles <- observeEvent(input$extractWavsButton, {
	  in_dir = correctedDir()
		reviewFile = getRevFile()
		if (reviewFile == "") {
		  cat("\nNo review file detected. Cannot extract clips for review.\n")
		} else if (length(getWavs()) == 0) {
		  cat("\nDirectory contains no valid .wav files. Cannot extract clips.\n")
		} else {
		  extractWavs(reviewFile, in_dir, sox_path)
		}
	})
	
	# Not recursive; prediction file should be at the top level of the directory
	getPredFile <- eventReactive(input$checkdirbutton, {
		target_dir <- correctedDir()
		site_name <- getSiteName(target_dir)
		pred_file_path <- file.path(target_dir, sprintf("CNN_Predictions_%s.csv", site_name))
		if(file.exists(pred_file_path)) {
		  return( pred_file_path )
		} else {
		  return( "" )
		}
	})

	getRevFile <- eventReactive(input$checkdirbutton, {
	  target_dir <- correctedDir()
	  site_name <- getSiteName(target_dir)
	  review_file_path <- file.path(target_dir, sprintf("%s_review.csv", site_name))
	  if(file.exists(review_file_path)) {
	    return( review_file_path )
	  } else {
	    return( "" )
	  }
	})

	getSummaryFile <- eventReactive(input$checkdirbutton, {
	  target_dir <- correctedDir()
	  site_name <- getSiteName(target_dir)
	  summary_file_path <- file.path(target_dir, sprintf("%s_detection_summary.csv", site_name))
	  if(file.exists(summary_file_path)) {
	    return( summary_file_path )
	  } else {
	    return( "" )
	  }
	})
	
	getRenameLogFile <- eventReactive(input$checkdirbutton, {
	  target_dir <- correctedDir()
	  log_file_path <- file.path(target_dir, "Rename_Log.csv")
	  if(file.exists(log_file_path)) {
	    return( log_file_path )
	  } else {
	    return( "" )
	  }
	})

	getReviewSettingsFile <- eventReactive(input$checkdirbutton, {
	  target_dir <- correctedDir()
	  rev_settings_path <- file.path(target_dir, "Review_Settings.csv")
	  if(file.exists(rev_settings_path)) {
	    return( rev_settings_path ) 
	  } else {
	    return( "" )
	  }
	})
	
	detTable <- eventReactive(input$checkdirbutton, {
	  req(getSummaryFile())
    read_csv(getSummaryFile(), show_col_types = FALSE) %>% 
      mutate(Threshold = sprintf("%.2f", Threshold))
	})

	foundDetTable <- observeEvent(input$exploreDetsButton, {
	  req(getPredFile())
	  summaryFile <- getSummaryFile()
	  if(summaryFile == "") {
	    predFile <- getPredFile()
	    cat(sprintf("\nNo summary file found. Generating detection summaries based on %s...\n", 
	        basename(predFile)))
	    preds <- read_csv(predFile, show_col_types = FALSE)
	    dets <- buildDetTable(preds)
	    outname <- predFile %>% str_replace("CNN_Predictions_", "") %>% 
	      str_replace(".csv", "_detection_summary.csv")
	    dets %>% write_csv(outname)
	    cat("\nSummary file generated.\n")
	    click(id="checkdirbutton")
	    updateTabsetPanel(inputId = "controls", selected = "exploreControlPanel")
	    updateTabsetPanel(inputId = "mainTabs", selected = "exploreMainPanel")
	  } else { }
	})
	
	output$wavsFound <- renderText({
		if (checkDir() == "invalid") { 
			"Invalid directory." } else {
			wavSummary() }
	})

	output$predsFound <- renderText({
		predfile = getPredFile()
		if (predfile == "") { "No prediction file found." } else { 
			sprintf( "Found prediction file %s.", basename(predfile) ) }
	})

	output$revFilesFound <- renderText({
		revfile = getRevFile()
		if (revfile == "") { "No review file found." } else {
			sprintf( "Found review file %s.", basename(revfile) ) }
	})

	output$showFiles <- renderTable({
	  wavs <- getWavs()
	  target_dir <- correctedDir()
		if(length(wavs) == 0) {
		  return()
		} else {
		  makeWavTable(wavs, target_dir)
		}
	})
	
	settingsTable <- eventReactive(input$showReviewSettingsButton, {
	  settings_file <- getReviewSettingsFile()
	  if(input$useReviewSettings == "default" | settings_file == "") {
	    settings_table <- default_review_settings %>% 
	      filter(Version == input$pnw_cnet_version)
	  } else {
	    settings_table <- read_csv(settings_file, show_col_types = FALSE) %>% 
	      select(Class, Threshold) %>% 
	      inner_join(class_desc, by = "Class") %>% 
	      select(Class, Sound, Threshold)
	  }
	})
	
	# Make a table showing where filenames are not currently standardized
	previewTable <- eventReactive(input$checkdirbutton, {
	  wavs <- getWavs()
	  input_dir <- correctedDir()
	  if(length(wavs) == 0) {
	    return()
	  } else {
	    log_path <- getRenameLogFile()
	    if(!log_path=="") {
	      preview_table <- read_csv(log_path, show_col_types = FALSE) %>% 
	        mutate(Change = ifelse(Former == New, "No", "Yes"))
	    } else {
	      preview_table <- makeRenamePreviewTable(wavs, input_dir)
	    }
	  }
	})
	
	detectDuplicates <- eventReactive(input$showPreviewButton, {
	  preview_table <- previewTable()
	  if(nrow(preview_table) == 0) {
	    return(FALSE)
	  } else {
	    dups <- preview_table %>% select(Folder, New) %>% anyDuplicated()
	    if(dups > 0) {
	      return(TRUE)
	    } else {
	      return(FALSE)
	    }
	  }
	})
	
	nFilenameChanges <- eventReactive(input$showPreviewButton, {
	  preview_table <- previewTable()
	  n_changes <- preview_table %>% filter(Change == "Yes") %>% nrow()
	  return(n_changes)
	})
	
	previewSummary <- eventReactive(input$showPreviewButton, {
	  preview_table <- previewTable()
	  n_changes <- nFilenameChanges()
	  if(nrow(preview_table) == 0) {
	    msg <- ""
	  } else {
	      if(n_changes == 0) {
	        msg <- "All filenames are correct. No changes will be made."
	      } else {
  	        if(rv$files_renamed) {
  	          msg <- sprintf("Click Undo Renaming to reverse changes to %d filenames.",
  	                         n_changes)
  	        } else {
  	          msg <- sprintf("Click Rename Files to enact changes to %d filenames.", 
  	                         n_changes)
  	        }
	      }
	  }
	  return(msg)
	})
	
	output$renamePreviewTable <- renderTable({
    if(rv$show_rename_preview_table) {
  	  preview_table <- previewTable()
    } else {  }
	})
	
	output$reviewSettingsTable <- renderTable({
	  if(rv$show_review_settings_table) {
	    settings_table <- settingsTable() 
	  } else {  }
	})

	output$renamePreviewHeader <- renderText({
	  target_dir <- correctedDir()
	  if(rv$show_rename_preview_table) {
	    if(rv$files_renamed) {
	      header <- sprintf("Showing changes made to .wav filenames in %s.", target_dir)
	    } else {
  	    header <- sprintf("Previewing changes to .wav filenames in %s.", target_dir)
	    }
	  } else {  }
	})
	
	output$renamePreviewSummary <- renderText({
	  if(rv$show_rename_preview_table) {
  	  summary <- previewSummary()
	  } else {  }
	})
	
	renameWaveFiles <- observeEvent(input$renameFilesButton, {
	  preview_table <- previewTable()
	  top_dir <- correctedDir()
	  click(id = "hidePreviewButton")
	  renameFiles(preview_table, top_dir)
	  click(id = "checkdirbutton")
	  click(id = "settingsBackButton")
  })
	
	output$exploreTitle <- renderText({
	  predfile <- getPredFile()
	  if( predfile == "") { "" } else {
	    sprintf("Exploring apparent detections from %s.", basename(predfile))
	  }
	})

	output$detectionPlot <- renderPlot({
	  # req(detTable())
	  target_class <- str_split(input$class_selection, " - ")[[1]][1]
	  thresh <- input$plot_threshold
	  timescale <- input$plot_timescale
	  buildDetPlot(target_class, detTable(), thresh, timescale)
	})
	
	changeUseCores <- observeEvent(input$useCores, {
	  rv$use_cores <- input$useCores
	})
	
	# Define number of available cores
	output$use_nCores <- renderUI({
	  available_cores <- ifelse(input$useLogicalCores,
	                            ncores_logical,
	                            ncores_physical)
	  sliderInput("useCores",
	              label=h5("Number of processing nodes to use for spectrogram generation:"),
	              min=1,
	              max=available_cores,
	              value=available_cores,
	              step=1,
	              round=TRUE,
	              ticks=FALSE)
	})

	output$renameButton <- renderUI({
	  if(rv$files_renamed == FALSE) {
	    actionButton("renameFilesButton",
	                 label="Rename Files",
	                 disabled=TRUE)
	  } else {
	    actionButton("renameFilesButton",
	                 label="Undo Renaming",
	                 disabled=TRUE)
	  }
	})
	
	output$selectClassCategories <- renderUI({
	  checkboxGroupInput("class_categories",
	                     label="Filter classes by category",
	                     choices=class_cats,
	                     selected="Owls")
	})
	
	output$selectPlotClass <- renderUI({
	  req(detTable())
	  selectInput("class_selection",
	              label="Target class",
	              choices=rv$visible_classes)
	})

}


shinyApp(ui = ui, server = server)
