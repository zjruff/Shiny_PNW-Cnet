# Updated Aug 2022.
# Makes use of PNW-Cnet version 4 (51-class). See ./target_classes.csv for 
# descriptions of each class.

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
source_python("./scripts/pnw-cnet_v4_predict.py")

ncores_logical = detectCores()
ncores_physical = detectCores(logical = FALSE)

model_path = "./PNW-Cnet_v4_TF.h5"

# Change the following line if you installed SoX in a different location.
sox_dir = "C:/Program Files (x86)/sox-14-4-2"
sox_path = file.path(sox_dir, "sox")

class_desc <- read_csv("./target_classes.csv", show_col_types = FALSE)
class_list <- class_desc %>% 
  mutate(Str_Class = paste0(Class, " - ", Sound)) %>% 
  pull(Str_Class)
class_names <- class_desc %>% pull(Class)

ui = fluidPage(
	useShinyjs(),
	
	titlePanel("Neural net processing for audio data"),

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
		    tabPanelBody("inputControlPanel",
      		textInput("topdir", h5("Copy and paste (recommended) or type target directory path into box and click Check Directory:"),
      		          placeholder = "F:\\Path\\to\\directory"),
      		actionButton("checkdirbutton", label="Check Directory"),
      		br(),
      		h5("Click Process Files to generate spectrograms and use PNW-Cnet to predict class scores. This may take several hours. Please make sure all inputs are correct and that your computer will not go to sleep, shut down or restart during this process."),
      		actionButton("processbutton", label = "Process Files", disabled = TRUE),
      		br(),
      		h5("Click Create Review File to examine an existing CNN_Predictions file and determine which clips need to be validated."),
      		actionButton("revExtButton", label = "Create Review File", disabled = TRUE),
      		br(),
      		h5("Click Extract Review Clips to extract apparent detections as 12-second clips for further review or archival."),
      		actionButton("extractWavsButton", label = "Extract Review Clips", disabled = TRUE),
      		br(),
      		h5("Click Explore Detections to view apparent detections plotted over time by class and recording station."),
      		actionButton("exploreDetsButton", label = "Explore Detections", disabled = TRUE),
      		br(),
      		h5("Click Settings and Utilities to configure the app's behavior and find tools to assist in audio processing."),
      		actionButton("settingsButton", "Settings and Utilities")
		    ),
		    
		    tabPanelBody("exploreControlPanel",
		      textOutput({"exploreTitle"}),
		      h5(" Select a target class and a detection threshold to see apparent detections plotted by station over time."),
		      selectInput("class_selection", label = "Target class",
		                  choices = class_list),
		      
		      selectInput("plot_threshold", label = "Detection threshold",
		                  choices = c("0.99", "0.98", "0.95", "0.90", "0.85", "0.80",
		                              "0.75", "0.70", "0.65", "0.60", "0.55", "0.50",
		                              "0.45", "0.40", "0.35", "0.30", "0.25", "0.20",
		                              "0.15", "0.10", "0.05"), selected = "0.95"),
		      
		      selectInput("plot_timescale", label = "Summarize detections...",
		                  choices = c("Weekly", "Daily")),
		      
		      actionButton("backButton", label = "Back to inputs"),
		    ),
		    tabPanelBody("settingsControlPanel",
                     h3("Settings and Utilities"),
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
  	                                         style="height:500px;overflow-y:scroll"))))

  	  )
  	)
	)
)

# Reactive functions or variables that respond to events within the interface go here.
server = function(input, output, session) {
  rv <- reactiveValues()
  rv$show_rename_preview_table <- FALSE
  rv$files_renamed <- FALSE
  
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
		topdir = correctedDir()
		if(dir.exists(topdir)) { "valid" } else { "invalid" }
	})
  
	# Replaces backslashes with forward slashes in input path for neatness.
	correctedDir <- reactive({
		orig = input$topdir
		corrected = correctPath(orig)
	})
	
	correctedOutputDir <- reactive({
	  orig = input$customOutputDir
	  corrected = correctPath(orig)
	})
	
	# renameLogFile <- eventReactive(input$checkdirbutton, {
	#   top_dir <- correctedDir()
	#   log_path <- file.path(top_dir, "Rename_Log.csv")
	#   if(file.exists(log_path)) {
	#     return(log_path)
	#   } else {
	#     return("")
	#   }
	# })
	
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
	
	showPreviewTable <- observeEvent(input$showPreviewButton, {
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

	# Calculated when the button is pressed. Return a list of all wav files in the
	# directory tree rooted at the input directory.
	getWavs <- eventReactive(input$checkdirbutton, {
		in_dir = correctedDir()
		wavs_found = findFiles(in_dir, ".wav")
		valid_wavs = wavs_found[ lapply(wavs_found, file.size) >= 500000 ]
		valid_wavs[ lapply(valid_wavs, rmParts) == TRUE ]
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
	    # toggleState(id = "processbutton", condition = TRUE)
	    return()
	  }
    
	  total_duration <- sum(sapply(wavs, getDuration)) / 3600
		
    cnn_path <- model_path
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

		use_cores <- input$useCores
		
		spectro_start_time <- Sys.time()
		
		if(length(pngs) > 0) {
			cat(sprintf("\nImage data already present in %s.\n", image_dir))
		} else {
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
		
		cat(sprintf("\nProceeding to CNN classification at %s...\n", 
		            format(predict_start_time, "%X")))
		
		# Classify spectrograms, write predictions to output file
		predictions <- makePredictions(image_dir, cnn_path) %>% 
		  mutate(across(AEAC:ZEMA, function(x) 
		    round(x, digits=5)))
		
		output_file <- file.path(target_dir, sprintf("CNN_Predictions_%s.csv", outname))
		write_csv(predictions, output_file)
		
		predict_end_time <- Sys.time()
		
		cat(sprintf("\nFinished at %s.\n", 
		            format(predict_end_time, "%X")))
		cat(sprintf("\nClass scores written to %s in folder %s.\n", basename(output_file), target_dir))
		
		cat("\nSummarizing detections...\n")
		det_table <- buildDetTable(predictions)
		det_file_name <- sprintf("%s_detection_summary.csv", outname)
		det_file_path <- file.path(target_dir, det_file_name)
		write_csv(det_table, det_file_path)
		cat(sprintf("\nDetection summary written to file.\n"))
		
		run_time <- as.numeric(predict_end_time - spectro_start_time, units="hours")
		total_duration_pretty <- total_duration %>% round(digits=1) %>% format(big.mark=",")
		
		cat(sprintf("\nSuccessfully processed %s hours of data in %.1f hours (ratio: %.1f).\n",
		            total_duration_pretty, run_time, total_duration / run_time))
		
		if(input$deleteSpectrogramsWhenDone) {
		  cat("\nDeleting spectrograms and removing temporary directories... ")
		  temp_dir <- dirname(image_dir)
		  unlink(temp_dir, recursive=TRUE)
		  cat("Done.\n")
		}
		
		showModal(modalDialog(title = "Processing complete",
		                      "Audio processing complete. Class scores written to file.",
		                      size = "m",
		                      footer = modalButton("Dismiss"),
		                      easyClose = TRUE))

		toggleState(id = "checkdirbutton", condition = TRUE)
		click(id = "checkdirbutton")
	})

	# Creates a review file based on the CNN results file in the folder specified.
	mkReviewFile <- observeEvent(input$revExtButton, {
	  in_dir = correctedDir()
		predFiles = list.files(path = in_dir, 
			pattern = "CNN_[Pp]+redictions_[[:alnum:][:punct:]]+.csv", 
			full.names=TRUE)
		if(length(predFiles) == 0) {
		  cat("\nNo prediction files found in target directory.\n")
		  return()
		} else { predFile = predFiles[[1]] }
		cat(sprintf("\nAtttempting to create review file based on %s...\n", 
		            basename(predFile)))
		nrevlines <- makeReviewFile(predFile)
    if(nrevlines == 0) {
      cat("\nPrediction file contains no potential detections. No review file created.\n")
    } else {
      cat(sprintf("\n%d potential detections written to review file.\n", nrevlines))
    }
		click(id = "checkdirbutton")
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

	detTable <- eventReactive(input$exploreDetsButton, {
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
	  req(detTable())
	  target_class <- str_split(input$class_selection, " - ")[[1]][1]
	  thresh <- input$plot_threshold
	  timescale <- input$plot_timescale
	  buildDetPlot(target_class, detTable(), thresh, timescale)
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

}


shinyApp(ui = ui, server = server)
