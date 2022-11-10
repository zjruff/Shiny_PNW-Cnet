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
  file.create("./settings") 
  }

custom_output_dir <- ""

source("./settings")
source("./functions.r")
source_python("./scripts/pnw-cnet_v4_predict.py")

ncores = detectCores(logical = FALSE)
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

	# Tabset panel allows switching between two modes: Input, in which the user
	# can designate a target directory and run the neural network, and Explore,
	# in which the user can examine detections from a CNN prediction file plotted
	# by class, day/week, station, and detection threshold. 
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
      		h5("Click Settings to configure the app's behavior."),
      		actionButton("settingsButton", "Settings")
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
		                 h4("Settings"),
		                 textInput("customOutputDir",
		                           h5("Choose the directory where spectrograms will be created. This folder will be created if it does not already exist. If left blank, spectrograms will be created in the directory containing your WAV files."),
		                           placeholder = "F:\\Path\\to\\directory",
		                           value=custom_output_dir),
		                 actionButton("saveOutputDirButton",
		                              label = "Save"),
		                 actionButton("clearOutputDirButton",
		                              label = "Clear"),

		                 checkboxInput("splitSpectrogramGeneration", label="Split spectrograms across multiple folders for faster processing (recommended)", value=TRUE),
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
  	                 plotOutput({ "detectionPlot" })
  	    ),
  	    tabPanelBody("settingsMainPanel",
  	                 # This creates a list of check boxes with labels but is very
  	                 # ugly and not super useful
  	                 # checkboxGroupInput("selectClasses",
  	                 #                    label=h4("Select classes"),
  	                 #                    choices=setNames(seq(1, length(class_names)),
  	                 #                                     class_names))
  	                 )
  	  )
  	)
	)
)

# Reactive functions or variables that respond to events within the interface go here.
server = function(input, output, session) {
  
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
	
	clearCustomOutputDir <- observeEvent(input$clearOutputDirButton, {
	  updateTextInput(session, "customOutputDir", value="")
	})
	
	saveCustomOutputDir <- observeEvent(input$saveOutputDirButton, {
	  output_dir <- correctedOutputDir()
	  config_file_path <- "./settings"
	  cat(sprintf('custom_output_dir="%s"', output_dir), file=config_file_path)
	})
	
	# Set the state of various actionButtons when Check Directory is clicked, based
	# on files that were or were not found in the target directory.
	updateButtons <- observeEvent(input$checkdirbutton, {
	  wavs <- listWavs()
	  predfile <- getPredFile()
	  revfile <- getRevFile()
	  if ( length(wavs) > 0 ) {
	    toggleState(id = "processbutton", condition = TRUE)
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
	
	# The list of all .wav files in the target directory, including subdirectories.
	listWavs <- reactive({
		in_dir = correctedDir()
		wavs_found = findFiles(in_dir, ".wav")
		valid_wavs = wavs_found[ lapply(wavs_found, file.size) >= 500000 ] # Ignore wavs with size < 500 KB or so
		valid_wavs[ lapply(valid_wavs, rmParts) == TRUE ]
	})
	
	# List of .csv files in the target directory. Used to update some values when
	# output is written to file.
	listCsvs <- reactive({
	  in_dir <- correctedDir()
	  csvs_found <- list.files(in_dir, pattern = "[[:alnum:][:punct:]]+.csv")
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

	# Starts processing wav files when user hits the button.. The envir argument 
	# to clusterExport is necessary when starting a cluster from inside a function, 
	# otherwise it defaults to the global environment, where variables created 
	# inside the function don't exist.
	processWavs <- observeEvent(input$processbutton, {
	  toggleState(id = "processbutton", condition = FALSE)
	  
	  wavs = listWavs()
	  
	  if(length(wavs) == 0) {
      cat("\nNo .wav files available to process.\n")
	    toggleState(id = "processbutton", condition = TRUE)
	    return()
	  }

		cnn_path <- model_path
		target_dir <- correctedDir()
    custom_output_dir <- correctedOutputDir()
	  
    # Check if the user entered a custom output directory for the spectrograms,
	  # create it if necessary, and use this value for image_dir. On error, it will
	  # use the default version.
		if(custom_output_dir == "") {
  		image_dir = file.path(target_dir, "Temp", "images")
		} else { 
  		  image_dir = makeOutputDir(target_dir, custom_output_dir) }
		
    if(!dir.exists(image_dir)) { dir.create(image_dir, recursive=TRUE) }
    
    # Try to split up spectrograms so there are no more than ~50,000 in a folder
    split_spectro_gen <- input$splitSpectrogramGeneration
    if( split_spectro_gen ) {
      durs <- sapply(wavs, getDuration)
      n_images <- sum(durs) / 12
      n_parts <- floor(n_images / 50000)
      spectro_dirs <- makeSpectroDirList(wavs, image_dir, n_parts)
    } else { spectro_dirs <- rep(image_dir, length(wavs)) }
    
		comps = strsplit(basename(target_dir), "_")[[1]]
		outname = if ( comps[1] == "Stn" ) {
			paste(basename(dirname(target_dir)), comps[2], sep = "-") } else {
			basename(target_dir) }

		# Check if spectrograms have already been generated. If so, skip that step.
		pngs = findFiles(image_dir, ".png")

		if(length(pngs) > 0) {
			cat(sprintf("\nImage data already present in %s.\n", image_dir))
		} else {
		  cat(sprintf("\nUsing %d processors.\n", ncores))
		  cat(sprintf("\nSpectrograms will be created in %s.\n", image_dir))
		  cat(sprintf("\nGenerating spectrograms starting at %s... \n", format(Sys.time(), "%X")))
			
		  # Build the multiprocessing cluster
			cl = makeCluster(ncores)
			clusterEvalQ(cl, c(library(stringr), library(tuneR)))
			clusterExport(cl = cl, varlist=c('makeImgs', 'image_dir', 'spectro_dirs',
			                                 'wavs', 'sox_path'), envir = environment())
			# Use the multiprocessing cluster to generate the spectrograms
			clusterMap(cl, wavs, spectro_dirs, fun = makeImgs, sox_path = sox_path)
			stopCluster(cl)
			cat(sprintf("\nFinished at %s.\n", format(Sys.time(), "%X")))
		}
		
		cat(sprintf("\nProceeding to CNN classification at %s...\n", format(Sys.time(), "%X")))
		
		# Classify spectrograms, write predictions to output file
		predictions <- makePredictions(image_dir, cnn_path)
		output_file <- file.path(target_dir, sprintf("CNN_Predictions_%s.csv", outname))
		write_csv(predictions, output_file)
		
		cat(sprintf("\nFinished at %s.\n", format(Sys.time(), "%X")))
		cat(sprintf("\nOutput written to %s in folder %s.\n", basename(output_file), target_dir))
		
		cat("\nSummarizing detections...\n")
		det_table <- buildDetTable(predictions)
		det_file_name <- sprintf("%s_detection_summary.csv", outname)
		det_file_path <- file.path(target_dir, det_file_name)
		write_csv(det_table, det_file_path)
		cat(sprintf("\nDetection summary written to file.\n"))
		
		showModal(modalDialog(title = "Processing complete",
		                      "Audio processing complete. CNN output written to file.",
		                      size = "s",
		                      footer = modalButton("Dismiss"),
		                      easyClose = TRUE))
		toggleState(id = "processbutton", condition = TRUE)
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
		# req(getWavs())
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
		in_dir = correctedDir()
		predFiles = list.files(path = in_dir,
		                       pattern="CNN_[Pp]+redictions_[[:alnum:][:punct:]]+.csv", 
		                       full.names=TRUE)
		if(length(predFiles) >= 1) { 
		  return(predFiles[[1]]) 
		  } else { 
		    return("") 
		  }
	})

	getRevFile <- eventReactive(input$checkdirbutton, {
		in_dir = correctedDir()
		revFiles = list.files(path = in_dir, pattern="[[:alnum:][:punct:]]+review.csv",
			full.names=TRUE)
		if(length(revFiles) >= 1) { revFiles[[1]] } else { "" }
	})

	getSummaryFile <- eventReactive(input$checkdirbutton, {
	  in_dir = correctedDir()
	  summaryFiles <- list.files(path = in_dir, 
	                            pattern = "[[:alnum:][:punct:]]+_detection_summary.csv",
	                            full.names=TRUE)
	  if(length(summaryFiles) >= 1) { summaryFiles[[1]] } else { "" }
	})

	detTable <- eventReactive(input$exploreDetsButton, {
	  req(getSummaryFile())
    read_csv(getSummaryFile(), show_col_types = FALSE) %>% 
      mutate(Threshold = sprintf("%.2f", Threshold))
	})

	customOutDir <- eventReactive(input$saveOutputDirButton, {
	  output_dir <- correctedOutputDir()
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
		makeWavTable(wavs, correctedDir())
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

}


shinyApp(ui = ui, server = server)
