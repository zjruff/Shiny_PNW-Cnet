# Updated March 2022.
# Makes use of PNW-Cnet version 4 (51-class). See ./target_classes.csv for 
# descriptions of each class.

library(keras)
library(lubridate)
library(parallel)
library(shinyjs)
library(tidyverse)
library(tuneR)

source("./functions.r")

ncores = detectCores(logical = FALSE)
model_path = "./PNW-Cnet_v4_TF.h5"

# Change the following line if you installed SoX in a different location.
sox_dir = "C:/Program Files (x86)/sox-14-4-2"
sox_path = file.path(sox_dir, "sox")

# Size of batches of spectrogram images fed to the CNN. A larger batch size may
# be slightly faster but will increase memory usage.
batchsize = 16

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
      		textInput("topdir", h5("Copy and paste (recommended) or type target directory path into box and click Check Directory:")),
      		actionButton("checkdirbutton", label="Check Directory"),
      		br(),
      		h5("Click Process Files to generate spectrograms and use CNN to predict class scores. This may take several hours. Please make sure all inputs are correct and that your computer will not go to sleep, shut down or restart during this process."),
      		actionButton("processbutton", label = "Process Files", disabled = TRUE),
      		br(),
      		h5("Click Create Review File to examine an existing CNN output file and determine which clips need to be validated."),
      		actionButton("revExtButton", label = "Create Review File", disabled = TRUE),
      		br(),
      		h5("Click Extract Review Clips to extract apparent detections as 12-second clips for further review or archival."),
      		actionButton("extractWavsButton", label = "Extract Review Clips", disabled = TRUE),
      		br(),
      		h5("Click Explore Detections to view apparent detections plotted over time by class and recording station."),
      		actionButton("exploreDetsButton", label = "Explore Detections", disabled = TRUE)
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
		    )
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
      		h4(textOutput({"wavsFound"})),
      		fluidRow(column(12, div(tableOutput( {"showFiles"} ), 
      		                        style="height:500px;overflow-y:scroll"))),
      		# br(),
      		# fluidRow(column(12, div(tableOutput( {"detTable"} ),
      		#                         style="height:500px;overflow-y:scroll")))
  	    ),
  	    tabPanelBody("exploreMainPanel",
  	                 plotOutput({ "detectionPlot" })
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
  
  # Checks if the input directory exists
	checkDir <- eventReactive(input$checkdirbutton, {
		topdir = correctedDir()
		if(dir.exists(topdir)) { "valid" } else { "invalid" }
	})
  
	# Replaces backslashes with forward slashes in input path for neatness.
	correctedDir <- reactive({
		orig = input$topdir
		corrected = str_replace_all(orig, "\\\\", "/")
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
	    toggleState(id = "revExtButton", condition = TRUE)
	    toggleState(id = "exploreDetsButton", condition = TRUE) 
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

		cnn_path = model_path
		targetDir = correctedDir()

		comps = strsplit(basename(targetDir), "_")[[1]]
		outname = if ( comps[1] == "Stn" ) {
			paste(basename(dirname(targetDir)), comps[2], sep = "-") } else {
			basename(targetDir) }

		workDir = file.path(targetDir, "Temp")
		imgDir = file.path(workDir, "images")
		if (!dir.exists(workDir)) { 
			dir.create(workDir) 
			dir.create(imgDir) }

		# Check if spectrograms have already been generated. If so, skip that step.
		pngs = findFiles(imgDir, ".png")
		
		if(length(pngs) > 0) {
			cat("Image data already present.\n")
		} else {
		  cat(sprintf("Using %d processors.\n", ncores))
		  cat(sprintf("Generating spectrograms starting at %s... \n", format(Sys.time(), "%X")))
			# Build the processing cluster and generate spectrograms
			cl = makeCluster(ncores)
			clusterEvalQ(cl, c(library(stringr), library(tuneR)))
			clusterExport(cl=cl, varlist=c('makeImgs', 'imgDir', 'wavs', 'sox_path'), 
			              envir = environment())
			parLapply(cl, wavs, makeImgs, outdir = imgDir, sox_path = sox_path)
			stopCluster(cl)
			cat(sprintf("Finished at %s.\n", format(Sys.time(), "%X")))
		}
		cat("\nProceeding to CNN classification...\n")
		
		# Classify spectrograms, write predictions to output file
		predictions <- CNN_proc(workDir, cnn_path, class_names, batchsize)
		outfile <- file.path(targetDir, sprintf("CNN_Predictions_%s.csv", outname))
		write_csv(predictions, outfile)
		
		cat(sprintf("Finished at %s.\n", format(Sys.time(), "%X")))
		cat(sprintf("Output written to %s in folder %s.\n", basename(outfile), targetDir))
		
		cat("\nSummarizing detections...\n")
		det_table <- buildDetTable(predictions)
		det_file_name <- sprintf("%s_detection_summary.csv", outname)
		det_file_path <- file.path(targetDir, det_file_name)
		write_csv(det_table, det_file_path)
		cat(sprintf("Detection summary written to file."))
		
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
	
	foundDetTable <- observeEvent(input$exploreDetsButton, {
	  req(getPredFile())
	  summaryFile <- getSummaryFile()
	  if(summaryFile == "") {
	    predFile <- getPredFile()
	    cat(sprintf("No summary file found. Generating detection summaries based on %s...\n", 
	        basename(predFile)))
	    preds <- read_csv(predFile, show_col_types = FALSE)
	    dets <- buildDetTable(preds)
	    outname <- predFile %>% str_replace("CNN_Predictions_", "") %>% 
	      str_replace(".csv", "_detection_summary.csv")
	    dets %>% write_csv(outname)
	    cat("Summary file generated.\n")
	    click(id="checkdirbutton")
	    updateTabsetPanel(inputId = "controls", selected = "exploreControlPanel")
	    updateTabsetPanel(inputId = "mainTabs", selected = "exploreMainPanel")
	  } else { }
	})
	
	output$wavsFound <- renderText({
		if (checkDir() == "invalid") { 
			"Invalid directory." } else {
			sprintf("Directory tree contains %d valid .wav files.", length(getWavs())) }
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
		getWavs()
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
