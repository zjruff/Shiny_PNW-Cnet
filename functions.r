# Updated Feb 2022.
# Function definitions for the Shiny_PNW-Cnet app.

# Extract short clips from longer wav files. Assumes clips are 12 s in length.
wavExtract <- function(srcfile, dstfile, sox_path) {
	bdname = basename(dstfile)
	part = as.integer(strsplit(strsplit(bdname, "_")[[1]][6], "\\.")[[1]][1])
	offs = 12*(part - 1)
	sox_args = sprintf("%s %s trim %d 12", srcfile, dstfile, offs)
	system2(sox_path, sox_args)
}

# A function to generate spectrograms representing non-overlapping 12 s segments of
# long audio files. Passed to a cluster of [ncores] worker processes via parLapply.
makeImgs <- function(wav, outdir, sox_path) {
	params = readWave(wav, header=TRUE)
	wavlength = params$samples / params$sample.rate
	nclips = round(wavlength / 12, 0)
	fname = basename(wav)
	for (i in 1:nclips) {
		outname = sprintf("%s_part_%03d.png", str_sub(fname, 1, -5), i)
		outpath = file.path(outdir, outname)
		offs = 12*(i - 1)
		dur = if (offs + 12 > wavlength) { wavlength - 12 } else {12}
		sox_args = sprintf('-V1 "%s" -n trim %d %.3f remix 1 rate 8k spectrogram -x 1000 -y 257 -z 90 -m -r -o "%s"', wav, offs, dur, outpath)
		system2(sox_path, sox_args)
	}
}

# Function to get the rec date from properly structured filenames.
getDate <- Vectorize(function(filename) {
	rawdate = strsplit(filename, "_")[[1]][3]
	recdate = as.Date(rawdate, format="%Y%m%d")
} )

# Get the recording station from properly structured filenames. Station ID can be
# any format but must be preceded by a dash and followed by an underscore.
getStn <- Vectorize(function(filename) {
	stn = regmatches(filename, regexpr("-[[:alnum:]]+?[[:punct:]]*?_", filename))
	stn_trimmed = str_sub(stn, 2, -2) # Remove leading "-" and trailing "_"
	sprintf("Stn_%s", stn_trimmed)
} )

# Formats the Week column nicely
getStrWeek <- Vectorize(function(numweek) {
	sprintf("Week_%02d", numweek)
} )

# Search a directory tree for files with the right extension and return a list.
findFiles <- function(topdir, extension) {
	files = list.files(path = topdir,
						pattern = sprintf("%s$", extension),
						recursive = TRUE,
						full.names = TRUE)
	return(files)
}

# Returns the basename of the source file of a part_xxx.wav file.
getSrcFile <- Vectorize(function(filename) {
	srcfile = sprintf("%s.wav", str_split(filename, "_part_")[[1]][1])
	return(srcfile)
})

getStrPart <- Vectorize(function(filename) {
  part = regmatches(filename, regexpr("part_[[:digit:]]*?.png", filename))
  strpart = str_sub(part, 1, -5)
  return(strpart)
})

# Replaces the above; allows for part numbers >999 (for very long wav files).
getOffset <- function(filename) {
  part = regmatches(filename, regexpr("part_[[:digit:]]*?.png", filename))
  numpart = as.integer(str_sub(part, 6, -5))
  offs = 12*(numpart - 1)
  return(offs)
}

# Converts seconds to mm:ss string
getStrOffset <- Vectorize(function(offs) {
	hrs = floor(offs / 3600)
	mins = floor(offs / 60) - (hrs * 60)
	secs = offs - (hrs * 3600) - (mins * 60)
	str_offs = ifelse(hrs > 0, sprintf("%02d:%02d:%02d", hrs, mins, secs),
	                  sprintf("%02d:%02d", mins, secs))
	return(str_offs)
})

# Populate the FOLDER column in a Kaleidoscope review file; yields a subfolder
# relative to [indir].
getFolder <- Vectorize(function(filepath, indir) {
	dir = dirname(filepath)
	folder = str_sub(str_split(dir, indir)[[1]][-1], 2, -1)
	return(folder)
})

# Return the duration of a .wav file in seconds, or 0 if it cannot be read.
getDuration <- function(path) {
  wavLength <-tryCatch( { 
    params <- suppressWarnings(readWave(path, header = TRUE))
    params$samples / params$sample.rate
  }, error = function(cond) { 0 } )
}

# Use a Keras model file to make predictions on images within a folder.
# targetDir should be .../Temp, i.e. the parent folder.
CNN_proc <- function(targetDir, model_path, class_names, batchsize) {
	
	outputDir = dirname(targetDir)

	# Figure out how many images we're working with, store their names for output
	filenames = as.data.frame(list.files(file.path(targetDir, "images")))
	names(filenames) = c("Filename")
	nfiles = dim(filenames)[1]

	# Figure out how many batches the CNN has to process to get through all the images
	if(nfiles <= batchsize) { nsteps = 1 } else {
		if (nfiles %% batchsize == 0) { nsteps = nfiles / batchsize } else {
			nsteps = floor(nfiles / batchsize) + 1
			}
		}
	# Load the trained model from a local file; set up the generator to supply batches 
	# of data to the CNN
	cnn_model = load_model_hdf5(model_path)
	img_generator = flow_images_from_directory(targetDir, generator = image_data_generator(rescale=(1.0/255)),
		target_size = c(257,1000), color_mode = "grayscale", batch_size = batchsize, shuffle = FALSE)

	# Run through the images in batches until they have all been processed. Write output
	# to a CSV file.
	predictions = as.data.frame(predict(object = cnn_model, 
	                                    x = img_generator, 
	                                    steps = nsteps, 
	                                    verbose=1))
	names(predictions) = class_names
	output_frame = cbind(filenames, predictions)
	return(output_frame)
}

# Function to read an existing CNN prediction file and output a "review" file
# listing the clips that need to be validated. Optionally, can create a review 
# file that can be reviewed using Kaleidoscope (Pro or Lite) without the need to
# extract short clips.
# Also creates a "Detection_Summary" file listing number of apparent detections
# of each class at each station by week across a range of detection thresholds.
makeReviewFile <- function(predfile, outputmode="kscope") {
	
	out_dir <- dirname(predfile)
	in_name <- basename(predfile)
	preds <- read.csv(predfile)
	
	field_names <- names(preds)
	class_names <- field_names[2:length(field_names)]
	
	preds <- preds %>% mutate(RecDate = getDate(Filename)) %>% 
	  mutate(day1 = min(RecDate)) %>% 
	  mutate(Week = getStrWeek(floor((RecDate - day1) / 7) + 1),
	         Stn = getStn(Filename)) %>% 
	  select(Filename, all_of(class_names), Stn, Week)

	output_df <- preds %>% filter(FALSE) %>% 
	  mutate(Sortlabel = character())
	
	labeled <- character()

	# Pull STOC and STOC_IRREG clips first
	clips <- preds %>% filter(preds[, "STOC"] >= 0.25 | preds[, "STOC_IRREG"] >= 0.25)
	if (nrow(clips) > 0) { 
	  clips$Sortlabel = "STOC"
	  output_df <- bind_rows(output_df, clips)
	  labeled <- output_df %>% pull(Filename)
	  }
	
	for (label in class_names) {
		if (label %in% c("STOC", "STOC_IRREG")) {
			clips <- preds[FALSE, ]
		} else { 
			  clips <- preds %>% filter(!(Filename %in% labeled)) %>% 
			    filter(get(label) >= 0.95) }
  	    new_labeled <- clips %>% pull(Filename)
  	    labeled <- c(labeled, new_labeled)
  			if (nrow(clips) > 0) {
  				clips$Sortlabel <- label
  				output_df <- bind_rows(output_df, clips) 
			}
		}
	
	if(nrow(output_df) == 0) {
	  return(0)
	} else {
    	# Sort dataframe by label and then by filename, and write output to file
    	output_df <- output_df %>% arrange(Sortlabel, Filename)
    	out_name <- in_name %>% str_replace("CNN_Predictions_", "") %>% 
    	  str_replace(".csv", "_review.csv")
    	out_path <- file.path(out_dir, out_name)
    	write_csv(output_df, out_path)
      
    	if(outputmode == "kscope") {
    		kscope_table <- makeKscopeTable(output_df, out_dir)
    		kscope_path <- out_path %>% str_replace("review.csv", "review_kscope.csv")
    		write_csv(kscope_table, kscope_path)
    	}
    	return(nrow(output_df))
    }
}

# Creates a file that imitates the files created by the clustering function in 
# Kaleidoscope and can be read by the same program. 
makeKscopeTable <- function(review_df, srcdir) {
	review_df$TOP1DIST = sapply(seq_len(nrow(review_df)),
							  function(i) { review_df[i, review_df$Sortlabel[[i]], 
													  drop=TRUE] })
	workdf = review_df %>% select(c("Filename", "Sortlabel", "TOP1DIST", "Stn", "Week"))
	workdf <- workdf %>% mutate(OFFSET = getOffset(Filename),
							  TOP1MATCH = Sortlabel,
							  "IN FILE" = getSrcFile(Filename),
							  "SORT" = sprintf("%s_%s_%s", Sortlabel, Stn, Week),
							  PART = getStrPart(Filename),
							  MMSS = getStrOffset(OFFSET))

	wavs = as.data.frame(findFiles(srcdir, ".wav"))
	names(wavs) = "Path"
	wavs$"IN FILE" = basename(wavs$Path)

	output_df = left_join(x=workdf, y=wavs, by = "IN FILE")

	output_df$FOLDER = getFolder(output_df$Path, indir=srcdir)
	output_df <- output_df %>% mutate(CHANNEL = 0, DURATION = 12) %>% 
	select(FOLDER, "IN FILE", CHANNEL, OFFSET, DURATION, TOP1MATCH,
		   TOP1DIST, MMSS, SORT, PART) %>%
	mutate(Fmin='', Fmean='', Fmax='', DATE='', TIME='', HOUR='', "DATE-12"='',
		   "TIME-12"='', "HOUR-12"='', .before=TOP1MATCH) %>%
	mutate(VOCALIZATIONS=1, "MANUAL ID" = '', .after=PART)
	return(output_df)
}

# Extracts short WAV files to be validated manually. Will be sorted into subfolders
# by presumptive label, then by station, then by week.
extractWavs <- function(reviewfile, topdir, soxpath) {
	# Read and restructure the Review file
	inframe = read.csv(reviewfile) %>% select(Filename, Sortlabel, Week)
	
	if(nrow(inframe) == 0) { 
	  cat("\nNo potential detections found.\n")
	  return() 
	  } else {
	    cat(sprintf("\nExtracting wav files beginning at %s...\n", format(Sys.time(), "%X")))
    	inframe$SrcFile = getSrcFile(as.character(inframe$Filename))
    	# Build the list of Wav files in the directory tree
    	wav_df = as.data.frame(findFiles(topdir, ".wav"))
    	names(wav_df) = "SrcPath"
    	wav_df$SrcFile = basename(as.character(wav_df$SrcPath))
    	# Merge information about the wavs in the directory tree with the clips that need
    	# to be extracted
    	newframe = merge(x=inframe, y=wav_df, by="SrcFile")
    	newframe$SrcFile = NULL
    	
    	# Create review directories for each target class found in review file
    	newframe$Stn = getStn(as.character(newframe$Filename))
    	#newframe$StrWeek = getStrWeek(newframe$Week)
    	newframe$DstDir = file.path(topdir, "Review", newframe$Sortlabel, newframe$Stn, newframe$Week)
    
    	for (x in unique(newframe$DstDir)) {
    		dir.create(x, showWarnings=FALSE, recursive=TRUE) }
    	
    	newframe$DstPath = file.path(newframe$DstDir, str_replace(newframe$Filename, "png", "wav"))
    	
    	# Set up the cluster to extract wav files
    	ncores = detectCores()
    	cl = makeCluster(ncores)
    	clusterExport(cl = cl, varlist = c("wavExtract", "newframe", "soxpath"), envir = environment())
    	clusterMap(cl = cl, wavExtract, newframe$SrcPath, newframe$DstPath, soxpath)
    	stopCluster(cl)
    	
    	# Report success
    	newwavs = findFiles(file.path(topdir, "Review"), ".wav")
    	cat(sprintf("Finished at %s. %d clips extracted.\n", format(Sys.time(), "%X"), length(newwavs)))
  	  }
}

# Just filters out the wav files produced by extractWavs so we don't reprocess them.
rmParts <- function(fname) {
	if (str_detect(fname, "_part_")) { FALSE } else { TRUE }
}

# Builds a table of apparent detections by station and week across a range of 
# detection thresholds based on a set of CNN predictions.
buildDetTable <- function(predictions) {
  class_names <- names(predictions[2:ncol(predictions)])
  predictions <- predictions %>%
    mutate(Rec_Date = getDate(Filename),
           Station = getStn(Filename),
           day1 = min(Rec_Date),
           Week = floor((Rec_Date - day1) / 7) + 1) %>% 
    select(Filename, Station, Rec_Date, Week, all_of(class_names))
  
  thresh_list <- c(seq(from=0.05, to=0.95, by=0.05), 0.98, 0.99)
  
  det_table <- predictions %>% filter(FALSE) %>% 
    select(Station, Rec_Date, Week, AEAC:ZEMA) %>% 
    mutate(Threshold = NA, .before = AEAC)
  
  for(thresh in thresh_list) {
    dets <- predictions %>% group_by(Station, Rec_Date, Week) %>% 
      summarize(across(AEAC:ZEMA, function(x) length(which(x >= thresh))),
                .groups = "keep") %>% 
      mutate(Threshold = sprintf("%.2f", thresh), .after = Week)
      det_table <- rbind(det_table, dets)
  }
  det_table <- det_table %>% mutate(Week = as.integer(Week))
  return(det_table)
}

# Creates bar plot of apparent detections per week by recording station, using
# the table created by buildDetTable().
buildDetPlot <- function(class, det_table, threshold, timescale) {
  if(!(class %in% names(det_table))) {
    detPlot <- ggplot() + theme_void()
  } else {
      time_group <- ifelse(timescale == "Daily", "Date", "Week")
      
      day1 <- as_date(min(det_table$Rec_Date))
      str_day1 <- strftime(day1, "%d %b %Y")
      dayn <- as_date(max(det_table$Rec_Date))
      
      dets <- det_table %>%
        filter(Threshold == threshold) %>%
        select(Station, Rec_Date, Week, Threshold, all_of(class)) %>%
        rename(Count = class) %>% 
        mutate(Date = as_date(Rec_Date))
      
      detPlot <- dets %>% ggplot() +
          geom_bar(mapping = aes(x = get(time_group), y = Count, fill = Station), 
                   stat="identity", show.legend = FALSE) +
          facet_wrap(~Station) +
          labs(x = time_group, y = "Apparent detections", 
               title = sprintf("Apparent %s detections by %s, threshold = %s",
                               class, str_to_lower(time_group), threshold),
               subtitle = sprintf("Earliest recording date = %s", str_day1)) +
          theme_bw(base_size = 14) +
          scale_fill_brewer(palette = "Dark2")
          
      if(time_group == "Date") {
        detPlot <- detPlot + coord_cartesian(xlim = c(day1, dayn))
      }
  }
  return(detPlot)
}
