# Updated Nov 2022.
# Function definitions for the Shiny_PNW-Cnet app.

# Extract short clips from longer wav files. Assumes clips are 12 s in length.
wavExtract <- function(src_file, dst_file, sox_path) {
	bdname = basename(dst_file)
	part = as.integer(strsplit(strsplit(bdname, "_")[[1]][6], "\\.")[[1]][1])
	offs = 12*(part - 1)
	sox_args = sprintf("%s %s trim %d 12", src_file, dst_file, offs)
	system2(sox_path, sox_args)
}

# A function to generate spectrograms representing non-overlapping 12 s segments of
# long audio files. Executed by a cluster of worker processes using clusterMap.
makeImgs <- function(wav_path, output_dir, sox_path) {
  if(!dir.exists(output_dir)) { dir.create(output_dir, recursive=TRUE) }
  
  params = readWave(wav_path, header=TRUE)
  wav_length = params$samples / params$sample.rate
	n_clips = round(wav_length / 12, 0)
	fname = basename(wav_path)
	
	for (i in seq(n_clips)) {
		image_filename = sprintf("%s_part_%03d.png", str_sub(fname, 1, -5), i)
		image_path = file.path(output_dir, image_filename)
		offs = 12*(i - 1)
		duration = if (offs + 12 > wav_length) { wav_length - 12 } else {12}
		sox_args = sprintf('-V1 "%s" -n trim %d %.3f remix 1 rate 8k spectrogram -x 1000 -y 257 -z 90 -m -r -o "%s"', wav_path, offs, duration, image_path)
		system2(sox_path, sox_args)
	}
	return()
}

# Make a nicely formatted table showing the wavs found in the directory tree
makeWavTable <- function(wav_list, top_dir) {
  wav_table <- as.data.frame(wav_list) %>% 
    rename(Path = wav_list) %>% 
    mutate(Folder = getFolder(Path, top_dir),
           File = basename(Path),
           `Size (MB)` = sprintf("%.1f", file.size(Path) / 1024**2),
           `Duration (s)` = sprintf("%.0f", getDuration(Path))) %>% 
    select(-Path)
  return(wav_table)
}

# Replace backslashes with forward slashes in user-supplied paths
correctPath <- function(raw_path) {
  corrected_path <- str_replace_all(raw_path, "\\\\", "/")
  return(corrected_path)
}

# Make an output folder in output_dir with the same name as target_dir and 
# containing the ./Temp/images structure we want. Return the path to the images
# directory.
makeOutputDir <- function(target_dir, output_dir) {
  target_dir <- correctPath(target_dir)
  output_dir <- correctPath(output_dir)
  target_dir_name = basename(target_dir)
  image_dir = file.path(output_dir, target_dir_name, "images")
  return(image_dir)
}

# Make a list of output directories part_01, part_02, ..., part_n to break up
# the spectrogram generation. Windows seems to have an easier time when folders 
# don't contain hundreds of thousands of files.
# Not quite optimal because wave files can vary in length. Would be better to try 
# to equalize the number of *spectrograms* that wind up in each folder.
makeSpectroDirList <- function(wav_list, out_dir, n_chunks) {
  chunk_size <- floor(length(wav_list) / n_chunks) + 1
  out_dirs <- sapply(seq_along(wav_list),
                     function(i) file.path(out_dir,
                                           sprintf("part_%02d", 
                                                   (floor(i / chunk_size) + 1))))
  return(out_dirs)
}

# Function to get the rec date from properly structured filenames.
getDate <- Vectorize(function(filename) {
	raw_date = strsplit(filename, "_")[[1]][3]
	rec_date = as.Date(raw_date, format="%Y%m%d")
} )

# Get the recording station from properly structured filenames. Station ID can be
# any format but must be preceded by a dash and followed by an underscore.
getStn <- Vectorize(function(filename) {
	stn = regmatches(filename, regexpr("-[[:alnum:]]+?[[:punct:]]*?_", filename))
	stn_trimmed = str_sub(stn, 2, -2) # Remove leading "-" and trailing "_"
	sprintf("Stn_%s", stn_trimmed)
} )

# Formats the Week column nicely
getStrWeek <- Vectorize(function(num_week) {
	sprintf("Week_%02d", num_week)
} )

# Search a directory tree for files with the right extension and return a list.
findFiles <- function(top_dir, extension) {
	files = list.files(path = top_dir,
						pattern = sprintf("%s$", extension),
						recursive = TRUE,
						full.names = TRUE)
	return(files)
}

# Returns the basename of the source file of a part_xxx.wav file.
getSrcFile <- Vectorize(function(filename) {
	src_file = sprintf("%s.wav", str_split(filename, "_part_")[[1]][1])
	return(src_file)
})

getStrPart <- Vectorize(function(filename) {
  part = regmatches(filename, regexpr("part_[[:digit:]]*?.png", filename))
  str_part = str_sub(part, 1, -5)
  return(str_part)
})

# Replaces the above; allows for part numbers >999 (for very long wav files).
getOffset <- function(filename) {
  part = regmatches(filename, regexpr("part_[[:digit:]]*?.png", filename))
  num_part = as.integer(str_sub(part, 6, -5))
  offs = 12*(num_part - 1)
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
# relative to [in_dir].
getFolder <- Vectorize(function(file_path, in_dir) {
	dir = dirname(file_path)
	folder = str_sub(str_split(dir, in_dir)[[1]][-1], 2, -1)
	return(folder)
})

# Return the duration of a .wav file in seconds, or 0 if it cannot be read.
getDuration <- Vectorize(function(path) {
  wav_length <-tryCatch( { 
    params <- suppressWarnings(readWave(path, header = TRUE))
    params$samples / params$sample.rate
  }, error = function(cond) { 0 } )
})

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
makeKscopeTable <- function(review_df, input_dir) {
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

	wavs = as.data.frame(findFiles(input_dir, ".wav"))
	names(wavs) = "Path"
	wavs$"IN FILE" = basename(wavs$Path)

	output_df = left_join(x=workdf, y=wavs, by = "IN FILE")

	output_df$FOLDER = getFolder(output_df$Path, in_dir=input_dir)
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
extractWavs <- function(review_file, top_dir, sox_path) {
	# Read and restructure the Review file
	inframe = read.csv(review_file) %>% select(Filename, Sortlabel, Week)
	
	if(nrow(inframe) == 0) { 
		cat("\nNo potential detections found.\n")
		return() 
	} else {
		cat(sprintf("\nExtracting wav files beginning at %s...\n", format(Sys.time(), "%X")))
		inframe$SrcFile = getSrcFile(as.character(inframe$Filename))
		# Build the list of Wav files in the directory tree
		wav_df = as.data.frame(findFiles(top_dir, ".wav"))
		names(wav_df) = "SrcPath"
		wav_df$SrcFile = basename(as.character(wav_df$SrcPath))
		# Merge information about the wavs in the directory tree with the clips that need
		# to be extracted
		newframe = merge(x=inframe, y=wav_df, by="SrcFile")
		newframe$SrcFile = NULL
		
		# Create review directories for each target class found in review file
		newframe$Stn = getStn(as.character(newframe$Filename))
		#newframe$StrWeek = getStrWeek(newframe$Week)
		newframe$DstDir = file.path(top_dir, "Review", newframe$Sortlabel, newframe$Stn, newframe$Week)
	
		for (x in unique(newframe$DstDir)) {
			dir.create(x, showWarnings=FALSE, recursive=TRUE) }
		
		newframe$DstPath = file.path(newframe$DstDir, str_replace(newframe$Filename, "png", "wav"))
		
		# Set up the cluster to extract wav files
		ncores = detectCores()
		cl = makeCluster(ncores)
		clusterExport(cl = cl, varlist = c("wavExtract", "newframe", "sox_path"), envir = environment())
		clusterMap(cl = cl, wavExtract, newframe$SrcPath, newframe$DstPath, sox_path)
		stopCluster(cl)
		
		# Report success
		newwavs = findFiles(file.path(top_dir, "Review"), ".wav")
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
buildDetPlot <- function(class_name, det_table, threshold, timescale) {
  if(!(class_name %in% names(det_table))) {
    det_plot <- ggplot() + theme_void()
  } else {
      time_group <- ifelse(timescale == "Daily", "Date", "Week")
      
      day1 <- as_date(min(det_table$Rec_Date))
      str_day1 <- strftime(day1, "%d %b %Y")
      dayn <- as_date(max(det_table$Rec_Date))
      
      dets <- det_table %>%
        filter(Threshold == threshold) %>%
        select(Station, Rec_Date, Week, Threshold, all_of(class_name)) %>%
        rename(Count = class_name) %>% 
        mutate(Date = as_date(Rec_Date))
      
      det_plot <- dets %>% ggplot() +
          geom_bar(mapping = aes(x = get(time_group), y = Count, fill = Station), 
                   stat="identity", show.legend = FALSE) +
          facet_wrap(~Station) +
          labs(x = time_group, y = "Apparent detections", 
               title = sprintf("Apparent %s detections by %s, threshold = %s",
                               class_name, str_to_lower(time_group), threshold),
               subtitle = sprintf("Earliest recording date = %s", str_day1)) +
          theme_bw(base_size = 14) +
          scale_fill_brewer(palette = "Dark2")
          
      if(time_group == "Date") {
        det_plot <- det_plot + coord_cartesian(xlim = c(day1, dayn))
      }
  }
  return(det_plot)
}
