# Updated Aug 2023.
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

# Remove all PNG files and subfolders from the image directory, then delete the
# image directory and its parent folder (either Temp or the remote directory)
clearImageDir <- function(image_dir) {
  temp_dir <- dirname(image_dir)
  unlink(temp_dir, recursive=TRUE)
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

# Show the results of renaming files using the RenameWavFiles script
makeRenamePreviewTable <- function(wav_list, top_dir) {
  wav_table <- as.data.frame(wav_list) %>% 
    rename(Path = wav_list) %>% 
    mutate(Folder = getFolder(Path, top_dir), 
           Current = basename(Path),  
           New = fixFilename(Path),
           Change = ifelse(Current == New, "No", "Yes") ) %>% 
    select(-Path)
  return(wav_table)
}

renameFiles <- function(preview_table, top_dir) {
  log_path <- file.path(top_dir, "Rename_Log.csv")
  n_change <- preview_table %>% filter(Change == "Yes") %>% nrow()
  n_total <- preview_table %>% nrow()
  
  if("Current" %in% names(preview_table)) {
    cat(sprintf("\nRenaming files. %d of %d filenames will be changed.\n",
                n_change, n_total))
    preview_table <- preview_table %>%
      mutate(In_Dir = file.path(top_dir, Folder),
             Current_Path = file.path(In_Dir, Current),
             New_Path = file.path(In_Dir, New))
    mapply(file.rename,
           preview_table$Current_Path,
           preview_table$New_Path)
    cat(sprintf("Filenames changed. Changes written to %s.\n", log_path))
    preview_table %>%
      select(Folder, Current, New) %>%
      rename(Former = Current) %>% 
      write_csv(file = log_path)
    } else {
        cat(sprintf("\nUndoing previous renaming operation. %d of %d filenames will be changed.\n",
                    n_change, n_total))
        preview_table <- preview_table %>%
          mutate(In_Dir = file.path(top_dir, Folder),
                 Current_Path = file.path(In_Dir, New),
                 Old_Path = file.path(In_Dir, Former))
        mapply(file.rename,
               preview_table$Current_Path,
               preview_table$Old_Path)
        file.remove(log_path)
        cat("Filenames reverted. Log file removed.")
      }
}

# Standardize filenames to have a location prefix based on the parent and grand-
# parent folders and a timestamp either copied from preexisting info or generated
# from each file's modification time.
fixFilename <- Vectorize(function(file_path) {
  fname_orig <- basename(file_path)
  folder <- dirname(file_path)
  stn <- str_split(basename(folder), '_')[[1]][-1] 
  site <- basename(dirname(folder))
  loc_code <- paste(site, stn, sep='-')
  tstamp <- regmatches(fname_orig, 
                       regexpr("[[:digit:]]{8}_[[:digit:]]{6}.wav", 
                               fname_orig,
                               ignore.case=TRUE))
  if(length(tstamp) == 0) {
    mod_time <- file.mtime(file_path)
    tstamp <- strftime(mod_time, "%Y%m%d_%H%M%S.wav")
  }
  fname_new <- paste(loc_code, tstamp, sep='_')
  return(fname_new)
})

# Just replaces backslashes with forward slashes so the app will not choke on 
# user-supplied paths.
correctPath <- function(raw_path) {
  corrected_path <- str_replace_all(raw_path, "\\\\", "/")
  return(corrected_path)
}

# Make an output folder in output_dir with the same name as target_dir and 
# containing the ./Temp/images structure we want. Return the path to the images
# directory.
makeImageDir <- function(target_dir, output_dir) {
  target_dir <- correctPath(target_dir)
  output_dir <- correctPath(output_dir)
  target_dir_name <- basename(target_dir)
  image_dir <- file.path(output_dir, target_dir_name, "images")
  return(image_dir)
}

getSiteName <- function(target_dir) {
  comps <-strsplit(basename(target_dir), "_")[[1]]
  if(length(comps) == 0) {
    return("")
  } else {
      if ( comps[1] == "Stn" ) {
        site_name <- paste(basename(dirname(target_dir)), comps[2], sep = "-") 
        } else {
          site_name <- basename(target_dir) 
        }
      return(site_name)
  }
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
	raw_date <- strsplit(filename, "_")[[1]][3]
	rec_date <- as.Date(raw_date, format="%Y%m%d")
} )

# Get the recording station from properly structured filenames. Station ID can be
# any format but must be preceded by a dash and followed by an underscore.
getStn <- Vectorize(function(filename) {
	stn <- regmatches(filename, regexpr("-[[:alnum:]]+?[[:punct:]]*?_", filename))
	stn_trimmed <- str_sub(stn, 2, -2) # Remove leading "-" and trailing "_"
	sprintf("Stn_%s", stn_trimmed)
} )

# Formats the Week column nicely
getStrWeek <- Vectorize(function(num_week) {
	sprintf("Week_%02d", num_week)
} )

getStrDay <- Vectorize(function(num_day, n_digits) {
  sprintf("Day_%s", 
          str_pad(as.character(num_day), n_digits, pad="0", side="left"))
})

# Search a directory tree for files with the right extension and return a list.
findFiles <- function(top_dir, extension) {
	files <- list.files(path = top_dir,
						pattern = sprintf("%s$", extension),
						recursive = TRUE,
						full.names = TRUE,
						ignore.case = TRUE)
	return(files)
}

# Returns the basename of the source file of a part_xxx.wav file.
getSrcFile <- Vectorize(function(filename) {
	src_file <- sprintf("%s.wav", str_split(filename, "_part_")[[1]][1])
	return(src_file)
})

getStrPart <- Vectorize(function(filename) {
  part <- regmatches(filename, regexpr("part_[[:digit:]]*?.png", filename))
  str_part <- str_sub(part, 1, -5)
  return(str_part)
})

# Replaces the above; allows for part numbers >999 (for very long wav files).
getOffset <- function(filename) {
  part <- regmatches(filename, regexpr("part_[[:digit:]]*?.png", filename))
  num_part <- as.integer(str_sub(part, 6, -5))
  offs <- 12*(num_part - 1)
  return(offs)
}

# Converts seconds to mm:ss string
getStrOffset <- Vectorize(function(offs) {
	hrs <- floor(offs / 3600)
	mins <- floor(offs / 60) - (hrs * 60)
	secs <- offs - (hrs * 3600) - (mins * 60)
	str_offs <- ifelse(hrs > 0, sprintf("%02d:%02d:%02d", hrs, mins, secs),
	                  sprintf("%02d:%02d", mins, secs))
	return(str_offs)
})

# Populate the FOLDER column in a Kaleidoscope review file; yields a subfolder
# relative to [in_dir].
getFolder <- Vectorize(function(file_path, in_dir) {
	dir <- dirname(file_path)
	folder <- str_sub(str_split(dir, in_dir)[[1]][-1], 2, -1)
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
makeReviewFile <- function(predfile, outputmode="kscope") {
	
	out_dir <- dirname(predfile)
	in_name <- basename(predfile)
	preds <- read_csv(predfile, show_col_types = FALSE)
	
	field_names <- names(preds)
	class_names <- field_names[2:length(field_names)]
	
	preds <- preds %>% 
	  mutate(RecDate = getDate(Filename)) %>% 
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
	review_df$TOP1DIST <- sapply(seq_len(nrow(review_df)),
							  function(i) { review_df[i, review_df$Sortlabel[[i]], 
													  drop=TRUE] })
	workdf <- review_df %>% 
	  select(c("Filename", "Sortlabel", "TOP1DIST", "Stn", "Week")) %>% 
	  mutate(OFFSET = getOffset(Filename), 
	         TOP1MATCH = Sortlabel,
	         INFILEUPPER = toupper(getSrcFile(Filename)),
	         SORT = sprintf("%s_%s_%s", Sortlabel, Stn, Week),
	         PART = getStrPart(Filename),
	         MMSS = getStrOffset(OFFSET))

	wavs <- data.frame("Path" = findFiles(input_dir, ".wav")) %>% 
	  mutate(`IN FILE` = basename(Path),
	         INFILEUPPER = toupper(`IN FILE`))
  
	output_df <- left_join(x = workdf, y = wavs, by = "INFILEUPPER") 
	
	output_df <- output_df %>% 
	  mutate(FOLDER = getFolder(Path, input_dir),
	         CHANNEL = 0, 
	         DURATION = 12) %>%
	  select(FOLDER, `IN FILE`, CHANNEL, OFFSET, DURATION, TOP1MATCH,
		   TOP1DIST, MMSS, SORT, PART) %>%
	  mutate(Fmin='', Fmean='', Fmax='', DATE='', TIME='', HOUR='', `DATE-12`='',
		   `TIME-12`='', `HOUR-12`='', .before=TOP1MATCH) %>%
	  mutate(VOCALIZATIONS = 1, `MANUAL ID` = '', .after=PART)
	
	return(output_df)
}

# Creates tables for both the _review and _review_kscope files including a 
# customizable set of target classes and corresponding score thresholds.
# Detections can be grouped daily or weekly. This pretty much supersedes both
# makeReviewFile() and makeKscopeTable().
makeCustomKscopeTable <- function(pred_path, class_df, timescale="Weekly") {
  time_group <- ifelse(timescale == "Daily", "Date", "Week")
  input_dir <- dirname(pred_path)
  in_name <- basename(pred_path)
  
  wavs <- data.frame("Path" = findFiles(input_dir, ".wav")) %>% 
    mutate(`IN FILE` = basename(Path),
           INFILEUPPER = toupper(`IN FILE`))
  
  # Read the prediction file and add date fields
  pred_df <- read_csv(pred_path, show_col_types=FALSE)
  pred_classes <- names(pred_df)[2:ncol(pred_df)]
  pred_df <- pred_df %>%
    mutate(RecDate = getDate(Filename),
           day1 = min(RecDate))
  
  # Only look for classes that actually appear in the prediction dataframe
  target_classes <- class_df %>% 
    filter(Class %in% pred_classes) %>% 
    pull(Class)

  # Filter the full set of predictions to just the lines that meet the score 
  # threshold for at least one class
  done <- character()
  for(i in seq_along(target_classes)) {
    class_name <- target_classes[i]
    threshold <- class_df %>% 
      filter(Class == class_name) %>% 
      distinct(Class, Threshold) %>% 
      pull(Threshold)
    new_dets <- pred_df %>% 
      filter(get(class_name) >= threshold,
             !(Filename %in% done)) %>% 
      mutate(TOP1MATCH = class_name,
             TOP1DIST = get(class_name),
             Threshold = threshold)
    done <- c(done, pull(new_dets, Filename))
    if(i == 1) {
      dets <- new_dets
    } else {
      dets <- bind_rows(dets, new_dets)
    }
  }

  # Add all the columns up front so we can use Select() to pare it down later.
  if(nrow(dets) == 0) {
    return()
  } else {
    dets <- dets %>%
      mutate(Week = getStrWeek(floor((RecDate - day1) / 7) + 1),
             Day = RecDate - day1 + 1,
             Stn = getStn(Filename),
             OFFSET = getOffset(Filename), 
             INFILEUPPER = toupper(getSrcFile(Filename)),
             PART = getStrPart(Filename),
             MMSS = getStrOffset(OFFSET),
             CHANNEL = 0,
             DURATION = 12) %>% 
      left_join(wavs, by = "INFILEUPPER") %>% 
      mutate(FOLDER = getFolder(Path, input_dir))
    
    # Add the SORT column combining top class, stn, and time group (day or week)
    if(time_group=="Date") {
      max_date <- max(dets$Day)
      n_digits <- floor(log10(max_date)) + 1
      dets <- dets %>% mutate(Day = getStrDay(Day, n_digits),
                              SORT = sprintf("%s_%s_%s", TOP1MATCH, Stn, Day))
    } else {
      dets <- dets %>% mutate(SORT = sprintf("%s_%s_%s", TOP1MATCH, Stn, Week))
    }
    
    # Create the _review table (just the filtered CNN_Predictions file with a few
    # extra columns)
    review_df <- dets %>% 
      select(Filename, all_of(pred_classes), Stn, Week, TOP1MATCH, Threshold) %>% 
      rename(Sortlabel = TOP1MATCH) %>% 
      arrange(Sortlabel, Stn, Week, Filename)
    
    # Create the review_kscope table (readable by Kaleidoscope)
    kscope_df <- dets %>% 
      select(FOLDER, `IN FILE`, CHANNEL, OFFSET, DURATION, TOP1MATCH,
             TOP1DIST, Threshold, MMSS, SORT, PART) %>%
      mutate(Fmin='', Fmean='', Fmax='', DATE='', TIME='', HOUR='', `DATE-12`='',
             `TIME-12`='', `HOUR-12`='', .before=TOP1MATCH) %>%
      mutate(VOCALIZATIONS = 1,
             `MANUAL ID` = '',
             .after=PART) %>% 
      arrange(SORT, `IN FILE`, PART)
    
    # Return both the regular review file dataframe and the review_kscope frame;
    # list can be indexed with result[[1]] and result[[2]]
    return(list(review_df, kscope_df))
  }
}

# Extracts short WAV files to be validated manually. Will be sorted into subfolders
# by presumptive label, then by station, then by week.
extractWavs <- function(review_file, top_dir, sox_path) {
	inframe <- read.csv(review_file) %>% select(Filename, Sortlabel, Week)
	
	if(nrow(inframe) == 0) { 
		cat("\nNo potential detections found.\n")
		return()
	} else {
		cat(sprintf("\nExtracting wav files beginning at %s...\n", format(Sys.time(), "%X")))
		
	  inframe <- inframe %>% 
		  mutate(SrcFile = toupper(getSrcFile(Filename)))
		
		wav_df <- data.frame("SrcPath" = findFiles(top_dir, ".wav")) %>% 
		  mutate(SrcFile = toupper(basename(SrcPath)))
		
		newframe <- merge(x = inframe, y = wav_df, by = "SrcFile") %>% 
		  mutate(Stn = getStn(Filename),
		         DstDir = file.path(top_dir, "Review", Sortlabel, Stn, Week))
		
		for (x in unique(newframe$DstDir)) {
			dir.create(x, showWarnings=FALSE, recursive=TRUE) }
		
		newframe <- newframe %>% 
		  mutate(DstPath = file.path(DstDir, str_replace(Filename, "png", "wav")))
		
		ncores <- detectCores()
		cl <- makeCluster(ncores)
		clusterExport(cl = cl, varlist = c("wavExtract", "newframe", "sox_path"), envir = environment())
		clusterMap(cl = cl, wavExtract, newframe$SrcPath, newframe$DstPath, sox_path)
		stopCluster(cl)
		
		newwavs <- findFiles(file.path(top_dir, "Review"), ".wav")
		cat(sprintf("Finished at %s. %d clips extracted.\n", 
		            format(Sys.time(), "%X"), 
		            length(newwavs)))
	  }
}

# Just filters out the wav files produced by extractWavs so we don't reprocess them.
# rmParts <- function(fname) {
# 	if (str_detect(fname, "_part_")) { FALSE } else { TRUE }
# }

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
	select(Station, Rec_Date, Week, all_of(class_names)) %>% 
	mutate(Threshold = NA, .before = first(class_names))

	for(thresh in thresh_list) {
	dets <- predictions %>% group_by(Station, Rec_Date, Week) %>% 
	  summarize(across(all_of(class_names), function(x) length(which(x >= thresh))),
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
        rename(Week_Num = Week, Count = all_of(class_name)) %>% 
        mutate(Date = as_date(Rec_Date)) %>% 
        group_by(Week_Num) %>%
        mutate(Week = min(Date))
      
      det_plot <- dets %>% ggplot() +
          geom_bar(mapping = aes(x = get(time_group), y = Count, fill = Station), 
                   stat="identity", show.legend = FALSE) +
          scale_x_date(date_breaks="1 week", date_labels="%m/%d") +
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
