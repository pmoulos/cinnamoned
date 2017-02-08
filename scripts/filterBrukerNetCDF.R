##########################################################################################
# A note... It is not really possible to filter intensity at the very raw data lavel...  #
# The data would become too sparse... What may be done is to convert intensities above   #
# or below a maximum/minimum value to a constant value, the one given. We should forget  #
# generally the missing values... They are not well tolerated by analysis software...	 #
##########################################################################################

# filterBrukerNetCDF is an R function that accepts as input a set of M/S NetCDF files and
# additional raw data filtering parameters and rewrites the files having filtered them based
# on the requested filters.
#
# Usage: filterBrukerNetCDF(cdf.files,
#							out.path=NULL,
#							filter.time=c(900,3000),
#							filter.mz=NULL,
#							filter.inten=NULL,
#							missing.value=-9999,
#							data.time="seconds")
#
# Input arguments:
#
# cdf.files    : The input .CDF files
# out.path	   : An optional path to write the produced files
# filter.time  : The minimum and maximum retention time (in seconds) below and avove of
#				 which the M/S data should be truncated. If filtering not wished, it should
#				 be NULL, else a vector of length 2. Defaults to c(900,3000).
# filter.mz    : The minimum and maximum m/z values below and avove of which the M/S data
# 			     will be truncated for each M/S scan. If filtering not wished, it should
#			     be NULL, else a vector of length 2. Defaults to NULL (no filtering).
# filter.inten : The baseline and saturated intensity that M/S below and above these values
# 			     data will be given  for each M/S scan. If this procedure is not wished,
#			     it should be NULL, else a vector of length 2. Defaults to NULL.
# data.time    : In what unit the retention time lives in the data. Defaults to seconds.
# 
# 
# Output:	   : NetCDF files with auto-generated file names
#
#
# Usage examples:
#
# Author: Panagiotis Moulos (pmoulos@eie.gr)
# Version: 1.0
# Creation date: 11-07-2011 (dd-mm-yyyy)
# Last update: 12-07-2011 (dd-mm-yyyy)
#
# TODO :
#

filterBrukerNetCDF <- function(cdf.files,out.path=NULL,filter.time=c(900,3000),
							   filter.mz=NULL,filter.inten=NULL,
							   missing.value=-9999,data.time="seconds")

{
	if (!require(RNetCDF))
		stop("R package RNetCDF is required!")

	# Some checks
	if (is.numeric(missing.value))
		missing.type <- "NC_DOUBLE"
	else if (is.character(missing.value) | is.na(missing.value))
		missing.type <- "NC_CHAR"
	else stop("The missing value must be numeric, character or NA.")
	if (!is.null(filter.time) & length(filter.time)!=2)
		stop("filter.time must be either NULL either a vector of length 2.")
	if (!is.null(filter.mz) & length(filter.mz!=2))
		stop("filter.mz must be either NULL either a vector of length 2.")
	if (!is.null(filter.inten) & length(filter.inten!=2))
		stop("filter.inten must be either NULL either a vector of length 2.")
	
	for (cdf in cdf.files)
	{
		disp("")
		disp(paste("Now processing file ",cdf,"...",sep=""))

		o<-open.nc(cdf)
		n.points <- dim.inq.nc(o,"scan_number")$length

		# General variables
		disp("Getting general variables...")
		scan.index <- var.get.nc(o,"scan_index")
		point.count <- var.get.nc(o,"point_count")

		# Measurements
		disp("Getting actual measurements...")
		mass <- var.get.nc(o,"mass_values")
		inten <- var.get.nc(o,"intensity_values")
		total.inten <- var.get.nc(o,"total_intensity")
		time <- var.get.nc(o,"scan_acquisition_time")

		# Get dummy variables
		disp("Getting dummy variables...")
		error.log <- var.get.nc(o,"error_log")
		a.d.sampling.rate <- var.get.nc(o,"a_d_sampling_rate")
		a.d.coaddition.factor <- var.get.nc(o,"a_d_coaddition_factor")
		scan.duration <- var.get.nc(o,"scan_duration")
		inter.scan.time <- var.get.nc(o,"inter_scan_time")
		resolution <- var.get.nc(o,"resolution")
		flag.count <- var.get.nc(o,"flag_count")

		# Time unit in case we need to convert
		default.time.unit <- "seconds"
		tryCatch(
		{
			time.unit <- as.character(att.get.nc(o,"NC_GLOBAL","units"))
		},
		error=function(e)
		{
			disp(":units global attribute is not defined... User input or default will be used")
			time.unit <- default.time.unit
		},
		finally="")
		# We must deal with the case time being in minutes
		if (length(grep("min",time.unit,ignore.case=TRUE))!=0 & !is.null(filter.time))
			filter.time <- 60*filter.time
		
		# Filter the time
		if (!is.null(filter.time))
		{
			disp("Filtering retention time...")
			time.ind <- which(time<filter.time[1] | time>filter.time[2])
			time <- time[-time.ind]
		}
		# Redefine the time ranges (missing anyway)
		minima.time <- as.numeric(matrix(NA,1,length(time)))
		maxima.time <- as.numeric(matrix(NA,1,length(time)))

		# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
		# We must also exclude the m/z and intensity corresponding to excluded RT
		# Whether time.ind is sorted or not we must update scan.index in every step
		# 1. Create 2 list objects with mass/intensity containing a scan in each plot
		# 2. Delete the list members given by the retention time excluding indices
		# 3. Update n.points (length(time))
		# 4. Perform the necessary filters: m/z FIRST, intensity follows
		# 5. Based on the remaining list objects, reconstruct:
		#	a. scan_index (C-like indexing)
		#	b. point_count (length of each list object)
		# 6. Collapse mass and intensity lists to create the new data
		# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
		
		# 1.
		disp("Getting m/z and intensity in list objects...") 
		l.mass <- l.inten <- vector("list",n.points)
		for (i in 1:(n.points-1))
		{
			# Get current scan indices, C-like indexing
			curr.ind <- getRindex(i,scan.index)
			l.mass[[i]] <- mass[curr.ind]
			l.inten[[i]] <- inten[curr.ind]
		}
		# Last vector
		curr.ind <- (scan.index[n.points]:(length(mass-1)))+1
		l.mass[[n.points]] <- mass[curr.ind]
		l.inten[[n.points]] <- inten[curr.ind]
		# 2.
		if (!is.null(filter.time))
		{
			l.mass <- l.mass[-time.ind]
			l.inten <- l.inten[-time.ind]
		}
		# 3.
		n.points <- length(time)
		# 4. To filter mass...
		minima.mz <- maxima.mz <- numeric(n.points)
		if (!is.null(filter.mz))
		{
			# Filter
			disp("Filtering m/z values...")
			for (i in 1:n.points)
			{	
				filter.mass <- which(l.mass[[i]]<filter.mz[1] | l.mass[[i]]>filter.mz[2])
				l.mass[[i]] <- l.mass[[i]][-filter.mass]
				# Exclude corresponding intensity values of the filtered masses
				l.inten[[i]] <- l.inten[[i]][-filter.mass]
				minima.mz[i] <- min(l.mass[[i]],na.rm=TRUE)
				maxima.mz[i] <- max(l.mass[[i]],na.rm=TRUE)
			}
		}
		else
		{
			for (i in 1:n.points)
			{	
				minima.mz[i] <- min(l.mass[[i]],na.rm=TRUE)
				maxima.mz[i] <- max(l.mass[[i]],na.rm=TRUE)
			}
		}

		# To filter intensity...
		total.inten <- numeric(n.points)
		if (!is.null(filter.inten))
		{
			# Filter
			disp("Filtering intensity values...")	
			for (i in 1:n.points)
			{
				sat.inten.min <- which(l.inten[[i]]<filter.inten[1])
				sat.inten.max <- which(l.inten[[i]]>filter.inten[2])
				l.inten[[i]][sat.inten.min] <- filter.inten[1]
				l.inten[[i]][sat.inten.max] <- filter.inten[2]
				# If intensity has been filtered, we must recalculate total_intensity
				total.inten[i] <- sum(l.inten[[i]],na.rm=TRUE)
			}
		}
		else
		{
			for (i in 1:n.points)
				total.inten[i] <- sum(l.inten[[i]],na.rm=TRUE)
		}

		# 5.
		disp("Recalculating scan indices and scan point counts...")
		scan.index <- point.count <- numeric(n.points)
		point.count[1] <- length(l.mass[[1]])
		for (i in 2:n.points)
		{
			curr.n <- length(l.mass[[i]])
			scan.index[i] <- scan.index[i-1] + curr.n
			point.count[i] <- curr.n
		}

		# 5.
		disp("Collapsing m/z and intensity lists...")
		mass <- unlist(l.mass)
		inten <- unlist(l.inten)

		# Valid missing value for current file
		disp("Assigning possible missing data values...")
		mass[is.na(mass)] <- missing.value
		inten[is.na(inten)] <- missing.value
		minima.time[is.na(minima.time)] <- missing.value
		maxima.time[is.na(maxima.time)] <- missing.value

		# There is no way of update, we have to write a new netCDF file...
		out <- create.nc(createOutFile(cdf,out.path),prefill=FALSE)
		# Copy static elements, global attributes first
		disp("Writing global attributes...")
		global.attributes <- c("dataset_completeness","ms_template_revision","netcdf_revision",
							   "languages","netcdf_file_date_time_stamp","experiment_title",
							   "experiment_date_time_stamp","operator_name","pre_experiment_program_name",
							   "post_experiment_program_name","source_file_reference","source_file_format",
							   "experiment_type","sample_state","test_separation_type","test_ms_inlet",
							   "test_ionization_mode","test_ionization_polarity","test_detector_type",
							   "test_resolution_type","test_scan_function","test_scan_direction",
							   "test_scan_law","raw_data_mass_format","raw_data_time_format",
							   "raw_data_intensity_format","units","scale_factor")
		for (i in global.attributes)					   
			att.copy.nc(o,"NC_GLOBAL",i,out,"NC_GLOBAL")
		
		# Redefine static dimensions
		disp("Writing dimensions...")
		dimensions <- c("_2_byte_string","_4_byte_string","_8_byte_string","_16_byte_string",
						"_32_byte_string","_64_byte_string","_128_byte_string","_255_byte_string",
						"range","error_number","instrument_number")
		for (i in dimensions)
			dim.def.nc(out,i,dim.inq.nc(o,i)$length)
		# Redefine the non-static dimension "scan_number" and "point_number"
		dim.def.nc(out,"scan_number",n.points)
		dim.def.nc(out,"point_number",dimlength=length(mass))
		
		# Redefine all the variables (and attributes), can't loop because of different dimensions... :-@
		disp("Defining variables and their attributes...")
		var.def.nc(out,"error_log","NC_CHAR",c("_64_byte_string","error_number"))
		var.def.nc(out,"a_d_sampling_rate","NC_DOUBLE","scan_number")
		var.def.nc(out,"a_d_coaddition_factor","NC_SHORT","scan_number")
		var.def.nc(out,"scan_acquisition_time","NC_DOUBLE","scan_number")
		att.put.nc(out,"scan_acquisition_time","units","NC_CHAR",time.unit)
		var.def.nc(out,"scan_duration","NC_DOUBLE","scan_number")
		var.def.nc(out,"inter_scan_time","NC_DOUBLE","scan_number")
		var.def.nc(out,"resolution","NC_DOUBLE","scan_number")
		var.def.nc(out,"actual_scan_number","NC_INT","scan_number")
		var.def.nc(out,"total_intensity","NC_DOUBLE","scan_number")
		att.put.nc(out,"total_intensity","missing_value",missing.type,missing.value)
		att.put.nc(out,"total_intensity","units","NC_CHAR","Arbitrary Intensity Units")
		var.def.nc(out,"mass_range_min","NC_DOUBLE","scan_number")
		var.def.nc(out,"mass_range_max","NC_DOUBLE","scan_number")
		var.def.nc(out,"time_range_min","NC_DOUBLE","scan_number")
		att.put.nc(out,"time_range_min","missing_value","NC_CHAR","NA")
		var.def.nc(out,"time_range_max","NC_DOUBLE","scan_number")
		att.put.nc(out,"time_range_max","missing_value","NC_CHAR","NA")
		var.def.nc(out,"scan_index","NC_INT","scan_number")
		var.def.nc(out,"point_count","NC_INT","scan_number")
		var.def.nc(out,"flag_count","NC_INT","scan_number")
		var.def.nc(out,"mass_values","NC_DOUBLE","point_number")
		att.put.nc(out,"mass_values","missing_value",missing.type,missing.value)
		att.put.nc(out,"mass_values","units","NC_CHAR","M/Z")
		att.put.nc(out,"mass_values","scale_factor","NC_DOUBLE",1)
		var.def.nc(out,"intensity_values","NC_DOUBLE","point_number")
		att.put.nc(out,"intensity_values","missing_value",missing.type,missing.value)
		att.put.nc(out,"intensity_values","units","NC_CHAR","Arbitrary Intensity Units")
		att.put.nc(out,"intensity_values","scale_factor","NC_DOUBLE",1)
		att.put.nc(out,"intensity_values","scale_factor","NC_DOUBLE",1)
		# Some here that can be looped
		vars.instrument <- c("instrument_name","instrument_id","instrument_mfr","instrument_model",
							 "instrument_serial_no","instrument_sw_version","instrument_fw_version",
							 "instrument_os_version","instrument_app_version","instrument_comments")
		for (i in vars.instrument)
			var.def.nc(out,i,"NC_CHAR",c("_32_byte_string","instrument_number"))	

		# Fill variables
		disp("Writing variable values...")
		
		# Fill the instrument variables
		var.put.nc(out,"instrument_name",var.get.nc(o,"instrument_name"))
		var.put.nc(out,"instrument_id",var.get.nc(o,"instrument_id"))
		var.put.nc(out,"instrument_mfr",var.get.nc(o,"instrument_mfr"))
		var.put.nc(out,"instrument_model",var.get.nc(o,"instrument_model"))
		var.put.nc(out,"instrument_serial_no",var.get.nc(o,"instrument_serial_no"))
		var.put.nc(out,"instrument_sw_version",var.get.nc(o,"instrument_sw_version"))
		var.put.nc(out,"instrument_fw_version",var.get.nc(o,"instrument_sw_version"))
		var.put.nc(out,"instrument_os_version",var.get.nc(o,"instrument_os_version"))
		var.put.nc(out,"instrument_app_version",var.get.nc(o,"instrument_app_version"))
		var.put.nc(out,"instrument_comments",var.get.nc(o,"instrument_comments"))

		# Fill the other variables
		var.put.nc(out,"error_log"," ") # Hack because bug? Otherwise crashes or very big file
		if (!is.null(filter.time))
		{
			var.put.nc(out,"a_d_sampling_rate",a.d.sampling.rate[-time.ind])
			var.put.nc(out,"a_d_coaddition_factor",a.d.coaddition.factor[-time.ind])
			var.put.nc(out,"scan_duration",scan.duration[-time.ind])
			var.put.nc(out,"inter_scan_time",inter.scan.time[-time.ind])
			var.put.nc(out,"resolution",resolution[-time.ind])
			var.put.nc(out,"flag_count",flag.count[-time.ind])
		}
		else
		{
			var.put.nc(out,"a_d_sampling_rate",a.d.sampling.rate)
			var.put.nc(out,"a_d_coaddition_factor",a.d.coaddition.factor)
			var.put.nc(out,"scan_duration",scan.duration)
			var.put.nc(out,"inter_scan_time",inter.scan.time)
			var.put.nc(out,"resolution",resolution)
			var.put.nc(out,"flag_count",flag.count)
		}
		var.put.nc(out,"scan_acquisition_time",time)
		var.put.nc(out,"actual_scan_number",(1:n.points)-1)
		var.put.nc(out,"total_intensity",total.inten)
		var.put.nc(out,"mass_range_min",minima.mz)
		var.put.nc(out,"mass_range_max",maxima.mz)
		var.put.nc(out,"time_range_min",minima.time)
		var.put.nc(out,"time_range_max",maxima.time)
		var.put.nc(out,"scan_index",scan.index)
		var.put.nc(out,"point_count",point.count)
		var.put.nc(out,"mass_values",mass)
		var.put.nc(out,"intensity_values",inten)

		# Close
		close.nc(o)
		close.nc(out)

		# Clear system
		disp("Clearing garbage...")
		clear()
		disp(paste("Output written to",createOutFile(cdf,out.path)))
		disp("Finished!") 
	}
}

getRindex <- function(i,scan.index)
{
	curr.ind <- scan.index[i]:(scan.index[i+1]-1)
	curr.ind <- curr.ind+1 #R-like indexing
	return(curr.ind)
}

createOutFile <- function(infile,outpath)
{
	if (is.null(outpath))
	{
		p.list <- strsplit(infile,"\\.")
		ext <- p.list[[1]][[length(p.list[[1]])]]
		p.list[[1]] <- p.list[[1]][-length(p.list[[1]])]
		#output <- paste(lapply(p.list,paste,collapse="."),"_Filtered.",ext,sep="")
		output <- paste(lapply(p.list,paste,collapse="."),".",ext,sep="")
	}
	else
	{
		p.list <- strsplit(basename(infile),"\\.")
		ext <- p.list[[1]][[length(p.list[[1]])]]
		p.list[[1]] <- p.list[[1]][-length(p.list[[1]])]
		nam <- lapply(p.list,paste,collapse=".")
		#output <- file.path(outpath,paste(nam,"_Filtered.",ext,sep=""))
		output <- file.path(outpath,paste(nam,".",ext,sep=""))
	}
	return(output)
}

disp <- function(msg)
{
    cat(paste(msg,"\n",sep=""))
    flush.console()
}

clear <- function()
{
	rm(list=ls())
	gc()
}

# Testing stuff
#plot(mass[1:64800],inten[1:64800],type="l",pch=21,col="red",bg="red",lwd=2,main="",xlab="",ylab="")
