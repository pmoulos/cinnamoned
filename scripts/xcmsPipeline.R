# A general xcms pipeline function, from raw files to annotation
# It will be expanded in the future
#
# TODO: -Find a way to get the retention time deviation after retcor so as to be able to
#		 automatically select the best bw
#		-Extend the functionality of the script to perform single file analysis. In this
#		 case, no annotation will be performed, only peak finding and the return will be
#		 the peak matrix from the function peakTable. The sample info file should be
#		 treated similarly. Try to make use of mclapply. 
#

xcmsPipeline <- function(path.to.raw,info.file,param.file,path.to.trunc=NULL,annotate=FALSE,
						 persample=FALSE,multicore=FALSE,plotspec=NULL,plottype="x11")
{
	require(tools)
	if (!require(xcms))
		stop("Are you trying to run xcmsPipeline without xcms package? ;-)")
	if (!require(yaml))
		warning("R package yaml not present... yml parameter file will not be read and might result in error...",
				call.=FALSE)

	if (multicore)
	{
		if (!require(parallel))
		{
			warning("R package parallel not present... Switching to single core...",
					call.=FALSE)
			multicore <- FALSE
		}
		else
		{
			require(parallel)
			n.core <- parallel:::detectCores()
			if (n.core==1)
			{
				warning("Only one core detected... Switching to single core...",
						call.=FALSE)
				multicore <- FALSE
			}
		}
	}
	if (!is.null(plotspec))
	{
		if (plottype=="x11")
		{
			warning("Plotting output has been specified... Changing plot type to pdf...",call.=FALSE)
			plottype <- "pdf"
		}
		if (!file.exists(plotspec))
			dir.create(plotspec,recursive=TRUE)
	}

	# Read the parameters
	if (missing(param.file))
	{
		disp("Parameters file missing! The default parameters will be used...")
		param.list <- default.param.list()
	}
	else # Read the parameters to use in the general pipeline...
	{
		f <- tolower(file_ext(param.file))
		if (f %in% c("yaml","yml"))
		{
			param.list <- tryCatch(read.verify.yaml(param.file),
								error=function(e) {
									disp("Bad parameter file! Defaults will be used...")
									return(default.param.list()) },
								finally="")
		}
		else
		{
			param.list <- tryCatch(read.verify.custom(param.file),
								error=function(e) {
									disp("Bad parameter file! Defaults will be used...")
									return(default.param.list()) },
								finally="")
		}
	}

	# Read meta-data
	meta.data <- read.info(info.file)
	
	the.raw.files <- file.path(path.to.raw,meta.data$Filename)
	if (param.list$filter$do) # We have to perform initial filtering of netcdf files
	{
		disp("--- TRUNCATING INPUT FILE(S) FOR RETENTION TIME LIMITS ---")
		if (is.null(path.to.trunc))
		{
			path.to.trunc <- file.path(path.to.raw,"Trunc")
			if (!file.exists(path.to.trunc))
				dir.create(path.to.trunc,recursive=TRUE,mode="0755")
		}
		filterBrukerNetCDF(the.raw.files,out.path=path.to.trunc,filter.time=c(param.list$filter$min,param.list$filter$max))
		the.raw.files <- file.path(path.to.trunc,meta.data$Filename)
	}

	# Basic runtime parameters
	if (is.null(param.list$read$nSlaves))
		param.list$read$nSlaves <- 1
	if (is.null(param.list$find$scanrange))
		param.list$find$scanrange <- integer(0)
	if (is.null(param.list$find$mzdiff))
		param.list$find$mzdiff <- 0.8-param.list$find$step*param.list$find$steps

	disp("--- PREPROCESSING FILE(S) WITH XCMS ---")
	if (persample)
	{
		if (multicore)
		{
			the.xfiles <- as.list(the.raw.files)
			n.core <- parallel:::detectCores()
			if (length(the.xfiles)<=6)
				options(cores=length(the.xfiles))
			else
				options(cores=ceiling(n.core/2))
			
			xset.raw <- mclapply(the.xfiles,xcmsSet,nSlaves=1,
											profmethod=param.list$read$profmethod,
											profparam=list(),
											method=param.list$find$method[1],
											fwhm=param.list$find$fwhm,
											sigma=param.list$find$sigma,
											snthresh=param.list$find$snthresh,
											max=param.list$find$max,
											step=param.list$find$step,
											steps=param.list$find$steps,
											mzdiff=param.list$find$mzdiff,
											sleep=param.list$find$sleep)
			if (annotate)
			{
				peak.tab <- mclapply(xset.raw,annotatePeaks,
											  group=p$annotate$group,
											  sigma=p$annotate$sigma,
											  perfwhm=p$annotate$perfwhm,
											  cor_eic_th=p$annotate$cor_eic_th,
											  maxiso=p$annotate$maxiso,
											  mzabs.add=p$annotate$mzabs.add,
											  mzabs.iso=p$annotate$mzabs.iso,
											  ppm=p$annotate$ppm,
											  find.adducts=p$annotate$find.adducts,
											  polarity=p$annotate$polarity,
											  rulefile=p$annotate$rulefile,
											  filter.valid=p$annotate$filter.valid,
											  filter.score=p$annotate$filter.score,
											  peak.val=p$annotate$maxo,
											  filter.dbe=p$annotate$filter.dbe,
											  fail.rules=p$annotate$fail.rules,
											  run.par=p$annotate$run.par,
											  write.output=p$annotate$write.output,
											  export.what=p$annotate$export.what)
			}
			else
				peak.tab <- mclapply(xset.raw,peakTable,method="maxint",value="into",intensity="into")
			for (i in 1:length(peak.tab))
				peak.tab[[i]] <- as.data.frame(peak.tab[[i]])
		}
		else # multicore not present or single core machine
		{
			xset.raw <- peak.tab <- list(length(the.raw.files))
			for (i in 1:length(the.raw.files))
			{
				xset.raw[[i]] <- xcmsSet(files=the.raw.files[i],nSlaves=1,
											profmethod=param.list$read$profmethod,
											profparam=list(),
											method=param.list$find$method[1],
											fwhm=param.list$find$fwhm,
											sigma=param.list$find$sigma,
											snthresh=param.list$find$snthresh,
											max=param.list$find$max,
											step=param.list$find$step,
											steps=param.list$find$steps,
											mzdiff=param.list$find$mzdiff,
											sleep=param.list$find$sleep)

				if (annotate)
				{
					peak.tab[[i]] <- annotatePeaks(xset.raw[[i]],
												   group=p$annotate$group,
												   sigma=p$annotate$sigma,
												   perfwhm=p$annotate$perfwhm,
												   cor_eic_th=p$annotate$cor_eic_th,
												   maxiso=p$annotate$maxiso,
												   mzabs.add=p$annotate$mzabs.add,
												   mzabs.iso=p$annotate$mzabs.iso,
												   ppm=p$annotate$ppm,
												   find.adducts=p$annotate$find.adducts,
												   polarity=p$annotate$polarity,
												   rulefile=p$annotate$rulefile,
												   filter.valid=p$annotate$filter.valid,
												   filter.score=p$annotate$filter.score,
												   peak.val=p$annotate$maxo,
												   filter.dbe=p$annotate$filter.dbe,
												   fail.rules=p$annotate$fail.rules,
												   run.par=p$annotate$run.par,
												   write.output=p$annotate$write.output,
												   export.what=p$annotate$export.what)
				}
				else
					peak.tab[[i]] <- as.data.frame(peakTable(xset.raw[[i]],method="maxint",value="into",intensity="into"))
			}
		}
		peaks <- peak.tab
		names(peaks) <- meta.data$Samplename
		attr(peaks,"meta.data") <- meta.data
	}
	else # multiple samples
	{
		# RTFD
		switch(param.list$find$method[1],
			matchedFilter = {
								xset.raw <- xcmsSet(files=the.raw.files,
													nSlaves=param.list$read$nSlaves,
													profmethod=param.list$read$profmethod,
													profparam=list(),
													method=param.list$find$method[1],
													fwhm=param.list$find$fwhm,
													sigma=param.list$find$sigma,
													snthresh=param.list$find$snthresh,
													max=param.list$find$max,
													step=param.list$find$step,
													steps=param.list$find$steps,
													mzdiff=param.list$find$mzdiff,
													sleep=param.list$find$sleep)
							},
			centWave = {
							xset.raw <- xcmsSet(files=the.raw.files,
												nSlaves=param.list$read$nSlaves,
												profmethod=param.list$read$profmethod,
												profparam=list(),
												method=param.list$find$method[1],
												ppm=param.list$find$ppm,
												peakwidth=param.list$find$peakwidth,
												prefilter=param.list$find$prefilter,
												mzCenterFun=param.list$find$mzCenterFun,
												integrate=param.list$find$integrate,
												mzdiff=param.list$find$mzdiff.cw,
												fitgauss=param.list$find$fitgauss,
												scanrange=param.list$find$scanrange,
												noise=param.list$find$noise,
												verbose.columns=param.list$find$verbose.columns)
						})

		# Annotate the xcmsSet
		pd <- as.data.frame(as.character(meta.data$Class),row.names=as.character(meta.data$Replicate))
		names(pd) <- c("Class")
		phenoData(xset.raw) <- pd

		# Grouping and RT correction total for multiple bw (should be the case!)
		xset.group <- list(length(param.list$group$bw))
		xset.rt <- list(length(param.list$group$bw)-1)
		cat("Grouping for bw = ",param.list$group$bw[1],"\n")
		switch(param.list$group$method[1],
			density = {
						xset.group[[1]] <- group(xset.raw,
												 method=param.list$group$method[1],
												 bw=param.list$group$bw[1],
												 minfrac=param.list$group$minfrac,
												 minsamp=param.list$group$minsamp,
												 mzwid=param.list$group$mzwid,
												 max=param.list$group$max,
												 sleep=param.list$group$sleep)
					},
			mzClust = {
						xset.group[[1]] <- group(xset.raw,
												 method=param.list$group$method[1],
												 bw=param.list$group$bw[1],
												 minfrac=param.list$group$minfrac,
												 minsamp=param.list$group$minsamp,
												 mzppm=param.list$group$mzppm,
												 mzabs=param.list$group$mzabs)
				  },
			nearest = {
						xset.group[[1]] <- group(xset.raw,
												 method=param.list$group$method[1],
												 bw=param.list$group$bw[1],
												 minfrac=param.list$group$minfrac,
												 minsamp=param.list$group$minsamp,
												 kNN=param.list$group$kNN,
												 mzVsRTbalance=param.list$group$mzVsRTbalance,
												 mzCheck=param.list$group$mzCheck,
												 rtCheck=param.list$group$rtCheck)
				  })
		for (i in 2:length(param.list$group$bw))
		{
			switch(param.list$retcor$method[1],
				loess = {
							xset.rt[[i-1]] <- retcor(xset.group[[i-1]],
													 method=param.list$retcor$method[1],
													 span=param.list$retcor$span,
													 family=param.list$retcor$family,
													 plottype=param.list$retcor$plottype)
						},
				obiwarp = {
							xset.rt[[i-1]] <- retcor(xset.group[[i-1]],
													 method=param.list$retcor$method[1],
													 profStep=param.list$retcor$profStep,
													 center=param.list$retcor$center,
													 response=param.list$retcor$response,
													 score=param.list$retcor$score,
													 gapInit=param.list$retcor$gapInit,
													 gapExtend=param.list$retcor$gapExtend,
													 factorDiag=param.list$retcor$factorDiag,
													 factorGap=param.list$retcor$factorGap,
													 localAlignment=param.list$retcor$localAlignment,
													 initPenalty=param.list$retcor$initPenalty,
													 plottype=param.list$retcor$plottype,
													 col=param.list$retcor$col,
													 ty=param.list$retcor$ty)
				  })
			cat("Grouping for bw = ",param.list$group$bw[i],"\n")
			switch(param.list$group$method[1],
				density = {
							xset.group[[i]] <- group(xset.rt[[i-1]],
													 method=param.list$group$method[1],
													 bw=param.list$group$bw[i],
													 minfrac=param.list$group$minfrac,
													 minsamp=param.list$group$minsamp,
													 mzwid=param.list$group$mzwid,
													 max=param.list$group$max,
													 sleep=param.list$group$sleep)
							},
				mzClust = {
							xset.group[[i]] <- group(xset.rt[[i-1]],
													 method=param.list$group$method[1],
													 bw=param.list$group$bw[i],
													 minfrac=param.list$group$minfrac,
													 minsamp=param.list$group$minsamp,
													 mzppm=param.list$group$mzppm,
													 mzabs=param.list$group$mzabs)
							},
				nearest = {
							xset.group[[i]] <- group(xset.rt[[i-1]],
													 method=param.list$group$method[1],
													 bw=param.list$group$bw[i],
													 minfrac=param.list$group$minfrac,
													 minsamp=param.list$group$minsamp,
													 kNN=param.list$group$kNN,
													 mzVsRTbalance=param.list$group$mzVsRTbalance,
													 mzCheck=param.list$group$mzCheck,
													 rtCheck=param.list$group$rtCheck)
							})
		}

		# Try to fill missing data
		#xset.group.fi <- fillPeaks(xset.group.fi,method="chrom")
		xset.group.fi <- fillPeaks(xset.group[[length(param.list$group$bw)]],method="chrom")
		
		# And now the annotation
		if (annotate)
		{
			peaks <- annotatePeaks(xset.group.fi,
								   group=p$annotate$group,
								   sigma=p$annotate$sigma,
								   perfwhm=p$annotate$perfwhm,
								   cor_eic_th=p$annotate$cor_eic_th,
								   maxiso=p$annotate$maxiso,
								   mzabs.add=p$annotate$mzabs.add,
								   mzabs.iso=p$annotate$mzabs.iso,
								   ppm=p$annotate$ppm,
								   find.adducts=p$annotate$find.adducts,
								   polarity=p$annotate$polarity,
								   rulefile=p$annotate$rulefile,
								   filter.valid=p$annotate$filter.valid,
								   filter.score=p$annotate$filter.score,
								   peak.val=p$annotate$maxo,
								   filter.dbe=p$annotate$filter.dbe,
								   fail.rules=p$annotate$fail.rules,
								   run.par=p$annotate$run.par,
								   write.output=p$annotate$write.output,
								   export.what=p$annotate$export.what)
		}
		else
		{
			peaks <- as.data.frame(peakTable(xset.group.fi,method="maxint",value="into",intensity="into"))
		}
	}

	# Plot the requested spectra
	if (!is.null(plotspec))
	{
		if (is.list(peaks))
		{
			for (i in 1:length(peaks))
			{
				plot.mzrt(peaks[[i]]$rt,peaks[[i]]$mz,inten=peaks[[i]]$into,output=plottype,
						  fil=file.path(plotspec,paste(meta.data$Samplename[i],plottype,sep=".")))
			}
		}
		else
			plot.mzrt(peaks$rt,peaks$mz,output=plottype,fil=file.path(plotspec,paste("xcmsPeaks",plottype,sep=".")))
	}

	disp("--- FINISHED! ---")
	# We should be returning an object or a list of objects ready to feed to summarizePeaks
	if (persample)
		return(list(peaks=peaks,xset=xset.raw))
	else
		return(list(peaks=peaks,xset=xset.group.fi))
}

read.verify.yaml <- function(param.file)
{
	library(yaml)
	
	accept.params <- static.names()
	pars <- yaml.load_file(param.file)
	for (n in names(pars))
	{
		valid <- sapply(names(pars[[n]]),check.param.names,accept.params[[n]])
		if (!all(valid))
		{
			invalid <- which(!valid)
			for (i in invalid)
				disp(paste("Unknown",n,"parameter :",pars[[n]][[i]],"Ignoring..."))
			pars[[n]] <- pars[[n]][-invalid]
		}
	}
	
	return(pars)
}

read.verify.custom <- function(param.file)
{
	accept.params <- static.names()
	param.group.names <- names(accept.params)

	pars.table <- read.delim(param.file,header=FALSE,comment.char="#",row.names=NULL,
							 colClasses="character",col.names=c("Parameter","Value"))

	b <- grep("filter",pars.table$Parameter,fixed=TRUE)
	r <- grep("read",pars.table$Parameter,fixed=TRUE)
	f <- grep("find",pars.table$Parameter,fixed=TRUE)
	g <- grep("group",pars.table$Parameter,fixed=TRUE)
	t <- grep("retcor",pars.table$Parameter,fixed=TRUE)
	e <- grep("extract",pars.table$Parameter,fixed=TRUE)
	a <- grep("annotate",pars.table$Parameter,fixed=TRUE)

	nams <- list(pars.table$Parameter[(b+1):(r-1)],pars.table$Parameter[(r+1):(f-1)],
				 pars.table$Parameter[(r+1):(f-1)],pars.table$Parameter[(f+1):(g-1)],
				 pars.table$Parameter[(g+1):(t-1)],pars.table$Parameter[(t+1):(e-1)],
				 pars.table$Parameter[(e+1):(a-1)],pars.table$Parameter[(a+1):nrow(pars.table)])
	vals <- list(parse.list.members(as.list(pars.table$Value[(b+1):(r-1)])),
				 parse.list.members(as.list(pars.table$Value[(r+1):(f-1)])),
				 parse.list.members(as.list(pars.table$Value[(f+1):(g-1)])),
				 parse.list.members(as.list(pars.table$Value[(g+1):(t-1)])),
				 parse.list.members(as.list(pars.table$Value[(t+1):(e-1)])),
				 parse.list.members(as.list(pars.table$Value[(e+1):(a-1)])),
				 parse.list.members(as.list(pars.table$Value[(a+1):nrow(pars.table)])))
	names(nams) <- names(vals) <- param.group.names
	
	for (n in names(nams))
	{
		valid <- sapply(nams[[n]],check.param.names,accept.params[[n]])
		if (!all(valid))
		{
			invalid <- which(!valid)
			for (i in invalid)
				disp(paste("Unknown",n,"parameter :",nams[[n]][[i]],"Ignoring..."))
			nams[[n]] <- nams[[n]][-invalid]
			vals[[n]] <- vals[[n]][-invalid]
		}
		names(vals[[n]]) <- nams[[n]]
	}

	# Maintain a common format between this and YAML file
	vals <- lapply(vals,nullify.emptiness)

	return(vals)
}

parse.list.members <- function(li)
{
	for (i in 1:length(li))
	{
		s <- strsplit(li[[i]],",")
		s[[1]] <- gsub("^\\s+|\\s+$","",s[[1]])
		if (!suppressWarnings(any(is.na(as.numeric(s[[1]])))))
			li[[i]] <- as.numeric(as.numeric(s[[1]]))
		else
			li[[i]] <- s[[1]]
	}
	return(li)
}

check.param.names <- function(p,ap) { return(ifelse(p %in% ap,TRUE,FALSE)) }

static.names <- function()
{
	accept.param <- list(c("do","min","max"),
						 c("nSlaves","profstep","profmethod"),
						 c("method","fwhm","sigma","max","step","steps",
						   "mzdiff","ppm","peakwidth","prefilter","mzCenterFun","integrate",
						   "mzdiff.cw","fitgauss","scanrange","noise","verbose.columns",
						   "snthresh","sleep"),
						 c("method","bw","mzwid","max","mzppm","mzabs","mzVsRTbalance",
						   "mzCheck","rtCheck","kNN","minfrac","minsamp","sleep"),
						 c("method","missing","extra","smooth","span","family","profStep",
						   "center","response","score","gapInit","gapExtend","factorDiag",
						   "factorGap","localAlignment","initPenalty","plottype","col","ty"),
						 c("method","value","intensity"),
						 c("group","iso.flow","sigma","perfwhm","pval","cor_eic_th","find.adducts",
						   "maxiso","mzabs.add","mzabs.fiso","mzabs.diso","ppm.fiso","ppm.diso",
						   "polarity","rulefile","filter.valid","filter.score","peak.val","filter.dbe",
						   "write.output","run.par","more.adducts","fail.rules","export.what"))
	names(accept.param) <- c("filter","read","find","group","retcor","extract","annotate")
	return(accept.param)
}

default.param.list <- function()
{
	p <- list()

	p$filter$do <- TRUE
	p$filter$min <- 600
	p$filter$max <- 3000

	p$read$nSlaves <- 1
	p$read$profstep <- 1
	p$read$profmethod <- "binlin"
	
	p$find$method <- c("matchedFilter","centWave")
	p$find$fwhm <- 30
	p$find$sigma <- 7
	p$find$max <- 5
	p$find$step <- 0.1
	p$find$steps <- 3
	p$find$mzdiff <- 0.8-p$find$step*p$find$steps
	p$find$ppm <- 25
	p$find$peakwidth <- c(10,40)
	p$find$prefilter <- c(3,100)
	p$find$mzCenterFun <- "wMean"
	p$find$integrate <- 1
	p$find$mzdiff.cw <- -0.001
	p$find$fitgauss <- FALSE
	p$find$scanrange <- integer(0)
	p$find$noise <- 0
	p$find$verbose.columns <- FALSE
	p$find$snthresh <- 7
	p$find$sleep <- 0
	
	p$group$method <- c("density","mzClust","nearest")
	p$group$bw <- c(50,30,20,10,5)
	p$group$mzwid <- 0.25
	p$group$max <- 50
	p$group$mzppm <- 20
	p$group$mzabs <- 0
	p$group$mzVsRTbalance <- 10
	p$group$mzCheck <- 0.2
	p$group$rtCheck <- 15
	p$group$kNN <- 10
	p$group$minfrac <- 0.5
	p$group$minsamp <- 1
	p$group$sleep <- 0
	
	p$retcor$method <- c("loess","obiwarp")
	p$retcor$missing <- 1
	p$retcor$extra <- 1
	p$retcor$smooth <- "loess"
	p$retcor$span <- 0.2
	p$retcor$family <- "symmetric"
	p$retcor$profStep <- 1
	p$retcor$center <- NULL
	p$retcor$response <- 1
	p$retcor$score <- "cor"
	p$retcor$gapInit <- 0
	p$retcor$gapExtend <- 0
	p$retcor$factorDiag <- 2
	p$retcor$factorGap <- 1
	p$retcor$localAlignment <- 0
	p$retcor$initPenalty <- 0
	p$retcor$plottype <- "mdevden"
	p$retcor$col <- NULL
	p$retcor$ty <- NULL
	
	p$extract$method <- "maxint"
	p$extract$value <- "into"
	p$extract$intensity <- "into"

	p$annotate$group <- "both"
	p$annotate$iso.flow <- "fwhm"
	p$annotate$sigma <- 6
	p$annotate$perfwhm <- 0.6
	p$annotate$pval <- 0.05
	p$annotate$cor_eic_th <- 0.75
	p$annotate$find.adducts <- TRUE
	p$annotate$maxiso <- 5
	p$annotate$mzabs.add <- 0.01
	p$annotate$mzabs.fiso <- 0.01
	p$annotate$mzabs.diso <- 0.001
	p$annotate$ppm.fiso <- 10
	p$annotate$ppm.diso <- 5
	p$annotate$polarity <- "positive"
	p$annotate$rulefile <- NULL
	p$annotate$filter.valid <- "valid"
	p$annotate$filter.score <- 0.75
	p$annotate$peak.val <- "maxo"
	p$annotate$filter.dbe <- NULL
	p$annotate$write.output <- NULL
	p$annotate$run.par <- TRUE
	p$annotate$more.adducts <- FALSE
	p$annotate$fail.rules <- "internal"
	p$annotate$export.what <- "all"

	return(p)
}

nullify.emptiness <- function(l)
{
	for (n in names(l))
	{
		if (length(l[[n]])==0)
			l[n] <- list(NULL)
	}
	return(l)
}

# info.file MUST contain fields (case sensitive!): Class, Filename
read.info <- function(info.file)
{
	require(tools) # Should load...

	# Read information file and construct a consistent correspondence between filenames
	# and sample info
	data.info <- read.delim(info.file,colClasses="character")
	# Create some order, some human names and remove non-calibrated meta-info
	meta.data <- data.info[order(data.info$Class,data.info$Filename),]
	ext <- paste(".",file_ext(meta.data$Filename[1]),sep="")
	samplenames <- sub(ext,"",meta.data$Filename)
	meta.data <- cbind(meta.data,samplenames)
	names(meta.data)[ncol(meta.data)] <- "Samplename"
	classes <- meta.data$Class
	hash <- numeric(length(unique(classes)))
	human.names <- character(nrow(meta.data))
	names(hash) <- as.character(unique(classes))
	for (i in 1:length(classes))
	{
		hash[classes[i]] <- hash[classes[i]]+1
		human.names[i] <- paste(classes[i],hash[classes[i]],sep="_")
	}
	meta.data <- cbind(meta.data,human.names)
	names(meta.data)[ncol(meta.data)] <- "Replicate"

	return(meta.data)
}

# Might not be needed in the end...
reorder.raw <- function(raw.files,ord.names)
{
	org.ind <- numeric(length(raw.files))
	for (i in 1:length(ord.names))
		org.ind[i] <- which(raw.files==ord.names[i])
	ord.files <- raw.files[org.ind]
	return(ord.files)
}
