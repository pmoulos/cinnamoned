# peaks: object returned from xcmsPipeline, single or list
# time.range: a list with time ranges for each sample (remove salt and plastic after visual inspection)

# peaks: object returned from xcmsPipeline, single or list
# time.range: a list with time ranges for each sample (remove salt and plastic after visual inspection)
# cutrat : a threshold to exclude possible non-stable normalizers (in log2 that means ratio)
# cutq : a quantile to exclude non-aligned RT points
# correctfor : both for both, time for only RT correction, intensity for only intensity correction
#				and none for just m/z matching

normalizeSamples <- function(peaks,dbdata,method=c("geom","rlm","both"),normalize=c("loess","rlm","lm","simple","cluster"),
							 correctfor=c("both","time","intensity","none"),time.range=NULL,tol=0.01,tspan=0.75,ispan=0.75,tit=3,corrfac=2,export=NULL,
							 cutrat=2,cutq=0.98,diagplot=NULL,plottype="x11",export.type=c("all","armada","none"))
{
	meta.data <- tryCatch(attr(peaks,"meta.data"),error=function(e) { return(NULL) },finally="")
	if (!is.list(peaks))
	{
		p <- list()
		p[[1]] <- peaks
		if (is.null(meta.data))
			names(p) <- "peakdata"
		else
			names(p) <- meta.data$Samplename
		peaks <- p
	}

	if (missing(dbdata))
	{
		if (!require(RCurl))
			stop("R package RCurl is required OR provide database connection data!")
		else
		{
			dbdata <- getdbdata()
			dbdata[1] <- base64Decode(dbdata[1])
			dbdata[2] <- base64Decode(dbdata[2])
		}
	}
	if (!is.null(diagplot))
	{
		if (plottype=="x11")
		{
			warning("Plotting output has been specified... Changing plot type to pdf...",call.=FALSE)
			plottype <- "pdf"
		}
		if (!file.exists(diagplot))
			dir.create(diagplot,recursive=TRUE)
		
		# We must make sure that we have something for filenames...
		if (!is.null(meta.data))
			diagnames <- meta.data$Samplename
		else
		{
			if (!is.null(names(peaks)))
				diagnames <- names(peaks)
			else
				diagnames <- as.character(1:length(peaks))
		}
	}

	method <- tolower(method[1])
	if (!is.element(method,c("geom","rlm","both")))
		stop("method must be one of \"geom\", \"rlm\" or \"both\"!")
	norm <- tolower(normalize[1])
	if (!is.element(norm,c("loess","rlm","lm","simple","cluster")))
		stop("method must be one of \"loess\", \"rlm\", \"lm\", \"simple\" or \"cluster\"!")
	correctfor <- tolower(correctfor[1])
	if (!is.element(correctfor,c("both","time","intensity","none")))
		stop("correctfor must be one of \"both\", \"time\", \"intensity\",or \"none\"!")
	export.type <- tolower(export.type[1])
	if (!is.element(export.type,c("all","armada","none")))
		stop("method must be one of \"all\", \"armada\" or \"none\"!")
	if (is.character(tspan))
	{
		tspan <- tolower(tspan)
		if (tspan != "adaptive")
			stop("Span for RT correction must be \"adaptive\" when not numeric!")
	}
	else if (is.numeric(tspan))
	{
		if (tspan<0 || tspan>1)
			stop("RT correction span must be a number between 0 and 1, when numeric!")
		if (tspan==0) tspan <- "adaptive"
	}
	if (is.character(ispan))
	{
		ispan <- tolower(ispan)
		if (ispan != "adaptive")
			stop("Intensity correction span must be \"adaptive\" when not numeric!")
	}
	else if (is.numeric(ispan))
	{
		if (ispan<0 || ispan>1)
			stop("Intensity correction span must be a number between 0 and 1, when numeric!")
		if (ispan==0) ispan <- "adaptive"
	}
	if (cutq<0 || cutq>1)
		stop("The RT deviation exlusion quantile must be a number between 0 and 1, when numeric!")

	# Clear the peaks from unecessary confusing fields and sort to mz
	peaks <- lapply(peaks,function(x) return(x[,c("mz","rt","into")]))
	peaks <- lapply(peaks,function(x) return(x[order(x[,"mz"]),]))
	
	# Remove mz outside time range
	if (!is.null(time.range))
	{
		if (length(time.range)!=length(peaks))
			stop("The filtering time intervals list should have the same length as the list of peak tables!")
		for (i in 1:length(peaks))
		{
			rem.idx <- which(peaks[[i]]$rt<time.range[[i]][1] | peaks[[i]]$rt>time.range[[i]][2])
			if (length(rem.idx)>0)
				peaks[[i]] <- peaks[[i]][-rem.idx,]
		}
	}

	# Get normalizers and reference data from the database and create the match lists
	ref <- getRefData(method,dbdata)
	iset.ref <- which(ref[,paste("is_",method,sep="")]==1)
	match.ref <- iset.match <- matched <- iset.inten <- vector("list",length(peaks))

	cat("\nNormalizing peaks using standards from reference database...\n")
	
	# Match!
	for (i in 1:length(peaks))
		match.ref[[i]] <- match.mz(peaks[[i]]$mz,ref$mz,tol=tol)
	
	# Normalize times and counts (the indexing makes my head spin...)
	for (i in 1:length(peaks))
	{	
		cat("\n=== Normalizing sample",i,"===\n")
		matched[[i]]$mz <- matched[[i]]$rt <- matched[[i]]$rawinten <- matched[[i]]$norminten <- numeric(nrow(ref))
		
		iset.match[[i]] <- intersect(match.ref[[i]]$ref.idx,iset.ref)
		# Add an extra field to the match.ref list for stats purposes
		match.ref[[i]]$pct.iset <- sprintf("%.3f",100*length(iset.match[[i]])/length(iset.ref))
		
		iset <- numeric(length(iset.match[[i]]))
		tmp  <- rtn <- rtr <- list(length(iset.match[[i]]))
		#mzr <- mzn <- list(length(iset.match[[i]]))
		pos <- 0
		for (j in iset.match[[i]])
		{
			pos <- pos+1
			tmp[[pos]] <- which(match.ref[[i]]$ref.idx==j)
			rtr[[pos]] <- ref$rt[match.ref[[i]]$ref.idx[tmp[[pos]]]]
			rtn[[pos]] <- peaks[[i]]$rt[match.ref[[i]]$new.idx[tmp[[pos]]]]
			#mzr[[pos]] <- ref$mz[match.ref[[i]]$ref.idx[tmp[[pos]]]]
			#mzn[[pos]] <- peaks[[i]]$mz[match.ref[[i]]$new.idx[tmp[[pos]]]]
			d <- abs(rtr[[pos]]-rtn[[pos]])
			iset[pos] <- tmp[[pos]][which(d==min(d))]
		}

		# Times
		peaks[[i]]$rt[-match.ref[[i]]$new.idx] <- NA
		match.ref[[i]]$pct.iset.rt <- 0
		if (correctfor=="both" || correctfor=="time")
		{
			rtnew <- peaks[[i]]$rt[match.ref[[i]]$new.idx]
			rtref <- ref$rt[match.ref[[i]]$ref.idx]
			rtclist <- is.retcor(rtnew,rtref,iset,tspan=tspan,it=tit,corrfac=corrfac)
			rtcor <- rtclist$rtcor
			
			if (!is.null(cutq))
			{
				badrt.ref <- which(abs(rtcor-rtref)>quantile(abs(rtcor-rtref),cutq))
				rtcor[badrt.ref] <- -99999
			}

			peaks[[i]]$rt[!is.na(peaks[[i]]$rt)] <- rtcor
			match.ref[[i]]$pct.iset.rt <- sprintf("%.3f",100*(length(iset.match[[i]])-rtclist$excl)/length(iset.ref))
		}
		
		# Counts (record the raw for diagnostic purposes)
		matched[[i]]$rawinten[match.ref[[i]]$ref.idx] <- peaks[[i]]$into[match.ref[[i]]$new.idx]
		# Intensities
		peaks[[i]]$into[-match.ref[[i]]$new.idx] <- NA
		match.ref[[i]]$pct.iset.inten <- 0
		if (correctfor=="both" || correctfor=="intensity")
		{
			intnew <- peaks[[i]]$into[match.ref[[i]]$new.idx]
			intnew <- log2(remove.zeros(intnew,strategy="offset"))
			intref <- log2(ref[match.ref[[i]]$ref.idx,paste("summarized_intensity_",method,sep="")])

			iset.inten[[i]]$new <- intnew[iset]
			iset.inten[[i]]$ref <- intref[iset]

			if (!is.null(cutrat))
				iset <- iset[which(abs(intnew[iset]-intref[iset])<=cutrat)]
			
			intcor <- is.intcor(intnew,intref,iset,method=normalize,span=ispan,corrfac=corrfac,adapt.factor=length(iset.ref))
			#intcor <- intc$intcor
			peaks[[i]]$into[!is.na(peaks[[i]]$into)] <- 2^intcor
			match.ref[[i]]$pct.iset.inten <- sprintf("%.3f",100*((length(iset.match[[i]])-length(which(abs(intnew[iset]-intref[iset])>cutrat)))/length(iset.ref)))
		}

		# Create the rest of the matched tables
		matched[[i]]$mz[match.ref[[i]]$ref.idx] <- peaks[[i]]$mz[match.ref[[i]]$new.idx]
		matched[[i]]$rt[match.ref[[i]]$ref.idx] <- peaks[[i]]$rt[match.ref[[i]]$new.idx]
		matched[[i]]$norminten[match.ref[[i]]$ref.idx] <- peaks[[i]]$into[match.ref[[i]]$new.idx]
		# And once more for the RT mismatched samples...
		badrt.new <- which(matched[[i]]$rt==-99999)
		matched[[i]]$mz[badrt.new] <- NA
		matched[[i]]$rt[badrt.new] <- NA
		matched[[i]]$norminten[badrt.new] <- NA

		# Diagnostic plots if requested
		if (!is.null(diagplot))
		{
			plot.match(rtref,ref$mz[match.ref[[i]]$ref.idx],
					   rtcor,peaks[[i]]$mz[match.ref[[i]]$new.idx],
					   rtnew,peaks[[i]]$mz[match.ref[[i]]$new.idx],
					   output=plottype,fil=file.path(diagplot,paste(diagnames[i],"_ALIGNMENT.",plottype,sep="")))
			x <- cbind(rtref,rtref)
			y <- cbind(rtnew,rtcor)
			a <- cbind(intref,intref)
			b <- cbind(intnew,intcor)
			aa <- iset.inten[[i]]$new
			bb <- iset.inten[[i]]$ref
			plot.rtdev(x,y,l=iset,exclude=badrt.ref,output=plottype,fil=file.path(diagplot,paste(diagnames[i],"_DEVIATION.",plottype,sep="")))
			plot.rvn(aa,bb,lim=cutrat,output=plottype,fil=file.path(diagplot,paste(diagnames[i],"_STANDARDS.",plottype,sep="")))
			plot.rvn(a[,1],b[,1],aa,bb,lim=cutrat,output=plottype,fil=file.path(diagplot,paste(diagnames[i],"_RAWINT.",plottype,sep="")))
			plot.rvn(a[,2],b[,2],aa,bb,lim=cutrat,output=plottype,fil=file.path(diagplot,paste(diagnames[i],"_NORMINT.",plottype,sep="")))
			boxplot.mat(cbind(intref,intnew,intcor),name=c("Reference","Raw","Normalized"),
						output=plottype,fil=file.path(diagplot,paste(diagnames[i],"_BOXPLOT.",plottype,sep="")))
		}
	}

	# Now construct the final table...
	# x = [reference_ID, reference_mz, reference_rt, []
	norm <- list()
	norm$reference <- ref[,c("id","mz","rt")]
	#norm$mz <- do.call("cbind",lapply(match.ref,function(x) x$mz.new))
	norm$mz <- do.call("cbind",lapply(matched,function(x) x$mz))
	norm$rt <- do.call("cbind",lapply(matched,function(x) x$rt))
	norm$rawinten <- do.call("cbind",lapply(matched,function(x) x$rawinten))
	norm$norminten <- do.call("cbind",lapply(matched,function(x) x$norminten))
	norm$mz[norm$mz==0] <- NA
	norm$rt[norm$rt==0] <- NA

	norm$iiset <- iset.inten
	
	#pcts <- as.numeric(do.call("c",lapply(match.ref,function(x) x$pct)))
	#names(pcts) <- names(peaks)
	pcts <- do.call("rbind",lapply(match.ref,function(x) as.numeric(c(x$pct,x$pct.iset,x$pct.iset.rt,x$pct.iset.inten))))
	rownames(pcts) <- names(peaks)
	colnames(pcts) <- c("total","is","is_rt","is_inten")

	if (!is.null(diagplot))
	{
		if (!is.null(meta.data))
			name <- meta.data$Replicate
		else
			name <- NULL
		
		u <- norm$rawinten
		u[u==0] <- NA
		n <- norm$norminten
		n[n==0] <- NA
		boxplot.mat(u,name=name,output=plottype,fil=file.path(diagplot,paste("boxplot_raw.",plottype,sep="")),
					main="log2 intensity distributions - raw",cex.main=0.9)
		boxplot.mat(n,name=name,output=plottype,fil=file.path(diagplot,paste("boxplot_normalized.",plottype,sep="")),
					main="log2 intensity distributions - normalized",cex.main=0.9)
	}

	if (!is.null(export) && export.type!="none")
	{
		if (!is.null(meta.data))
			expnames <- meta.data$Replicate
		else
		{
			if (!is.null(names(peaks)))
				expnames <- names(peaks)
			else
				expnames <- paste("Sample",1:length(peaks))
		}

		norm.ref <- as.data.frame(norm$reference)
		norm.mz <- as.data.frame(norm$mz)
		norm.rt <- as.data.frame(norm$rt)
		norm.inten <- as.data.frame(norm$norminten)
		names(norm.mz) <- paste("mz - ",expnames)
		names(norm.rt) <- paste("rt - ",expnames)
		names(norm.inten) <- paste("Intensity - ",expnames)

		if (export.type=="all")
		{
			final <- cbind(norm.ref,norm.mz,norm.rt,norm.inten)
			write.table(final,file=export,quote=FALSE,sep="\t",na="-",row.names=FALSE)
		}
		else if (export.type=="armada")
		{
			tmp.norm <- norm$norminten
			tmp.norm[which(tmp.norm==0)] <- "NaN"
			tmp.norm <- as.data.frame(tmp.norm)
			names(tmp.norm) <- paste("Intensity - ",expnames)
			final <- cbind(norm.ref[,"id"],tmp.norm)
			nam <- names(final)
			nam[1] <- "id"
			names(final) <- nam
			write.table(final,file=export,quote=FALSE,sep="\t",na="NaN",row.names=FALSE)
		}
			
		#write.table(final,file=export,quote=FALSE,sep="\t",na="-",row.names=FALSE)
	}
		
	return(list(norm=norm,iset=iset.match,pct=pcts))
}

match.mz.fun <- function(x,ref,tol=0.01)
{
	return(match.mz(x$mz,ref,tol=tol))
}
