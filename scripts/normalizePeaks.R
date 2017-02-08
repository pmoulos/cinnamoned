# An R file containing a framework for peak intensity normalization for the specific needs
# of the INSERM project. Briefly, it uses normalization algorithms similar (or same) to the
# concept of rank invariant normalization taken from the microarray bibliography. It should
# return, apart from the normalized values, also the indices of the rank invariant entities,
# as these constitute the basis of the data mining problem that will define the final
# database of housekeeping metabolites
#
#
# Usage: ? <- normalizePeaks(peaks,
#								)
#
# Input arguments:
#
# peaks 	   : A set of peaks derived from analysis with xcms
# method	   : Which rank invariant method to use? Can be one of "raffy" for the method
#				 from affy package, "rlumi" for the method from lumi package or "rgrsn" for
#				 the method presented in Pelz et al BMC paper (2008). From our methods, can
#				 be also "rrlm" or "rgeom"
# ...		   : Extra parameters to pass to the normalization methods	
#
# Author: Panagiotis Moulos (pmoulos@eie.gr)
# Version: 1.1
# Creation date: 08-09-2011 (dd-mm-yyyy)
# Last update: 18-01-2012 (dd-mm-yyyy)
#
# TODO :
#

normalizePeaks <- function(peaks,method="raffy",...)
{
	if (!is.element(method,c("raffy","rlumi","rgrsn","rrlm","rgeom")))
		stop("Normalization method must be one of \"raffy\", \"rlumi\", \"rgrsn\", \"rrlm\" or \"rgeom\"!")

	if (method=="raffy" && !require(affy))
		stop("Bioconductor package affy is required!")
	if (method=="rlumi" && !require(lumi))
		stop("Bioconductor package lumi is required!")
	if (method=="rrlm" && !require(MASS))
		stop("R package MASS is required!")

	switch(method,
		raffy = { n.peaks <- ri.norm.affy(peaks,...) },
		rlumi = { n.peaks <- ri.norm.lumi(peaks,...) },
		rgrsn = { n.peaks <- ri.norm.grsn(peaks,...) },
		rrlm = { n.peaks <- rlm.norm(peaks,...) },
		rgeom = { n.peaks <- geom.norm(peaks,...) },
		none = { n.peaks <- peaks })

	return(n.peaks)
}

bootstrap.IS <- function(peaks,baseline=NULL,iset,method="raffy",opt="data",measure="mad",
						 n=100,multicore=FALSE,...)
{
	require(utils)

	if (!is.element(method,c("raffy","rlumi","rgrsn","rrlm","rgeom")))
		stop("Normalization method must be one of \"raffy\", \"rlumi\", \"rgrsn\", \"rrlm\" or \"rgeom\"!")

	if (is.null(baseline))
		ref <- apply(X,1,median)
	else if (is.numeric(baseline) && length(baseline)==nrow(X))
		ref <- baseline
	else if (is.numeric(baseline) && length(baseline)==1)
		ref <- X[,baseline]
	else
		stop("The baseline argument is not correct, please check!")

	measure <- tolower(measure)
	if (!is.element(measure,c("mad","qn","sn")))
		stop("measure parameter must be one of \"mad\", \"qn\" or \"sn\"!")

	if ((measure=="qn" || measure=="sn") && !require(robustbase))
		stop("R package robustbase is required!")

	if(multicore && !require(multicore))
	{
		disp("Package multicore required to run in parallel... Switching to single core...")
		multicore=FALSE
	}

	# Data must be logged
	if (max(peaks) > 107) # How possible it is to have intensity>1e+32
	{
		log.scaled <- FALSE
		X <- remove.zeros(peaks,strategy="offset",off.set=2)
		X <- log2(X)
		ref <- log2(ref)
	}
	else
	{
		log.scaled <- TRUE
		X <- peaks
	}

	# Create the random resamplings
	randomize <- list(n)
	for (i in 1:n)
		randomize[[i]] <- sample(nrow(X),length(iset))
	
	if (method=="rgrsn")
		Yt = i.correct.grsn(iset,X,ref,...)
	else
		Yt = i.correct(X,ref,iset,...) # Only total supported for affy
	
	switch(measure,
		mad = {
			ground.truth.iset = mad(apply(X[iset,],2,median,na.rm=TRUE));
			ground.truth.data = mad(apply(Yt,2,median,na.rm=TRUE));
			if (multicore)
			{
				if (method=="rgrsn")
					Y.list <- mclapply(randomize,p.i.correct,X,ref,...)
				else
					Y.list <- mclapply(randomize,i.correct.grsn,X,ref,...)
				im.list <- mclapply(randomize,par.median,X,2)
				meds <- lapply(Y.list,sap.median,2)
				m <- sapply(meds,mad)
				im <- sapply(im.list,mad)
			}
			else
			{
				m <- im <- numeric(n)
				pb <- txtProgressBar(0,length(iset),style=3,width=50)
				for (i in 1:n)
				{
					if (method=="rgrsn")
						Y <- i.correct.grsn(randomize[[i]],X,ref,...)
					else
						Y <- i.correct(X,ref,iset=randomize[[i]],...)
					m[i] <- mad(apply(Y,2,median,na.rm=TRUE))
					im[i] <- mad(apply(X[randomize[[i]],],2,median,na.rm=TRUE))
					setTxtProgressBar(pb,i)
				}
			}
		},
		qn = {
			ground.truth.iset = Qn(apply(X[iset,],2,median,na.rm=TRUE));
			ground.truth.data = Qn(apply(Yt,2,median,na.rm=TRUE));
			if (multicore)
			{
				if (method=="rgrsn")
					Y.list <- mclapply(randomize,p.i.correct,X,ref,...)
				else
					Y.list <- mclapply(randomize,i.correct.grsn,X,ref,...)
				im.list <- mclapply(randomize,par.median,X,2)
				meds <- lapply(Y.list,sap.median,2)
				m <- sapply(meds,Qn)
				im <- sapply(im.list,Qn)
			}
			else
			{
				m <- im <- numeric(n)
				pb <- txtProgressBar(0,length(iset),style=3,width=50);
				for (i in 1:n)
				{
					if (method=="rgrsn")
						Y <- i.correct.grsn(randomize[[i]],X,ref,...)
					else
						Y <- i.correct(X,ref,iset=randomize[[i]],...)
					m[i] <- Qn(apply(Y,2,median,na.rm=TRUE))
					im[i] <- Qn(apply(X[randomize[[i]],],2,median,na.rm=TRUE))
					setTxtProgressBar(pb,i)
				}
			}
		},
		sn = {
			ground.truth.iset = Sn(apply(X[iset,],2,median,na.rm=TRUE));
			ground.truth.data = Sn(apply(Yt,2,median,na.rm=TRUE));
			if (multicore)
			{
				if (method=="rgrsn")
					Y.list <- mclapply(randomize,p.i.correct,X,ref,...)
				else
					Y.list <- mclapply(randomize,i.correct.grsn,X,ref,...)
				im.list <- mclapply(randomize,par.median,X,2)
				meds <- lapply(Y.list,sap.median,2)
				m <- sapply(meds,Sn)
				im <- sapply(im.list,Sn)
			}
			else
			{
				m <- im <- numeric(n)
				pb <- txtProgressBar(0,length(iset),style=3,width=50);
				for (i in 1:n)
				{
					if (method=="rgrsn")
						Y <- i.correct.grsn(randomize[[i]],X,ref,...)
					else
						Y <- i.correct(X,ref,iset=randomize[[i]],...)
					m[i] <- Sn(apply(Y,2,median,na.rm=TRUE))
					im[i] <- Sn(apply(X[randomize[[i]],],2,median,na.rm=TRUE))
					setTxtProgressBar(pb,i)
				}
			}
		})

	if (opt=="data")
		p <- length(which(ground.truth.data<m))/n
	else if (opt=="standards")
		p <- length(which(ground.truth.iset<im))/n
	else if (opt=="both")
	{
		gt <- (ground.truth.iset+ground.truth.data)/2
		mm <- (m+im)/2
		p <- length(which(gt<mm))/n
	}
		
	return(list(p=p,truth.iset=ground.truth.iset,truth.data=ground.truth.data,
				boot.iset=im,boot.data=m))
}

# type : should we normalize as affy normalized by default (that is each array against the
#	   : baseline) (pairwise) or gather all the invariant set and use it for normalization
#	   : (total), the latter is more fair for other methods and also what we want to do
ri.norm.affy <- function(peaks,baseline=NULL,pct=60,type=c("pairwise","total"),out.scale="log2",
						 mcalc=c("none","mad","qn","sn"),...)
{
	if (!require(affy))
		stop("Bioconductor package affy is required!")

	type <- tolower(type[1])
	if (!is.element(type,c("pairwise","total")))
		stop("type must be either \"pairwise\" or \"total\"")
	mcalc <- tolower(mcalc[1])
	if (!is.element(mcalc,c("none","mad","qn","sn")))
		stop("mcalc must be one of \"none\", \"mad\", \"qn\" or \"sn\"")

	# Data must not be logged
	if (max(peaks) > 107) # How possible it is to have intensity>1e+32
		log.scaled <- FALSE
	else
	{ 
		log.scaled <- TRUE
		peaks = 2^peaks
	}
	
	if (is.null(baseline))
		bl.sample <- apply(peaks,1,median)
	else
		bl.sample <- peaks[,baseline]

	n.peaks <- matrix(NA,nrow(peaks),ncol(peaks))
	ri.set <- list(ncol(peaks))
	tmp <- list(ncol(peaks))

	for (i in 1:ncol(peaks))
	{
		tmp[[i]] <- normalize.invariantset(peaks[,i],bl.sample)
		ri.set[[i]] <- which(tmp[[i]]$i.set)
	}
	ri.set.fi <- ri.affy.select(ri.set,pct)

	if (type=="pairwise")
	{
		for (i in 1:ncol(peaks))
		{
			#pred <- approx(tmp[[i]]$n.curve$y,tmp[[i]]$n.curve$x,xout=peaks[,i],rule=2)
			#n.peaks[,i] <- as.numeric(pred$y)
			pred <- extrap1(tmp[[i]]$n.curve$y,tmp[[i]]$n.curve$x,xo=peaks[,i]) # Use real extrapolation
			n.peaks[,i] <- pred
		}
	}
	else if (type=="total")
	{
		X <- remove.zeros(peaks,strategy="offset",off.set=2)
		base <- remove.zeros(bl.sample,strategy="offset",off.set=2)
		X <- log2(X)
		base <- log2(base)
		n.peaks <- i.correct(X,base,ri.set.fi)
		n.peaks <- 2^n.peaks
	}

	# Calculate the metrics for affy matrix, peaks are NEVER logged
	x <- remove.zeros(peaks,strategy="offset",off.set=2)
	x <- log2(x)
	y <- log2(n.peaks)

	# Create the attributes of the normalized matrix
	if (mcalc=="none")
		attr(n.peaks,"ri.info") <- list(ri.set=ri.set.fi,ri.all=ri.set,
										metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
										w.metric=NULL,w.imetric=NULL,w.rcv=NULL,w.ircv=NULL)
	else
	{
		xx <- apply(x[ri.set.fi,],2,median,na.rm=TRUE);
		yy <- apply(y,2,median,na.rm=TRUE);
		q1.xx <- apply(x[ri.set.fi,],2,quantile,0.25,na.rm=TRUE)
		q1.yy <- apply(y,2,quantile,0.25,na.rm=TRUE)
		q2.xx <- apply(x[ri.set.fi,],2,quantile,0.75,na.rm=TRUE)
		q2.yy <- apply(y,2,quantile,0.75,na.rm=TRUE)

		switch(mcalc,
			none = {
				w.imetric <- NULL;
				w.metric <- NULL;
				w.ircv <- NULL;
				w.rcv <- NULL;
			},
			mad = {
				#w.imetric <- mad(xx);
				#w.metric <- mad(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"mad");
				w.metric <- metric(yy,q1.yy,q2.yy,"mad");
				w.ircv <- rcv.mad(xx);
				w.rcv <- rcv.mad(yy);
			},
			qn = {
				#w.imetric <- Qn(xx);
				#w.metric <- Qn(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"Qn");
				w.metric <- metric(yy,q1.yy,q2.yy,"Qn");
				w.ircv <- rcv.qn(xx);
				w.rcv <- rcv.qn(yy);
			},
			sn = {
				#w.imetric <- Sn(xx);
				#w.metric <- Sn(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"Sn");
				w.metric <- metric(yy,q1.yy,q2.yy,"Sn");
				w.ircv <- rcv.sn(xx);
				w.rcv <- rcv.sn(yy);
			})
		attr(n.peaks,"ri.info") <- list(ri.set=ri.set.fi,ri.all=ri.set,
										metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
										w.metric=w.metric,w.imetric=w.imetric,w.rcv=w.rcv,w.ircv=w.ircv)
	}

	# Convert back to original form or according to user's preference
	if (!log.scaled && out.scale=="log2")
		return(log2(n.peaks))
	else if (!log.scaled && out.scale=="natural")
		return(n.peaks)
	else if (log.scaled && out.scale=="natural")
		return(2^n.peaks)
	else
		return(n.peaks)
}

# Hack a bit the rank invariant function of lumi in order to return the index or RIs
# Also, we change the defaults for the more dispersed metabolomic peak data
ri.norm.lumi <- function (x.lumi,targetArray=NULL,rrc=0.05,lowRank=seq(0.1,0.05,-0.01),highRank=0.5,
		                  minSize=0.02,maxit=200,out.scale="log2",mcalc=c("none","mad","qn","sn"))
{
	if (is(x.lumi,"ExpressionSet"))
	{
		expr <- exprs(x.lumi)
	}
	else if (is.numeric(x.lumi))
	{
		expr <- as.matrix(x.lumi)
	}
	else
		stop("The object should be a matrix or class \"ExpressionSet\" inherited!")

	stopifnot(min(lowRank)>0)
	stopifnot(max(lowRank)<1)
	stopifnot(length(highRank)==1)
	stopifnot(highRank<1)
	stopifnot(max(lowRank)<highRank)

	mcalc <- tolower(mcalc[1])
	if (!is.element(mcalc,c("none","mad","qn","sn")))
		stop("mcalc must be one of \"none\", \"mad\", \"qn\" or \"sn\"")

	externalTarget<-FALSE
	if (!is.null(targetArray))
	{
		if (is(targetArray,"ExpressionSet"))
		{
			targetArray<-exprs(targetArray)[,1]
		}
		if (length(targetArray)>1)
		{
			if (length(targetArray)!=nrow(expr)) 
				stop("targetArray should be an index or a vector has the same length as other samples.")
			expr <- cbind(targetArray, expr)
			targetArray <- 1
			externalTarget <- TRUE
		}
		if (is.numeric(targetArray))
		{
			if (targetArray<1 || targetArray>nrow(expr))
			{
				warning("The provided targetArray is invalid and will be set as NULL!")
				targetArray <- NULL
			}
		}
		else if (is.character(targetArray))
		{
			if (!(targetArray %in% colnames(expr)))
			{
				warning("The provided targetArray is invalid and will be set as NULL!")
				targetArray <- NULL
			}
		}
		else
		{
			warning("The provided targetArray is invalid and will be set as NULL!")
			targetArray <- NULL
		}
	}
	if (max(expr,na.rm=TRUE)>100)
	{
		log2Trans <- FALSE
	}
	else
	{
		log2Trans <- TRUE
		expr <- 2^(expr)
	}
	if (is.null(targetArray))
	{
		expr <- cbind(rowMeans(expr),expr)
		targetArray <- 1
		externalTarget <- TRUE
	}
	
	nrows <- nrow(expr)
	nArray <- ncol(expr)
	normalized <- expr
	er <- apply(expr,2,rank)
	ri <- rep(F,nrow(expr))
	rankscores <- apply(er,2,function(r,v) abs((r-v)/length(v)), er[,targetArray])

	for (i in 1:nArray)
	{
		if (i==targetArray) 
			next
		for (lowrank in lowRank)
		{
			if (getOption("verbose")) 
				cat("Sample",i-ifelse(externalTarget, 1, 0),"using lowrank ",lowrank,"\n")

			ri <- rankscores[,i]<rrc & er[,i]>nrows*lowrank & er[,i]<nrows*highRank
			if (getOption("verbose"))
				cat("Nr rankinvariant probes=",sum(ri),"\n")
			if (sum(ri)>=minSize*nrows)
			{
				coef <- rlm(expr[ri,targetArray] ~ expr[ri,i],psi=psi.bisquare,maxit=maxit)$coef
				normalized[,i] <- (expr[,i]*coef[2])+coef[1]
				break
			}
		}
		if (lowrank==tail(lowRank,1)) 
			stop(paste("No rankinvariant set for sample", i - 
				ifelse(externalTarget,1,0), "use broader lowRank/highRank or use another normalization method"))
	}
	if (externalTarget) 
		normalized <- normalized[,-1]
	if (log2Trans)
	{
		if (min(normalized)<=0)
		{
			normalized <- lumiB(normalized,method="forcePositive")
		}
		normalized <- log2(normalized)
	}
	if (is(x.lumi, "ExpressionSet"))
	{
		exprs(x.lumi) <- normalized
	}
	else
	{
		x.lumi <- normalized
	}

	#attr(x.lumi,"ri.set") <- which(ri) # The legacy hack!
	# Calculate the metrics for lumi matrix and set the attributes
	is <- which(ri) # The hack
	y <- log2(x.lumi)
	if (!log2Trans)
		x <- log2(remove.zeros(expr,strategy="offset",off.set=2))
	else
		x <- expr
	if (mcalc=="none")
		attr(x.lumi,"ri.info") <- list(ri.set=is,ri.all=NULL,
									   metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
									   w.metric=NULL,w.imetric=NULL,w.rcv=NULL,w.ircv=NULL)
	else
	{
		xx <- apply(x[is,],2,median,na.rm=TRUE);
		yy <- apply(y,2,median,na.rm=TRUE);
		q1.xx <- apply(x[is,],2,quantile,0.25,na.rm=TRUE)
		q1.yy <- apply(y,2,quantile,0.25,na.rm=TRUE)
		q2.xx <- apply(x[is,],2,quantile,0.75,na.rm=TRUE)
		q2.yy <- apply(y,2,quantile,0.75,na.rm=TRUE)

		switch(mcalc,
			none = {
				w.imetric <- NULL;
				w.metric <- NULL;
			},
			mad = {
				#w.imetric <- mad(xx);
				#w.metric <- mad(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"mad");
				w.metric <- metric(yy,q1.yy,q2.yy,"mad");
				w.ircv <- rcv.mad(xx);
				w.rcv <- rcv.mad(yy);
			},
			qn = {
				#w.imetric <- Qn(xx);
				#w.metric <- Qn(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"Qn");
				w.metric <- metric(yy,q1.yy,q2.yy,"Qn");
				w.ircv <- rcv.qn(xx);
				w.rcv <- rcv.qn(yy);
			},
			sn = {
				#w.imetric <- Sn(xx);
				#w.metric <- Sn(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"Sn");
				w.metric <- metric(yy,q1.yy,q2.yy,"Sn");
				w.ircv <- rcv.sn(xx);
				w.rcv <- rcv.sn(yy);
			})
		attr(x.lumi,"ri.info") <- list(ri.set=which(ri),ri.all=NULL,
									   metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
									   w.metric=w.metric,w.imetric=w.imetric,w.rcv=w.rcv,w.ircv=w.ircv)
	}
	
	# Output scale according to user's preference
	if (out.scale=="log2")
		return(log2(x.lumi))
	else
		return(x.lumi)
}

# Adapted from the code presented with the paper of Pelz et al., BMC Bioinformatics, 2008
ri.norm.grsn <- function(peaks,count=ceiling(0.1*nrow(peaks)),f=0.25,out.scale="log2",
						 cut.type=c("fixed","mad","qn","sn"),opt=c("data","standards","both"),
						 multicore=FALSE,mcalc=c("none","mad","qn","sn"),minstat=c("sumq","rcv"))
{
	# Check cut.type and opt
	cut.type <- tolower(cut.type[1])
	if (!is.element(cut.type,c("quantile","fixed","mad","qn","sn")))
		stop("cut.type parameter must be a \"quantile\", \"fixed\", \"mad\", \"qn\", \"sn\"!")
	opt <- tolower(opt[1])
	if (opt!="data" && opt!="standards" && opt!="both")
		stop("opt must be one of \"data\", \"standards\" or \"both\"!")
	mcalc <- tolower(mcalc[1])
	if (!is.element(mcalc,c("none","mad","qn","sn")))
		stop("mcalc must be one of \"none\", \"mad\", \"qn\" or \"sn\"")
	if (minstat!="sumq" && minstat!="rcv")
		stop("minstat must be one of \"sumq\" or \"rcv\"!")

	# Data must not be log scaled
	if (max(peaks) > 107) # How possible it is to have intensity>1e+32
		log.scaled <- FALSE
	else
	{ 
		log.scaled <- TRUE
		peaks = 2^peaks
	}

	# Data to normalize
	adjust <- max(0,(0.25-min(peaks)))
	M1 <- log2(peaks+adjust)

	# Get the average of the reference set.
	# Do a trimmed mean to be robust, but eliminate the "artifact" that 
	# shows up when doing median on an odd number of samples.
	Mavg <- apply(M1[, ],1,mean,trim=0.25)

	# New method for a global invariant set.
	total <- dim(M1)[[1]]
	idx <- 1:total
	subSet <- 1:total

	# Calculate number of elements to exclude at each iteration.
	discardNumber <- (total-count)/4

	# Main iteration loop to get approximate GRiS.
	while (TRUE)
	{
		total <- floor(max(total-discardNumber,count))
		M2 <- cbind(apply(M1[idx, ],2,rank))
		V2 <- apply(M2,1,var)
		subSet <- order(V2,decreasing=FALSE)[1:total]
		idx <- idx[subSet]
		if (total==count)
			break
	}

	# Apply our extra rule to further optimize the invariant set (starting from a bit larger set)
	if (cut.type=="fixed")
		invariantIdx <- idx
	else if (is.element(cut.type,c("mad","qn","sn")))
	{
		# A little tip to reduce arguments in iterative.select
		attr(M1,"caller") <- "grsn"
		disp("Pruning iteratively the number of invariant elements and calculating evaluation metrics...")
		disp("It might take some time... No C/C++ backend... Please be patient...")
		iset.init <- idx
		iter.out <- iterative.select(M1,Mavg,iset.init,scal=cut.type,output="list",
									 multicore=multicore,opt=opt,span=f,minstat=minstat)
		invariantIdx <- iter.out[[1]]
	}
	if (multicore)
	{
		cat("\n")
		gc(verbose=FALSE)
	}
	
	# Use invariant set to normalize all samples to the average.
	Mnew <- i.correct.grsn(invariantIdx,M1,Mavg,f)

	# Update matrix values and set attributes
	if (is.element(cut.type,c("mad","qn","sn")))
		attr(Mnew,"ri.info") <- iter.out
	else
	{
		xx <- apply(M1[invariantIdx,],2,median,na.rm=TRUE);
		yy <- apply(Mnew,2,median,na.rm=TRUE);
		q1.xx <- apply(M1[invariantIdx,],2,quantile,0.25,na.rm=TRUE)
		q1.yy <- apply(Mnew,2,quantile,0.25,na.rm=TRUE)
		q2.xx <- apply(M1[invariantIdx,],2,quantile,0.75,na.rm=TRUE)
		q2.yy <- apply(Mnew,2,quantile,0.75,na.rm=TRUE)

		switch(mcalc,
			none = {
				w.imetric <- NULL;
				w.metric <- NULL;
			},
			mad = {
				#w.imetric <- mad(xx);
				#w.metric <- mad(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"mad");
				w.metric <- metric(yy,q1.yy,q2.yy,"mad");
				w.ircv <- rcv.mad(xx);
				w.rcv <- rcv.mad(yy);
			},
			qn = {
				#w.imetric <- Qn(xx);
				#w.metric <- Qn(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"Qn");
				w.metric <- metric(yy,q1.yy,q2.yy,"Qn");
				w.ircv <- rcv.qn(xx);
				w.rcv <- rcv.qn(yy);
			},
			sn = {
				#w.imetric <- Sn(xx);
				#w.metric <- Sn(yy);
				w.imetric <- metric(xx,q1.xx,q2.xx,"Sn");
				w.metric <- metric(yy,q1.yy,q2.yy,"Sn");
				w.ircv <- rcv.mad(xx);
				w.rcv <- rcv.sn(yy);
			})

		attr(Mnew,"ri.info") <- list(ri.set=invariantIdx,ri.all=NULL,
									 metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
									 w.metric=w.metric,w.imetric=w.imetric,w.rcv=w.rcv,w.ircv=w.ircv)
	}

	# Convert back to original form or according to user's preference
	if (!log.scaled && out.scale=="log2")
		return(Mnew)
	else if (!log.scaled && out.scale=="natural")
		return(2^Mnew)
	else if (log.scaled && out.scale=="natural")
		return(2^Mnew)
	else
		return(Mnew)
}

# n : number of features to acquire from each model (either pairwise or baseline) if method="fixed"
# p : percentile of the total residuals distribution to use for dynamically selecting features
#	  if method="quantile"
# pct : percentage of dataset variables
# cut.type : how to select the invariant set? "quantile", "fixed" or "auto" for iterative
#			 selection
# off.set : the offset in case of replace.zero="offset"
# dyn.select : run the position.order function in order to have a certain ordering according
#			   to feature appearance, instead of simply tabling the unlist? the latter will
#			   definitely fail if n=ncol(peaks)
# opt : optimize the final selection based on the variability of the normalized matrix, the
#		standards or both
# ... further inputs to rlm
rlm.norm <- function(peaks,baseline=NULL,pairwise=FALSE,replace.zero=c("constant","offset","minpos","minpos.noise","none"),
					 cut.type=c("quantile","fixed","mad","qn","sn"),n=ceiling(nrow(peaks)/3),p=50,pct=60,span=0.25,multicore=FALSE,
					 out.scale="log2",off.set=1,dyn.select=FALSE,opt=c("data","standards","both"),coverage=c("none","range"),
					 minmax=c(100,n),minstat=c("sumq","rcv"),...)
{
	if (!require(MASS))
		stop("R package MASS is required!")

	# Data must be logged
	if (max(peaks) > 107) # How possible it is to have intensity>1e+32
	{
		log.scaled <- FALSE
		X <- remove.zeros(peaks,strategy=replace.zero)
		X <- log2(X)
	}
	else
		log.scaled <- TRUE
	
	if (is.null(baseline))
		bl.sample <- apply(X,1,median)
	else if (is.numeric(baseline) && length(baseline)==nrow(X))
		bl.sample <- baseline
	else if (is.numeric(baseline) && length(baseline)==1)
		bl.sample <- X[,baseline]
	else
		stop("The baseline argument is not correct, please check!")

	# Check n
	if (is.numeric(n))
	{
		if (n>nrow(peaks))
		{
			cat("The size of invariant set cannot exceed the number of variables!")
			cat("Using the default...")
			n <- ceiling(nrow(peaks)/3)
		}
	}
	else
		stop("n parameter must be a number between 1...nrow(X)!")

	# Check cut.type and opt
	cut.type <- tolower(cut.type[1])
	if (!is.element(cut.type,c("quantile","fixed","mad","qn","sn")))
		stop("cut.type parameter must be a \"quantile\", \"fixed\", \"mad\", \"qn\", \"sn\"!")
	opt <- tolower(opt[1])
	if (opt!="data" && opt!="standards" && opt!="both")
		stop("opt must be one of \"data\", \"standards\" or \"both\"!")
	coverage <- tolower(coverage[1])
	if (!is.element(coverage,c("none","range")))
		stop("coverage must be either \"none\" or \"range\"")
	replace.zero <- tolower(replace.zero[1])
	if (!is.element(replace.zero,c("constant","offset","minpos","minpos.noise","none")))
		stop("replace.zero must be one of \"constant\",\"offset\",\"minpos\",\"minpos.noise\" or \"none\"")
	if (minstat!="sumq" && minstat!="rcv")
		stop("minstat must be one of \"sumq\" or \"rcv\"!")

	# RLM modeling... this must have an aggregation feature in the end
	m <- ncol(X)
	fits <- list()
	resids <- list()

	if (pairwise) # Pairwise models in X...
	{
		disp("Calculating RLM pairwise model parameters...")
		ind <- 0
		# The actual fit
		for (i in 1:(m-1))
		{
			for (j in i:m)
			{
				if (i!=j)
				{
					ind <- ind+1
					fits[[ind]] <- rlm(X[,i],X[,j],maxit=100,...)
					resids[[ind]] <- sort(abs(residuals(fits[[ind]])),index.return=TRUE)
				}
			}
		}
	}
	else # Robust linear model of each variable against the baseline
	{
		disp("Calculating RLM baseline model parameters...")
		for (i in 1:m)
		{
			#fits[[i]] <- rlm(X[,i],bl.sample,maxit=100,...)
			fits[[i]] <- rlm(bl.sample,X[,i],maxit=100,...)
			resids[[i]] <- sort(abs(residuals(fits[[i]])),index.return=TRUE)
		}
		#fit <- rlm(X,bl.sample,maxit=100,...)
		#resids <- sort(abs(residuals(fit)),index.return=TRUE)
	}
	
	cands <- list(length(resids))
	if (cut.type=="quantile") # This should give cands of different lengths
	{
		r.p <- correctBoot(unlist(resids,use.names=FALSE),pct=p)
		for (i in 1:length(resids))
			cands[[i]] <- which(resids[[i]]<r.p)
		
		cand.tab <- table(unlist(cands))
		tmp.set <- cand.tab[which(cand.tab>=ceiling((pct/100)*length(resids)))]
		tmp.set <- sort(tmp.set,decreasing=TRUE)
		i.set <- as.numeric(names(tmp.set))
	}
	else if (cut.type=="fixed") # In case of n=nrow(X) this will fail... Need an ordering
	{
		for (i in 1:length(resids))
			cands[[i]] <- resids[[i]]$ix[1:n]
		
		if (dyn.select)
		{
			PM <- position.matrix(cands,nrow(X))
			i.set <- position.order(PM)
		}
		else
		{
			cand.tab <- table(unlist(cands))
			tmp.set <- cand.tab[which(cand.tab>=ceiling((pct/100)*length(resids)))]
			tmp.set <- sort(tmp.set,decreasing=TRUE)
			i.set <- as.numeric(names(tmp.set))
		}
	}
	else if (is.element(cut.type,c("mad","qn","sn")))
	{
		# In this case n is used to initialize the invariant set... But if it is significantly
		# lower than at least 2/3rds of the dataset AND the dataset is small, the method might
		# not converge... so issue a warning in case...
		if (n<ceiling(nrow(peaks)/3) && nrow(peaks)<1000)
			warning("n is smaller than 2/3rds of the dataset! Method might not converge...",call.=FALSE)
		# A little tip to reduce arguments in iterative.select
		attr(X,"caller") <- "rlm"
		
		for (i in 1:length(resids))
			cands[[i]] <- resids[[i]]$ix[1:n]
		
		if (dyn.select)
		{
			PM <- position.matrix(cands,nrow(X))
			iset.init <- position.order(PM)
		}
		else
		{
			if (n==ncol(peaks))
			{
				cat("The selected cut.type will fail with n=ncol(peaks)... Reverting to dynamical selection")
				PM <- position.matrix(cands,nrow(X))
				iset.init <- position.order(PM)
			}
			else
			{
				cand.tab <- table(unlist(cands))
				tmp.set <- cand.tab[which(cand.tab>=ceiling((pct/100)*length(resids)))]
				tmp.set <- sort(tmp.set,decreasing=TRUE)
				iset.init <- as.numeric(names(tmp.set))
			}
		}
		
		disp("Pruning iteratively the number of invariant elements and calculating evaluation metrics...")
		disp("It might take some time... No C/C++ backend... Please be patient...")
		#i.set <- iterative.select(X,bl.sample,iset.init,scal=cut.type,span=span)
		iter.out <- iterative.select(X,bl.sample,iset.init,scal=cut.type,output="list",multicore=multicore,
									 span=span,opt=opt,coverage=coverage,minmax=minmax,minstat=minstat)
		i.set <- iter.out[[1]]
	}

	if (multicore)
	{
		cat("\n")
		gc(verbose=FALSE)
	}
	
	Xn <- i.correct(X,bl.sample,i.set,span=span)
	if (is.element(cut.type,c("mad","qn","sn")))
		attr(Xn,"ri.info") <- iter.out
	else
	{
		tmp <- apply(Xn,2,median,na.rm=TRUE)
		attr(Xn,"ri.info") <- list(ri.set=i.set,ri.all=NULL,
								   metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
								   w.metric=NULL,w.imetric=NULL,w.rcv=rcv.mad(tmp),w.ircv=NULL)
	}
	
	# Convert back to original form
	if (!log.scaled && out.scale=="log2")
		return(Xn)
	else if (!log.scaled && out.scale=="natural")
		return(2^Xn)
	else if (log.scaled && out.scale=="natural")
		return(2^Xn)
	else
		return(Xn)
}

# grid.density : the number of points in the line Y=X
# n : number of features to acquire from the residuals
# p : percentile of the total residuals distribution to use for dynamically selecting
#	  features if method="quantile"
# off.set : the offset in case of replace.zero="offset"
# cut.type : how to select the invariant set? "quantile", "fixed" or "auto" for iterative
#			 selection
# opt : optimize the final selection based on the variability of the normalized matrix, the
#		standards or both
# pct : percentage of dataset variables
geom.norm <- function(peaks,baseline=NULL,grid.size=5*nrow(peaks),squared=FALSE,p=50,n=ceiling(nrow(peaks)/3),
					  replace.zero=c("constant","offset","minpos","minpos.noise","none"),span=0.25,multicore=FALSE,
					  cut.type=c("quantile","fixed","mad","qn","sn"),out.scale="log2",minmax=c(100,n),
					  off.set=1,opt=c("data","standards","both"),coverage=c("none","range"),minstat=c("sumq","rcv"))
{
	# Check arguments
	cut.type <- tolower(cut.type)
	if (!is.element(cut.type,c("quantile","fixed","mad","qn","sn")))
		stop("cut.type parameter must be a \"quantile\", \"fixed\", \"mad\", \"qn\", \"sn\"!")
	opt <- tolower(opt[1])
	if (opt!="data" && opt!="standards" && opt!="both")
		stop("opt must be one of \"data\", \"standards\" or \"both\"!")
	coverage <- tolower(coverage[1])
	if (!is.element(coverage,c("none","range")))
		stop("coverage must be either \"none\" or \"range\"")
	replace.zero <- tolower(replace.zero[1])
	if (!is.element(replace.zero,c("constant","offset","minpos","minpos.noise","none")))
		stop("replace.zero must be one of \"constant\",\"offset\",\"minpos\",\"minpos.noise\" or \"none\"")
	if (minstat!="sumq" && minstat!="rcv")
		stop("minstat must be one of \"sumq\" or \"rcv\"!")
	
	if (multicore && !require(multicore))
	{
		stop("R package multicore is required! Switching to single core...")
		multicore <- FALSE
	}
		
	# Data must be logged
	if (max(peaks) > 107) # How possible it is to have intensity>1e+32
	{
		log.scaled <- FALSE
		X <- remove.zeros(peaks,strategy=replace.zero)
		X <- log2(X)
	}
	else
		log.scaled <- TRUE

	#X <- peaks
	#log.scaled <- TRUE

	# Check baseline
	if (is.null(baseline))
		bl.sample <- apply(X,1,median)
	else if (is.numeric(baseline) && length(baseline)==nrow(X))
		bl.sample <- baseline
	else if (is.numeric(baseline) && length(baseline)==1)
		bl.sample <- X[,baseline]
	else
		stop("The baseline argument is not correct, please check!")

	# Check n
	if (is.numeric(n))
	{
		if (n>nrow(peaks))
		{
			cat("The size of invariant set cannot exceed the number of variables!")
			cat("Using the default...")
			n <- ceiling(nrow(peaks)/3)
		}
	}
	else
		stop("n parameter must be a number between 1...nrow(X)!")
	
	# We need a rather dense grid for calculating minimum distances
	disp("Calculating distances from Y=X...")
	r <- seq(0,ceiling(max(X)),length.out=grid.size)
	R <- matrix(NA,nrow=length(r),ncol=ncol(X))
	for (i in 1:ncol(R))
		R[,i] <- r

	if (multicore)
	{
		disp("Parallel calculation of distances from Y=X...")
		cores <- multicore:::detectCores()
		Y <- split.data(X,cores)
		d.list <- mclapply(Y,geom.lapply,R,squared)
		dists <- unlist(d.list)
	}
	else
	{
		dists <- numeric(nrow(X))
		for (i in 1:nrow(X))
		{
			if (i%%100==0) # Track a bit
				cat(i,"...",sep=" ")
			ran <- range(X[i,])
			sub.R <- R[which(R[,1]>=(ran[1]-0.5) & R[,1]<=(ran[2]+0.5)),]
			tmp <- apply(sub.R,1,euc.dist,X[i,],squared)
			dists[i] <- min(tmp)
		}
		cat("\n")
	}
	
	d <- sort(dists,index.return=TRUE)

	# Select the optimal number of invariant metabolites using one of the evaluation
	# metrics or other methods...
	if (cut.type=="quantile")
	{
		r.p <- correctBoot(d$x,pct=p)
		i.set <- which(d$x<r.p)
		i.all <- d$ix
	}
	else if (cut.type=="fixed")
	{
		#i.set <- d$ix[1:n]
		i.set <- d$ix[1:max(minmax)]
		i.all <- d$ix
	}
	else if (is.element(cut.type,c("mad","qn","sn")))
	{
		# In this case n is used to initialize the invariant set... But if it is significantly
		# lower than at least 2/3rds of the dataset AND the dataset is small, the method might
		# not converge... so issue a warning in case...
		if (n<ceiling(nrow(peaks)/3) && nrow(peaks)<1000)
			warning("n is smaller than 2/3rds of the dataset! Method might not converge...",call.=FALSE)
		# A little tip to reduce arguments in iterative.select
		attr(X,"caller") <- "geom"

		disp("Pruning iteratively the number of invariant elements and calculating evaluation metrics...")
		disp("It might take some time... No C/C++ backend... Please be patient...")
		iset.init <- d$ix[1:n]
		iter.out <- iterative.select(X,bl.sample,iset.init,scal=cut.type,output="list",multicore=multicore,
									 span=span,opt=opt,coverage=coverage,minmax=minmax,minstat=minstat)
		i.set <- iter.out[[1]]
		#i.all <- iter.out[[2]]
	}

	if (multicore)
	{
		cat("\n")
		gc(verbose=FALSE)
	}

	# Normalize according to the final selected invariant set
	Xn <- i.correct(X,bl.sample,i.set,span=span)
	#attr(Xn,"ri.set") <- i.set
	#attr(Xn,"ri.all") <- i.all
	if (is.element(cut.type,c("mad","qn","sn")))
	{
		attr(Xn,"ri.info") <- iter.out
		#attr(Xn,"metric") <- iter.out[[3]]
		#attr(Xn,"i.metric") <- iter.out[[4]]
	}
	else
	{
		tmp <- apply(Xn,2,median,na.rm=TRUE)
		attr(Xn,"ri.info") <- list(ri.set=i.set,ri.all=i.all,
								   metric=NULL,imetric=NULL,rcv=NULL,ircv=NULL,
								   w.metric=NULL,w.imetric=NULL,w.rcv=rcv.mad(tmp),w.ircv=NULL)
		#attr(Xn,"metric") <- NULL
		#attr(Xn,"i.metric") <- NULL
	}
		
	# Convert back to original form
	if (!log.scaled && out.scale=="log2")
		return(Xn)
	else if (!log.scaled && out.scale=="natural")
		return(2^Xn)
	else if (log.scaled && out.scale=="natural")
		return(2^Xn)
	else
		return(Xn)
}

# Correct the intensity values by interpolating on MD diagram
# md : correct the mean-difference plot or the original?
i.correct <- function(X,ref,iset,span=0.25,md=TRUE,extrap=TRUE)
{
	if (is.vector(X))
		X <- as.matrix(X)

	if (missing(ref))
		ref <- apply(X,1,median)
	
	Xn <- matrix(NA,nrow(X),ncol(X))
	for (i in 1:ncol(X))
	{
		if (md)
		{
			mm <- X[,i]-ref
			m <- X[iset,i]-ref[iset]
			a <- (ref[iset]+X[iset,i])/2
			lc <- lowess(m ~ a,f=span)
			pred <- extrap1(lc$x,lc$y,xo=(ref+X[,i])/2,extrap=extrap)
			Xn[,i] <- (mm-pred)+ref
			#Xn[,i] <- X[,i]-pred

#			# Lowess smoother for data outside the range of iset
#			min.ind <- which(X[,i]<min(X[iset,i]))
#			if (length(min.ind)>0)
#			{
#				mo.min <- X[min.ind,i]-ref[min.ind]
#				ao.min <- (ref[min.ind]+X[min.ind,i])/2
#				s.min <- lowess(ao.min,mo.min)
#				Xn[min.ind,i] <- (mo.min-s.min$y)+ref[min.ind]
#			}
#			max.ind <- which(X[,i]>max(X[iset,i]))
#			if (length(max.ind)>0)
#			{
#				mo.max <- X[max.ind,i]-ref[max.ind]
#				ao.max <- (ref[max.ind]+X[max.ind,i])/2
#				s.max <- lowess(ao.max,mo.max)
#				Xn[max.ind,i] <- (mo.max-s.max$y)+ref[max.ind]
#			}
		}
		else
		{
			m <- X[iset,i]
			a <- ref[iset]
			lc <- lowess(m ~ a,f=span)
			pred <- extrap1(lc$x,lc$y,xo=ref,extrap=extrap)
			Xn[,i] <- pred

#			# Lowess smoother for data outside the range of iset
#			min.ind <- which(X[,i]<min(X[iset,]))
#			if (length(min.ind)>0)
#			{
#				mo.min <- X[min.ind,i]
#				ao.min <- ref[min.ind]
#				s.min <- lowess(ao.min,mo.min)
#				Xn[min.ind,i] <- s.min$y
#			}
#			max.ind <- which(X[,i]>max(X[iset,]))
#			if (length(max.ind)>0)
#			{
#				mo.max <- X[max.ind,i]
#				ao.max <- ref[max.ind]
#				s.max <- lowess(ao.max,mo.max)
#				Xn[max.ind,i] <- s.max$y
#			}
		}
	}

	return(Xn)
}

# Correct the intensity values by interpolating on MD diagram
# md : correct the mean-difference plot or the original?
# Version to be used in parallel mode
p.i.correct <- function(iset,X,ref,span=ceiling((2/3)*nrow(X)),md=TRUE)
{
	if (is.vector(X))
		X <- as.matrix(X)

	if (missing(ref))
		ref <- apply(X,1,median)

	if (length(iset)%%10==0)
		cat(length(iset)," ")
	
	Xn <- matrix(NA,nrow(X),ncol(X))
	for (i in 1:ncol(X))
	{
		if (md)
		{
			mm <- X[,i]-ref
			m <- X[iset,i]-ref[iset]
			a <- (ref[iset]+X[iset,i])/2
			lc <- lowess(m ~ a,f=span)
			pred <- extrap1(lc$x,lc$y,xo=(ref+X[,i])/2,extrap=TRUE)
			Xn[,i] <- (mm-pred)+ref
			#Xn[,i] <- X[,i]-pred

#			# Lowess smoother for data outside the range of iset
#			min.ind <- which(X[,i]<min(X[iset,i]))
#			if (length(min.ind)>0)
#			{
#				mo.min <- X[min.ind,i]-ref[min.ind]
#				ao.min <- (ref[min.ind]+X[min.ind,i])/2
#				s.min <- lowess(ao.min,mo.min)
#				Xn[min.ind,i] <- (mo.min-s.min$y)+ref[min.ind]
#			}
#			max.ind <- which(X[,i]>max(X[iset,i]))
#			if (length(max.ind)>0)
#			{
#				mo.max <- X[max.ind,i]-ref[max.ind]
#				ao.max <- (ref[max.ind]+X[max.ind,i])/2
#				s.max <- lowess(ao.max,mo.max)
#				Xn[max.ind,i] <- (mo.max-s.max$y)+ref[max.ind]
#			}
		}
		else
		{
			m <- X[iset,i]
			a <- ref[iset]
			lc <- lowess(m ~ a,f=span)
			pred <- extrap1(lc$x,lc$y,xo=(ref+X[,i])/2,extrap=TRUE)
			Xn[,i] <- pred

#			# Lowess smoother for data outside the range of iset
#			min.ind <- which(X[,i]<min(X[iset,]))
#			if (length(min.ind)>0)
#			{
#				mo.min <- X[min.ind,i]
#				ao.min <- ref[min.ind]
#				s.min <- lowess(ao.min,mo.min)
#				Xn[min.ind,i] <- s.min$y
#			}
#			max.ind <- which(X[,i]>max(X[iset,]))
#			if (length(max.ind)>0)
#			{
#				mo.max <- X[max.ind,i]
#				ao.max <- ref[max.ind]
#				s.max <- lowess(ao.max,mo.max)
#				Xn[max.ind,i] <- s.max$y
#			}
		}
	}

	return(Xn)
}

i.correct.grsn <- function(ri,M1,x,span)
{
	if (missing(x))
		x <- apply(M1[, ],1,mean,trim=0.25)
	if (length(ri)%%10==0)
		cat(length(ri)," ")

	Mnew <- NULL
	for (b in 1:dim(M1)[[2]])
	{
		y <- M1[,b]
		# M vs. A transformed data.  ###
		M <- y-x
		A <- (y+x)/2
		# Lowess curve based on M vs. A transformed data.
		curve <- lowess(x=A[ri],y=M[ri],f=span)
		# Create evenly space lookup from calibration curve.
		aCurve <- curve[[1]]
		mCurve <- curve[[2]]
		steps <- 1000
		sampleMin <- min(A)
		sampleMax <- max(A)
		step <- (sampleMax-sampleMin)/steps
		position <- seq(sampleMin,sampleMax,length=steps+1)
		adjust <- array(0,c(steps+1))
		count <- length(aCurve)
		idxL <- 1
		idxR <- 2
		for (i in 1:(steps + 1))
		{
			while (idxR < count && position[i] > aCurve[idxR])
			{
				idxR <- idxR + 1
			}
			while ((idxL + 1) < idxR && position[i] > aCurve[idxL + 1])
			{
				idxL <- idxL + 1
			}
			while (idxR < count && aCurve[idxL] >= aCurve[idxR])
			{
				idxR <- idxR + 1
			}
			if (aCurve[idxL] < aCurve[idxR])
			{
				adjust[i] <- (((mCurve[idxR]-mCurve[idxL])/(aCurve[idxR]-aCurve[idxL]))
								*(position[i]-aCurve[idxL])+mCurve[idxL])
			}
		}
		# Apply lookup to data. Can be applied to transformed or untransformed data.
		yPrime <- y-adjust[(A-sampleMin)/step+1.5]
		mPrime <- yPrime-x
		Mnew <- cbind(Mnew,yPrime)
	}
	return(Mnew)
}

# Select a set based on percentage of presence
# set : a list of index vectors, the indices represent invariant features
ri.affy.select <- function(set,pct=60)
{
	n <- length(set)
	tmp <- table(unlist(set))
	pass <- which(tmp>=pct*n/100)
	return(as.numeric(names(pass)))
}

# This function should be used only for the rlm and geom methods
# Xo : The original matrix, corrected for zeros, to be used for iterative fitting...
# f : the housekeeping set in case Xn does not have an ri.set attribute
# scal : measure of scale, one of mad, qn or sn for Coux and Rousseew Qn, Sn
# set.only : return only the new set or also re-normalize? In the second case, a newly
#			 normalized matrix will be returned with attribute, the new ri.set
# opt : optimize the final selection based on the variability of the normalized matrix, the
#		standards or both
# caller : NULL or grsn to call the GRSN algorithm own smoothing
# ... : further arguments to i.correct
# Returns a new set based on the minimization of the normalized matrix variability
iterative.select <- function(Xo,ref,f,scal="mad",output=c("set","list","matrix"),multicore=FALSE,opt="data",
							 coverage="none",minmax=c(1,length(f)),minstat="sumq",...)
{
	require(utils)

	if(multicore && !require(multicore))
	{
		disp("Package multicore required to run in parallel... Switching to single core...")
		multicore=FALSE
	}

	if (is.null(attr(Xo,"ri.info")))
	{
		if (missing(f))
			stop("Input matrix does not have an \"ri.set\" attribute! Input f must be provided!")
	}
	else
		f <- attr(Xo,"ri.info")$ri.set

	if (missing(ref))
		ref <- apply(Xo,1,median)
			
	scal <- tolower(scal)
	if ((scal=="qn" || scal=="sn") && !require(robustbase))
		stop("R package robustbase is required!")
	output <- tolower(output[1])
	if (output!="set" && output!="list" && output!="matrix")
		stop("output must be one of \"set\", \"list\" or \"matrix\"!")
	opt <- tolower(opt)
	coverage <- tolower(coverage)
	
	# Data must be logged
	if (max(Xo) > 107) # How possible it is to have intensity>1e+32
	{
		log.scaled <- FALSE
		Xo <- log2(Xo)
	}
	else
		log.scaled <- TRUE

	m <- m.q1 <- m.q2 <- rcv <- numeric(length(f))
	im <- im.q1 <- im.q2 <- ircv <- numeric(length(f))
	caller = attr(Xo,"caller")

	if (multicore)
	{
		# We have to create a list of all possible feature vectors...
		f.list <- list(length(f))
		for (i in 1:length(f))
			f.list[[i]] <- f[1:i]
		f.list <- f.list[2:length(f.list)]

		# Now the rest
		n.core <- multicore:::detectCores()
		options(cores=ceiling(n.core/2)) # Half or RAM screwed!
		disp("Parallel iterative selection... Please wait...")
		switch(scal,
			mad = {
					if (caller=="grsn")
						Y.list <- mclapply(f.list,i.correct.grsn,Xo,ref,...)
					else
						Y.list <- mclapply(f.list,p.i.correct,Xo,ref,...)
					im.list <- mclapply(f.list,par.median,Xo,2)
					meds <- lapply(Y.list,sap.median,2)

					q1.list <- mclapply(f.list,par.quantile,Xo,2,0.25)
					q2.list <- mclapply(f.list,par.quantile,Xo,2,0.75)
					q1s <- lapply(Y.list,sap.quantile,2,0.25)
					q2s <- lapply(Y.list,sap.quantile,2,0.75)
					
					m[1] <- mad(apply(Xo,2,median,na.rm=TRUE))
					im[1] <- mad(Xo[f[1],],na.rm=TRUE)
					m[2:length(f)] <- sapply(meds,mad)
					im[2:length(f)] <- sapply(im.list,mad)

					m.q1[1] <- mad(apply(Xo,2,quantile,0.25,na.rm=TRUE))
					m.q2[1] <- mad(apply(Xo,2,quantile,0.75,na.rm=TRUE))
					im.q1[1] <- mad(Xo[f[1],],na.rm=TRUE)
					im.q2[1] <- mad(Xo[f[1],],na.rm=TRUE)
					m.q1[2:length(f)] <- sapply(q1s,mad,na.rm=TRUE)
					m.q2[2:length(f)] <- sapply(q2s,mad,na.rm=TRUE)
					im.q1[2:length(f)] <- sapply(q1.list,mad,na.rm=TRUE)
					im.q2[2:length(f)] <- sapply(q2.list,mad,na.rm=TRUE)

					tmp <- apply(Xo,2,median,na.rm=TRUE)
					rcv[1] <- mad(tmp)/median(tmp)
					ircv[1] <- mad(Xo[f[1],],na.rm=TRUE)
					rcv[2:length(f)] <- sapply(meds,rcv.mad)
					ircv[2:length(f)] <- sapply(im.list,rcv.mad)

					im <- im+im.q1+im.q2
					m <- m+m.q1+m.q2
				  },
			qn = {
					if (caller=="grsn")
						Y.list <- mclapply(f.list,i.correct.grsn,Xo,ref,...)
					else
						Y.list <- mclapply(f.list,p.i.correct,Xo,ref,...)
					im.list <- mclapply(f.list,par.median,Xo,2)
					meds <- lapply(Y.list,sap.median,2)

					q1.list <- mclapply(f.list,par.quantile,Xo,2,0.25)
					q2.list <- mclapply(f.list,par.quantile,Xo,2,0.75)
					q1s <- lapply(Y.list,sap.quantile,2,0.25)
					q2s <- lapply(Y.list,sap.quantile,2,0.75)

					m[1] <- Qn(apply(Xo,2,median,na.rm=TRUE))
					im[1] <- Qn(Xo[f[1],])
					m[2:length(f)] <- sapply(meds,Qn)
					im[2:length(f)] <- sapply(im.list,Qn)

					m.q1[1] <- Qn(apply(Xo,2,quantile,0.25,na.rm=TRUE))
					m.q2[1] <- Qn(apply(Xo,2,quantile,0.75,na.rm=TRUE))
					im.q1[1] <- Qn(Xo[f[1],])
					im.q2[1] <- Qn(Xo[f[1],])
					m.q1[2:length(f)] <- sapply(q1s,Qn)
					m.q2[2:length(f)] <- sapply(q2s,Qn)
					im.q1[2:length(f)] <- sapply(q1.list,Qn)
					im.q2[2:length(f)] <- sapply(q2.list,Qn)

					tmp <- apply(Xo,2,median,na.rm=TRUE)
					rcv[1] <- Qn(tmp)/median(tmp)
					ircv[1] <- Qn(Xo[f[1],])
					rcv[2:length(f)] <- sapply(meds,rcv.qn)
					ircv[2:length(f)] <- sapply(im.list,rcv.qn)

					im <- im+im.q1+im.q2
					m <- m+m.q1+m.q2
				  },
			sn = {
					if (caller=="grsn")
						Y.list <- mclapply(f.list,i.correct.grsn,Xo,ref,...)
					else
						Y.list <- mclapply(f.list,p.i.correct,Xo,ref,...)
					im.list <- mclapply(f.list,par.median,Xo,2)
					meds <- lapply(Y.list,sap.median,2)

					q1.list <- mclapply(f.list,par.quantile,Xo,2,0.25)
					q2.list <- mclapply(f.list,par.quantile,Xo,2,0.75)
					q1s <- lapply(Y.list,sap.quantile,2,0.25)
					q2s <- lapply(Y.list,sap.quantile,2,0.75)

					m[1] <- Sn(apply(Xo,2,median,na.rm=TRUE))
					im[1] <- Sn(Xo[f[1],])
					m[2:length(f)] <- sapply(meds,Sn)
					im[2:length(f)] <- sapply(im.list,Sn)

					m.q1[1] <- Sn(apply(Xo,2,quantile,0.25,na.rm=TRUE))
					m.q2[1] <- Sn(apply(Xo,2,quantile,0.75,na.rm=TRUE))
					im.q1[1] <- Sn(Xo[f[1],])
					im.q2[1] <- Sn(Xo[f[1],])
					m.q1[2:length(f)] <- sapply(q1s,Sn)
					m.q2[2:length(f)] <- sapply(q2s,Sn)
					im.q1[2:length(f)] <- sapply(q1.list,Sn)
					im.q2[2:length(f)] <- sapply(q2.list,Sn)

					tmp <- apply(Xo,2,median,na.rm=TRUE)
					rcv[1] <- Sn(tmp)/median(tmp)
					ircv[1] <- Sn(Xo[f[1],])
					rcv[2:length(f)] <- sapply(meds,rcv.sn)
					ircv[2:length(f)] <- sapply(im.list,rcv.sn)

					im <- im+im.q1+im.q2
					m <- m+m.q1+m.q2
				  })
	}
	else
	{
		switch(scal,
			mad = {
					pb <- txtProgressBar(0,length(f),style=3,width=50);
					m[1] <- mad(apply(Xo,2,median,na.rm=TRUE));
					im[1] <- mad(Xo[f[1],],na.rm=TRUE);
					for (i in 2:length(f))
					{
						if (caller=="grsn")
							Y <- i.correct.grsn(f[1:i],Xo,ref,...)
						else
							Y <- i.correct(Xo,ref,iset=f[1:i],...);
						m[i] <- mad(apply(Y,2,median,na.rm=TRUE))
						im[i] <- mad(apply(Xo[f[1:i],],2,median,na.rm=TRUE))
						setTxtProgressBar(pb,i)	
					}
				  },
			qn = {
					pb <- txtProgressBar(0,length(f),style=3,width=50);
					m[1] <- Qn(apply(Xo,2,median,na.rm=TRUE));
					im[1] <- Qn(Xo[f[1],]);
					for (i in 2:length(f))
					{
						if (caller=="grsn")
							Y <- i.correct.grsn(f[1:i],Xo,ref,...)
						else
							Y <- i.correct(Xo,ref,iset=f[1:i],...);
						m[i] <- Qn(apply(Y,2,median,na.rm=TRUE))
						im[i] <- Qn(apply(Xo[f[1:i],],2,median,na.rm=TRUE))
						setTxtProgressBar(pb,i)
					}
				  },
			sn = {
					pb <- txtProgressBar(0,length(f),style=3,width=50);
					m[1] <- Sn(apply(Xo,2,median,na.rm=TRUE));
					im[1] <- Sn(Xo[f[1],]);
					for (i in 2:length(f))
					{
						if (caller=="grsn")
							Y <- i.correct.grsn(f[1:i],Xo,ref,...)
						else
							Y <- i.correct(Xo,ref,iset=f[1:i],...);
						m[i] <- Sn(apply(Y,2,median,na.rm=TRUE))
						im[i] <- Sn(apply(Xo[f[1:i],],2,median,na.rm=TRUE))
						setTxtProgressBar(pb,i)	
					}
				  })

		cat("\n");
	}

	if (opt=="data")
	{
		mm <- m
		cc <- rcv
	}
	else if (opt=="standards")
	{
		mm <- im
		cc <- ircv
	}
	else if (opt=="both")
	{
		mm <- (m+im)/2
		cc <- (rcv+ircv)/2
	}

	if (coverage=="range")
	{
		for (i in 2:length(f))
		{
			mm[i] <- mm[i]/diff(range(Xo[f[2:i],]))
			cc[i] <- cc[i]/diff(range(Xo[f[2:i],]))
		}
	}

	ridx <- minmax[1]:minmax[2]
	if (minstat=="sumq")
		idx <- ridx[which(mm[ridx]==min(mm[ridx]))]
	else if (minstat=="rcv")
		idx <- ridx[which(cc[ridx]==min(cc[ridx]))]
	
	if (length(idx)>1) { idx <- sort(idx) }
	f.new <- f[1:idx[1]]
	m.f <- m[idx[1]]
	im.f <- im[idx[1]]
	rcv.f <- rcv[idx[1]]
	ircv.f <- ircv[idx[1]]

	switch(output,
		set = { return(f.new) },
		list = { return(list(ri.set=f.new,ri.all=f,
							 metric=m,imetric=im,rcv=rcv,ircv=ircv,
							 w.metric=m.f,w.imetric=im.f,w.rcv=rcv.f,w.ircv=ircv.f)) },
		matrix = {
					Yn <- i.correct(Xn,ref,iset=f.new,...);
					attr(Yn,"ri.info") <- list(ri.set=f.new,ri.all=f,
											   metric=m,imetric=im,rcv=rcv,ircv=ircv,
											   w.metric=m.f,w.imetric=im.f,w.rcv=rcv.f,w.ircv=ircv.f);
					return(Yn)
				 })
}

# m : the total number of features/variables/metabolites
# n : the number of 1st best features
position.matrix <- function(i.list,m,n)
{
	if (missing(n)) # Try to infer from length of list elements
	{
		l <- sapply(i.list,length)
		n <- unique(l)
		if (length(n)>1)
			stop("The i.list elements must have the same length!")
	}
	# m rows represent the feature
	# n columns represent the position in the ranked list
	PM <- matrix(0,nrow=m,ncol=n)
	for (i in 1:length(i.list))
		for (j in 1:n)
			PM[i.list[[i]][j],j] <- PM[i.list[[i]][j],j] + 1

	return(PM)
}

# ignore.ncol : Perform an exhaustive feature ranking according to appearances, taking or
#				not into account the number of columns in PM matrix which tells how many
#				"first best" features we have. If TRUE, then it will return more than
#				ncol(PM) features which express an ordering of all the features that appear
#				regardless of the "first best". FALSE will return an ordering of "first
#				best" Defaults to FALSE.
position.order <- function(PM,ignore.ncol=FALSE)
{
	features <- 1:nrow(PM)
	#final.order <- numeric(ncol(PM))
	final.order <- NULL
	visited <- logical(nrow(PM))
	total.iter <- 0
	vis.old <- -1
	vis.new <- -2

	while (!all(visited) && vis.new!=vis.old)
	{
		vis.old <- length(which(visited))
		
		total.iter <- total.iter+1
		cat("Matrix scan",total.iter,"\n")

		for (j in 1:ncol(PM))
		{
			s <- sort(PM[,j],decreasing=TRUE,index.return=TRUE)
			tmp.app <- s$x[s$x>0]
			tmp.fet <- s$ix[s$x>0]
			tmp.u <- unique(tmp.app) 
			
			if (all(visited[tmp.fet])) # If there is nothing to the current j, advance
				next

			if (length(tmp.u)>1) # We may have a clear winner a but it requires more checking
			{
				# Clear winner if not already in list
				if (length(which(tmp.app==max(tmp.u)))==1)
				{
					if (!visited[tmp.fet[1]])
					{
						add <- tmp.fet[1]
						lucky <- TRUE
					}
					else
						lucky <- FALSE
				} else
					lucky <- FALSE
				# More ties or the clear winner was visited... we have something like 2 2 1 1 1 1 ...
				# Complicated...
				if (!lucky)
				{
					for (k in tmp.u) # unique puts in order of appearance, so decrasing after sort
					{
						cf <- which(tmp.app==k)
						if (!all(visited[tmp.fet[cf]]))
						{
							tf <- tmp.fet[cf]
							select.from <- tf[which(!visited[tf])]
							if (length(select.from)>1)
								add <- sample(select.from,1)
							else
							{
								add <- select.from
								break
							}
						}
					}
				}
			}
			else # A single tie, easy to break
			{
				select.from <- tmp.fet[which(!visited[tmp.fet])]
				if (length(select.from)>1)
					add <- sample(select.from,1)
				else
					add <- select.from
			}		

			final.order <- c(final.order,add)
			visited[add] <- TRUE
			rem <- which(features==add)
			features <- features[-rem]

			vis.new <- length(which(visited))
		}
	}

	cat("Features ordered in",total.iter,"iterations...\n")
	#return(list(final.order,features,visited))
	if (!ignore.ncol)
		return(final.order[1:ncol(PM)])
	else
		return(final.order)
}

geom.lapply <- function(x,r,sq)
{
	di <- numeric(nrow(x))
	for (i in 1:nrow(x))
	{
		ran <- range(x[i,])
		sub.r <- r[which(r[,1]>=(ran[1]-0.5) & r[,1]<=(ran[2]+0.5)),]
		tmp <- apply(sub.r,1,euc.dist,x[i,],sq)
		di[i] <- min(tmp)
	}
	return(di)
}

split.data <- function(x,n)
{
	l <- list(n)
	if (!is.null(dim(x))) # x is a matrix/data.frame etc.
	{
		e <- ceiling(nrow(x)/n)
		for (i in 1:n)
			l[[i]] <- rep(i,e)
		f <- unlist(l)
		f <- f[1:nrow(x)]
		y <- split(as.data.frame(x),f)
		for (i in 1:n)
			y[[i]] <- as.matrix(y[[i]])
		return(y)
	}
	else # vector
	{
		e <- ceiling(length(x)/n)
		for (i in 1:n)
			l[[i]] <- rep(i,e)
		f <- unlist(l)
		f <- f[1:length(x)]
		return(split(x,f))
	}
}

sap.median <- function(x,d) { apply(x,d,median,na.rm=TRUE) }

sap.quantile <- function(x,d,p) { apply(x,d,quantile,p,na.rm=TRUE) }

sap.iqr <- function(x,d) { apply(x,d,IQR,na.rm=TRUE) }

sap.qn <- function(x,d) { apply(x,d,qn,na.rm=TRUE) }

sap.sn <- function(x,d) { apply(x,d,sn,na.rm=TRUE) }

par.median <- function(i,x,d) { apply(x[i,],d,median,na.rm=TRUE) }

par.quantile <- function(i,x,d,p) { apply(x[i,],d,quantile,p,na.rm=TRUE) }

par.iqr <- function(i,x,d) { apply(x[i,],d,IQR,na.rm=TRUE) }

par.qn <- function(i,x,d) { apply(x[i,],d,Qn) }

par.sn <- function(i,x,d) { apply(x[i,],d,Sn) }

rcv.mad <- function(x) { mad(x,na.rm=TRUE)/median(x) }

rcv.qn <- function(x) { Qn(x)/median(x) }

rcv.sn <- function(x) { Sn(x)/median(x) }

rcv.iqr <- function(x) { IQR(x,na.rm=TRUE)/median(x) }

iqr.dist <- function(i,x,d)
{
	a <- apply(x[i,],d,quantile,0.25)
	b <- apply(x[i,],d,quantile,0.75)
	ab <- cbind(a,b)
	return(sum(as.vector(dist(ab))))
}

yiqr.dist <- function(x,d,...)
{
	a <- apply(x,d,quantile,0.25,...)
	b <- apply(x,d,quantile,0.75,...)
	ab <- cbind(a,b)
	return(sum(as.vector(dist(ab))))
}

metric <- function(x,y,z,m)
{
	m <- tolower(m)
	#if (m=="mad")
	#	return(-log10(mad(x)*mad(y)*mad(z)))
	#else if (m=="qn")
	#	return(-log10(Qn(x)*Qn(y)*Qn(z)))
	#else if (m=="sn")
	#	return(-log10(Sn(x)*Sn(y)*Sn(z)))
	if (m=="mad")
		return(sum(c(mad(x),mad(y),mad(z))))
	else if (m=="qn")
		return(sum(c(Qn(x),Qn(y),Qn(z))))
	else if (m=="sn")
		return(sum(c(Sn(x),Sn(y),Sn(z))))
}

## Version of prosition.order with debugging information
#position.order <- function(PM)
#{
#	features <- 1:nrow(PM)
#	#final.order <- numeric(ncol(PM))
#	final.order <- NULL
#	visited <- logical(nrow(PM))
#	rem <- numeric(ncol(PM))

#	for (j in 1:ncol(PM))
#	{
#		s <- sort(PM[,j],decreasing=TRUE,index.return=TRUE)
#		tmp.app <- s$x[s$x>0]
#		tmp.fet <- s$ix[s$x>0]
#		tmp.u <- unique(tmp.app) 
		
#		if (all(visited[tmp.fet])) # If there is nothing to the current j, advance
#		{
#			cat("Evertyhing is visited... Continuing...\n")
#			cat("--------------------\n")
#			next
#		}

#		if (length(tmp.u)>1) # We may have a clear winner a but it requires more checking
#		{
#			# Clear winner if not already in list
#			if (length(which(tmp.app==max(tmp.u)))==1)
#			{
#				if (!visited[tmp.fet[1]])
#				{
#					cat("Clear winner!\n")
#					add <- tmp.fet[1]
#					lucky <- TRUE
#				}
#				else lucky <- FALSE
#			} else lucky <- FALSE
#			# More ties or the clear winner was visited... we have something like 2 2 1 1 1 1 ...
#			# Complicated...
#			if (!lucky)
#			{
#				cat("Multiple ties\n")
#				for (k in tmp.u) # unique puts in order of appearance, so decrasing after sort
#				{
#					cf <- which(tmp.app==k)
#					if (!all(visited[tmp.fet[cf]]))
#					{
#						tf <- tmp.fet[cf]
#						select.from <- tf[which(!visited[tf])]
#						if (length(select.from)>1)
#							add <- sample(select.from,1)
#						else
#						{
#							add <- select.from
#							break
#						}
#					}
#				}
#			}
#		}
#		else # A single tie, easy to break
#		{
#		#	dec <- sample(which(!visited[tmp.ind]),1)
#		#	add <- tmp.ind[dec]
#			cat("Single tie\n")
#			#add <- tmp.ind[sample(which(!visited[tmp.fet]),1)]
#			cat(visited[tmp.fet],"\n")
#			cat(which(!visited[tmp.fet]),"\n")
#			cat(tmp.fet,"\n")
#			cat(tmp.fet[which(!visited[tmp.fet])],"\n")
			
#			select.from <- tmp.fet[which(!visited[tmp.fet])]
#			if (length(select.from)>1)
#				add <- sample(select.from,1)
#			else
#				add <- select.from
#		}
#		#final.order[j] <- add
#		#print(add)
#		cat("j=",j,"\n",sep="")
#		cat(is.element(add,final.order),"\n")
#		final.order <- c(final.order,add)
#		visited[add] <- TRUE
#		#rem[j] <- which(features==add)
#		cat("add=",add,"\n",sep="")
#		rem <- which(features==add)
#		cat("rem=",rem,"\n",sep="")
#		cat("--------------------\n")
#		#features <- features[-rem[j]]
#		features <- features[-rem]
#	}
#	return(list(final.order,features,visited))
#}

#for (i in 1:nrow(X))
#{
#	if (i%%100==0) # Track a bit
#		cat(i,"...","\n",sep="")
#	tmp <- apply(R,1,euc.dist,X[i,],squared)
#	dists[i] <- min(tmp)
#}
