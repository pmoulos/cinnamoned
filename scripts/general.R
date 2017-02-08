# In order to apply interp1 from package signal x must be monotonic, so we write this
# small function to take care of this
extrap1 <- function(x,y,xo,extrap=NA,method="linear")
{
	if (!require(signal))
		stop("R package signal is required!")

	if (!is.numeric(x))
		x <- as.numeric(x)
	if (!is.numeric(y))
		y <- as.numeric(y)
	if (!is.numeric(xo))
		xo <- as.numeric(xo)
	
	if (is.unsorted(x))
	{
		xs <- sort(x,index.return=TRUE)
		yxs <- y[xs$ix]
		out <- interp1(xs$x,yxs,xi=xo,method=method,extrap=extrap)
	}
	else
		out <- interp1(x,y,xi=xo,method=method,extrap=extrap)

	return(out)
}

# constant : replace <=0 with one so no harm when log2
# offset : add an offset to <=0 instead of replacement, default 1 (like constant)
# minpos : replace <=0 with minimum value existing in the matrix
# minpos.noise : same as minpos with some gaussian noise
remove.zeros <- function(x,strategy=c("constant","offset","minpos","minpos.noise","none"),off.set=1)
{
	strategy <- strategy[1]
	switch(strategy,
		constant = { x[x<=0] <- 1 },
		offset = { x[x<=0] <- x[x<=0]+off.set },
		minpos = { x[x<=0] <- min(x[x>0]) },
		minpos.noise = {
						ind <- which(x[x<=0]);
						m <- min(x[x>0]);
						for (i in ind)
							x[i] <- m+runif(1)
						},
		none = { }) # Do nothing in case of "none"
	return(x)
}

# x : a table from xcms object WITH phenodata (possibly annotated)
#	  OR an annotated data.frame from the xcmsPipeline.R
construct.peaktable <- function(xs,n.class,filter.n=0,digits=6,stats.out=TRUE)
{
	# Determine if we have xsAnnotate or xcmsSet
	if (class(xs)=="xcmsSet")
	{
		if (!require(xcms))
			stop("Bioconductor package xcms is required!")

		pheno <- phenoData(xs)
		xs.table <- peakTable(xs,method="maxint",value="into",intensity="into")
		peaks.data <- split.peaktable(xs.table,n.class=n.class,filter.n=filter.n,digits=digits)
	}
	else if (class(xs)=="xsAnnotate")
	{
		if (!require(CAMERA))
			stop("Bioconductor package CAMERA is required!")
		
		pheno <- phenoData(xs)
		xs.table <- getPeaklist(xs,intval="into")
		peaks.data <- split.peaktable(xs.table,n.class=n.class,filter.n=filter.n,digits=digits)
		
	}
	else if (class(xs)=="data.frame") # After annotation procedure
		peaks.data <- split.peaktable(xs,n.class=n.class,filter.n=filter.n,digits=digits)

	return(peaks.data)
}

# x : a table from xcms peakTable, xsAnnotate getPeaklist or data.frame from annotatePeaks functions
split.peaktable <- function(x,n.class,filter.n=0,digits=6,stats.out=TRUE)
{
	mz <- round(x$mz,digits=digits)
	#rt <- round(x$rt,digits=digits)
	rt <- round(x$rt,digits=3)
	class.data <- x[,8:(8+n.class-1)]
	stats <- cbind(x$mz,x$rt,x$mzmin,x$mzmax,x$rtmin,x$rtmax,x$npeaks,class.data)

	cols <- c("isotopes","adduct","pcgroup","real.mass","prop.formula","theor.mass","delta.ppm")
	e <- length(intersect(cols,colnames(x)))
	if (e!=0)
	{
		num <- x[,(8+n.class):(ncol(x)-e)]
		ann <- x[,(ncol(x)-e+1):ncol(x)]
	}
	else
	{
		num <- x[,(8+n.class):ncol(x)]
		ann <- NULL
	}

	rnames <- paste(mz,rt,sep="/")
	rownames(num) <- rnames
	rownames(stats) <- rnames
	colnames(stats)[1:7] <- c("mz","rt","mzmin","mzmax","rtmin","rtmax","n.peaks")
	if (!is.null(ann))
		rownames(ann) <- rnames

	if (filter.n>0)
	{
		num <- num[stats$n.peaks>filter.n,]
		stats <- stats[stats$n.peaks>filter.n,]
		mz <- mz[stats$n.peaks>filter.n]
		rt <- rt[stats$n.peaks>filter.n]
		if (!is.null(ann))
			ann <- ann[stats$n.peaks>filter.n,]
	}
	if (stats.out)
		peaks.data <-list(num=num,stats=stats,mz=mz,rt=rt,ann=ann)
	else
		peaks.data <- num

	return(peaks.data)
}

euc.dist <- function(x,y,squared=FALSE)
{
	if (squared)
		return(sum((x-y)^2))
	else
		return(sqrt(sum((x-y)^2)))
}

# Derive a robust percentile using bootstrap correction 
correctBoot <- function(data.vec,old.val,pct=90,n=1000)
{
    if (missing(old.val))
        old.val<-quantile(data.vec,pct/100,type=5)
    if (!require(boot))
        stop("R package boot is required!")
        
    myq <- function(x,ind,...) { quantile(x[ind],...) }
    boot.obj <- boot(data.vec,myq,n,stype="i",probs=pct/100,type=7)
    q.obj <- imp.quantile(boot.obj,alpha=pct)
    return(q.obj$rat)
}

pairwise.fc <- function(x,log.it=TRUE)
{
	n <- length(x)
	d <- numeric(n*(n-1)/2)
	co <- 0

	if (log.it)
	{
		for (i in 1:(n-1))
		{
			for (j in (i+1):n)
			{
				co <- co+1
				d[co] <- log2(x[j])-log2(x[i])
			}
		}
	}
	else
	{
		for (i in 1:(n-1))
		{
			for (j in (i+1):n)
			{
				co <- co+1
				d[co] <- x[j]/x[i]
			}
		}
	}

	return(d)
}

# t : threshold above which to return elements
simple.aggregate <- function(x,t)
{
	if (!is.list(x))
		stop("Input to simple.aggregate must be a list")
	
	y <- table(unlist(x))
	return(which(y>=t))
}

coef.var <- function(x)
{
	return(sd(x)/mean(x))
}

distr.mode <- function(x)
{
	return(as.numeric(names(sort(-table(x)))[1]))
}

source.dir <- function(path,trac=TRUE,...)
{
	for (nm in list.files(path,pattern="\\.[RrSsQq]$"))
	{
		if(trac) cat(nm,":")           
		source(file.path(path,nm),...)
		if(trac) cat("\n")
	}
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

clear.but.this <- function(this)
{
	rm(list=setdiff(ls(),this))
}

match.mz <- function(mznew,mzref,tol=0.01)
{
	m <- length(mzref)
	n <- length(mznew)
	match.ref <- list(mz.ref=numeric(m),mz.new=numeric(m),mz.diff=numeric(n),ref.idx=numeric(n),new.idx=NULL,pct=NULL)
	for (i in 1:n)
	{
		#if (i%%100==0) # Track a bit
		#	cat(i,"...",sep="")
		sub.idx <- which(mzref>=mznew[i]-1 & mzref<=mznew[i]+1)
		sub.m <- mzref[sub.idx]
		tmp <- sub.m-mznew[i]
		atmp <- abs(tmp)
		d <- suppressWarnings(min(atmp))
		if (d<=tol)
		{
			mi <- which(atmp==min(atmp))[1]
			match.ref$ref.idx[i] <- sub.idx[mi]
			#match.ref$mz.new[i] <- mzref[match.ref$ref.idx[i]]
			match.ref$mz.diff[i] <- tmp[mi]
		}	
		else
			match.ref$ref.idx[i] <- match.ref$mz.diff[i] <- NA
			#match.ref$ref.idx[i] <- match.ref$mz.new[i] <- match.ref$mz.diff[i] <- NA
	}
	match.ref$new.idx <- which(!is.na(match.ref$ref.idx))
	match.ref$ref.idx <- match.ref$ref.idx[!is.na(match.ref$ref.idx)]
	match.ref$mz.ref[match.ref$ref.idx] <- mzref[match.ref$ref.idx]
	match.ref$mz.ref[match.ref$mz.ref==0] <- NA
	match.ref$mz.new[match.ref$ref.idx] <- mznew[match.ref$new.idx]
	match.ref$mz.new[match.ref$mz.new==0] <- NA
	p <- 100*length(match.ref$new.idx)/length(mznew)
	p <- sprintf("%.3f",p)
	cat("\n ",p,"% of new mz matched to reference mz with ",tol," mz tolerance!\n",sep="")
	match.ref$pct <- p
	return(match.ref)
}

# ... : further arguments to be passed in plot.rtdev
# fail : a case to be used when kmeans returns a unite cluster (e.g. the original rtref)
# tol : deviation tolerance in each cluster
is.retcor <- function(rtnew,rtref,iset,tspan=0.75,it=3,tol=2,fail=rtref,corrfac=2,plotd=FALSE,...)
{
	its <- 1
	mdev <- rep(999,length(iset))

	# Exclude bad standards
	D <- rtnew-rtref
	bad <- which(abs(D[iset])>quantile(abs(D[iset]),0.95))
	iset <- iset[-bad]

	while (!all(mdev<tol) && its<=it)
	{
		cat("\n---Retention time correction, iteration ",its,sep="")

		rtdev <- rtnew-rtref
		centers <- cbind(rtref[iset],rtdev[iset])
		dupe <- which(duplicated(centers))
		if (length(dupe)>0)
			centers <- centers[-dupe,]

		datamat <- cbind(rtref,rtdev)
		km <- kmeans(datamat,centers,iter.max=20)
		while(any(km$size==1))
		{
			singleton <- which(km$size==1)
			centers <- centers[-singleton,]
			km <- kmeans(datamat,centers,iter.max=20)
		}
		rtcor <- numeric(length(rtref))
		cldev <- numeric(length(unique(km$cluster)))
		curv <- vector("list",length(unique(km$cluster)))
		for (i in unique(km$cluster))
		{
			if (median(abs(rtnew[km$cluster==i]-rtref[km$cluster==i])) < 1e-6)
			{
				curv[[i]]$x <- r$rtdevc$x
				curv[[i]]$y <- r$rtdevc$y
				next
			}
			else
			{
				if (tspan=="adaptive" || tspan==0)
					span <- adapt.span(km$size[i]/max(km$size))
				r <- rt.smooth(rtnew[km$cluster==i],rtref[km$cluster==i],span=span,fail=fail[km$cluster==i])
				#rtcor[km$cluster==i] <- r$rtcor
				rtcor[km$cluster==i] <- rtnew[km$cluster==i] - r$rtdevmo
				#cldev[i] <- abs(median(r$rtcor-rtref[km$cluster==i]))
				cldev[i] <- abs(median(rtcor[km$cluster==i]-rtref[km$cluster==i]))
				curv[[i]]$x <- r$rtdevc$x
				curv[[i]]$y <- r$rtdevc$y
			}
		}

		# Correct for possible smoother singularities
		rtdevtot <- rtcor - rtref
		singidx <- which(abs(rtdevtot)>quantile(abs(rtdevtot),0.9)*corrfac)
		rtdevtot[singidx] <- NA
		naidx <- which(is.na(rtdevtot))
		if (length(naidx)>0)
		{
			rtdevtot[naidx] <- suppressWarnings(approx(na.omit(data.frame(rtcor,rtdevtot)),xout=rtcor[naidx],rule=2)$y)
			rtcor[naidx] <- rtnew[naidx] - rtdevtot[naidx]
		}

		if (plotd)
		{
			xx <- cbind(rtref,rtref)
			yy <- cbind(rtnew,rtcor)
			plot.rtdev(xx,yy,l=iset,lcl=curv,...)
		}
		
		rtnew <- rtcor
		its <- its+1
		mdev <- cldev
	}
	cat("\n")

	#return(rtcor)
	return(list(rtcor=rtcor,rtdevc=curv,excl=length(bad)))
	#return(list(rtcor=rtnew,rtdevc=curv))
}

rt.smooth <- function(rtnew,rtref,span,fail=NULL,corrfac=2)
{
	rtdev <- rtnew-rtref
	y <- rtdev
	x <- rtref
	
	if (length(rtdev)<2) # This is not likely to happen anymore after singleton removal
	{
		warning("Cannot correct with one point! Global median correction will be applied. Consider also lowering the number of iterations...",call.=FALSE)
		if (!is.null(fail)) rtdevmo <- 0
			#rtdevmo <- rtnew - median(fail)
		else
			stop("\nCannot correct with only one pair of points... Please provide the \"fail\" option...",call.=FALSE)
	}
	else if (length(rtdev)<8)
	{
		warning(paste("Not enough points (",length(rtdev),") for a local smoother... Will use a linear",sep=""),call.=FALSE)
		fit <- suppressWarnings(lsfit(x,y))
		rtdevmo <- fit$coef[1] + rtref*fit$coef[2]
		
		# Correct prediction outside time interval singularities
		rang <- range(rtnew)
		minidx <- rtref < rang[1]
		maxidx <- rtref > rang[2]
		if (!any(minidx)) # If not any, then we don't need singularity correction
			rtdevmo[minidx] <- rtdevmo[head(which(!minidx),1)]
		if (!any(maxidx))
			rtdevmo[maxidx] <- rtdevmo[tail(which(!maxidx),1)]
	}
	else
	{
		lc <- loess(y ~ x,data.frame(x=x,y=y),span=span,degree=1,family="symmetric")
		rtdevmo <- predict(lc,data.frame(x=x))
		# Correct for possible loess singularities
		singidx <- which(abs(rtdevmo)>quantile(abs(rtdevmo),0.9)*corrfac)
		rtdevmo[singidx] <- NA
		naidx <- which(is.na(rtdevmo))
		if (length(naidx)>0)
			rtdevmo[naidx] <- suppressWarnings(approx(na.omit(data.frame(rtnew,rtdevmo)),xout=rtnew[naidx],rule=2)$y)
	}

	#return(list(rtcor=rtnew-rtdevmo,rtdevc=data.frame(x=rtref,y=rtdevmo)))
	return(list(rtdevmo=rtdevmo,rtdevc=data.frame(x=rtref,y=rtdevmo)))
}

is.intcor <- function(new,ref,iset,method=c("loess","rlm","lm"),span=0.75,adapt.factor=length(iset),corrfac=2,maxit=100)
{
	method <- tolower(method[1])
	if (method=="loess")
	{
		if (span=="adaptive" || span==0)
			span <- adapt.span(length(iset)/adapt.factor)
		norm <- int.cor.loess(new,ref,iset,span=span,corrfac=corrfac)
	}
	else if (method=="rlm")
		norm <- int.cor.rlm(new,ref,iset,maxit=maxit)
	else if (method=="lm")
		norm <- int.cor.lm(new,ref,iset)
	else if (method=="simple")
		norm <- int.cor.simple(new,ref,iset)
	else if (method=="cluster")
		norm <- int.cor.cluster(new,ref,iset)

	return(norm)
}

int.cor.loess <- function(x,ref,iset,span=0.25,corrfac=2,extrap=TRUE)
{
	M <- x-ref
	m <- x[iset]-ref[iset]
	#a <- (ref[iset]+x[iset])/2
	a <- ref[iset]
	lc <- loess(m ~ a,data.frame(a=a,m=m),span=span,degree=1,family="symmetric")
	pred <- extrap1(lc$x,lc$fitted,xo=(ref+x)/2,extrap=extrap)
	# Correct for possible loess singularities
	singidx <- which(abs(pred)>quantile(abs(pred),0.9)*corrfac)
	pred[singidx] <- NA
	naidx <- which(is.na(pred))
	if (length(naidx)>0)
		pred[naidx] <- suppressWarnings(approx(na.omit(data.frame(ref,pred)),xout=pred[naidx],rule=2)$y)
	return((M-pred)+ref)
}

int.cor.rlm <- function(x,ref,iset,maxit=100)
{
	if (!require(MASS))
		stop("R package MASS is required!")
	#fit <- rlm(y ~ x, data=data.frame(x=ref[iset],y=x[iset]),maxit=maxit)
	fit <- rlm(y ~ x, data=data.frame(x=ref[iset],y=x[iset]-ref[iset]),maxit=maxit)
	#fit <- rlm(y ~ x, data=data.frame(x=(ref[iset]+x[iset])/2,y=x[iset]-ref[iset]),maxit=maxit)
	sig <- ifelse(fit$coefficients[2]>0,"+","")
	cat("\n","Intensity Normalization Curve: y = ",fit$coefficients[1],sig,fit$coefficients[2],"x","\n",sep="")
	#return(x-fit$coefficients[2])
	return((x-ref) - (fit$coefficients[1]+fit$coefficients[2]*ref) + ref)
	#return((x-ref) - (fit$coefficients[1]+fit$coefficients[2]*(ref+x)/2) + ref)
	# Because we are in the log scale, x-f is the same as dividing the original intensities
	# by a factor f or multiplying by 1/f
}

int.cor.lm <- function(x,ref,iset,maxit=100)
{
	#fit <- suppressWarnings(lsfit(ref[iset],x[iset]))
	fit <- suppressWarnings(lsfit(ref[iset],x[iset]-ref[iset]))
	sig <- ifelse(fit$coef[2]>0,"+","")
	cat("\n","Intensity Normalization Curve: y = ",fit$coef[1],sig,fit$coef[2],"x","\n",sep="")
	#return(x-fit$coef[2])
	return((x-ref) - (fit$coef[1]+fit$coef[2]*ref) + ref)
}

int.cor.simple <- function(x,ref,iset)
{
	#return(x-abs(mean(x[iset]-ref[iset])))
	cat("\n","Intensity Normalization Factor = ",mean((x[iset]-ref[iset])^2),"\n",sep="")
	return(x-mean((x[iset]-ref[iset])^2))
	# To have a comparable measurement with linear regression where the squared distances
	# are minimized and the slope is dependent on this fact, we calculate the average of
	# squared differences
}

# Does not work... The relationship is quite linear for this method to fix...
int.cor.cluster <- function(x,ref,iset)
{
	xn <- numeric(length(x))
	centers <- cbind(ref[iset],x[iset])
	dupe <- which(duplicated(centers))
	if (length(dupe)>0)
		centers <- centers[-dupe,]
	datamat <- cbind(ref,x)
	km <- kmeans(datamat,centers,iter.max=20)
	while(any(km$size==1))
	{
		singleton <- which(km$size==1)
		centers <- centers[-singleton,]
		km <- kmeans(datamat,centers,iter.max=20)
	}
	for (i in unique(km$cluster))
		xn[km$cluster==i] <- x[km$cluster==i] - abs((centers[i,2]-centers[i,1]))
	return(xn)
}

adapt.span <- function(rel.size)
{
	if (rel.size>0.9)
		return(0.1)
	else if (rel.size<0.1)
		return(0.9)
	else
		return(1-rel.size)
}

##########################################################################################
# LEGACY
##########################################################################################

# rtnew should be a vector only of matched to mass retention times, something like 
# rtnew <- rtnew[match.ref$new.idx]
# same for rtref which should be something like
# rtref <- rtref[match.ref$ref.idx]
# so in the end both vectors have the same length
# iset is the matched IS set indices which should be something like
is.retcor.old <- function(rtnew,rtref,iset,span=0.25,extrap=NA,method="spline",corrfac=2)
{
	rtdev <- rtnew-rtref
	y <- rtdev[iset]
	x <- rtref[iset]
	if (length(iset)<10)
	{
		warning("Not enough points for a local smoother... Will use a linear")
		if (length(iset<3))
			stop("Not enough points for retention time correction!")
		fit <- lsfit(x,y)            
		rtdevmo <- fit$coef[1] + rtnew*fit$coef[2]
		
		# Correct prediction outside time interval singularities
		rang <- range(rtnew)
		minidx <- rtnew < rang[1]
		maxidx <- rtnew > rang[2]
		rtdevmo[minidx] <- rtdevmo[head(which(!minidx),1)]
		rtdevmo[maxidx] <- rtdevmo[tail(which(!maxidx),1)]
	}
	else
	{
		lc <- loess(y ~ x,data.frame(x=x,y=y),span=span,degree=1,family="symmetric")
		rtdevmo <- extrap1(lc$x,lc$fitted,xo=rtref,method=method,extrap=extrap)
		
		# Correct for possible extrapolation singularities
		singidx <- which(abs(rtdevmo)>quantile(abs(rtdevmo),0.9)*corrfac)
		rtdevmo[singidx] <- NA
		naidx <- which(is.na(rtdevmo))
		if (length(naidx)>0)
			rtdevmo[naidx] <- suppressWarnings(approx(na.omit(data.frame(rtnew,rtdevmo)),xout=rtnew[naidx],rule=2)$y)
		dif <- diff(rtnew-rtdevmo)
		decidx <- which(dif<0)
		while (length(decidx)>0)
		{
			d <- dif[tail(decidx,1)]
			rtdevmo[tail(decidx,1)] <- rtdevmo[tail(decidx,1)]-d
			if (abs(d)<=1e-06)
				break;
			dif <- diff(rtnew-rtdevmo)
			decidx <- which(dif<0)
		}
	}

	return(list(rtcor=rtnew-rtdevmo,rtdevmo=rtdevmo))
}

is.intcor.old <- function(intnew,intref,iset,method="loess",span=0.75,it=2,fail=intref,corrfac=2,plotd=FALSE,...)
{
	its <- 1

	while (its<=it)
	{
		cat("\n---Intensity correction, iteration ",its,sep="")

		M <- intnew-intref
		A <- (intnew+intref)/2
		centers <- cbind(A[iset],M[iset])
		dupe <- which(duplicated(centers))
		if (length(dupe)>0)
			centers <- centers[-dupe,]
		datamat <- cbind(A,M)
		km <- kmeans(datamat,centers,iter.max=20)
		intcor <- numeric(length(intref))
		curv <- vector("list",length(unique(km$cluster)))
		for (i in unique(km$cluster))
		{
			r <- int.smooth.old(intnew[km$cluster==i],intref[km$cluster==i],span=span,fail=fail[km$cluster==i])
			intcor[km$cluster==i] <- r$intcor
			curv[[i]]$x <- r$intc$x
			curv[[i]]$y <- r$intc$y
		}

		if (plotd)
		{
			xx <- cbind(intref,intref)
			yy <- cbind(intnew,intcor)
			plot.intma(xx,yy,l=iset,lcl=curv,...)
		}
		
		intnew <- intcor
		its <- its+1
	}
	cat("\n")

	#return(rtcor)
	return(list(intcor=intcor,intc=curv))
}

int.smooth.old <- function(intnew,intref,span,fail=NULL,corrfac=2)
{
	m <- intnew-intref
	a <- (intnew+intref)/2
	if (!is.null(fail))
		fail <- m
	
	if (length(m)<2)
	{
		warning("Cannot correct with one point! Global median correction will be applied. Consider also lowering the number of iterations...",call.=FALSE)
		if (!is.null(fail))
			mmo <- m - median(fail)
		else
			stop("\nCannot correct with only one pair of points... Please provide the \"fail\" option...",call.=FALSE)
	}
	else if (length(m)<8)
	{
		warning(paste("Not enough points (",length(m),") for a local smoother... Will use a linear",sep=""),call.=FALSE)
		fit <- suppressWarnings(lsfit(a,m))
		mmo <- fit$coef[1] + a*fit$coef[2]
		
		# Correct prediction outside time interval singularities
		rang <- range(a)
		minidx <- a < rang[1]
		maxidx <- a > rang[2]
		mmo[minidx] <- mmo[head(which(!minidx),1)]
		mmo[maxidx] <- mmo[tail(which(!maxidx),1)]
	}
	else
	{
		lc <- loess(m ~ a,data.frame(a=a,m=m),span=span,degree=1,family="symmetric")
		mmo <- predict(lc,data.frame(a=a))
		# Correct for possible loess singularities
		singidx <- which(abs(mmo)>quantile(abs(mmo),0.9)*corrfac)
		mmo[singidx] <- NA
		naidx <- which(is.na(mmo))
		if (length(naidx)>0)
			mmo[naidx] <- suppressWarnings(approx(na.omit(data.frame(a,mmo)),xout=a[naidx],rule=2)$y)
	}

	return(list(intcor=intnew-mmo,intc=data.frame(x=intref,y=mmo)))
}

int.cor.rlm.old <- function(x,ref,iset,maxit=100)
{
	if (!require(MASS))
		stop("R package MASS is required!")
	fit <- rlm(yy ~ xx, data=data.frame(xx=(ref[iset]+x[iset])/2,yy=x[iset]-ref[iset]),maxit=maxit)
	diffmodel <- fit$coefficients[1] + fit$coefficients[2]*(x+ref)/2
	return((x-ref)-diffmodel+ref)
}
