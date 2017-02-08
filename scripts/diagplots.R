# This file will contain several diagnostic plots

# name : vector of names to be used
# y.lim : how to construct y-axis limits? "default", "auto" or a 2-length vector, defaults
# to "default" (internal R plotting system is taking care of this)
# output : output plotting device (pdf, png etc.), defaults to x11 and if "same", no device
#		   will be called so that user can plot many bopxlots using par externally. In this
#		   case the second matrix (tam, if given) will be disarded
# ... further arguments to boxplot
boxplot.mat <- function(mat,tam,name=NULL,log.it="auto",y.lim="default",output="x11",fil=NULL,...)
{
	# We have both normalized and un-normalized data?
	two <- TRUE
	if (missing(tam))
		two <- FALSE

	# Need to log?
	if (log.it=="auto")
	{
		if (diff(range(mat,na.rm=TRUE))>1000) # Data would rather be log scaled
		{
			mat <- log2disp(mat)
			# Check the same for tam
			if (two)
				if (diff(range(tam,na.rm=TRUE))>1000)
					tam <- log2disp(tam)
		}
	}
	else if (log.it=="yes")
	{
		mat <- log2disp(mat)
		if (two)
			tam <- log2disp(tam)
	}

	# Define the axis limits based on user input
	if (!is.numeric(y.lim) && y.lim=="auto")
	{
		if (two)
		{
			min.y <- floor(min(min(mat),min(tam)))
			max.y <- ceiling(max(max(mat),max(tam)))
		}
		else
		{
			min.y <- floor(min(mat))
			max.y <- ceiling(max(mat))
		}
	}
	else if (is.numeric(y.lim))
	{
		min.y <- y.lim[1]
		max.y <- y.lim[2]
	}
	
	if (is.null(name))
		nams <- paste("Sample",1:ncol(mat),sep=" ")
	else if (length(name)==1 && name=="none")
		nams <- rep("",ncol(mat))
	else
		nams <- name
	
	cols <- c("red","green","blue","yellow","aquamarine","orange","burlywood")
	mat.list <- list()
	for (i in 1:ncol(mat))
		mat.list[[i]] <- mat[,i]
	if (two)
	{
		tam.list <- list()
		for (i in 1:ncol(tam))
			tam.list[[i]] <- tam[,i]
	}

	if (output!="same") # So as to be able to use par(mfxxx) from outside
		openGraphics(output,fil)

	if (two)
		par(mfrow=c(1,2))
	if (!is.numeric(y.lim) && y.lim=="default")
	{
		boxplot(mat.list,names=nams,col=cols,las=2,...)
		if (two)
			boxplot(tam.list,names=nams,col=cols,las=2,...)
	}	
	else
	{
		boxplot(mat.list,names=nams,col=cols,ylim=c(min.y,max.y),las=2,...);
		if (two)
			boxplot(tam.list,names=nams,ylim=c(min.y,max.y),col=cols,las=2,...)
	}

	if (output!="same")
		closeGraphics(output)
}

# type : correlation calculation given to function cor
# effect : "simple" for a heatmap, "fancy" (default) for fancier graph
# output : output plotting device (pdf, png etc.), defaults to x11 and currently not used
# ... further arguments to corrplot of package corrplot (pretty cool package!) or
#	  or to levelplot of package lattice
image.cor <- function(mat,type="pearson",effect="fancy",output="x11",log.mat=FALSE,fil=NULL,...)
{
	if (!require(corrplot) && effect=="fancy")
		stop("R package corrplot is required!")
	if (!require(lattice) && effect=="simple")
		stop("R package lattice is required!")

	if (log.mat)
		cor.mat <- cor(log2(mat),method=type)
	else
		cor.mat <- cor(mat,method=type)

	openGraphics(output,fil)

	if (effect=="fancy")
		corrplot(cor.mat,...)
	else if (effect=="simple")
	{
		m <- cor.mat[,ncol(cor.mat):1]
		levelplot(m,col.regions=cm.colors(100)[100:1],at=seq(-1,1,length.out=100),...)
	}

	closeGraphics(output)
}

# type : correlation calculation given to function cor
# effect : "simple" for a simple pairwise scatterplot, "fancy" (default) for fancier one
#		   and "large" for a summarized color scatterplot pairs
# output : output plotting device (pdf, png etc.), defaults to x11 and currently not used
# ... further arguments to cpairs of package gclus or to pairs of package grDevices
scatter.cor <- function(mat,type="pearson",effect="fancy",output="x11",log.mat=FALSE,log.plot=FALSE,fil=NULL,...)
{
	# Package grDevices is basic
	if (!require(gclus) && effect=="fancy")
		stop("R package gclus is required!")
	if (!require(IDPmisc) && effect=="large")
		stop("R package IDPmisc is required!")

	if (log.mat)
		cor.mat <- cor(log2(mat),method=type)
	else
		cor.mat <- cor(mat,method=type)

	if (log.plot)
		plot.mat <- log2(mat)
	else
		plot.mat <- mat

	openGraphics(output,fil)
	
	if (effect=="fancy")
	{
		dta.col<-dmat.color(cor.mat)
		cpairs(plot.mat,panel.colors=dta.col,gap=0.5,pch=20)
	}
	else if (effect=="large")
		ipairs(plot.mat)
	else if (effect=="simple")
		pairs(plot.mat,col="blue",pch=20)

	closeGraphics(output)
}

# A function to quickly display points, line fit, ri set...
# Type can also be mean-difference
# iset contains the indices
norm.diag.plot <- function(x,y,lx=NULL,ly=NULL,iset,xn=NULL,yn=NULL,type="normal",log.it="auto",output="x11",fil=NULL,...)
{
	openGraphics(output,fil)
	
	# Normalized or both
	both <- FALSE
	if (!is.null(xn) && !is.null(yn))
	{
		both <- TRUE
		par(mfrow=c(1,2))
	}

	# Mean-difference
	if (type=="md")
	{
		xt <- (x+y)/2
		yt <- y-x
		lxt <- (lx+ly)/2
		lyt <- ly-lx
		x <- xt
		y <- yt
		lx <- lxt
		ly <- lyt

		if (both)
		{
			xnt <- (xn+yn)/2
			ynt <- yn-xn
			xn <- xnt
			yn <- ynt
		}
	}

	# Plot original points
	plot(x,y,pch=20,col="blue",cex=0.5,...)
	# Plot normalization curve
	lines(lx,ly,type="l",col="red",lwd=2)
	# Plot normalization set
	points(x[iset],y[iset],pch=20,col="green")
	# For the second part...
	grid()

	# Second part...
	if (both)
	{
		plot(xn,yn,pch=20,col="blue",cex=0.5,...)
		points(xn[iset],yn[iset],pch=20,col="green")
		grid()
	}

	closeGraphics(output)
}

norm.diag.pairs <- function(x,iset,l=NULL,log.it="NYI",output="x11",fil=NULL,...)
{
	n <- ncol(x)

	if (output!="same")
		openGraphics(output,fil)
	
	# Setup the grid
	par(mfrow=c(n,n),mar=c(1,1,1,1),oma=c(1,1,0,0),mgp=c(2,0.5,0),cex.axis=0.7,cex.lab=0.7)

	# Assign colors in case...
	if (is.list(iset))
		cols <- sample(colors(),length(iset))

	# Plot
	for (i in 1:n)
	{
		for (j in 1:n)
		{
			if (i==j && i!=n && j!=n)
			{
				plot(0:10,0:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="") # Diagonal
				text(c(5,5,5,5),c(7,5.5,3.5,2),
					 c("X-Y for",paste("(",j,",",n,") to (",n,",",n,")",sep=""),
					   "M-D for",paste("(",i,",",n,") to (",n,",",n,")",sep="")),cex=1.5,font=2)
				arrows(6,9,9,9,angle=25,length=0.15,lwd=1.5)
				arrows(1,4,1,1,angle=25,length=0.15,lwd=1.5)
			}
			else if (i<j) # XY plot
			{
				plot(x[,i],x[,j],pch=20,col="blue",cex=0.5,...)
				if (!is.null(l))
					lines(l[,i],l[,j],type="l",col="red")
				if (is.list(iset))
				{
					for (k in 1:length(iset))
						points(x[iset[[k]],i],x[iset[[k]],j],pch=20,cex=0.5,col=cols[k],...)
				}
				else
					points(x[iset,i],x[iset,j],pch=20,col="green",...)
				grid()
			}
			else if (i>j) # MD plot
			{
				plot((x[,i]+x[,j])/2,x[,j]-x[,i],pch=20,col="blue",cex=0.5,...)
				# Meaningless to plot the possible normalization curve, just the iset
				if (is.list(iset))
				{
					for (k in 1:length(iset))
						points((x[iset[[k]],i]+x[iset[[k]],j])/2,x[iset[[k]],j]-x[iset[[k]],i],
								pch=20,cex=0.5,col=cols[k],...)
				}
				else
					points((x[iset,i]+x[iset,j])/2,x[iset,j]-x[iset,i],pch=20,col="green",...)
				grid()
			}
			else if (i==n && j==n)
			{
				np <- nrow(x)
				ni <- length(iset) # Careful!
				plot(0:10,0:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="") # Free space!
				text(c(5,5),c(7,3),c(paste("#Points:",np),paste("#I.Set:",ni)),cex=1.5,font=2)
			}
		}
	}

	if (output!="same")
		closeGraphics(output)
}

# This should be used in the case of normalizing against a baseline and iset must be only
# a list
norm.diag.base <- function(x,b,iset,l=NULL,log.it="NYI",output="x11",fil=NULL,...)
{
	if (missing(b))
		stop("You must provide the baseline used for the normalization!")
	n <- ncol(x)
	if (length(iset)!=n)
			stop("iset list must have a length equal to the number of columns in x")

	openGraphics(output,fil)
	
	# Setup the grid
	grid.size <- findOptGrid(n)
	par(mfrow=c(grid.size[1],grid.size[2]),mar=c(1,1,1,1),oma=c(1,1,0,0),mgp=c(2,0.5,0),
		cex.axis=0.7,cex.lab=0.7)

	# Plot
	for (i in 1:n)
	{				
		plot(b,x[,i],pch=20,col="blue",cex=0.5,...)
		if (!is.null(l))
			lines(b,l[,i],type="l",col="red")
		points(b[iset[[i]]],x[iset[[i]],i],pch=20,col="green",...)
		grid()
	}

	closeGraphics(output)
}

# A function to hist the percentage of zeros to one or two matrices
# na : contains NA's instead of zeros?
hist.zeros <- function(X,Y=NULL,what=c("each","total"),na=FALSE,output="x11",fil=NULL)
{	
	what <- what[1]

	if (!is.element(what,c("each","total")))
		stop("what must be one of \"each\" or \"total\"")
	if (!is.null(Y))
		type <- "double"
	else
		type <- "single"

	if (na)
	{
		X[is.na(X)] <- 0
		if (!is.null(Y))
			Y[is.na(Y)] <- 0
	}

	openGraphics(output,fil)

	if (type=="single")
	{
		m <- nrow(X)
		n <- ncol(X)
		l <- length(X)
		if (what=="each")
		{
			zero.list <- list(n)
			zero.vec <- numeric(n)
			for (i in 1:n)
			{
				zero.list[[i]] <- which(X[,i]==0)
				zero.vec[i] <- 100*length(zero.list[[i]])/m
			}
			names(zero.list) <- names(zero.vec) <- colnames(X)
		}
		else if (what=="total")
			zero.vec <- 100*length(which(X==0))/l

		barplot(zero.vec,col="darkred",border="yellow",las=2,ylab="Percentage of 0's")
		grid(nx=NA,ny=NULL)
	}
	else if (type=="double")
	{
		mx <- nrow(X)
		nx <- ncol(X)
		lx <- length(X)
		my <- nrow(Y)
		ny <- ncol(Y)
		ly <- length(Y)

		if (what=="each")
		{
			zero.list.X <- list(nx)
			zero.vec.X <- numeric(nx)
			zero.list.Y <- list(ny)
			zero.vec.Y <- numeric(ny)
			for (i in 1:nx)
			{
				zero.list.X[[i]] <- which(X[,i]==0)
				zero.vec.X[i] <- 100*length(zero.list.X[[i]])/mx
			}
			names(zero.list.X) <- names(zero.vec.X) <- colnames(X)
			for (i in 1:ny)
			{
				zero.list.Y[[i]] <- which(Y[,i]==0)
				zero.vec.Y[i] <- 100*length(zero.list.Y[[i]])/my
			}
			names(zero.list.Y) <- names(zero.vec.Y) <- colnames(Y)

			par(mfrow=c(2,1))
			barplot(zero.vec.X,col="darkred",border="yellow",las=2,ylab="Percentage of 0's")
			barplot(zero.vec.Y,col="darkgreen",las=2,ylab="Percentage of 0's")
			grid(nx=NA,ny=NULL)
		}
		else if (what=="total")
		{
			zero.vec.X <- 100*length(which(X==0))/lx
			zero.vec.Y <- 100*length(which(Y==0))/ly
			barplot(c(zero.vec.X,zero.vec.Y),col="darkred",border="yellow",las=2,ylab="Percentage of 0's")
			grid(nx=NA,ny=NULL)
		}
	}

	closeGraphics(output)
}

plot.mzrt <- function(rt,mz,inten=NULL,iset=NULL,output="x11",fil=NULL,...)
{	
	if (!is.null(inten))
	{
		if (max(inten) > 107) # How possible it is to have intensity>1e+32
		{
			log.scaled <- FALSE
			inten[which(inten<1)] <- 1
			inten <- log2(inten)
		}
		else
			log.scaled <- TRUE

		ref <- inten/max(inten)
		# Jet colormap like MatLab and to hecadecimal for use with plot
		jet.fun <- colorRamp(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
		my.jet <- round(jet.fun(ref))
		class(my.jet) <- "hexmode"
		my.hex <- paste("#",apply(as.character(my.jet),1,paste,collapse=""),sep="")
	}
	else
	{
		log.scaled <- FALSE
		my.hex <- "orange"
	}

	if (output!="same") # So as to be able to use par(mfxxx) from outside
	{
		if (output %in% c("pdf","ps","x11"))
			openGraphics(output,fil,width=12,height=6)
		else
			openGraphics(output,fil,width=1200,height=600)
	}

	# Time limits to be able to determine 30 second tickmarks and 60 second gridlines
	xmin <- min(rt)-min(rt)%%60
	xmax <- max(rt)-max(rt)%%60+60
	at <- seq(xmin,xmax,by=30)
	gt <- seq(xmin-60,xmax+60,by=60)
	
	par(mar=c(4,4,4,4),oma=c(0.5,0.5,0,0),cex.lab=1.2,cex.axis=0.8,font.lab=2,bg="lightgrey")
	plot(rt,mz,pch=20,col=my.hex,main="Peak data",xlab="Retention Time (sec)",ylab="m/z",xaxt="n",yaxt="n",...)
	rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="darkblue")
	points(rt,mz,pch=20,col=my.hex,cex=0.5)
	axis(1,at=at,labels=at,las=2,cex.axis=0.7)
	axis(2,cex.axis=1)
	grid(nx=0,ny=NULL)
	abline(v=gt,lty=3,col="lightgrey")
	if (!is.null(iset))
		points(rt[iset],mz[iset],pch=15,col="red",cex=1)
	
	if (output!="same")
		closeGraphics(output)
}

# l : a set of indices
# lx, ly: set of points (e.g. a curve)
plot.rtdev <- function(x,y,l=NULL,lx=NULL,ly=NULL,lcl=NULL,exclude=NULL,log.it=FALSE,output="x11",fil=NULL,...)
{
	if (is.matrix(x) || is.data.frame(x)) # Assuming pairs of unnormalized-normalized values
	{
		if (!is.matrix(y) && !is.data.frame(y))
			stop("Both x and y must be matrices or data frames!")
		if (ncol(x)!=2 && ncol(y)!=2)
			stop("Both x and y must have two columns, 1st with raw times and 2nd with normalized!")
		two <- TRUE
	}
	else
		two <- FALSE
	
	if (is.null(lcl) && ((is.null(lx) && !is.null(ly)) || (is.null(ly) && !is.null(lx))))
	{
		warning("Both lx and ly must be provided! Ignoring...")
		lx <- ly <- NULL
	}
	if (!is.null(lcl) && (!is.null(lx) || !is.null(ly)))
	{
		warning("Only one of lcl or the (lx,ly) pair can be provided! Ignoring (lx,ly)...")
		lx <- ly <- NULL
	}

	if (log.it)
	{
		d <- log2(y/x)
		m <- log2(x)
	}
	else
	{
		d <- y-x
		m <- x
	}
	
	# Text and symbols for legend
	legt <- "All RT data"
	legc <- "blue"
	legp <- 20
	if (!is.null(l))
	{
		legt <- c(legt,"IS RT data")
		legc <- c(legc,"red")
		legp <- c(legp,20)
		legtt <- legt
		legcc <- legc
		legpp <- legp
	}
	if (!is.null(lcl) || !is.null(lx))
	{
		legt <- c(legt,"Normalization curve(s)")
		legc <- c(legc,"orange")
		legp <- c(legp,20)
	}

	if (output!="same") # So as to be able to use par(mfxxx) from outside
		openGraphics(output,fil)

	if (two)
	{
		par(mfrow=c(2,1),mar=c(3,3,3,3),oma=c(0.5,0.5,0,0),mgp=c(2,0.5,0),cex.axis=0.7,cex.lab=0.8,cex.main=0.9)
		# Plot un-normalized
		if (!is.null(exclude))
		{
			ylim=c(min(d[-exclude,1])-0.5,max(d[-exclude,1]+0.5))
			plot(m[,1],d[,1],pch=20,col="blue",cex=0.5,main="Retention time deviation - raw",
				 xlab="Reference RT (sec)",ylab="RT deviation from reference",ylim=ylim)
		}
		else
			plot(m[,1],d[,1],pch=20,col="blue",cex=0.5,main="Retention time deviation - raw",
				 xlab="Reference RT (sec)",ylab="RT deviation from reference")
		if (!is.null(l))
			points(m[l,1],d[l,1],pch=20,col="red",cex=0.8)
		if (!is.null(lx) && !is.null(ly))
		{
			if (is.unsorted(lx))
			{
				s <- sort(lx,index.return=TRUE)
				lx <- s$x
				ly <- ly[s$ix]
			}
			points(lx,ly,pch=20,col="orange",type="l",lwd=1.5)
		}
		else if (!is.null(lcl))
		{
			for (i in 1:length(lcl))
			{
				s <- sort(lcl[[i]]$x,index.return=TRUE)
				x <- s$x
				y <- lcl[[i]]$y[s$ix]
				#points(x,y,type="l",col="orange",lwd=1.2)
				points(x,y,pch=20,col="orange",cex=0.2)
			}
		}
		grid()
		legend(x="topright",legend=legt,col=legc,pch=legp,cex=0.7,bg="white")
		# Plot normalized
		if (!is.null(exclude))
		{
			ylim=c(min(d[-exclude,2])-0.5,max(d[-exclude,2]+0.5))
			plot(m[,2],d[,2],pch=20,col="blue",cex=0.5,main="Retention time deviation - normalized",
				 xlab="Reference RT (sec)",ylab="RT deviation from reference",ylim=ylim)
		}
		else
			plot(m[,2],d[,2],pch=20,col="blue",cex=0.5,main="Retention time deviation - normalized",
				 xlab="Reference RT (sec)",ylab="RT deviation from reference")
		if (!is.null(l))
			points(m[l,2],d[l,2],pch=20,col="red",cex=0.8)
		#if (!is.null(exclude))
		#	points(m[exclude,2],d[exclude,2],pch=20,col="darkgray",cex=0.8)
		grid()
		legend(x="topright",legend=legtt,col=legcc,pch=legpp,cex=0.7,bg="white")
	}
	else
	{
		if (!is.null(exclude))
		{
			ylim=c(min(d[-exclude,2])-0.5,max(d[-exclude,2]+0.5))
			plot(m,d,pch=20,col="blue",cex=0.5,ylim=ylim)
		}
		else
			plot(m,d,pch=20,col="blue",cex=0.5)
		if (!is.null(l))
			points(m[l],d[l],pch=20,col="red",cex=0.8)
		if (!is.null(lx) && !is.null(ly))
		{
			if (is.unsorted(lx))
			{
				s <- sort(lx,index.return=TRUE)
				lx <- s$x
				ly <- ly[s$ix]
			}
			points(lx,ly,pch=20,col="orange",type="l",lwd=1.5)
		}
		#if (!is.null(exclude))
		#	points(m[exclude],d[exclude],pch=20,col="darkgray",cex=0.8)
		grid()
	}

	if (output!="same")
		closeGraphics(output)
}

# l : a set of indices
# lx, ly: set of points (e.g. a curve)
plot.intma <- function(x,y,l=NULL,lx=NULL,ly=NULL,lcl=NULL,log.it=FALSE,output="x11",fil=NULL,...)
{
	if (is.matrix(x) || is.data.frame(x)) # Assuming pairs of unnormalized-normalized values
	{
		if (!is.matrix(y) && !is.data.frame(y))
			stop("Both x and y must be matrices or data frames!")
		if (ncol(x)!=2 && ncol(y)!=2)
			stop("Both x and y must have two columns, 1st with raw times and 2nd with normalized!")
		two <- TRUE
	}
	else
		two <- FALSE
	
	if (is.null(lcl) && ((is.null(lx) && !is.null(ly)) || (is.null(ly) && !is.null(lx))))
	{
		warning("Both lx and ly must be provided! Ignoring...")
		lx <- ly <- NULL
	}
	if (!is.null(lcl) && (!is.null(lx) || !is.null(ly)))
	{
		warning("Only one of lcl or the (lx,ly) pair can be provided! Ignoring (lx,ly)...")
		lx <- ly <- NULL
	}

	if (log.it)
	{
		m <- log2(y)-log2(x)
		a <- 0.5*(log2(x)+log2(y))
	}
	else
	{
		m <- y-x
		a <- (x+y)/2
	}
	
	# Text and symbols for legend
	legt <- "All intensity data"
	legc <- "blue"
	legp <- 20
	if (!is.null(l))
	{
		legt <- c(legt,"IS intensity data")
		legc <- c(legc,"red")
		legp <- c(legp,20)
		legtt <- legt
		legcc <- legc
		legpp <- legp
	}
	if (!is.null(lcl) || !is.null(lx))
	{
		legt <- c(legt,"Normalization curve(s)")
		legc <- c(legc,"orange")
		legp <- c(legp,20)
	}

	if (output!="same") # So as to be able to use par(mfxxx) from outside
		openGraphics(output,fil)

	if (two)
	{
		par(mfrow=c(2,1),mar=c(3,3,3,3),oma=c(0.5,0.5,0,0),mgp=c(2,0.5,0),cex.axis=0.7,cex.lab=0.8,cex.main=0.9)
		# Plot un-normalized
		plot(a[,1],m[,1],pch=20,col="blue",cex=0.5,main="Intensity mean-difference - raw",
			 xlab="Mean intensity (with reference)",ylab="Intensity difference")
		if (!is.null(l))
			points(a[l,1],m[l,1],pch=20,col="red",cex=0.8)
		if (!is.null(lx) && !is.null(ly))
		{
			if (is.unsorted(lx))
			{
				s <- sort(lx,index.return=TRUE)
				lx <- s$x
				ly <- ly[s$ix]
			}
			points(lx,ly,pch=20,col="orange",type="l",lwd=1.5)
		}
		else if (!is.null(lcl))
		{
			for (i in 1:length(lcl))
			{
				s <- sort(lcl[[i]]$x,index.return=TRUE)
				x <- s$x
				y <- lcl[[i]]$y[s$ix]
				#points(x,y,type="l",col="orange",lwd=1.2)
				points(x,y,pch=20,col="orange",cex=0.2)
			}
		}
		grid()
		legend(x="topright",legend=legt,col=legc,pch=legp,cex=0.7,bg="white")
		# Plot normalized
		plot(a[,2],m[,2],pch=20,col="blue",cex=0.5,main="Intensity mean-difference - normalized",
			 xlab="Mean intensity (with reference)",ylab="Intensity difference")
		if (!is.null(l))
			points(a[l,2],m[l,2],pch=20,col="red",cex=0.8)
		grid()
		legend(x="topright",legend=legtt,col=legcc,pch=legpp,cex=0.7,bg="white")
	}
	else
	{
		plot(a,m,pch=20,col="blue",cex=0.5)
		if (!is.null(l))
			points(a[l],m[l],pch=20,col="red",cex=0.8)
		if (!is.null(lx) && !is.null(ly))
		{
			if (is.unsorted(lx))
			{
				s <- sort(lx,index.return=TRUE)
				lx <- s$x
				ly <- ly[s$ix]
			}
			points(lx,ly,pch=20,col="orange",type="l",lwd=1.5)
		}
		grid()
	}

	if (output!="same")
		closeGraphics(output)
}

plot.match <- function(rtr,mzr,rtn,mzn,rto=NULL,mzo=NULL,output="x11",fil=NULL,...)
{
	if (!is.null(rto) && !is.null(mzo))
		two <- TRUE
	else
		two <- FALSE

	if (output!="same") # So as to be able to use par(mfxxx) from outside
	{	
		if (two)
		{
			if (output %in% c("pdf","ps","x11"))
				openGraphics(output,fil,width=12,height=12)
			else
				openGraphics(output,fil,width=1200,height=1200)
		}
		else
		{
			if (output %in% c("pdf","ps","x11"))
				openGraphics(output,fil,width=12,height=6)
			else
				openGraphics(output,fil,width=1200,height=600)
		}
	}

	# Time limits to be able to determine 60 second tickmarks and 120 second gridlines
	if (two)
	{
		xmin <- min(c(rtr,rtn,rto))-min(c(rtr,rtn,rto))%%60
		xmax <- max(c(rtr,rtn,rto))-max(c(rtr,rtn,rto))%%60+60
	}
	else
	{
		xmin <- min(c(rtr,rtn))-min(c(rtr,rtn))%%60
		xmax <- max(c(rtr,rtn))-max(c(rtr,rtn))%%60+60
	}
	at <- seq(xmin,xmax,by=60)
	gt <- seq(xmin-60,xmax+60,by=120)

	if (two)
	{
		par(mfrow=c(2,1),mar=c(3,3,3,3),oma=c(0.5,0.5,0,0),cex.lab=0.9,cex.main=1,font.lab=2,bg="lightgrey")
		# Before alignment
		plot(rtr,mzr,col="yellow",main="Peak data before alignment",xlab="Retention Time (sec)",ylab="m/z",xaxt="n",yaxt="n",...)
		rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="darkblue")
		points(rtr,mzr,col="yellow",cex=1)
		points(rto,mzo,col="red",pch=20,cex=0.7)
		axis(1,at=at,labels=at,las=2,cex.axis=0.7)
		axis(2,cex.axis=1)
		grid(nx=0,ny=NULL)
		abline(v=gt,lty=3,col="lightgrey")
		# After alignment
		plot(rtr,mzr,col="yellow",main="Peak data after alignment",xlab="Retention Time (sec)",ylab="m/z",xaxt="n",yaxt="n",...)
		rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="darkblue")
		points(rtr,mzr,col="yellow",cex=1)
		points(rtn,mzn,col="red",pch=20,cex=0.7)
		axis(1,at=at,labels=at,las=2,cex.axis=0.7)
		axis(2,cex.axis=0.9)
		grid(nx=0,ny=NULL)
		abline(v=gt,lty=3,col="lightgrey")
	}
	else
	{
		par(mar=c(4,4,4,4),oma=c(0.5,0.5,0,0),cex.lab=1.2,cex.axis=0.8,font.lab=2,bg="lightgrey")
		plot(rtr,mzr,col="yellow",main="Peak data",xlab="Retention Time (sec)",ylab="m/z",xaxt="n",yaxt="n",...)
		rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="darkblue")
		points(rtr,mzr,col="yellow",cex=1)
		points(rtn,mzn,col="red",pch=20,cex=0.7)
		axis(1,at=at,labels=at,las=2,cex.axis=0.7)
		axis(2,cex.axis=1)
		grid(nx=0,ny=NULL)
		abline(v=gt,lty=3,col="lightgrey")
	}
	
	if (output!="same")
		closeGraphics(output)
}

plot.rvn <- function(x,y,sx=NULL,sy=NULL,lim=NULL,output="x11",fil=NULL,...)
{
	main.xy <- "Standards X-Y plot"
	main.md <- "Standards M-D plot"
	m <- y-x
	a <- (x+y)/2
	if ((is.null(sx) & !is.null(sy)) || (!is.null(sx) & is.null(sy)))
	{
		warning("Only one of sx or sy has been provided! Ignoring (sx,sy)...")
		sx <- sy <- NULL
	}
	else if (!is.null(sx) & !is.null(sy))
	{
		sm <- sy-sx
		sa <- (sx+sy)/2
		main.xy <- "Data and Standards X-Y plot"
		main.md <- "Data and Standards M-D plot"
	}
	
	if (output!="same") # So as to be able to use par(mfxxx) from outside
	{
		if (output %in% c("pdf","ps","x11"))
			openGraphics(output,fil,width=8,height=4)
		else
			openGraphics(output,fil,width=800,height=400)
	}

	par(mfcol=c(1,2),mar=c(4,4,4,4),oma=c(1,1,0,0),pch=20,cex=0.8,cex.lab=1.3,cex.axis=1.2,cex.main=1.4,font.lab=2)
	plot(x,y,pch=20,col="darkgreen",xlab="Reference intensity (log2)",ylab="Raw intensity (log2)",main=main.xy)
	if (!is.null(sx)) # No need to check both, it has been done above
		points(sx,sy,pch=20,col="orange")
	#abline(lsfit(x,y),col="darkred",lwd=1.5,lty=2)
	abline(0,1,col="darkred",lwd=1.5,lty=2)
	grid()
	plot(a,m,pch=20,col="blue",xlab="0.5*(Reference + Raw) (log2)",ylab="Raw - Reference (log2)",main=main.md)
	if (!is.null(sx)) # No need to check both, it has been done above
		points(sa,sm,pch=20,col="red")
	if (!is.null(lim) & !is.null(sx))
		points(sa[abs(sm)>=lim],sm[abs(sm)>=lim],pch=20,col="darkgrey")
	else if (!is.null(lim) & is.null(sx))
		points(a[abs(m)>=lim],m[abs(m)>=lim],pch=20,col="darkgrey")
	grid()

	if (output!="same")
		closeGraphics(output)
}

log2disp <- function(mat)
{
	mat[mat==0] <- NA # Not for imputation but for display purposes
	return(log2(mat))
}	

findOptGrid <- function(n)
{
	m <- 0
	while (n > m*m)
		m <- m+1
	if (n < m*m)
	{
		k <- m-1
		if (n > m*k)
			k <- k+1
		else
		{
			while (n > m*k)
				k=k-1
		}
	}
	else
		k <- m

	return(c(m,k))
}

openGraphics <- function(o,f,...)
{
	if(!checkGraphicsType(o))
		stop("Invalid graphics output type!")
	if(checkGraphicsFile(o) && is.null(f))
		stop("Please specify an output file name for your plot")
	
	switch(o,
		x11 = { x11() },
		pdf = { pdf(file=f,pointsize=10,...) },
		ps = { postscript(file=f,pointsize=10,...) },
		png = { png(file=f,pointsize=12,...) },
		jpg = { jpeg(file=f,pointsize=12,quality=100,...) },
		bmp = { bmp(file=f,pointsize=12,...) },
		tiff = { tiff(file=f,pointsize=12,...) })
}

closeGraphics <- function(o)
{
	if (o!="x11")
		dev.off()
}

checkGraphicsType <- function(o)
{
	if (!is.element(o,c("x11","png","jpg","tiff","bmp","pdf","ps")))
		return(FALSE)
	else
		return(TRUE)
}

checkGraphicsFile <- function(o)
{
	if (is.element(o,c("png","jpg","tiff","bmp","pdf","ps")))
		return(TRUE)
	else
		return(FALSE)
}
