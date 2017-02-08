# An R program to annotate the peaks found with xcms given a resulting xcmsSet object
# and several parameters controling mostly the tolerance in mass deviation for the isotope
# decomposition and the formula generation as well as the isotope cluster formation
#
# Usage: ann.peaks <- annotatePeaks(xset,
#									group="fwhm",
#									sigma=6,
#									perfwhm=0.6,
#									cor_eic_th=0.75,
#									maxiso=5,
#									mzabs.add=0.01,
#									mzabs.iso=0.001,
#									ppm=5,
#									find.adducts=FALSE,
#									polarity="positive",
#									rulefile=NULL,
#									filter.valid="valid",
#									filter.score=0.75,
#									peak.val="maxo",
#									filter.dbe=NULL,
#									fail.rules="internal",
#									run.par=TRUE,
#									write.output=NULL,
#									export.what="all")
#
# Input arguments:
#
# xset 		   : The input xcms object
# group 	   : CAMERA peak grouping to calculate isotope clusters, can be "fwhm", "corr"
# 		  		 or "both". It defaults to "both". If one of the other is given, the process
#				 is deviated (see Note) but faster
# iso.flow	   : The way to calculate isotopic distributions (using only FWHM grouping or
#				 both FWHM and corr) as the ordering becomes not relevant if the isotopes
#				 are calculated using only one method. Can be "fwhm" (default) or "both"
# sigma		   : sigma for the groupFWHM method, see CAMERA man page
# perfwhm	   : perfwhm for the groupFWHM method, see CAMERA man page
# cor_eic_th   : parameter for the groupCorr method, see CAMERA man page
# pval		   : p-value for EIC correlation, see CAMERA man page
# maxiso 	   : The number of maximum isotopes to fetch for a mass
# mzabs.add	   : Absolute mass difference for tolerance in adduct finding. Defaults to 0.01
# mzabs.fiso   : Absolute mass difference for tolerance in isotope finding. Defaults to 0.01
# mzabs.diso   : Absolute mass difference for tolerance in isotope decomposing for formula
#				 calculation. Defaults to 0.001
# ppm.fiso 	   : Same as mzabs.fiso, but in ppm and the two are added
# ppm.diso 	   : Same as mzabs.diso, but in ppm and the two are added
# polarity 	   : ESI polarity of MS machine. Defaults to "positive" and is only used if
#            	 group="corr" and/or find.adducts=TRUE
# rulefile 	   : File containing rules for adducts... If NULL, the default from package
#            	 CAMERA will be used
# peak.val	   : What value to use as intensity of peak when calculating isotopes. Defaults
#				 to peak maxima ("maxo"). This value is different from the value that is
#				 used to calculate the final peak intensity and is used only for the definition
#				 of isotope clusters
# filter.valid : Filter output formulas on validity of N-rule
# filter.score : Filter output formulas based on probability (see pub of "SIRIOUS")
# filter.dbe   : Filter on DBE rule, NYI
# write.output : Export output? If not NULL a valid filename should be given
# more.adducts : Try to find more adducts on the given dataset by also running groupCorr
#				 alone in the raw xcms objet without groupFWHM first? If yes, it should be
#				 set to TRUE, however, the result is not guaranteed and it takes REALLY a
#				 lot of time for large datasets
# fail.rules   : What to do if the rules file fails to be read. Defaults to "internal"
#				 for constructing and internal rule for M+nH adducts only (n up to 3) or
#				 NULL for using the default from CAMERA package
# run.par	   : Run in parallel what can be run in parallel
# export.what  : What other values to export apart from formulas.
#				 The following options are available:
#                	"peaks"      : Export peaks with the results of CAMERA, Rdisop
#                   "formulas"   : Attach proposed formulas
#                   "theor.mass" : Attach proposed formula mass
#                   "delta.ppm"  : Attach difference in ppm
#                   "all"        : Export everything
#                   Defaults to export.what="all"
# find.adducts : Use the findAdducts column of CAMERA to calculate ion additions. Defaults
# 				 to "TRUE", not used in the default method and used only for compatibility
#				 with annotatePeaks.old
#
# Author: Panagiotis Moulos (pmoulos@eie.gr)
# Version: 1.0
# Creation date: 21-07-2011 (dd-mm-yyyy)
# Last update: 18-10-2011 (dd-mm-yyyy)
#
# Note : The new annotation pipeline includes an iterative procedure containing the calling
#		 of each one of groupFWHM and groupCorr as well as both of them. In order not to
#		 deconvolute 3 times, since isotopes are been taken only from the groupFWHM output
#		 we change the annotation routine to include this procedure. So, if someone wishes
#		 to use only "corr" or "fwhm" for finding compounds, the routine will revert to the
#		 annotatePeaks.old function in the end of the file.
#
# TODO : Implement DBE filter
#

annotatePeaks <- function(xset,group="both",iso.flow="fwhm",sigma=6,perfwhm=0.6,pval=0.05,
						  cor_eic_th=0.75,find.adducts=TRUE,maxiso=5,mzabs.add=0.01,
						  mzabs.fiso=0.01,mzabs.diso=0.001,ppm.fiso=10,ppm.diso=5,
						  polarity="positive",rulefile=NULL,filter.valid="valid",filter.score=0.75,
						  peak.val="maxo",filter.dbe=NULL,write.output=NULL,run.par=TRUE,
						  more.adducts=FALSE,fail.rules="internal",export.what="all")
{
	# Library checking
	if (!require(xcms))
		stop("R package xcms is required!")
	if (!require(CAMERA))
		stop("R package CAMERA is required!")
	if (!require(Rdisop))
		stop("R package Rdisop is required!")

	# Some input checking
	if (!("fwhm" %in% group) && !("corr" %in% group) && !("both" %in% group))
		stop("group should be one of \"fwhm\", \"corr\" or \"both\"")
	if (!is.numeric(maxiso) || !is.numeric(mzabs.add) || !is.numeric(mzabs.diso) || !is.numeric(mzabs.fiso) ||
		!is.numeric(ppm.fiso) || !is.numeric(ppm.diso) || !is.numeric(filter.score) ||
		!is.numeric(sigma) || !is.numeric(perfwhm) || !is.numeric(cor_eic_th))
		stop("maxiso, mzabs.add, mzabs.fiso, mzabs.diso, ppm.fiso, ppm.diso, sigma, perfwhm and filter.score must be numeric")
	if (filter.score<0 || filter.score>1)
		stop("filter.score represents a probability so it must be between 0 and 1")

	# Issue a small warning regarding the more.adducts input variable
	if(more.adducts && nrow(xset@peaks)>3000)
	{
		wmsg <- paste("The number of peak group is rather large and groupCorr will be slow...",
					  "Consider setting more.adducts to FALSE...")
		warning(wmsg,call.=FALSE)
	}

	if (run.par)
	{
		if(!require(multicore))
		{
			disp("R package multicore is not installed, assuming there are at least 2 cores...")
			n.core <- 2
		}
		else
		{
			cores <- multicore:::detectCores()
			n.core <- ceiling(cores/2)
		}
	}		
	
	# If group not "both" revert to annotatePeaks.old
	if (group %in% c("fwhm","corr"))
	{
		peaklist <- annotatePeaks.old(xset,group=group,sigma=sigma,perfwhm=perfwhm,pval=pval,
									  cor_eic_th=cor_eic_th,find.adducts=find.adducts,
									  maxiso=maxiso,mzabs.add=mzabs.add,mzabs.iso=mzabs.iso,
									  ppm=ppm,polarity=polarity,rulefile=rulefile,
									  filter.valid=filter.valid,filter.score=filter.score,
									  peak.val=peak.val,filter.dbe=filter.dbe,export.what=export.what)
		return(peaklist)
	}

	# CAMERA stuff
	disp("Grouping peaks to define isotope clusters...")
	if (run.par)
		xsa <- my.xsAnnotate(xset,nSlaves=n.core,paral="snow")
	else
		xsa <- my.xsAnnotate(xset)
	#xsa <- ifelse(run.par,my.xsAnnotate(xset,nSlaves=n.core,paral="snow"),my.xsAnnotate(xset))
	xsa.fwhm <- groupFWHM(xsa,sigma=sigma,perfwhm=perfwhm)
	xsa.fwhm <- findIsotopes(xsa.fwhm,maxiso=maxiso,mzabs=mzabs.fiso,ppm=ppm.fiso)
	xsa.both <- groupCorr(xsa.fwhm,cor_eic_th=cor_eic_th,pval=pval,calcCaS=TRUE)
	if (more.adducts)
	{
		xsa.corr <- groupCorr(xsa,cor_eic_th=cor_eic_th,pval=pval,calcCaS=TRUE)
		#xsa.corr <- findIsotopes(xsa.corr,maxiso=maxiso,mzabs=mzabs.fiso,ppm=ppm.fiso)
	}
	
	disp("Finding adducts...")
	rul <- tryCatch(read.csv(rulefile), # If problems there are...
			error=function(e)
			{
				if (!is.null(fail.rules) && fail.rules=="internal")
				{
					disp("Problem with reading rulefile or not given... Using internal for M+nH ONLY!")
					rul <- rules.int()
				}
				else
				{
					disp("Problem with reading rulefile... Default from CAMERA will be used...")
					rul <- NULL
				}
			},finally="")
	disp("Finding adducts using both EIC correlation methods...")
	xsa.both <- findAdducts(xsa.both,mzabs=mzabs.add,polarity="positive",rules=rul)
	disp("Finding adducts using FWHM grouped only...")
	xsa.fwhm <- findAdducts(xsa.fwhm,mzabs=mzabs.add,polarity="positive",rules=rul)
	if (more.adducts)
	{
		disp("Finding adducts using EIC correlation only... It might take long, please wait...")
		xsa.corr <- findAdducts(xsa.corr,mzabs=mzabs.add,polarity="positive",rules=rul)
	}

	if (iso.flow=="both")
	{
		disp("Calculating new isotopic distribution...")
		xsa.both <- findIsotopes(xsa.both,maxiso=maxiso,mzabs=mzabs.fiso,ppm=ppm.fiso)
		peaklist <- getPeaklist(xsa.both)
		addu.fwhm <- getPeaklist(xsa.fwhm)$adduct
		addu.both <- peaklist$adduct
		disp("Getting isotope clusters...")
		isolist <- getIsotopeCluster(xsa.both,value=peak.val)
	}
	else if (iso.flow=="fwhm")
	{
		peaklist <- getPeaklist(xsa.fwhm)
		addu.fwhm <- peaklist$adduct
		addu.both <- getPeaklist(xsa.both)$adduct
		disp("Getting isotope clusters...")
		isolist <- getIsotopeCluster(xsa.fwhm,value=peak.val)
	}
	
	disp("Refining adducts...")
	all.adducts <- character(nrow(peaklist))
	# Get adducts from all calculated groupings
	in.both <- which(addu.both!="")
	in.fwhm <- which(addu.fwhm!="")
	add.fwhm <- setdiff(in.fwhm,in.both)
	all.adducts[in.both] <- peaklist$adduct[in.both]
	all.adducts[add.fwhm] <- addu.fwhm[add.fwhm]
	if (more.adducts)
	{
		addu.corr <- getPeaklist(xsa.corr)$adduct
		in.corr <- which(addu.corr!="")
		in.both.fwhm <- union(in.both,add.fwhm)
		all.adducts[add.corr] <- addu.corr[setdiff(in.corr,in.both.fwhm)]
	}

	# Replace with the maximized adducts
	peaklist$adduct <- all.adducts
	#peaklist$isotopes <- isotopes

	disp("Adding monoisotopic corrected masses from isotopes...")
	real.mass <- character(nrow(peaklist))
	isolist <- calcMono(isolist,type="ESI")
	monoiso.ind <- grep("\\[M\\]",peaklist$isotopes)
	#for.fill <- setdiff(grep("\\[M\\]",peaklist$isotopes),which(all.adducts!=""))
	for (i in monoiso.ind)
	{
		m <- regexpr("\\[\\d+\\]",peaklist$isotopes[i],perl=TRUE)
		iso.index <- as.numeric(substring(peaklist$isotopes[i],m+1,attr(m,"match.length")-1))
		real.mass[i] <- as.character(round(isolist[[iso.index]]$mass,digits=6))
	}

	disp("Adding monoisotopic corrected masses from calculated adducts...")
	for.fill <- setdiff(which(all.adducts!=""),grep("\\[M\\]",peaklist$isotopes))
	for (i in for.fill)
	{
		m <- gregexpr("(([0-9]+\\.[0-9]*)|(\\.[0-9]+))([eE][+-]?[0-9]+)?",peaklist$adduct[i],perl=TRUE)
		m <- m[[1]]
		tmp.mass <- numeric(length(m))
		at <- attr(m,"match.length")
		for (j in 1:length(m))
			tmp.mass[j] <- as.numeric(substring(peaklist$adduct[i],m[j],m[j]+at[j]-1))
		real.mass[i] <- paste(tmp.mass,collapse=", ")
	}

	# Clear the masses from the adducts
	nea <- which(peaklist$adduct!="")
	addu.t1 <- peaklist$adduct[nea]
	addu.t2 <- gsub("\\s?(([0-9]+\\.[0-9]*)|(\\.[0-9]+))([eE][+-]?[0-9]+)?","",addu.t1) # FIXME!
	peaklist$adduct[nea] <- gsub("\\s+",", ",addu.t2)

	# Rdisop stuff
	disp("Decomposing isotopes...")
	mol.list <- lapply(isolist,decompIso,ppm=ppm.diso,mzabs=mzabs.diso)
	disp("Filtering formulas...")
	filt.ind <- lapply(mol.list,filterMolecules,filter.valid,filter.score)

	disp("Refining formulas...")
	prop.formula <- theor.mass <- delta.ppm <- character(nrow(peaklist))
	for (i in 1:nrow(peaklist))
	{
		iso <- peaklist[i,"isotopes"]
		if (iso!="")
		{
			m <- regexpr("\\[\\d+\\]",iso,perl=TRUE)
			iso.index <- as.numeric(substring(iso,m+1,attr(m,"match.length")-1))
			if (length(filt.ind[[iso.index]])>0)
			{
				formulae <- correctCharge(mol.list[[iso.index]]$formula[filt.ind[[iso.index]]],
										  isolist[[iso.index]]$charge,"H")
				masses <- getMassAlt(formulae) # Apparently $exactmass was OK...
				prop.formula[i] <- paste(formulae,collapse=", ")
				theor.mass[i] <- paste(masses,collapse=", ")
				if (length(grep("\\[M\\]",iso))>0) # Monoisotopic peak
				{
					#delta.ppms <- to.ppm(peaklist[i,"mz"],getMassAlt(mol.list[[iso.index]]$formula[filt.ind[[iso.index]]]))
					delta.ppms <- to.ppm.char(real.mass[i],theor.mass[i])
					delta.ppms <- round(delta.ppms,digits=6)
					delta.ppm[i] <- paste(delta.ppms,collapse=", ")
					
				}
			}
		}
	}
	
	peaklist <- exportPeaks(export.what,write.output,peaklist,real.mass,prop.formula,theor.mass,delta.ppm)
	
	return(peaklist)
}

exportPeaks <- function(export.what,write.output,peaklist,real.mass,prop.formula,theor.mass,delta.ppm)
{
	if ("all" %in% export.what)
		peaklist <- cbind(peaklist,real.mass,prop.formula,theor.mass,delta.ppm)
	else
	{
		if ("real.mass" %in% export.what)
			peaklist <- cbind(peaklist,real.mass)
		if ("formulas" %in% export.what)
			peaklist <- cbind(peaklist,prop.formula)
		if ("theor.mass" %in% export.what)
			peaklist <- cbind(peaklist,theor.mass)
		if ("delta.ppm" %in% export.what)
			peaklist <- cbind(peaklist,delta.ppm)
	}
	# Order peak list according to compounds and mz
	peaklist <- peaklist[order(as.numeric(peaklist$pcgroup),peaklist$mz),]
	if (!is.null(write.output))
	{
		disp("Exporting...")
		write.table(peaklist,file=write.output,quote=FALSE,sep="\t",row.names=FALSE)
	}
	return(peaklist)
}

decompIso <- function(il,ppm,mzabs)
{
	if (is.vector(il$peaks))	
		il$peaks <- as.matrix(t(il$peaks))
    decomposeIsotopes(il$peaks[,1],il$peaks[,2],z=il$charge,ppm=ppm,mzabs=mzabs)
}

# n.rule "valid" for return valid, invalid or NULL for no filter
# score for >= score else NULL
filterMolecules <- function(mol,n.rule="valid",f.score=NULL)
{	
	form <- getFormula(mol)
	mass <- getMass(mol)
	score <- getScore(mol)
	valid <- getValid(mol)

	if (!is.null(n.rule))
	{
		n.rule <- paste(toupper(substring(n.rule,1,1)),substring(n.rule,2),sep="")
		ind.valid <- which(valid==n.rule)
	}
	else
		ind.valid <- 1:length(valid)
	if (!is.null(score))
		ind.score <- which(score>=f.score)
	else
		ind.score <- 1:length(score)

	return(intersect(ind.valid,ind.score))
}

correctCharge <- function(molecules,charge,element)
{
	# TODO: Some checking for element, e.g. must be in H, Na, O etc.
	corrected <- character(length(molecules))
	for (i in 1:length(molecules))
		corrected[i] <- mysubmol(molecules[i],element,charge)
	return(corrected)
}

# This function could work well only for [M+nH]+ adducts...
mysubmol <- function(form,elem,charge)
{
	# Parse formula
	m <- gregexpr("[a-zA-Z]\\d*",form,perl=TRUE)[[1]]
	l <- attr(m,"match.length")
	# Decompose
	atoms <- character(length(m))
	abund <- character(length(m))
	for (i in 1:length(m))
	{
		atoms[i] <- substring(form,m[i],m[i])
		abund[i] <- substring(form,m[i]+1,m[i]+l[i]-1)
	}
	abund <- as.numeric(abund)
	# If only one atom, it becomes NA
	abund[is.na(abund)] <- 1
	names(abund) <- atoms
	# Subtract charge IF they contain elem (no comments...)
	if (elem %in% atoms)
	{
		abund[elem] <- abund[elem]-charge
		if (any(abund==0))
		{
			ze <- which(abund==0)
			n.ze <- atoms[-ze]
			abund <- abund[-ze]
			abund <- as.character(abund)
			abund[abund=="1"] <- ""
			names(abund) <- n.ze
		}
		else
		{
			abund <- as.character(abund)
			abund[abund=="1"] <- ""
			names(abund) <- atoms
		}
	}
	# Recompose
	deconv <- character(length(abund))
	for (i in 1:length(abund))
		deconv[i] <- paste(names(abund[i]),abund[i],sep="")
	return(paste(deconv,collapse=""))
}

getMassAlt <- function(form)
{
	corrmass <- numeric(length(form))
	for (i in 1:length(form))
		corrmass[i] <- getMolecule(form[i])$exactmass
	return(corrmass)
}

# Only ESI implemented, the immediate interest for the Bruker machine of INSERM
calcMono <- function(iso,type=c("ESI","CI"))
{
	type <- toupper(type[1])
	switch(type,
		ESI = {
				the.proton.mass <- 1.007276;
				for (i in 1:length(iso))
					iso[[i]]$mass <- as.numeric(iso[[i]]$charge*(iso[[i]]$peaks[1,1]-the.proton.mass))
			},
		CI = { iso <- iso })

	return(iso)
}

to.ppm <- function(obs,ex) { return(abs((obs-ex)/ex*1e+6)) }

to.ppm.char <- function(obs,ex)
{
	# obs could be a character with multiple values
	obs.v <- strsplit(obs,", ")
	obs.v <- as.numeric(obs.v[[1]])
	ex.v <- strsplit(ex,", ")
	ex.v <- as.numeric(ex.v[[1]])
	return(abs((obs.v-ex.v)/ex.v*1e+6))
}

rules.int <- function()
{
	name <- c("[M+H]+","[M+2H]2+","[M+3H]3+","[2M+H]+","[2M+2H]2+","[2M+3H]3+",
			  "[3M+H]+","[3M+2H]2+","3M+3H]3+")
	nmol <- rep(1:3,each=3)
	charge <- rep(1:3,3)
	massdiff <- rep(c(1.007276,2.014552,3.021828),3)
	oidscore <- rep(1:3,3)
	quasi <- c(1,rep(0,8))
	ips <- c(1.00,0.75,0.75,0.50,0.50,0.50,0.25,0.25,0.25)

	rule <- as.data.frame(cbind(name,nmol,charge,massdiff,oidscore,quasi,ips))
	rule$nmol <- as.integer(rule$nmol)
	rule$charge <- as.integer(rule$charge)
	rule$massdiff <- as.numeric(rule$massdiff)
	rule$oidscore <- as.integer(rule$oidscore)
	rule$quasi <- as.integer(rule$quasi)
	rule$ips <- as.numeric(rule$ips)
	
	return(rule)
}

my.xsAnnotate <- function (xs = NULL, sample = NA, nSlaves = 1, paral = "snow") 
{
	if (is.null(xs))
		stop("no argument was given")
	else if (!class(xs) == "xcmsSet") 
		stop("xs is no xcmsSet object ")
		
	object <- new("xsAnnotate")
	if (length(sampnames(xs)) > 1)
	{
		if (!nrow(xs@groups) > 0)
			stop("First argument must be a xcmsSet with group information or contain only one sample.")

		if (is.null(sample) || is.na(sample))
			object@sample <- as.numeric(NA)
		else
		{
			if (sample == -1)
				object@sample = sample
			else if (length(xs@filepaths) < sample | sample < 1)
				stop("Parameter sample must be lower equal than number of samples and greater than 0.\n")
			else
				object@sample <- sample
		}
		object@groupInfo <- my.getPeaks_selection(xs)
	}
	else if (length(sampnames(xs)) == 1)
	{
		object@sample = 1
		object@groupInfo <- my.getPeaks_selection(xs)
	}
	else
		stop("Unknown error with a grouped xcmsSet")

	runParallel <- list()
	runParallel$enable <- 0
	if (nSlaves > 1)
	{
		if (paral=="Rmpi") 
		{
			if (require(rmpi, character.only = TRUE) && !is.null(nSlaves)) # Try Rmpi first
			{
				if (is.loaded("mpi_initialize"))
				{
					if (mpi.comm.size() > 0)
					{
						warning("There are already intialized mpi slaves on your machine.\nCamera will try to uses them!\n")
						runParallel$enable <- 1
						runParallel$mode <- paral
					}
					else
					{
						mpi.spawn.Rslaves(nslaves = nSlaves, needlog = FALSE)
						if (mpi.comm.size() > 1)
						{
							runParallel$enable <- 1
							runParallel$mode <- paral
						}
						else
							warning("Spawning of mpi slaves have failed. CAMERA will run without parallelization.\n")
					}
				}
				else
				{
					snow = "snow"
					if (try(require(snow, character.only = TRUE, quietly = TRUE)))
					{
						cat("Starting snow cluster with", nSlaves, "local sockets.\n")
						snowclust <- makeCluster(nSlaves, type = "SOCK")
						runParallel$enable <- 1
						runParallel$mode <- snow
						runParallel$cluster <- snowclust
					}
				}
			}
		}
		else if (paral=="snow")
		{
			snow = "snow"
			if (try(require(snow, character.only = TRUE, quietly = TRUE)))
			{
				cat("Starting snow cluster with", nSlaves, "local sockets.\n")
				snowclust <- makeCluster(nSlaves, type = "SOCK")
				runParallel$enable <- 1
				runParallel$mode <- snow
				runParallel$cluster <- snowclust
			}
		}
	}
	object@runParallel <- runParallel
	colnames(object@annoID) <- c("id", "grp_id", "rule_id")
	colnames(object@annoGrp) <- c("id", "mass", "ips", "psgrp")
	colnames(object@isoID) <- c("mpeak", "isopeak", "iso", "charge")
	object@xcmsSet <- xs
	return(object)
}

my.getPeaks_selection <- function(xs,method="medret",value="into")
{
	if (nrow(xs@groups) > 0)
	{
		groupmat <- groups(xs)
		ts <- data.frame(cbind(groupmat,groupval(xs, method=method, value=value)),row.names = NULL)
		cnames <- colnames(ts)
		if (cnames[1] == 'mzmed') cnames[1] <- 'mz' else stop ('Peak information ?!?')
		if (cnames[4] == 'rtmed') cnames[4] <- 'rt' else stop ('Peak information ?!?')
		colnames(ts) <- cnames
	}
	else if (length(sampnames(xs)) == 1)
		ts <- xs@peaks
	else stop ('First argument must be a xcmsSet with group information or contain only one sample.')
	return(as.matrix(ts))
}


##########################################################################################

annotatePeaks.old <- function(xset,group="fwhm",sigma=6, perfwhm=0.6,cor_eic_th=0.75,pval=0.05,
							  find.adducts=FALSE,maxiso=5,mzabs.add=0.01,mzabs.iso=0.001,ppm=5,
						      polarity="positive",rulefile=NULL,filter.valid="valid",filter.score=0.75,
						      peak.val="maxo",filter.dbe=NULL,export.what="all")
{
	# CAMERA stuff
	disp("Grouping peaks to define isotope clusters...")
	xsa <- xsAnnotate(xset)
	switch(group,
		fwhm = { xsa <- groupFWHM(xsa,sigma=sigma,perfwhm=perfwhm)
				 xsa <- findIsotopes(xsa,maxiso=maxiso)	},
		corr = { xsa <- groupCorr(xsa,cor_eic_th=cor_eic_th,pval=pval,calcCaS=TRUE)
				 xsa <- findIsotopes(xsa,maxiso=maxiso) },
		both = { xsa <- groupFWHM(xsa,sigma=sigma,perfwhm=perfwhm)
				 xsa <- findIsotopes(xsa,maxiso=maxiso)
				 xsa <- groupCorr(xsa,cor_eic_th=cor_eic_th,pval=pval,calcCaS=TRUE) })
	if (find.adducts)
	{
		disp("Finding adducts...")
		if (!is.null(rulefile))
			rul <- read.csv(rulefile)
		xsa <- findAdducts(xsa,mzabs=mzabs.add,polarity="positive",rules=rul)
	}
	peaklist <- getPeaklist(xsa)
	disp("Getting isotope clusters...")
	isolist <- getIsotopeCluster(xsa,value=peak.val)

	# Rdisop stuff
	disp("Decomposing isotopes...")
	mol.list <- lapply(isolist,decompIso,ppm=ppm,mzabs=mzabs.iso)
	disp("Filtering formulas...")
	filt.ind <- lapply(mol.list,filterMolecules,filter.valid,filter.score)

	disp("Reconstructing peak list...")
	prop.formula <- theor.mass <- character(nrow(peaklist))
	for (i in 1:dim(peaklist)[1])
	{
		iso <- peaklist[i,"isotopes"]
		if (iso!="")
		{
			m <- regexpr("\\[\\d+\\]",iso,perl=TRUE)
			iso.index <- as.numeric(substring(iso,m+1,attr(m,"match.length")-1))
			if (length(filt.ind[[iso.index]])>0)
			{
				formulae <- correctCharge(mol.list[[iso.index]]$formula[filt.ind[[iso.index]]],
										  isolist[[iso.index]]$charge,"H")
				masses <- getMassAlt(formulae) # Apparently $exactmass was OK...
				prop.formula[i] <- paste(formulae,collapse=" ,")
				theor.mass[i] <- paste(masses,collapse=" ,")
				if (length(grep("\\[M\\]",iso))>0) # Monoisotopic peak
				{
					delta.ppms <- to.ppm(peaklist[i,"mz"],getMassAlt(mol.list[[iso.index]]$formula[filt.ind[[iso.index]]]))
					delta.ppms <- round(delta.ppms,digits=6)
					delta.ppm[i] <- paste(delta.ppms,collapse=", ")
				}
			}
		}
	}
	if (find.adducts)
	{
		real.mass <- character(nrow(peaklist))
		for (i in 1:nrow(peaklist))
		{
			addu <- peaklist$adduct[i]
			if (addu!="")
			{
				m <- gregexpr("(([0-9]+\\.[0-9]*)|(\\.[0-9]+))([eE][+-]?[0-9]+)?",addu,perl=TRUE)
				m <- m[[1]]
				tmp.mass <- numeric(length(m))
				at <- attr(m,"match.length")
				for (j in 1:length(m))
					tmp.mass[j] <- as.numeric(substring(addu,m[j],m[j]+at[j]-1))
				real.mass[i] <- paste(tmp.mass,collapse=" ,")
			}
			
		}
	} else real.mass <- NULL
	theor.mass[which(theor.mass==0)] <- NA

	peaklist <- exportPeaks(export.what,write.output,peaklist,prop.formula,theor.mass,real.mass,delta.ppm)
	
	return(peaklist)
}

###################################### DEBUG #############################################

trackValues <- function(i,mz,alt.mass,real.mass,delta.ppms,mol.val,prop.formula,theor.mass)
{
	cat("i :",i,"\n")
	cat("mz :",mz,"\n")
	cat("th :",alt.mass,"\n")
	cat("rm :",real.mass,"\n")
	cat("dp :",delta.ppms,"\n")
	cat("fo :",mol.val,"\n")
	cat("fc :",prop.formula,"\n")
	cat("tm :",theor.mass,"\n")
	cat("\n")
}

#trackValues(i,peaklist[i,"mz"],getMassAlt(mol.list[[iso.index]]$formula[filt.ind[[iso.index]]]),
#			real.mass[i],delta.ppms,mol.list[[iso.index]]$formula[filt.ind[[iso.index]]],
#			prop.formula[i],theor.mass[i])
