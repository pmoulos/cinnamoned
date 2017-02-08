# An R file containing several functions for querying some metabolomic databases. Currently,
# HMDB, ChEBI and KEGG Compound. This might evolve in the future to be able to handle local
# databases (currently trying for METLIN)
#
# HMDB online querying based on work of Adam Woznica, UNIGE
#
# Author: Panagiotis Moulos (pmoulos@eie.gr)
# Version: 1.0
# Creation date: 27-07-2011 (dd-mm-yyyy)
# Last update: 21-10-2011 (dd-mm-yyyy)
#
# TODO : - METLIN
#

# Query KEGG compound database, given either one mass and a tolerance (in Da) to create a
# search interval, or two masses that will form an interval
# TODO: Somehow incorporate RT and MZ values in ID???
queryKEGG.mass <- function(m,m.tol=0.01,m.int=NULL)
{
	if (!require(KEGGSOAP))
		stop("R package KEGGSOAP is required!")

	if (missing(m) && is.null(m.int))
		stop("Either a specific m value or an m interval MUST be specified!")
	if (!missing(m) && !is.null(m.int))
	{
		warning("Only ONE of m or m.int can be specified! Proceeding with m.int")
		m <- NULL
	}
	if (!is.null(m.int) && length(m.int)!=2)
		stop("m.int must be a vector of length 2")

	if (missing(m) && !is.null(m.int))
	{
		m <- mean(m.int)
		m.tol <- m.int[2] - m
	}

	# Search KEGG for compounds
	disp(paste("Querying KEGG COMPOUND for m between",m-m.tol,"and",m+m.tol,"...",sep=" "))
	comps <- search.compounds.by.mass(m,m.tol)
	if (length(comps)==0)
	{
		cat("No results found for m between",m-m.tol,"and",m+m.tol,"in KEGG COMPOUND.\n",sep=" ")
		return("")
	}
	else # We have to bget every compound and parse the result to get the formula
	{
		form <- sapply(comps,strip.kegg.formula.name)
		form <- as.data.frame(t(form),stringsAsFactors=FALSE)
		names(form) <- c("id","formula","name")
		return(form)
		#return(paste(form,collapse=", "))
	}
}

# Query ChEBI database, given either one mass and a tolerance (in Da) to create a
# search interval, or two masses that will form an interval
# dbdata : username and password for the database
queryChEBI.mass <- function(m,m.tol=0.01,m.int=NULL,dbdata=NULL)
{
	if (!require(RMySQL))
		stop("R package RMySQL is required!")

	if (missing(m) && is.null(m.int))
		stop("Either a specific m value or an m interval MUST be specified!")
	if (!missing(m) && !is.null(m.int))
	{
		warning("Only ONE of m or m.int can be specified! Proceeding with m.int")
		m <- NULL
	}
	if (!is.null(m.int) && length(m.int)!=2)
		stop("m.int must be a vector of length 2")
	if (is.null(dbdata) || length(dbdata)!=2)
		stop("dbdata must be a vector of length 2, in order to connect to the DB")

	if (missing(m) && !is.null(m.int))
	{
		m <- mean(m.int)
		m.tol <- m.int[2] - m
	}

	# Search the local copy of ChEBI for compounds
	disp(paste("Querying ChEBI for m between",m-m.tol,"and",m+m.tol,"...",sep=" "))
	
	con <- dbConnect(MySQL(),user=dbdata[1], password=dbdata[2], dbname="ChEBI", host="localhost")
	
	mass.query <- paste("SELECT chemical_data, compound_id FROM chemical_data WHERE type='MASS' AND (chemical_data>",
					   m-m.tol," AND chemical_data<",m+m.tol,")",sep="")
	mass.comp <- dbGetQuery(con,mass.query)

	if (nrow(mass.comp)==0)
	{
		cat("No results found for mass between",m-m.tol,"and",m+m.tol,"in ChEBI.\n",sep=" ")
		dbDisconnect(con)
		return("")
	}
	else # Charge correction is not required... Average mass is recorded, independently of charge
	{
		# Query for the formulas
		form <- character(nrow(mass.comp))
		compound_ids <- paste(mass.comp$compound_id,collapse=",")
		formula.query <- paste("SELECT chemical_data.compound_id,chemical_data,name FROM chemical_data ",
							   "INNER JOIN names ON chemical_data.compound_id=names.compound_id ",
							   "WHERE chemical_data.type='FORMULA' ",
							   "AND names.type LIKE '%NAME' ",
							   "AND chemical_data.compound_id IN (",compound_ids,") ",
							   "GROUP BY names.compound_id",sep="")
		form <- dbGetQuery(con,formula.query)
		dbDisconnect(con)

		# It's still possible that a compound_id does not correspond to a formula
		if (nrow(form)==0)
		{
			cat("No results found for mass between",m-m.tol,"and",m+m.tol,"in ChEBI.\n",sep=" ")
			dbDisconnect(con)
			return("")
		} else { return(form) }
	}
}

# Query a local copy of the HMDB database, created using the raw metabocards file provided
# by the HMDB website, given either one mass and a tolerance (in Da) to create a search
# interval, or two masses that will form an interval
# dbdata : username and password for the database
# mono : search weight_mono or weight_average
# bioflu : Add a biofluid filter e.g. Urine
# exact : If TRUE when used with bioflu then only hits appearing only in bioflu and not
# 		  other bioflus are returned
queryHMDB_Compact.mass <- function(m,m.tol=0.01,m.int=NULL,dbdata=NULL,mono=TRUE,bioflu=NULL,exact=FALSE)
{
	if (!require(RMySQL))
		stop("R package RMySQL is required!")

	if (missing(m) && is.null(m.int))
		stop("Either a specific mass value or a mass interval MUST be specified!")
	if (!missing(m) && !is.null(m.int))
	{
		warning("Only ONE of m or m.int can be specified! Proceeding with m.int")
		m <- NULL
	}
	if (!is.null(m.int) && length(m.int)!=2)
		stop("m.int must be a vector of length 2")
	if (is.null(dbdata) || length(dbdata)!=2)
		stop("dbdata must be a vector of length 2, in order to connect to the DB")

	if (missing(m) && !is.null(m.int))
	{
		m <- mean(m.int)
		m.tol <- m.int[2] - m
	}

	# Search the compact local copy of HMDB for compounds
	disp(paste("Querying HMDB for m between",m-m.tol,"and",m+m.tol,"...",sep=" "))
	con <- dbConnect(MySQL(),user=dbdata[1],password=dbdata[2],dbname="HMDB_Compact",host="localhost")

	w.s <- ifelse(mono,"mono","average")
	if (!is.null(bioflu))
	{
		if (exact)
			bioflu.like <- "LIKE"
		else
			bioflu.like <- "RLIKE"

		form.query <- paste("SELECT hmdb_id,chemical_formula,name FROM HMDB_Essential WHERE weight_",w.s,">",
							m-m.tol," AND weight_",w.s,"<",m+m.tol," AND biofluid_location ",
							bioflu.like," '",bioflu,"'",sep="")
	}
	else
		form.query <- paste("SELECT hmdb_id,chemical_formula,name FROM HMDB_Essential WHERE weight_",w.s,">",
							m-m.tol," AND weight_",w.s,"<",m+m.tol,sep="")
	form <- dbGetQuery(con,form.query)
	dbDisconnect(con)

	if (length(form)==0)
	{
		cat("No results found for m between",m-m.tol,"and",m+m.tol,"in HMDB_Compact.\n",sep=" ")
		return("")
	}
	else
	{
		#form <- as.character(form[,1])
		#return(paste(form,collapse=", "))
		return(form)
	}
}

# Query the HMDB database, given either one mass and a tolerance (in Da) to create a search
# interval, or two masses that will form an interval
# TODO: Somehow incorporate RT and MZ values in ID???
queryHMDB.mass <- function(m,m.tol=0.01,m.int=NULL)
{
	if (!require(scrapeR))
		stop("R package scrapeR is required!")

	if (missing(m) && is.null(m.int))
		stop("Either a specific mass value or a mass interval MUST be specified!")
	if (!missing(m) && !is.null(m.int))
	{
		warning("Only ONE of m or m.int can be specified! Proceeding with m.int")
		m <- NULL
	}
	if (!is.null(m.int) && length(m.int)!=2)
		stop("m.int must be a vector of length 2")

	if (!missing(m) && is.null(m.int))
		m <- c(m-m.tol,m+m.tol)
	else
		m <- m.int

	# HMDB query URL
	url <- "http://www.hmdb.ca/search/chemquery/run"
	# Query the HMDB database
	disp(paste("Querying HMDB for mass between",m[1],"and",m[2],"...",sep=" "))
	hmdb.page <- postForm(url,"query_from"=as.character(m[1]),"query_to"=as.character(m[2]),"search"="molecular_weight")
	s.p <- scrape(object="hmdb.page")
	
	hmdb <- tryCatch(paste(sub("\n\t\n",xpathSApply(s.p[[1]],"//table//tr/td[@rowspan=\"2\"]",xmlValue)),collapse=", "),
		error=function(e)
		{
			cat("No results found for mass between",m[1],"and",m[2],"in HMDB online.\n",sep=" ")
		},finally="")
	return(hmdb)
}

# Query the METLIN database, given either one mass and a tolerance (in Da) to create a search
# interval, or two masses that will form an interval
# TODO: FIX IT!!! It's not working... Only template...
queryMETLIN.mass <- function(m,m.tol=0.01,m.int=NULL)
{
	if (!require(scrapeR))
		stop("R package scrapeR is required!")

	if (missing(m) && is.null(m.int))
		stop("Either a specific mass value or a mass interval MUST be specified!")
	if (!missing(m) && !is.null(m.int))
	{
		warning("Only ONE of m or m.int can be specified! Proceeding with m.int")
		m <- NULL
	}
	if (!is.null(m.int) && length(m.int)!=2)
		stop("m.int must be a vector of length 2")

	if (missing(m) && !is.null(m.int))
	{
		m <- mean(m.int)
		m.tol <- m.int[2] - m
	}

	# HMDB query URL
	url <- "http://metlin.scripps.edu/metabo_list_simple.php"
	# Query the METLIN database
	disp(paste("Querying METLIN for m between",m-m.tol,"and",m+m.tol,"...",sep=" "))
	metlin.page <- postForm(url,"mass_mid"=as.character(m),"mass_tol"=as.character(m.tol),"mass_mode"="0")
	assign("metlin.page",metlin.page)
	scr.page <- scrape(object="metlin.page")

	metlin <- tryCatch(paste(xpathSApply(scr.page[[1]],"//table/tr/td[1]/b/a",xmlValue),collapse=", "),
				error=function(e)
				{
					cat("No results found for mass between",m-m.tol,"and",m+m.tol,"in METLIN online.\n",sep=" ")
				},finally="")
	return(metlin)
}

getRefData <- function(type="geom",dbdata=NULL)
{
	if (!require(RMySQL))
		stop("R package RMySQL is required!")
	type <- tolower(type)
	if (type!="geom" && type!="rlm" && type!="both")
		stop("type must be one of \"geom\", \"rlm\" or \"both\"")
	if (is.null(dbdata) || length(dbdata)!=2)
		stop("dbdata must be a vector of length 2, in order to connect to the DB")

	con <- dbConnect(MySQL(),user=dbdata[1],password=dbdata[2],dbname="RFLab_MetaboDB",host="localhost")
	query <- paste("SELECT id,mz,rt,is_",type,",summarized_intensity_",type," FROM peak_info ORDER BY mz",sep="")
	dbd <- dbGetQuery(con,query)
	dbDisconnect(con)
	return(dbd)
}

getISData <- function(type="geom",dbdata=NULL)
{
	if (!require(RMySQL))
		stop("R package RMySQL is required!")
	type <- tolower(type)
	if (type!="geom" && type!="rlm" && type!="both")
		stop("type must be one of \"geom\", \"rlm\" or \"both\"")
	if (is.null(dbdata) || length(dbdata)!=2)
		stop("dbdata must be a vector of length 2, in order to connect to the DB")

	con <- dbConnect(MySQL(),user=dbdata[1],password=dbdata[2],dbname="RFLab_MetaboDB",host="localhost")
	query <- paste("SELECT id,mz,rt,summarized_intensity_",type," FROM peak_info WHERE is_",type,"=1 ORDER BY mz",sep="")
	dbd <- dbGetQuery(con,query)
	dbDisconnect(con)
	return(dbd)
}

strip.kegg.formula.name <- function(comp)
{
	entry <- strsplit(bget(comp),"\n")
	ind.form <- grep("FORMULA",entry[[1]])
	ind.name <- grep("NAME",entry[[1]])
	form <- gsub("^\\s+|\\s+$","",substr(entry[[1]][ind.form],8,nchar(entry[[1]][ind.form])))
	if (ind.form-ind.name>1) # Multiple names
	{
		names.dirty <- paste(entry[[1]][ind.name:(ind.form-1)],collapse="")
		names.clear <- gsub("^\\s+|\\s+$|\\s+","",substr(names.dirty,5,nchar(names.dirty)))
	}
	else # One name
		names.clear <- gsub("^\\s+|\\s+$|\\s+","",substr(entry[[1]][ind.name],5,nchar(entry[[1]][ind.name])))

	if (length(ind.form)>0 || length(ind.name)>0)
		return(c(comp,form,names.clear))
	else
		return(NA)
}

getdbdata <- function()
{
	out <- tryCatch({ d <- read.table("db.dat",colClasses="character")
					  as.character(c(d[1,],d[2,])) },
					 error=function(e) { c("bWV0YWJvX25vYm9keQ==","eWRvYm9uX29iYXRlbQ==") },
					 finally="")
	return(out)
}

disp <- function(msg)
{
    cat(paste(msg,"\n",sep=""))
    flush.console()
}

## Snippet for mass correction given a charge... not needed in ChEBI after all
#the.proton.mass <- 1.007276
#charge <- numeric(dim(mass.comp)[1])
#for (i in 1:length(mass.comp$compound_id))
#{
#	charge.query = paste("SELECT chemical_data FROM chemical_data WHERE type='CHARGE' AND compound_id=",
#						 mass.comp$compound_id[i],sep="")
#	charge[i] <- as.numeric(dbGetQuery(con,charge.query))
#	if (charge[i]>0)
#		mass.comp$chemical_data[i] <- charge[i]*(as.numeric(mass.comp$chemical_data[i]) - the.proton.mass)
#	else if (charge[i]<0)
#		mass.comp$chemical_data[i] <- abs(charge[i])*(as.numeric(mass.comp$chemical_data[i]) + the.proton.mass)
#}
#mass.comp <- cbind(mass.comp,charge)
## Recheck the masses now
#ind <- which(as.numeric(mass.comp$chemical_data)<m-m.tol || as.numeric(mass.comp$chemical_data)>m+m.tol)
#if (length(ind)!=0)
#	mass.comp <- mass.comp[-ind,]
