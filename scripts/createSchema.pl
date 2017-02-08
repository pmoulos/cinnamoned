#!/usr/bin/perl -w

# createSchema.pl
# A Perl script to create the MySQL schema for the CinnamonDB
#
# Author      : Panagiotis Moulos (pmoulos@eie.gr)
# Created     : 29 - 03 - 2012 (dd - mm - yyyy)
# Last Update : 14 - 11 - 2012 (dd - mm - yyyy)
# Version     : 1.0

use strict;
use Getopt::Long;
use DBI;

# Make sure output is unbuffered
select(STDOUT);
$|=1;

# Set defaults
our $scriptname = "createSchema.pl";
our @dbdata;	  # Database name, username and password for the DB to avoid hardcoding
our $silent = 0;  # Display verbose messages
our $help = 0;    # Help?

# Check inputs
&checkInputs;

# Do the job
disp("Creating $dbdata[0] schema...");
&createDB();
disp("$dbdata[0] schema created!\n");


# Process inputs
sub checkInputs
{
    my $stop;
    GetOptions("dbdata|d=s{,}" => \@dbdata,
    		   "silent|s" => \$silent,
    		   "help|h" => \$help);
    # Check if the required arguments are set
    if ($help)
    {
    	&programUsage;
    	exit;
    }
    $stop .= "--- --dbdata should be consisted of three strings! ---\n"
		if (@dbdata && $#dbdata+1 != 3);
    if ($stop)
    {
            print "\n$stop\n";
            print "Type perl $scriptname --help for help in usage.\n\n";
            exit;
    }
}

sub createDB
{
	my $crq = "CREATE DATABASE `$dbdata[0]` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;";
	my $drq = "DROP DATABASE `$dbdata[0]`";
	my $conn = DBI->connect("dbi:mysql:;host=localhost;port=3306",$dbdata[1],$dbdata[2]);
	
	if (&checkExistence($dbdata[0])) # Drop if already exists
	{
		$conn->do($drq);
		$conn->do($crq);
	}
	else { $conn->do($crq); }
	&closeConnection($conn);
	$conn = undef;

	# Create the tables now
	$conn = &openConnection();

	my $ri_cq = "CREATE TABLE `run_info` (
				`run_id` VARCHAR(14) NOT NULL,
				`date` TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP,
				`project_name` VARCHAR(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL,
				`project_path` VARCHAR(50) NOT NULL,
				`diagnostic_preprocess_path` VARCHAR(100) NULL,
				`diagnostic_normalization_path` VARCHAR(100) NULL,
				`yaml_file` VARCHAR(100) NULL,
				`class_file` VARCHAR(100) NULL,
				`rdata_peaks_file` VARCHAR(100) NULL,
				`rdata_norm_file` VARCHAR(100) NULL,
				`result_file` VARCHAR(100) NULL,
				`peak_detection_script` VARCHAR(100) NULL,
				`normalization_script` VARCHAR(100) NULL,
				PRIMARY KEY (`run_id`)
				) ENGINE = INNODB;";

	my $rp_cq = "CREATE TABLE `run_parameters` (
				`id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
				`ref_run_id` VARCHAR(14) NOT NULL,
				`xcms_filter_do` TINYINT(1) UNSIGNED NULL,
				`xcms_filter_min` SMALLINT(5) UNSIGNED NULL,
				`xcms_filter_max`  SMALLINT(8) UNSIGNED NULL,
				`xcms_read_profstep` DOUBLE(6,3) NULL,
				`xcms_read_profmethod` VARCHAR(12) NULL,
				`xcms_find_snthresh` DOUBLE(6,3) NULL,
				`xcms_find_step` DOUBLE(9,6) NULL,
				`xcms_find_fwhm` DOUBLE(9,3) NULL,
				`xcms_find_sigma` DOUBLE(9,3) NULL,
				`xcms_find_steps` TINYINT(2) UNSIGNED NULL,
				`xcms_find_max` TINYINT(2) UNSIGNED NULL,
				`xcms_find_mzdiff` DOUBLE(9,6) NULL,
				`norm_method`  VARCHAR(5) NULL,
				`norm_tol` DOUBLE(9,6) NULL,
				`norm_correctfor` VARCHAR(12) NULL,
				`norm_export` VARCHAR(12) NULL,
				`norm_diagplot` TINYINT(1) UNSIGNED NULL,
				`norm_tspan` DOUBLE(9,6) NULL,
				`norm_tit` TINYINT(3) UNSIGNED NULL,
				`norm_corrfac` DOUBLE(6,3) NULL,
				`norm_ispan` DOUBLE(9,6) NULL,
				`norm_normalize` VARCHAR(12) NULL,
				`norm_cutrat` DOUBLE(6,3) NULL,
				`norm_cutq` DOUBLE(6,3) NULL,
				`norm_times` VARCHAR(100) NULL,
				FOREIGN KEY (`ref_run_id`) REFERENCES run_info(`run_id`)
				ON DELETE CASCADE ON UPDATE CASCADE
				) ENGINE = INNODB;";
	
	$conn->do($ri_cq);
	$conn->do($rp_cq);
	
	&closeConnection($conn);
}

sub checkExistensce
{
	my $dbcheck = shift @_;
	my $out = 1;
	
	my $conn = DBI->connect("dbi:mysql:database=information_schema;host=localhost;port=3306",$dbdata[1],$dbdata[2]);
	my $query = "SELECT `SCHEMA_NAME` FROM `SCHEMATA` WHERE `SCHEMA_NAME` = \"$dbcheck\"";
	my $sth = $conn->prepare($query);
	$sth->execute();
	$out = 0 if (!$sth->rows());
	$sth->finish();
	&closeConnection($conn);
	
	return($out);
}

sub openConnection
{   
    my $hostname = "localhost";
    my $database = $dbdata[0];
    
    my $conn = DBI->connect("dbi:mysql:database=$database;host=$hostname;port=3306",$dbdata[1],$dbdata[2]);
    
    return $conn;
}

sub closeConnection
{ 
    my $conn = shift @_;
    $conn->disconnect();
}

sub disp
{
	print "\n@_" if (!$silent);
}

sub programUsage 
{
	# The look sucks here but it is actually good in the command line
	my $usagetext = << "END";
	
$scriptname
Create the CinnamonDB database schema.

Author : Panagiotis Moulos (pmoulos\@eie.gr)

Main usage
$scriptname --dbdata username password [OPTIONS]

--- Required ---
  --dbdata|b		Connection data for the local database. It should
			be a vector of length two containing a username and
			a password.
--- Optional ---
  --silent|s		Use this option if you want to turn informative 
  			messages off.
  --help|h		Display this help text.
	
The main output of the program is a database or split metabocards.

END
	print $usagetext;
	exit;
}
