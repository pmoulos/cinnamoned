<?php
function initConfig()
{
	# Config
    $_SESSION['config']['base_path'] = "/media/HD3/cprocess/";
    $_SESSION['config']['source_path'] = "/var/www/cinnamoned/scripts/";

    # Location
    $_SESSION['session']['session_dir'] = date('dmYhms');
    $_SESSION['session']['project_path'] = $_SESSION['config']['base_path'].$_SESSION['session']['session_dir']."/";
    $_SESSION['session']['data_raw_path'] = $_SESSION['session']['project_path']."data/raw/";
    $_SESSION['session']['data_trunc_path'] = $_SESSION['session']['project_path']."data/trunc/";
    $_SESSION['session']['diagnostic_path'] = $_SESSION['session']['project_path']."diagnostic/";
    $_SESSION['session']['diagnostic_preprocess_path'] = $_SESSION['session']['diagnostic_path']."preprocess/";
    $_SESSION['session']['diagnostic_normalization_path'] = $_SESSION['session']['diagnostic_path']."normalization/";
    $_SESSION['session']['script_path'] = $_SESSION['session']['project_path']."scripts/";

    # Info
    $_SESSION['session']['project_name'] = "";
    $_SESSION['session']['yaml_file'] = "";
    $_SESSION['session']['class_file'] = "";
    $_SESSION['session']['rdata_peaks_file'] = "";
    $_SESSION['session']['rdata_norm_file'] = "";
    $_SESSION['session']['result_file'] = "";
	$_SESSION['session']['peak_detection_script'] = "";
	$_SESSION['session']['normalization_script'] = "";

    # Default run parameters
    $_SESSION['session']['xcms']['classdata'] = array();
	$_SESSION['session']['xcms']['filter']['do'] = "TRUE";
	$_SESSION['session']['xcms']['filter']['min'] = 300;
	$_SESSION['session']['xcms']['filter']['max'] = 3000;
	$_SESSION['session']['xcms']['read']['profstep'] = 1;
	$_SESSION['session']['xcms']['read']['profmethod'] = "binlin";
	$_SESSION['session']['xcms']['find']['snthresh'] = 7;
	$_SESSION['session']['xcms']['find']['step'] = 0.1;
	$_SESSION['session']['xcms']['find']['fwhm'] = 30;
	$_SESSION['session']['xcms']['find']['sigma'] = 7;
	$_SESSION['session']['xcms']['find']['steps'] = 3;
	$_SESSION['session']['xcms']['find']['max'] = 5;
	$_SESSION['session']['xcms']['find']['mzdiff'] = 0.8 - $_SESSION['session']['xcms']['find']['step']*$_SESSION['session']['xcms']['find']['steps'];
	$_SESSION['session']['norm']['method'] = "geom";
	$_SESSION['session']['norm']['normalize'] = "rlm";
	$_SESSION['session']['norm']['tol'] = 0.01;
	$_SESSION['session']['norm']['correctfor'] = "both";
	$_SESSION['session']['norm']['export'] = "none";
	$_SESSION['session']['norm']['diagplot'] = "TRUE";
	$_SESSION['session']['norm']['tspan'] = 0.75;
	$_SESSION['session']['norm']['it'] = 3;
	$_SESSION['session']['norm']['corrfac'] = 2;
	$_SESSION['session']['norm']['ispan'] = 0.75;
	$_SESSION['session']['norm']['cutrat'] = 2;
	$_SESSION['session']['norm']['cutq'] = 2;
	$_SESSION['session']['norm']['times'] = array();
}

function array_implode($glue,$separator,$array)
{
	if (!is_array($array)) { return($array); }
	$string = array();
	foreach ($array as $key => $val)
	{
		if (is_array($val))
		{
			$val = implode(",",$val);
		}
		$string[] = "{$key}{$glue}{$val}";
	}
	return implode($separator,$string);
}

function map_min_max($min,$max)
{
	return(array($min => $max));
}
?>
