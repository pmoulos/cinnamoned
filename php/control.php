<?php

session_start();
include_once('spyc.php');
include('utils.php');
include('connection.php');
include('queries.php');

if ($_REQUEST['sid'])
{
	echo(json_encode(session_id()));
}

if ($_REQUEST['preprocess'])
{
	// Get data, fill the rest of the preprocess array to construct YAML matrix
	$project_name = $_REQUEST['project'];
	$preprocess = $_REQUEST['preprocess'];
	$preprocess = json_decode($preprocess,$assoc=TRUE);
	$preprocess = fillPreprocess($preprocess);
	$yaml_file = writeYAML($_SESSION['session']['script_path'],$preprocess);
	updateSessionXCMS($preprocess);

	// Check what is going on with class data
	if (empty($_SESSION['session']['class_file']))
	{
		$classdata = $_REQUEST['classdata'];
		$classdata = json_decode($classdata,$assoc=TRUE);
		$info_file = writeClassFile($_SESSION['session']['project_path'],$classdata);
	}
	else
	{
		$classdata = array();
		$info_file = $_SESSION['session']['class_file'];
		$fh = fopen($info_file,"r");
		while (!feof($fh))
		{
			$line = fgets($fh);
			$tmp = explode("\t",trim($line));
			$classdata[$tmp[0]] = $tmp[1];
		}
		fclose($fh);
	}
	updateSessionClassData($classdata,$info_file);

	// Now write first R script to perform peak detection and give back its directory
	$detect_file = writeDetectionScript($_SESSION['session']['script_path'],$info_file,$yaml_file);
	echo json_encode($_SESSION['session']['script_path']);

	updateSessionInfo(array("project_name" => $project_name, "yaml_file" => $yaml_file,
							"peak_detection_script" => $detect_file));
}

if ($_REQUEST['run_xcms'])
{
	// ...and run it!
	$command = "R CMD BATCH --vanilla ".$_SESSION['session']['script_path']."xcms.R ".$_SESSION['session']['script_path']."xcms.Rout";
	exec($command);
	updateSessionInfo(array("rdata_peaks_file" => $_SESSION['session']['project_path']."peaks.RData"));
}

if ($_REQUEST['normalization'])
{
	$normalization = $_REQUEST['normalization'];
	$normalization = json_decode($normalization,$assoc=TRUE);
	updateSessionNorm($normalization);

	// Now write second R script to perform normalization and give back its directory
	$norm_file = writeNormalizationScript($_SESSION['session']['script_path'],$normalization);
	echo json_encode($_SESSION['session']['script_path']);

	updateSessionInfo(array("normalization_script" => $norm_file));
}

if ($_REQUEST['run_norm'])
{
	// ...and run it!
	$command = "R CMD BATCH --vanilla ".$_SESSION['session']['script_path']."norm.R ".$_SESSION['session']['script_path']."norm.Rout";
	exec($command);
	if (file_exists($_SESSION['session']['project_path']."norm.RData"))
	{
		updateSessionInfo(array("rdata_norm_file" => $_SESSION['session']['project_path']."norm.RData"));
	}
	if (file_exists($_SESSION['session']['project_path']."norm_output.txt"))
	{
		updateSessionInfo(array("result_file" => $_SESSION['session']['project_path']."norm_output.txt"));
	}
}

if ($_REQUEST['results'])
{
	$result_source_file = $_SESSION['session']['result_file'];
	$result_name = basename($result_source_file);
	//$result_target_path = $_SERVER['DOCUMENT_ROOT']."/tmp/".$_SESSION['session']['session_dir']."/";
	$result_target_path = $_SERVER['DOCUMENT_ROOT']."/cinnamoned/tmp/".$_SESSION['session']['session_dir']."/";
	$result_target_file = $result_target_path.$result_name;

	if (!file_exists($result_target_path)) { mkdir($result_target_path,$mode=0777,$recursive=TRUE); }
	copy($result_source_file,$result_target_file);
	$result_target_file = str_replace($_SERVER['DOCUMENT_ROOT'],"",$result_target_file);

	echo json_encode(array("location" => $result_target_file, "prid" => $_SESSION['session']['session_dir']));
}

if ($_REQUEST['rt_diagnostic'])
{
	$diagpics = array();
	$picnames = array();
	$classdata = $_SESSION['session']['xcms']['classdata'];
	//$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/tmp/".$_SESSION['session']['session_dir']."/xcms/";
	$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/cinnamoned/tmp/".$_SESSION['session']['session_dir']."/xcms/";
	if (!file_exists($tmp_pic_path)) { mkdir($tmp_pic_path,$mode=0777,$recursive=TRUE); }
	$contents = scandir($_SESSION['session']['diagnostic_preprocess_path']);
	if (!empty($contents)) { $contents = array_slice($contents,2); }
	foreach ($contents as $img)
	{
		$source_file = $_SESSION['session']['diagnostic_preprocess_path'].$img;
		$target_file = $tmp_pic_path.$img;
		copy($source_file,$target_file);
	}
	$contents = scandir($tmp_pic_path);
	if (!empty($contents)) { $contents = array_slice($contents,2); }
	$new_pic_path = str_replace($_SERVER['DOCUMENT_ROOT'],"",$tmp_pic_path);
	foreach ($contents as $img)
	{
		$diagpics[] = $new_pic_path.$img;
		$picnames[] = str_replace(".png","",$img)." - ".$classdata[str_replace(".png",".cdf",$img)];
	}
	echo json_encode(array("path" => $diagpics, "name" => $picnames, "prid" => $_SESSION['session']['session_dir']));
}

if ($_REQUEST['time_filter'])
{
	$times = $_REQUEST['time_filter'];
	$times = json_decode($times,$assoc=TRUE);
	updateSessionTimeFilter($times);
}

if ($_REQUEST['norm_diagnostic'])
{
	$matches = getMatchPcts($_SESSION['session']['script_path'],$_SESSION['session']['rdata_norm_file']);

	$diagpics = array("alignment" => array(), "deviation" => array(), "boxplot" => array(), "other" => array());
	$picnames = array();
	$classdata = $_SESSION['session']['xcms']['classdata'];
	//$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/tmp/".$_SESSION['session']['session_dir']."/norm/";
	$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/cinnamoned/tmp/".$_SESSION['session']['session_dir']."/norm/";
	if (!file_exists($tmp_pic_path)) { mkdir($tmp_pic_path,$mode=0777,$recursive=TRUE); }
	$contents = scandir($_SESSION['session']['diagnostic_normalization_path']);
	if (!empty($contents)) { $contents = array_slice($contents,2); }
	foreach ($contents as $img)
	{
		$source_file = $_SESSION['session']['diagnostic_normalization_path'].$img;
		$target_file = $tmp_pic_path.$img;
		copy($source_file,$target_file);
	}
	$contents = scandir($tmp_pic_path);
	if (!empty($contents)) { $contents = array_slice($contents,2); }
	$new_pic_path = str_replace($_SERVER['DOCUMENT_ROOT'],"",$tmp_pic_path);
	foreach ($contents as $img)
	{
		$tmp_name = str_replace(".png","",$img);
		if (preg_match('/ALIGNMENT/',$img))
		{
			$repl = str_replace("_ALIGNMENT.png","",$img);
			$picnames[] = $repl." - ".$classdata[str_replace("_ALIGNMENT.png",".cdf",$img)].
							"<br/>".$matches[$repl]["total"]."% of new m/z matching with reference". 
							"<br/>".$matches[$repl]["is"]."% of new sample IS matching with reference".
							"<br/>".$matches[$repl]["is_rt"]."% of new sample IS used for RT correction".
							"<br/>".$matches[$repl]["is_inten"]."% of new sample IS use for intensity normalization"; # Once
			#$picnames[] = str_replace("_ALIGNMENT.png","",$img)." - ".$classdata[str_replace("_ALIGNMENT.png",".cdf",$img)].
			#				" - ".$matches[str_replace("_ALIGNMENT.png","",$img)]."% of new m/z matching with reference"; # Once
			$diagpics['alignment'][] = $new_pic_path.$img;
		}
		else if (preg_match('/DEVIATION/',$img))
		{
			$diagpics['deviation'][] = $new_pic_path.$img;
		}
		else if (preg_match('/STANDARDS/',$img))
		{
			$diagpics['standards'][] = $new_pic_path.$img;
		}
		else if (preg_match('/RAWINT/',$img))
		{
			$diagpics['rawint'][] = $new_pic_path.$img;
		}
		else if (preg_match('/NORMINT/',$img))
		{
			$diagpics['normint'][] = $new_pic_path.$img;
		}
		else if (preg_match('/BOXPLOT/',$img))
		{
			$diagpics['boxplot'][] = $new_pic_path.$img;
		}
		else
		{
			$diagpics['other'][] = $new_pic_path.$img;
		}
	}
	echo json_encode(array("path" => $diagpics, "name" => $picnames, "prid" => $_SESSION['session']['session_dir']));
}

if ($_REQUEST['finish'])
{
	# All the database writing will be done here
	$runOK = dbWriteRunInfo();
	$parOK = dbWriteRunParameters();
	$parOK = TRUE;
	$resOK = TRUE;

	$allOK = (!$parOK || !$resOK) ? FALSE : TRUE;
	$exit = $allOK ? 0 : 1;

	echo json_encode(array("exit" => $exit));
}

if ($_REQUEST['discard'])
{
	discardRun();
}


function writeYAML($path,$params)
{
	if (!file_exists($path)) { mkdir($path,$mode=0777,$recursive=TRUE); }
	$filename = $path."xcms.yml";
	$yaml = Spyc::YAMLDump($params);
	$fh = fopen($filename,"wb");
	fwrite($fh,$yaml);
	fclose($fh);
	return($filename);
}

function writeClassFile($path,$classdata)
{
	if (!file_exists($path)) { mkdir($path,$mode=0777,$recursive=TRUE); }
	$filename = $path."sample_info.txt";
	$fh = fopen($filename,"wb");
	fwrite($fh,"Filename\tClass\n");
	foreach ($classdata as $f => $c)
	{
		fwrite($fh,$f."\t".$c."\n");
	}
	fclose($fh);
	return($filename);
}

function writeDetectionScript($path,$info_file,$yaml_file)
{
	$script_path = $_SESSION['config']['source_path'];
	$path_to_project = $_SESSION['session']['project_path'];
	$path_to_raw = substr($_SESSION['session']['data_raw_path'],0,-1);
	$path_to_trunc = substr($_SESSION['session']['data_trunc_path'],0,-1);
	$path_to_plot = substr($_SESSION['session']['diagnostic_preprocess_path'],0,-1);

	$args = array
	(
		"path.to.raw" => "\"".$path_to_raw."\"",
		"info.file" => "\"".$info_file."\"",
		"param.file" => "\"".$yaml_file."\"",
		"path.to.trunc" => "\"".$path_to_trunc."\"",
		"annotate" => "FALSE",
		"persample" => "TRUE",
		"multicore" => "TRUE",
		"plotspec" => "\"".$path_to_plot."\"",
		"plottype" => "\"png\""
	);
	
	$script =
	"source(\"".$script_path."sourceAll.R\")\n".
	"peaks <- xcmsPipeline(".array_implode("=",",",$args).")\n".
	"peaks <- peaks\$peaks\n".
	"save(peaks,file=\"".$path_to_project."peaks.RData\")\n";

	if (!file_exists($path)) { mkdir($path,$mode=0777,$recursive=TRUE); }
	if (!file_exists($path_to_trunc)) { mkdir($path_to_trunc,$mode=0777,$recursive=TRUE); }
	$filename = $path."xcms.R";
	$fh = fopen($filename,"wb");
	fwrite($fh,$script);
	fclose($fh);
	return($filename);
}

function writeNormalizationScript($path,$params)
{
	$script_path = $_SESSION['config']['source_path'];
	$path_to_project = $_SESSION['session']['project_path'];
	$path_to_plot = substr($_SESSION['session']['diagnostic_normalization_path'],0,-1);
	$rdata_file = $_SESSION['session']['rdata_peaks_file'];
	$times = $_SESSION['session']['norm']['times'];

	$time_range = array();
	$tmp = array_map("map_min_max",$times['low'],$times['high']);
	foreach ($tmp as $index => $minmax)
	{
		$time_range[] = "c(".array_implode(",",",",$minmax).")";
	}
	$time_range = "list(".implode(",",$time_range).")";
	
	$args = array
	(
		//"peaks" => "peaks\$peaks",
		"peaks" => "peaks",
		"method" => "\"".$params['method']."\"",
		"normalize" => "\"".$params['normalize']."\"",
		"correctfor" => "\"".$params['correctfor']."\"",
		"time.range" => $time_range,
		"tol" => $params['tol'],
		"tspan" => $params['tspan'],
		"ispan" => $params['ispan'],
		"tit" => $params['it'],
		"cutq" => $params['cutq'],
		"corrfac" => $params['corrfac'],
		"cutrat" => $params['cutrat'],
		"export" => "\"".$path_to_project."norm_output.txt\"",
		"diagplot" => $params['diagplot'] == "TRUE" ? "\"".$path_to_plot."\"" : "NULL",
		"plottype" => "\"png\"",
		"export.type" => "\"".$params['export']."\""
	);
	
	$script =
	"source(\"".$script_path."sourceAll.R\")\n".
	"load(\"".$rdata_file."\")\n".
	"norm <- normalizeSamples(".array_implode("=",",",$args).")\n".
	"save(norm,file=\"".$path_to_project."norm.RData\")\n";

	if (!file_exists($path)) { mkdir($path,$mode=0777,$recursive=TRUE); }
	$filename = $path."norm.R";
	$fh = fopen($filename,"wb");
	fwrite($fh,$script);
	fclose($fh);
	return($filename);
}
	
function fillPreprocess($preprocess)
{
	$preprocess['find']['ppm'] = 25;
	$preprocess['find']['peakwidth'] = array(10,40);
	$preprocess['find']['prefilter'] = array(3,100);
	$preprocess['find']['mzCenterFun'] = "wMean";
	$preprocess['find']['integrate'] = 1;
	$preprocess['find']['mzdiff.cw'] = -0.001;
	$preprocess['find']['fitgauss'] = "FALSE";
	$preprocess['find']['scanrange'] = "";
	$preprocess['find']['noise'] = 0;
	$preprocess['find']['verbose.columns'] = "FALSE";
	$preprocess['find']['sleep'] = 0;
	$preprocess['group'] = array
	(
		"method" => array("density","mzClust","nearest"),
		"bw" => array(50,30,20,10,10),
		"mzwid" => 0.25,		
		"max" => 50,
		"mzppm" => 20,
		"mzabs" => 0,
		"mzVsRTbalance" => 10,
		"mzCheck" => 0.2,
		"rtCheck" => 15,
		"kNN" => 10,
		"minfrac" => 0.5,
		"minsamp" => 1,
		"sleep" => 0
	);
	$preprocess['retcor'] = array
	(
		"method" => array("loess","obiwarp"),
		"missing" => 1,
		"extra" => 1,
		"smooth" => "loess",
		"span" => 0.5,
		"family" => "symmetric",
		"profStep" => 1,
		"center" => "NULL",
		"response" => 1,
		"score" => "cor",
		"gapInit" => 0,
		"gapExtend" => 0,
		"factorDiag" => 2,
		"factorGap" => 1,
		"localAlignment" => 0,
		"initPenalty" => 0,
		"plottype" => "none",
		"col" => "NULL",
		"ty" => "NULL"
	);
	$preprocess['extract'] = array
	(
		"method" => "maxint",
		"value" => "into",
		"intensity" => "into"
	);
	$preprocess['annotate'] = array
	(
		"group" => "both",
		"iso.flow" => "fwhm",
		"sigma" => 6,
		"perfwhm" => 0.6,
		"pval" => 0.05,
		"cor_eic_th" => 0.75,
		"find.adducts" => "TRUE",
		"maxiso" => 5,
		"mzabs.add" => 0.01,
		"mzabs.fiso" => 0.01,
		"mzabs.diso" => 0.001,
		"ppm.fiso" => 10,
		"ppm.diso" => 5,
		"polarity" => "positive",
		"rulefile" => "NULL",
		"filter.valid" => "valid",
		"filter.score" => 0.75,
		"peak.val" => "maxo",
		"filter.dbe" => "NULL",
		"write.output" => "NULL",
		"run.par" => "TRUE",
		"more.adducts" => "FALSE",
		"fail.rules" => "internal",
		"export.what" => "all"
	);
	return($preprocess);
}

function updateSessionXCMS($preprocess)
{
	foreach ($preprocess as $outerkey => $array)
	{
		foreach ($array as $innerkey => $value)
		{
			$_SESSION['session']['xcms'][$outerkey][$innerkey] = $value;
		}
	}
}

function updateSessionClassData($classdata,$file)
{
	$_SESSION['session']['xcms']['classdata'] = $classdata;
	$_SESSION['session']['class_file'] = $file;
}

function updateSessionInfo($info)
{
	foreach ($info as $key => $value)
	{
		$_SESSION['session'][$key] = $value;
	}
}

function updateSessionTimeFilter($times)
{
	$_SESSION['session']['norm']['times'] = $times;
}

function updateSessionNorm($norm)
{
	foreach ($norm as $key => $value)
	{
		$_SESSION['session']['norm'][$key] = $value;
	}
}

function getMatchPcts($path,$rfile)
{
	$matches = array();
	$path_to_project = $_SESSION['session']['project_path'];
	$script = "load(\"".$rfile."\")\n".
			  "p <- as.data.frame(norm\$pct)\n".
			  "p[,1] <- as.character(p[,1])\n".
			  "p[,2] <- as.character(p[,2])\n".
			  "p[,3] <- as.character(p[,3])\n".
			  "p[,4] <- as.character(p[,4])\n".
			  "write.table(p,file=\"".$path_to_project."pcts.txt\",quote=FALSE,sep=\"\t\",row.names=TRUE,col.names=FALSE)\n";
	if (!file_exists($path)) { mkdir($path,$mode=0777,$recursive=TRUE); }
	$filename = $path."pcts.R";
	$fh = fopen($filename,"wb");
	fwrite($fh,$script);
	fclose($fh);

	$command = "R CMD BATCH --vanilla ".$path."pcts.R ".$path."pcts.Rout";
	exec($command);

	$fh = fopen($path_to_project."pcts.txt","r");
	while (!feof($fh))
	{
		$line = fgets($fh);
		$tmp = explode("\t",trim($line));
		$matches[$tmp[0]] = array("total" => $tmp[1],"is" => $tmp[2],"is_rt" => $tmp[3],"is_inten" => $tmp[4]);
	}
	fclose($fh);

	return($matches);
}

function dbWriteRunParameters()
{
	$conn = open_connection();
	$data = array();
	
	$time_range = array();
	$times = $_SESSION['session']['norm']['times'];
	$tmp = array_map("map_min_max",$times['low'],$times['high']);
	foreach ($tmp as $index => $minmax)
	{
		$time_range[] = "c(".array_implode(",",",",$minmax).")";
	}
	$time_range = implode(",",$time_range);
	
	$data['`ref_run_id`'] = '\''.mysql_real_escape_string($_SESSION['session']['session_dir']).'\'';
	$data['`xcms_filter_do`'] = $_SESSION['session']['xcms']['filter']['do'] == "TRUE" ? 1 : 0;
	$data['`xcms_filter_min`'] = $_SESSION['session']['xcms']['filter']['min'];
	$data['`xcms_filter_max`'] = $_SESSION['session']['xcms']['filter']['max'];
	$data['`xcms_read_profstep`'] = $_SESSION['session']['xcms']['read']['profstep'];
	$data['`xcms_read_profmethod`'] = '\''.mysql_real_escape_string($_SESSION['session']['xcms']['read']['profmethod']).'\'';
	$data['`xcms_find_snthresh`'] = $_SESSION['session']['xcms']['find']['snthresh'];
	$data['`xcms_find_step`'] = $_SESSION['session']['xcms']['find']['step'];
	$data['`xcms_find_fwhm`'] = $_SESSION['session']['xcms']['find']['fwhm'];
	$data['`xcms_find_sigma`'] = $_SESSION['session']['xcms']['find']['sigma'];
	$data['`xcms_find_steps`'] = $_SESSION['session']['xcms']['find']['steps'];
	$data['`xcms_find_max`'] = $_SESSION['session']['xcms']['find']['max'];
	$data['`xcms_find_mzdiff`'] = $_SESSION['session']['xcms']['find']['mzdiff'];
	$data['`norm_method`'] = '\''.mysql_real_escape_string($_SESSION['session']['norm']['method']).'\'';
	$data['`norm_tol`'] = $_SESSION['session']['norm']['tol'];
	$data['`norm_correctfor`'] = '\''.mysql_real_escape_string($_SESSION['session']['norm']['correctfor']).'\'';
	$data['`norm_export`'] = '\''.mysql_real_escape_string($_SESSION['session']['norm']['export']).'\'';
	$data['`norm_diagplot`'] = $_SESSION['session']['norm']['diagplot'] == "TRUE" ? 1 : 0;
	$data['`norm_tspan`'] = $_SESSION['session']['norm']['tspan'];
	$data['`norm_tit`'] = $_SESSION['session']['norm']['it'];
	//$data['`norm_iit`'] = $_SESSION['session']['norm']['iit'];
	$data['`norm_normalize`'] = '\''.mysql_real_escape_string($_SESSION['session']['norm']['normalize']).'\'';
	$data['`norm_corrfac`'] = $_SESSION['session']['norm']['corrfac'];
	$data['`norm_ispan`'] = $_SESSION['session']['norm']['ispan'];
	$data['`norm_cutrat`'] = $_SESSION['session']['norm']['cutrat'];
	$data['`norm_cutq`'] = $_SESSION['session']['norm']['cutq'];
	$data['`norm_times`'] = '\''.mysql_real_escape_string($time_range).'\'';

	$query = 'INSERT INTO `run_parameters` ('.implode(", ",array_keys($data)).') '.
			 'VALUES ('.implode(", ",array_values($data)).')';
	mysql_query($query,$conn) or die(mysql_error($conn));
}

function dbWriteRunInfo()
{
	$conn = open_connection();
	$data = array();
	
	$data['`run_id`'] = '\''.mysql_real_escape_string($_SESSION['session']['session_dir']).'\'';
    $data['`project_path`'] = '\''.mysql_real_escape_string($_SESSION['session']['project_path']).'\'';
    $data['`diagnostic_preprocess_path`'] = '\''.mysql_real_escape_string($_SESSION['session']['diagnostic_preprocess_path']).'\'';
    $data['`diagnostic_normalization_path`'] = '\''.mysql_real_escape_string($_SESSION['session']['diagnostic_normalization_path']).'\'';
    $data['`project_name`'] = '\''.mysql_real_escape_string($_SESSION['session']['project_name']).'\'';
    $data['`yaml_file`'] = '\''.mysql_real_escape_string($_SESSION['session']['yaml_file']).'\'';
    $data['`class_file`'] = '\''.mysql_real_escape_string($_SESSION['session']['class_file']).'\'';
    $data['`rdata_peaks_file`'] = '\''.mysql_real_escape_string($_SESSION['session']['rdata_peaks_file']).'\'';
    $data['`rdata_norm_file`'] = '\''.mysql_real_escape_string($_SESSION['session']['rdata_norm_file']).'\'';
    $data['`result_file`'] = '\''.mysql_real_escape_string($_SESSION['session']['result_file']).'\'';
	$data['`peak_detection_script`'] = '\''.mysql_real_escape_string($_SESSION['session']['peak_detection_script']).'\'';
	$data['`normalization_script`'] = '\''.mysql_real_escape_string($_SESSION['session']['normalization_script']).'\'';

	$query = 'INSERT INTO `run_info` ('.implode(", ",array_keys($data)).') '.
			 'VALUES ('.implode(", ",array_values($data)).')';
	mysql_query($query,$conn) or die(mysql_error($conn));
}

function discardRun()
{
	$proj_path = $_SESSION['session']['project_path'];
	//$tmp_path = $_SERVER['DOCUMENT_ROOT']."/tmp/".$_SESSION['session']['session_dir'];
	$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/cinnamoned/tmp/".$_SESSION['session']['session_dir']."/norm/";
	$cmd = "rm -r ".$proj_path."/ ".$tmp_path."/";
	exec($cmd);
}
?>
