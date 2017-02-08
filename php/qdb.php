<?php
include('utils.php');
include('connection.php');
include('queries.php');

if ($_REQUEST['info_all'])
{
	$info = getRunInfoAll();
	echo json_encode($info);
}

if ($_REQUEST['info_date'])
{
	$dates = $_REQUEST['info_date'];
	$dates = json_decode($dates,$assoc=TRUE);
	$info = getRunInfoByDate($dates['low'],$dates['high']);
	echo json_encode($info);
}

if ($_REQUEST['run_params'])
{
	$id = $_REQUEST['run_params'];
	$report = getRunParameters($id);
	echo json_encode($report);
}

if ($_REQUEST['class_on_the_fly'])
{
	$id = $_REQUEST['class_on_the_fly'];
	$onthefly = getRunClassData($id);
	echo json_encode($onthefly);
}

if ($_REQUEST['diagnostics_on_the_fly'])
{
	$id = $_REQUEST['diagnostics_on_the_fly'];
	$onthefly = getRunDiagnostics($id);
	echo json_encode($onthefly);
}

if ($_GET['results_on_the_fly'])
{
	$id = $_GET['results_on_the_fly'];
	$onthefly = getRunResults($id);
	header('Content-type: text/plain');
	header('Content-disposition: attachment; filename="results_'.$id.'.txt"');
	print $onthefly;
}

if ($_GET['run_id'])
{
	$id = $_GET['run_id'];
	$terms = $_GET['metab_ids'];
	$terms = json_decode($terms,$assoc=TRUE);
	$filtered = getSomeResults($id,$terms);
	header('Content-type: text/plain');
	header('Content-disposition: attachment; filename="partial_'.$id.'.txt"');
	print $filtered;
}

if ($_REQUEST['suggest_term'])
{
	$term = $_REQUEST['suggest_term'];
	$result = getAutoMetabo($term);
	echo json_encode($result,JSON_FORCE_OBJECT);
}

if ($_REQUEST['mz_low'])
{
	$low = $_REQUEST['mz_low'];
	$high = $_REQUEST['mz_high'];
	$result = getMetabolites(array("low" => $low, "high" => $high),"range");
	echo json_encode($result);
}

if ($_REQUEST['by_id'])
{
	$ids = $_REQUEST['by_id'];
	$result = getMetabolites($ids,"id");
	echo json_encode($result);
}

if ($_REQUEST['metab_id'])
{
	$id = $_REQUEST['metab_id'];
	$report = getMetaboliteInfo($id);
	echo json_encode($report);
}

if ($_REQUEST['delete_run'])
{
	$id = $_REQUEST['delete_run'];
	$msg = deleteRun($id);
	echo json_encode($msg);
}


function getRunInfoAll()
{
	global $info_all;
	$info = array();
	$conn = open_connection();
	$query = $info_all;
	$result = mysql_query($query,$conn);
	while(list($id,$name,$date) = mysql_fetch_array($result))
	{
		$info[] = array("id" => $id,"name" => $name,"date" => $date);
	}
	close_connection($conn);
	return($info);
}

function getRunInfoByDate($low,$high)
{
	global $info_dates_1,$info_dates_2,$info_dates_3;
	$info = array();
	$lows = strtotime($low);
	$highs = strtotime($high);
	$sqllow = date("Y-m-d H:i:s",$lows);
	$sqlhigh = date("Y-m-d H:i:s",$highs);
	$conn = open_connection();
	$query = $info_dates_1.'\''.$sqllow.'\''.$info_dates_2.'\''.$sqlhigh.'\''.$info_dates_3;
	$result = mysql_query($query,$conn);
	while(list($id,$name,$date) = mysql_fetch_array($result))
	{
		$info[] = array("id" => $id,"name" => $name,"date" => $date);
	}
	close_connection($conn);
	return($info);
}

function getRunParameters($id)
{
	global $info_params;
	$info = array();
	$report = array();
	$labels = array();
	
	$conn = open_connection();
	$query = $info_params.'\''.$id.'\'';
	$result = mysql_query($query,$conn);
	while($row = mysql_fetch_array($result,MYSQL_NUM)) // Should be one
	{
		for ($i=0; $i<mysql_num_fields($result); $i++)
		{
			$info[mysql_field_name($result,$i)] = $row[$i];
		}
	}
	close_connection($conn);
	
	$info['xcms_filter_do'] = $info['xcms_filter_do'] == 1 ? "Yes" : "No";
	$info['norm_diagplot'] = $info['norm_diagplot'] == 1 ? "Yes" : "No";
	$info['result_class'] = "<span id=\"show_class\" class=\"pseudolink\" onclick=\"createSampleInfo('".$id."','page')\" onmouseover=\"createSampleInfo('".$id."','popup')\">view</span>".
							"<script>$(\"#show_class\").tooltip({tip:\".tooltip-class\",effect:\"fade\"}).dynamic();</script>";
	$info['result_diagnostic'] = "<span class=\"pseudolink\" onclick=\"normDiagnosticsOnTheFly('".$id."')\">view</span>";
	//$info['result_result'] = "<span class=\"pseudolink\" onclick=\"normResultsOnTheFly('".$id."')\">download</span>";
	$info['result_result'] = "download <a class=\"pseudolink\" href=\"qdb.php?results_on_the_fly=".$id."\">all</a> or <span class=\"pseudolink\" onclick=\"openFilterArea('".$id."')\">filter</span>";

	$report['general_info'] = array($info['ref_run_id'],$info['xcms_filter_do'],
									$info['xcms_filter_min'],$info['xcms_filter_max']);
	$report['xcms_info'] = array($info['xcms_read_profstep'],$info['xcms_read_profmethod'],
								 $info['xcms_find_snthresh'],$info['xcms_find_step'],
								 $info['xcms_find_fwhm'],$info['xcms_find_sigma'],
								 $info['xcms_find_steps'],$info['xcms_find_max'],
								 $info['xcms_find_mzdiff']);
	$report['norm_info'] = array($info['norm_method'],$info['norm_tol'],$info['norm_correctfor'],
								 $info['norm_export'],$info['norm_diagplot'],
								 $info['norm_tspan'],$info['norm_tit'],
								 $info['norm_corrfac'],$info['norm_cutq'],
								 $info['norm_normalize'],$info['norm_ispan'],
								 $info['norm_cutrat'],$info['norm_times']);
	$report['result_info'] = array($info['result_class'],$info['result_diagnostic'],
								   $info['result_result']);
	
	$labels['general_info'] = array("Analysis ID",
									"Input retention time truncation",
									"Lower truncation boundary",
									"Upper truncation boundary");
	$labels['xcms_info'] = array("Profile generation step",
								 "Profile generation method",
								 "Signal to noise threshold",
								 "Peak detection step",
								 "Peak full width at half maximum",
								 "Peak model standard deviation",
								 "EIBPC combine steps",
								 "Maximum peaks per EIBPC",
								 "Minimum m/z difference for overlapping peaks");
	$labels['norm_info'] = array("Standards selection method",
								 "Reference match m/z tolerance",
								 "Corrected elements requested",
								 "Export results type",
								 "Diagnostic plots requested",
								 "Retention time alignment LOESS span",
								 "Retention time iterations",
								 "Retention time alignment LOESS correction factor",
								 "Retention time alignment exclusion quantile",
								 "Intensity normalization method",
								 "Intensity normalization LOESS span",
								 "Non-standards filter threshold",
								 "Manual time filters");
	$labels['result_info'] = array("Sample files and classes","Normalization diagnostics","Results");
	
	return(array("labels" => $labels, "values" => $report));
}

function getRunClassData($id)
{
	global $class_data;
	$output = array();
	
	$conn = open_connection();
	$query = $class_data.'\''.$id.'\'';
	$result = mysql_query($query,$conn);
	while(list($proj_name,$proj_path,$class_file) = mysql_fetch_array($result))
	{
		$tmp['proj_name'] = $proj_name;
		$tmp['proj_path'] = $proj_path;
		$tmp['class_file'] = $class_file;
	}
	close_connection($conn);

	# We must detect if we have compressed data...
	$contents = scandir($tmp['proj_path']);
	if (!empty($contents))
	{
		$contents = array_slice($contents,2);
		foreach ($contents as $file)
		{
			if (preg_match('/\.tar\.gz/',$file))
			{
				$cmd1 = "tar -xvf ".$tmp['proj_path']."/".$file." -C ".$tmp['proj_path']."/";
				exec($cmd1);
				$fh = fopen($tmp['proj_path']."/sample_info.txt","r");
				while (!feof($fh))
				{
					$line = fgets($fh);
					$line = explode("\t",trim($line));
					$output[$line[0]] = $line[1];
				}
				fclose($fh);
				$cmd2 = "rm -r ".$tmp['proj_path']."/diagnostic ".$tmp['proj_path']."/scripts ".$tmp['proj_path']."/*.txt ".$tmp['proj_path']."/*.RData";
				exec($cmd2);
				break;
			}
			else if (preg_match('/sample_info/',$file))
			{
				$fh = fopen($tmp['proj_path']."/sample_info.txt","r");
				while (!feof($fh))
				{
					$line = fgets($fh);
					$line = explode("\t",trim($line));
					$output[$line[0]] = $line[1];
				}
				fclose($fh);
				break;
			}
		}
	}

	return(array_filter($output));
}

function getRunDiagnostics($id)
{
	global $diag_data;
	$compressed = FALSE;
	$classdata = array();
	$matches = array();
	$picnames = array();
	$diagpics = array("alignment" => array(), "deviation" => array(), "boxplot" => array(), "other" => array());
	
	$conn = open_connection();
	$query = $diag_data.'\''.$id.'\'';
	$result = mysql_query($query,$conn);
	while(list($proj_name,$proj_path,$diag_path,$class_file) = mysql_fetch_array($result))
	{
		$tmp['proj_name'] = $proj_name;
		$tmp['proj_path'] = $proj_path;
		$tmp['diag_path'] = $diag_path;
		$tmp['class_file'] = $class_file;
	}
	close_connection($conn);

	//$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/tmp/".$id."/norm/";
	$tmp_pic_path = $_SERVER['DOCUMENT_ROOT']."/cinnamoned/tmp/".$id."/norm/";
	if (!file_exists($tmp_pic_path)) { mkdir($tmp_pic_path,$mode=0777,$recursive=TRUE); }

	$contents = scandir($tmp['proj_path']);
	if (!empty($contents))
	{
		$contents = array_slice($contents,2);
		foreach ($contents as $file)
		{
			if (preg_match('/\.tar\.gz/',$file)) # Is compressed?
			{
				$compressed = TRUE;
				$cmd1 = "tar -xvf ".$tmp['proj_path']."/".$file." -C ".$tmp['proj_path']."/";
				exec($cmd1);
			}
		}

		# Get class data
		$fh = fopen($tmp['proj_path']."/sample_info.txt","r");
		while (!feof($fh))
		{
			$line = fgets($fh);
			$line = explode("\t",trim($line));
			$classdata[$line[0]] = $line[1];
		}
		fclose($fh);
		unset($classdata['Filename']);
		$classdata = array_filter($classdata);

		# Get percentage match data
		$fh = fopen($tmp['proj_path']."/pcts.txt","r");
		while (!feof($fh))
		{
			$line = fgets($fh);
			$line = explode("\t",trim($line));
			$matches[$line[0]] = $line[1];
		}
		fclose($fh);
		$matches = array_filter($matches);

		# Get pics
		$contents = scandir($tmp['diag_path']);
		if (!empty($contents)) { $contents = array_slice($contents,2); }
		foreach ($contents as $img)
		{
			$source_file = $tmp['diag_path']."/".$img;
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
				#						  " - ".$matches[str_replace("_ALIGNMENT.png","",$img)]."% of new m/z matching with reference";
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

		if ($compressed)
		{
			$cmd2 = "rm -r ".$tmp['proj_path']."/diagnostic ".$tmp['proj_path']."/scripts ".$tmp['proj_path']."/*.txt ".$tmp['proj_path']."/*.RData";
			exec($cmd2);
		}
	}

	return(array("path" => $diagpics, "name" => $picnames));
}


function getRunResults($id)
{
	global $res_data;
	
	$conn = open_connection();
	$query = $res_data.'\''.$id.'\'';
	$result = mysql_query($query,$conn);
	while(list($proj_path) = mysql_fetch_array($result))
	{
		$tmp['proj_path'] = $proj_path;
	}
	close_connection($conn);
	
	# We must detect if we have compressed data...
	$contents = scandir($tmp['proj_path']);
	if (!empty($contents))
	{
		$contents = array_slice($contents,2);
		foreach ($contents as $file)
		{
			if (preg_match('/\.tar\.gz/',$file))
			{
				$cmd1 = "tar -xvf ".$tmp['proj_path']."/".$file." -C ".$tmp['proj_path']."/";
				exec($cmd1);
				$f = file_get_contents($tmp['proj_path']."/norm_output.txt");
				$cmd2 = "rm -r ".$tmp['proj_path']."/diagnostic ".$tmp['proj_path']."/scripts ".$tmp['proj_path']."/*.txt ".$tmp['proj_path']."/*.RData";
				exec($cmd2);
				break;
			}
			else if (preg_match('/norm_output/',$file))
			{
				$f = file_get_contents($tmp['proj_path']."/norm_output.txt");
				break;
			}
		}
	}
	return($f);
}

function getSomeResults($id,$terms)
{
	global $res_data;
	$output = array();
	
	$conn = open_connection();
	$query = $res_data.'\''.$id.'\'';
	$result = mysql_query($query,$conn);
	while(list($proj_path) = mysql_fetch_array($result))
	{
		$tmp['proj_path'] = $proj_path;
	}
	close_connection($conn);
	
	# We must detect if we have compressed data...
	$contents = scandir($tmp['proj_path']);
	if (!empty($contents))
	{
		$contents = array_slice($contents,2);
		foreach ($contents as $file)
		{
			if (preg_match('/\.tar\.gz/',$file))
			{
				$cmd1 = "tar -xvf ".$tmp['proj_path']."/".$file." -C ".$tmp['proj_path']."/";
				exec($cmd1);
				$fh = fopen($tmp['proj_path']."/norm_output.txt","r") or die(print_r(error_get_last()));
				# Get header
				$line = fgets($fh);
				$line = explode("\t",trim($line));
				$curr_id = $line[0];
				$output[$curr_id] = implode("\t",$line);
				# Get rest
				while (!feof($fh))
				{
					$line = fgets($fh);
					$line = explode("\t",trim($line));
					$curr_id = $line[0];
					if (in_array($curr_id,$terms))
					{
						$output[$curr_id] = implode("\t",$line);
					}
				}
				fclose($fh);
				$cmd2 = "rm -r ".$tmp['proj_path']."/diagnostic ".$tmp['proj_path']."/scripts ".$tmp['proj_path']."/*.txt ".$tmp['proj_path']."/*.RData";
				exec($cmd2);
				break;
			}
			else if (preg_match('/norm_output/',$file))
			{
				$fh = fopen($tmp['proj_path']."/norm_output.txt","r");
				# Get header
				$line = fgets($fh);
				$line = explode("\t",trim($line));
				$curr_id = $line[0];
				$output[$curr_id] = implode("\t",$line);
				# Get rest
				while (!feof($fh))
				{
					$line = fgets($fh);
					$line = explode("\t",trim($line));
					$curr_id = $line[0];
					if (in_array($curr_id,$terms))
					{
						$output[$curr_id] = implode("\t",$line);
					}
				}
				fclose($fh);
				break;
			}
		}
	}
	return(implode("\n",$output));
}

function getMetabolites($input,$by)
{
	global $metab_by_range_1,$metab_by_range_2,$metab_by_range_3,$metab_by_id_1,$metab_by_id_2;
	$output = array();

	if (!empty($input))
	{
		switch($by)
		{
			case "range":
				$low = $input['low'];
				$high = $input['high'];
				$query = $metab_by_range_1.$low.$metab_by_range_2.$high.$metab_by_range_3;
				break;
			case "id":
				$ids = is_array($input) ? $input : array($input);
				$ids = '(\''.implode("', '",$ids).'\')';
				$query = $metab_by_id_1.$ids.$metab_by_id_2;
				break;
		}
		
		$conn = open_connection_s();
		$result = mysql_query($query,$conn);
		while(list($id,$mz,$rt) = mysql_fetch_array($result))
		{
			$output[] = array("id" => $id,"mz" => sprintf("%.6f",$mz),"rt" => sprintf("%.6f",$rt));
		}
		close_connection($conn);
	}

	return($output);
}

function getMetaboliteInfo($id)
{
	global $metab_info,$metab_an_1,$metab_an_2;
	$info = array();
	$anno = array();
	$report = array();
	$labels = array();
	$annotation = array();
	
	$conn = open_connection_s();
	
	$query = $metab_info.'\''.$id.'\'';
	$result = mysql_query($query,$conn);
	while($row = mysql_fetch_array($result,MYSQL_NUM)) // Should be one
	{
		for ($i=0; $i<mysql_num_fields($result); $i++)
		{
			$info[mysql_field_name($result,$i)] = $row[$i];
		}
	}
	$info['is_geom'] = $info['is_geom'] == 1 ? "Yes" : "No";
	$info['is_rlm'] = $info['is_rlm'] == 1 ? "Yes" : "No";
	$info['is_both'] = $info['is_both'] == 1 ? "Yes" : "No";
	$info['isotopes'] = empty($info['isotopes']) ? "" : $info['isotopes'];
	$info['adduct'] = empty($info['adduct']) ? "" : $info['adduct'];
	$info['real_mass'] = empty($info['real_mass']) || $info['real_mass'] < 1e-12 ? "" : sprintf("%.6f",$info['real_mass']);
	$info['prop_formula'] = empty($info['prop_formula']) ? "" : $info['prop_formula'];
	$info['theor_mass'] = empty($info['theor_mass']) ? "" : $info['theor_mass'];

	if (!empty($info['real_mass']))
	{
		$query = $metab_an_1.$info['real_mass'].$metab_an_2;
		$result = mysql_query($query,$conn);
		if (mysql_num_rows($result) > 0)
		{
			while(list($hid,$hform,$hname,$kid,$kform,$kname,$cid,$cform,$cname) = mysql_fetch_array($result))
			{
				$hid = empty($hid) ? $hid : "<a href=\"http://www.hmdb.ca/metabolites/".$hid."\" target=\"_blank\">".$hid."</a>";
				$kid = empty($kid) ? $kid : "<a href=\"http://www.genome.jp/dbget-bin/www_bget?".$kid."\" target=\"_blank\">".$kid."</a>";
				$cid = empty($cid) ? $cid : "<a href=\"http://www.ebi.ac.uk/chebi/searchId.do?chebiId=".$cid."\" target=\"_blank\">CHEBI:".$cid."</a>";
				$anno['HMDB'][] = array("ID" => $hid, "Formula" => $hform, "Name" => $hname);
				$anno['KEGG'][] = array("ID" => $kid, "Formula" => $kform, "Name" => $kname);
				$anno['ChEBI'][] = array("ID" => $cid, "Formula" => $cform, "Name" => $cname);
			}
		}
		foreach ($anno['HMDB'] as $key => $value)
		{
			$anno['HMDB'][$key] = array_filter($value);
		}
		foreach ($anno['KEGG'] as $key => $value)
		{
			$anno['KEGG'][$key] = array_filter($value);
		}
		foreach ($anno['ChEBI'] as $key => $value)
		{
			$anno['ChEBI'][$key] = array_filter($value);
		}
		$anno['HMDB'] = array_filter($anno['HMDB']);
		$anno['KEGG'] = array_filter($anno['KEGG']);
		$anno['ChEBI'] = array_filter($anno['ChEBI']);
	}

	close_connection($conn);
	
	$report['Spectrum'] = array($info['id'],sprintf("%.6f",$info['mz']),sprintf("%.6f",$info['rt']),
								sprintf("%.6f",$info['mzmin']),sprintf("%.6f",$info['mzmax']),
								sprintf("%.6f",$info['rtmin']),sprintf("%.6f",$info['rtmax']));
	$report['Deconvolution'] = array($info['isotopes'],$info['adduct'],$info['real_mass'],
									 $info['prop_formula'],$info['theor_mass']);
	$report['Intensity'] = array(sprintf("%.6f",$info['summarized_intensity_geom']),
								 sprintf("%.6f",$info['summarized_intensity_rlm']),
								 sprintf("%.6f",$info['summarized_intensity_both']));
	$report['Stability'] = array($info['is_geom'],$info['is_rlm'],$info['is_both']);
	if (!empty($anno)) { $report['Annotation'] = array_values($anno); }
	
	$labels['Spectrum'] = array("Peak ID","m/z","Retention time","Peak m/z minimum",
								"Peak m/z maximum","Peak RT minimum","Peak RT maximum");
	$labels['Deconvolution'] = array("Isotope","Adduct","Real mass","Proposed formula","Theoretical mass");
	$labels['Intensity'] = array("Geometrical IS normalized","RLM IS normalized","In both IS normalized");
	$labels['Stability'] = array("Geometrical","RLM","Both");
	if (!empty($anno)) { $labels['Annotation'] = array_keys($anno); }
	
	return(array("labels" => $labels, "values" => $report));
}

function deleteRun($id)
{
	global $res_data,$delete_run;
	$msg = "Fail!";
	if (!empty($id))
	{
		$conn = open_connection();
		$query = $res_data.'\''.$id.'\'';
		$result = mysql_query($query,$conn);
		while($row = mysql_fetch_array($result,MYSQL_NUM))
		{
			$proj_path = $row[0];
		}
		$cmd = "rm -r ".$proj_path."/";
		exec($cmd);

		$query = $delete_run.'\''.$id.'\'';
		$result = mysql_query($query,$conn);
		$msg = !$result ? "Failed to delete analysis ".$id."\nContact the administrator." : "Analysis ".$id." successfully deleted!";
		close_connection($conn);
	}
	return($msg);
}

function getAutoMetabo($term)
{		
	global $auto_metab_1,$auto_metab_2;
	$opts = array();
	$conn = open_connection_s();
	$query = $auto_metab_1.'\'%'.$term.'%\''.$auto_metab_2;
	$result = mysql_query($query,$conn);
	while ($m = mysql_fetch_array($result,MYSQL_NUM))
	{
		$opts[] = $m[0];
	}
	close_connection($conn);
	return($opts);
}
?>
