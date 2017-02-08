<?php
if ($_REQUEST['xcms_progress'])
{
	$path_for_info = $_REQUEST['xcms_progress'];
	$progress = file_get_contents($path_for_info."xcms.Rout");
	echo json_encode(nl2br($progress));
	ob_flush();
	flush();
}

if ($_REQUEST['norm_progress'])
{
	$path_for_info = $_REQUEST['norm_progress'];
	$progress = file_get_contents($path_for_info."norm.Rout");
	echo json_encode(nl2br($progress));
	ob_flush();
	flush();
}
?>
