<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">

<?php
session_start();
include('utils.php');
initConfig();
?>

<html><!-- #BeginTemplate "/templates/template.dwt" --><!-- DW6 -->
<head>
<!-- #BeginEditable "doctitle" --> 
<title>cINNaMoneD - Application</title>
<!-- #EndEditable -->
<meta http-equiv="Content-Type" content="text/html;">
<link rel="stylesheet" href="../css/styles.css" type="text/css">
<link rel="stylesheet" href="../css/uploadify.css" type="text/css">
<script type="text/javascript" src="../js/jquery-1.8.min.js"></script>
<script type="text/javascript" src="../js/jquery.json-2.3.min.js"></script>
<script type="text/javascript" src="../js/livevalidation.min.js"></script>
<script type="text/javascript" src="../js/swfobject.js"></script>
<script type="text/javascript" src="../js/jquery.uploadify.v2.1.4.min.js"></script>
<script type="text/javascript" src="../js/control.js"></script>
</head>
<body>
<table class="headContainer">
  <tr> 
    <td style="padding:0px; border-width:0px"><img src="../images/logo.png" alt="logo here"></td>
    <td style="padding:5px; border-width:0px; text-align:center">
		<span class="pageTitle"><span style="font-size:1.4em; font-weight:bold">CINNAMONED</span><br/>geometriCally INvariant NormAlization of MetabOlomic kidNEy Data</span>
	</td>
  </tr>
  <tr> 
    <td colspan=2 style="height:5px; background-color:#FF8542; padding:0px; border-width:0px;"></td>
  </tr>
</table>
<table class="menuContainer"><tr><td><table class="menuContainer" style="width:40%">
  <tr>
    <td class="menuCell"><a href="../index.html">Home</a></td>
    <td class="menuCell"><a href="cinnamoned.php">Application</a></td>
    <td class="menuCell"><a href="database.php">Database</a></td>
    <td class="menuCell"><a href="../help.html">Help</a></td>
    <td class="menuCell"><a href="../contact.html">Contact</a></td> 
  </tr>
</table></td></tr></table>
<table class="mainContainer">
	<tr> 
	<td class="mainContainer" style="height:80%;">
		<table class="mainContainer">
			<tr>
			<td class="leftContainer"></td>
			<td class="mainContainer">
				<p class="title">Application</p>
				<p class="text">
				<form name="cinnamoned" id="cinnamoned" method="post" action="" enctype="multipart/form-data" target="_self" onsubmit="return false;"></form>
				<fieldset><legend class="fieldSetTitle">Project info</legend>
				<table class="innerTable">
				<tr>
				<td class="innerCell" style="width:30%"><label for="project_name">Project name (100 chars max):</label></td>
				<td class="innerCell" style="width:70%"><input type="text" id="project_name" size="50" value=""/></td>
				</tr>
				<tr>
				<td class="innerCell" style="width:30%"><label for="upload_netcdf">Upload NetCDF files:</label></td>
				<td class="innerCell" style="width:70%">
					<table class="innerTable" style="width:50%"><tr>
					<td class="innerCell" style="padding:0px;">
					<input type="file" id="upload_netcdf" name="netcdf_files"/>
					</td>
					<td class="innerCell" style="padding:0px 5px; text-align:center; width: 60%">
					<a style="font-size:1.3em; font-weight:bold; cursor:pointer" onclick="beginUpload('upload_netcdf')">Upload Files</a>
					</td>
					</tr>
					<tr><td colspan=2 class="innerCell" style="padding:0px">
					<div id="queueCDF"></div>
					</td></tr>
					</table>
				</td>
				</tr>
				<tr>
				<td class="innerCell" style="width:30%"><label>Retention time filter (seconds):</label></td>
				<td class="innerCell" style="width:70%">
					<table class="innerTable" style="width: 60%">
					<tr>
						<td class="innerCell" style="padding:0px; width:30%">
							<label for="do_rt_filter" style="font-size:1.2em">Perform </label><input type="checkbox" id="do_rt_filter" onclick="checkMe('rt_trunc')" checked />
						</td>
						<td class="innerCell" style="padding:0px; width: 70%">
							<label for="min_time_trunc" style="font-size:1.2em">min: </label><input type="text" id="min_time_trunc" name="min_time_trunc" size="5" value="300"/>
							<label for="max_time_trunc" style="font-size:1.2em">max: </label><input type="text" id="max_time_trunc" name="max_time_trunc" size="5" value="3000"/>
						</td></span>
					</tr>
					</table>
                </td>
                </tr>
			</table>
			<div id="projValMsg"></div>
			</fieldset></p>
			<p><fieldset><legend class="fieldSetTitle">Sample(s) info</legend>
				<table class="innerTable">
					<tr>
					<td class="innerCell" style="width:50%">
						<label for="upload_class">Upload sample-class file (see <a href="../files/sample_class.txt" target="_blank">here</a> for a template):</label>
					</td>
					<td class="innerCell" style="width:70%">
						<table class="innerTable" style="width:65%">
						<tr>
						<td class="innerCell" style="padding:0px;">
							<input type="file" id="upload_class" name="class_file"/>
						</td>
						<td class="innerCell" style="padding:0px 5px; text-align:center; width: 55%">
							<a style="font-size:1.3em; font-weight:bold; cursor:pointer" onclick="beginUpload('upload_class')">Upload Files</a>
						</td>
						</tr>
						<tr><td colspan=2 class="innerCell" style="padding:0px">
							<div id="queueClass"></div>
						</td></tr>
						</table>
					</td>
					</tr>
                <tr>
                <td class="innerCell"><span style="font-weight:bold;">OR</span></td><td>&nbsp;</td>
                </tr>
                <tr>
                <td class="innerCell" style="width:50%">Manually enter sample-class information (after file upload)</span></td><td>&nbsp;</td>
                </tr>
                <tr>
                <td class="innerCell" style="border-bottom-style:dashed; border-bottom-width:1px; border-bottom-color:#A33307">Filename</td>
                <td class="innerCell" style="border-bottom-style:dashed; border-bottom-width:1px; border-bottom-color:#A33307">Class</td>
                </tr>
                <tr><td colspan=2 class="innerCell" style="padding:0px"><table id="file_class" class="innerTable"></table></td></tr>
            </table>
            <div id="classNameValMsg"></div>
            </fieldset>
            </p>
            <p><fieldset><legend class="fieldSetTitle">xcms peak detection parameters</legend>
				<table class="innerTable" style="width:40%; margin-top:5px; margin-bottom:10px"><tr><td class="innerCell">
					<label for="default_xcms">Use defaults</label><input type="radio" id="default_xcms" name="use_xcms_params" onclick="checkMe('xcms_params')" checked />
					<span style="font-weight:bold">&nbsp;&nbsp;&nbsp;OR&nbsp;&nbsp;&nbsp;</span>
					<label for="custom_xcms">Customize</label><input type="radio" id="custom_xcms" name="use_xcms_params" onclick="checkMe('xcms_params')"/>
				</td></tr></table>
				<fieldset style="margin-top:5px"><legend class="fieldSetTitle" style="font-size:0.8em; font-weight:normal">Data reading</legend>
					<table class="innerTable">
						<tr>
						<td class="innerCell">
							<label for="profstep">Profile reading step: </label><input type="text" id="profstep" name="profstep" size="4" value="1" disabled />
						</td>
						<td class="innerCell">
							<label for="profmethod">Profile generation method: </label>
							<select id="profmethod" name="profmethod" disabled>
								<option value="bin">bin</option>
								<option value="binlin" selected>binlin</option>
								<option value="binlinbase">binlinbase</option>
								<option value="intlin">intlinbase</option>
							</select>
						</td>
						</tr>
					</table>
					<div id="xcmsRunValMsg"></div>
				</fieldset>
				<fieldset style="margin-top:10px"><legend class="fieldSetTitle" style="font-size:0.8em; font-weight:normal">Peak detection</legend>
					<table class="innerTable">
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="snthresh">Signal-to-noise ratio: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="snthresh" name="snthresh" size="4" value="7" disabled />
						</td>
						<td class="innerCell" style="width:35%">
							<label for="step">EIBPC step size: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="step" name="step" size="4" value="0.1" disabled />
						</td>
						</tr>
						<tr>
							<td class="innerCell" style="width:35%">
								<label for="fwhm">Full width at half maximum: </label>
							</td>
							<td class="innerCell" style="width:15%">
								<input type="text" id="fwhm" name="fwhm" size="4" value="30" disabled />
							</td>
							<td class="innerCell" style="width:35%">
								<label for="sigma">Peak model standard deviation: </label>
							</td>
							<td class="innerCell" style="width:15%">
								<input type="text" id="sigma" name="sigma" size="4" value="7" disabled />
							</td>
						</tr>
						<tr>
							<td class="innerCell" style="width:35%">
								<label for="steps">EIBPC combine steps: </label>
							</td>
							<td class="innerCell" style="width:15%">
								<input type="text" id="steps" name="steps" size="4" value="3" disabled />
							</td>
							<td class="innerCell" style="width:35%">
								<label for="max">Maximum peaks per EIBPC: </label>
							</td>
							<td class="innerCell" style="width:15%">
								<input type="text" id="max" name="max" size="4" value="5" disabled />
							</td>
						</tr>
						<tr>
							<td colspan=3 class="innerCell" style="width:85%">
								<label for="mzdiff">Minimum m/z difference for peaks with overlapping retention times: </label>
							</td>
							<td class="innerCell" style="width:15%">
								<input type="text" id="mzdiff" name="mzdiff" size="4" value="auto" disabled />
							</td>
						</tr>
					</table>
					<div id="xcmsPeakValMsg"></div>
				</fieldset>
            </fieldset>
            <br/>
            <table class="innerTable" style="width:80%"><tr><td class="innerCell" style="text-align:right">
				<div id="loading" style="display:none; float:left;"><img src="../images/loading_small.gif"></div>
                <button type="button" id="resetXCMS" onclick="reset('cinnamoned.php')">Reset</button>
                <button type="button" id="submit_to_xcms" onclick="processPreprocess()" disabled>Engage!&nbsp;&raquo;</button>
                <!--<input type="submit" id="submit_to_xcms" name="submit" value="GO&raquo;" disabled />-->
            </td></tr></table>
            </form>
            <br>
            <table class="innerTable" style="width:80%; table-layout:fixed"><tr><td class="innerCell">
			<div id="warning"><span style="font-weight:bold">Please do not close or refresh the page while data process is running or your project will be lost!<br/>You can monitor its progress below instead.</span></div>
			<div id="progressDisplay">R output will be displayed here to monitor progress...</div>
			</td></tr></table>
            </p>
            </td>
			<td class="rightContainer"><div id="errorContainer"></div></td>
			</tr>
		</table>
	</td>
	</tr>
	<tr> 
    <td class="footer">Copyright &copy; 2012 <a href="http://www.renalfibrosis.fr" class="whitetext" target="_blank">RF Lab</a>/<a href="http://www.inserm.fr/" class="whitetext" target="_blank">Inserm</a>,  Designed by <a href="http://www.grissom.gr/pmoulos/" class="whitetext" target="_blank">Panagiotis Moulos</a></td>
	</tr>
	<tr> 
		<td style="height:5px; background-color:#FF8542; padding:0px; border-width:0px;"></td>
	</tr>
</table>

<script type="text/javascript">
var T = null;
initUploadify('cdf');
initUploadify('class');
initValidators('cinnamoned.php');
</script>
 
</body>
</html>
