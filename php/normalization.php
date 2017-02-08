<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">

<?php
session_start();
?>

<html><!-- #BeginTemplate "/templates/template.dwt" --><!-- DW6 -->
<head>
<!-- #BeginEditable "doctitle" --> 
<title>cINNaMoneD - Application</title>
<!-- #EndEditable -->
<meta http-equiv="Content-Type" content="text/html;">
<link rel="stylesheet" href="../css/styles.css" type="text/css">
<script type="text/javascript" src="../js/jquery-1.8.min.js"></script>
<script type="text/javascript" src="../js/jquery.json-2.3.min.js"></script>
<script type="text/javascript" src="../js/livevalidation.min.js"></script>
<script type="text/javascript" src="../js/control.js"></script>
</head>
<body>
<table class="headContainer">
  <tr> 
    <td style="padding:0px; border-width:0px;"><img src="../images/logo.png" alt="logo here"></td>
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
				<form name="normalization" id="normalization" method="post" action="" enctype="multipart/form-data" target="_self" onsubmit="return false;"></form>
				<p class="text"><fieldset><legend class="fieldSetTitle">Standards-based normalization parameters</legend>
				<table class="innerTable" style="width:40%; margin-top:5px; margin-bottom:10px"><tr><td class="innerCell">
					<label for="default_norm">Use defaults</label><input type="radio" id="default_norm" name="use_norm_params" onclick="checkMe('norm_params')" checked />
					<span style="font-weight:bold">&nbsp;&nbsp;&nbsp;OR&nbsp;&nbsp;&nbsp;</span>
					<label for="custom_norm">Customize</label><input type="radio" id="custom_norm" name="use_norm_params" onclick="checkMe('norm_params')"/>
				</td></tr></table>
				<fieldset style="margin-top:5px"><legend class="fieldSetTitle" style="font-size:0.8em; font-weight:normal">General</legend>
					<table class="innerTable">
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="method">Standards selection method: </label>
						</td>
						<td class="innerCell" style="width:15%; text-align:right">
							<select id="method" name="method" disabled>
								<option value="geom" selected>geometrical</option>
								<option value="rlm">rlm</option>
								<option value="both">both</option>
							</select>
						</td>
						<td class="innerCell" style="width:5%">&nbsp;</td>
						<td class="innerCell" style="width:30%">
							<label for="mztol">m/z tolerance: </label>
						</td>
						<td class="innerCell" style="width:20%">
							<input type="text" id="mztol" name="mztol" size="4" value="0.01" disabled />
						</td>
						</tr>
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="export">Correct for: </label>
						</td>
						<td class="innerCell" style="width:15%; text-align:right">
							<select id="correctfor" name="correctfor" disabled>
								<option value="both" selected>time-intensity</option>
								<option value="time">time</option>
								<option value="intensity">intensity</option>
								<option value="none">none (only match)</option>
							</select>
						</td>
						<td class="innerCell" style="width:5%">&nbsp;</td>
						<td class="innerCell" style="width:30%">
							<label for="diagplot">Plot diagnostics </label>
						</td>
						<td class="innerCell" style="width:20%">
							<input type="checkbox" id="diagplot" name="diagplot" checked disabled />
						</td>
						</tr>
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="export">Export results: </label>
						</td>
						<td class="innerCell" style="width:15%; text-align:right">
							<select id="export" name="export" disabled>
								<option value="none">Do not export</option>
								<option value="all">All data</option>
								<option value="armada" selected>Gene ARMADA</option>
							</select>
						</td>
						<td class="innerCell" style="width:35%">&nbsp;</td>
						<td class="innerCell" style="width:20%">&nbsp;</td>
						</tr>
					</table>
					<div id="normGenValMsg"></div>
				</fieldset>
				<fieldset style="margin-top:10px"><legend class="fieldSetTitle" style="font-size:0.8em; font-weight:normal">Retention time alignment</legend>
					<table class="innerTable">
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="tspan">LOESS span: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="tspan" name="tspan" size="4" value="0" disabled />
						</td>
						<td class="innerCell" style="width:35%">
							<label for="it">Alignment algorithm iterations: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="it" name="it" size="4" value="3" disabled />
						</td>
						</tr>
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="corrfac">LOESS singularity correction factor: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="corrfac" name="corrfac" size="4" value="2" disabled />
						</td>
						<td class="innerCell" style="width:35%">
							<label for="cutq">RT deviation exclusion quantile: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="cutq" name="cutq" size="4" value="0.98" disabled />
						</td>
						</tr>
					</table>
					<div id="normTimeValMsg"></div>
				</fieldset>
				<fieldset style="margin-top:10px"><legend class="fieldSetTitle" style="font-size:0.8em; font-weight:normal">Intensity normalization</legend>
					<table class="innerTable">
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="normalize">Normalization method: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<select id="normalize" name="normalize" disabled>
								<option value="loess">loess</option>
								<option value="rlm" selected>robustlinear</option>
								<option value="lm">linear</option>
								<option value="simple">ISfactor</option>
							</select>
						</td>
						<td class="innerCell" style="width:35%">
							<label for="ispan">LOESS span: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="ispan" name="ispan" size="4" value="0" disabled />
						</td>
						</tr>
						<tr>
						<td class="innerCell" style="width:35%">
							<label for="corrfac">Non-standards correction factor: </label>
						</td>
						<td class="innerCell" style="width:15%">
							<input type="text" id="cutrat" name="cutrat" size="4" value="2" disabled />
						</td>
						<td class="innerCell" style="width:35%">&nbsp;</td>
						<td class="innerCell" style="width:15%">&nbsp;</td>
						</tr>
					</table>
					<div id="normIntValMsg"></div>
				</fieldset>
            </fieldset></p>
            <br/>
            <table class="innerTable" style="width:80%"><tr><td class="innerCell" style="text-align:right">
				<div id="loading" style="display:none; float:left;"><img src="../images/loading_small.gif"></div>
                <button type="button" id="resetNorm" onclick="reset('normalization.php')">Reset</button>
                <button type="button" id="submit_to_final" onclick="preprocessNormalization()">Energize!&nbsp;&raquo;</button>
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
initValidators('normalization.php');
</script>

</body>
</html>
