<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">

<html><!-- #BeginTemplate "/templates/template.dwt" --><!-- DW6 -->
<head>
<!-- #BeginEditable "doctitle" --> 
<title>cINNaMoneD - Database</title>
<!-- #EndEditable -->
<meta http-equiv="Content-Type" content="text/html;">
<link rel="stylesheet" href="../css/jquery-ui-1.8.18.custom.css" type="text/css">
<link rel="stylesheet" href="../css/styles.css" type="text/css">
<script type="text/javascript" src="../js/jquery-1.8.min.js"></script>
<script type="text/javascript" src="../js/jquery-ui-1.8.18.custom.min.js"></script>
<script type="text/javascript" src="../js/jquery.json-2.3.min.js"></script>
<script type="text/javascript" src="../js/jquery.tools.min.js"></script>
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
            <p class="title">Database</p>
            <p class="text"><strong>Please select an activity:</strong></p>
            <ul class="bullet">
              <li class="text"><a style="cursor:pointer;" onclick="openMenu('view')">View/delete</a> past runs</li>
              <li class="text"><a style="cursor:pointer;" onclick="openMenu('modify')">Modify</a> metabolite</li>
              <li class="text"><a style="cursor:pointer;" onclick="openMenu('search')">Search</a> metabolite</li>
            </ul>
            <div id="localMenu"></div><br/>
			<div id="calendarContainer" style="display:none;"><span class="text">
				<label for="from">From&nbsp;</label><input type="text" id="from" name="from" size="8" readonly />&nbsp;
				<label for="to">To&nbsp;</label><input type="text" id="to" name="to" size="8" readonly />
				<button type="button" id="fetchRunsButton" onclick="fetchRuns('date')" disabled>GO</button>
            </span></div><br/>
            <div id="runInfo" class="runInfoDiv"></div><br/>
            <div id="runParameters" class="runInfoDiv"></div><br/>
            <div id="resultFilter" class="runInfoDiv"></div>
            <!-- The dataset description popup div -->
			<div id="class_popup" class="tooltip-class"></div>
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
</body>

<script type="text/javascript">
initCalendar();
</script>

</html>
