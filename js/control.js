function processPreprocess()
{   
	var urlBase = initMe();
	var filenames = [];
	var classnames = [];
	var classdata = {};
	var i = 0;

	var doFilter = $("#do_rt_filter").is(":checked") ? "TRUE" : "FALSE";
	var mzd = $("#mzdiff").val() === "auto" ? 0.8-$("#step").val()*$("#steps").val() : $("#mzdiff").val();
	var preprocessData =
	{
		filter:
		{
			do: doFilter,
			min: $("#min_time_trunc").val(),
			max: $("#max_time_trunc").val()
		},
		read:
		{
			nSlaves: "",
			profstep: $("#profstep").val(),
			profmethod: $("#profmethod").val()
		},
		find:
		{
			method: "matchedFilter",
			fwhm: $("#fwhm").val(),
			sigma: $("#sigma").val(),
			max: $("#max").val(),
			step: $("#step").val(),
			steps: $("#steps").val(),
			mzdiff: mzd,
			snthresh: $("#snthresh").val()
		}
	};

	$("input[name='filename']").each(function()
	{ 
		filenames.push(this.value);
	});
	$("input[name='classname']").each(function()
	{ 
		classnames.push(this.value);
	});
	for (i=0; i<filenames.length; i++)
	{
		classdata[filenames[i]] = classnames[i];
	}

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		beforeSend: function()
		{
			disable(['resetXCMS']);
			disable(['submit_to_xcms']);
		},
		data: { project: $("#project_name").val(), preprocess: $.toJSON(preprocessData), classdata: $.toJSON(classdata) },
		success: function(data)
		{
			startTimer('detection',data);
			runPreprocess();
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);						
		},
		dataType: "json"
	});
}

function runPreprocess()
{
	var urlBase = initMe();
	
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { run_xcms: "run_xcms" },
		timeout: 1800000,
		beforeSend: function()
		{
			$("#loading").show();
			$("#warning").show("slow");
			$("#progressDisplay").show("slow");
		},
		complete: function()
		{
			enable(['resetXCMS']);
			enable(['submit_to_xcms']);
			$("#loading").hide();
			endTimer();
		},
		success: function(data)
		{
			x = $("#min_time_trunc").val();
			y = $("#max_time_trunc").val();
			window.location.href = urlBase+'php/timefilter.php?min='+x+'&max='+y;
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);						
		},
		dataType: "json"
	});
}

function preprocessNormalization()
{
	var urlBase = initMe();
	var i = 0;

	var plotDiag = $("#diagplot").is(":checked") ? "TRUE" : "FALSE";
	var normData =
	{
		method: $("#method").val(),
		correctfor: $("#correctfor").val(),
		tol: $("#mztol").val(),
		tspan: $("#tspan").val(),
		ispan: $("#ispan").val(),
		it: $("#it").val(),
		normalize: $("#normalize").val(),
		corrfac: $("#corrfac").val(),
		cutrat: $("#cutrat").val(),
		cutq: $("#cutq").val(),
		export: $("#export").val(),
		diagplot: plotDiag
	};

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { normalization: $.toJSON(normData) },
		success: function(data)
		{
			startTimer('normalization',data);
			runNormalization();
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);						
		},
		dataType: "json"
	});
}

function runNormalization()
{
	var urlBase = initMe();
	
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { run_norm: "run_norm" },
		timeout: 900000,
		beforeSend: function()
		{
			disable(['submit_to_final']);
			$("#loading").show();
			$("#warning").show("slow");
			$("#progressDisplay").show("slow");
		},
		complete: function()
		{
			enable(['submit_to_final']);
			$("#loading").hide();
			endTimer();
		},
		success: function(data)
		{
			window.location.href = urlBase+'php/results.php';
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);						
		},
		dataType: "json"
	});
}

function fetchResults()
{
	var urlBase = initMe();
	
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { results: "results" },
		complete: function()
		{
			/*bhtml =
			"<tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Click&nbsp;" +
			"<a style=\"color:#E90000; font-weight:bold; cursor:pointer\" onclick=\"finishAndSave()\">here</a>" +
			"&nbsp;to save your analysis in the application database so you can later retrieve the results. If you " +
			"close this window without clicking on the save link your analysis will be lost!</span>"
			"</td></tr>";*/
			bhtml =
			"<tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Click&nbsp;" +
			"<a id=\"run_saver\" style=\"color:#E90000; font-weight:bold; cursor:pointer\" onclick=\"finishAndSave()\">here</a>" +
			"&nbsp;to have your analysis assimilated in the application database collective so you can later retrieve the results or click " +
			"<a id=\"run_discarder\" style=\"color:#E90000; font-weight:bold; cursor:pointer\" onclick=\"discardRun()\">here</a>" +
			"&nbsp;to have your analysis discarded and prevented from perfection! " +
			//"If you close this window without clicking on the save or discard link your analysis will be destroyed! " +
			"Comply! <span style=\"color:#2C6200\">Resistance is futile!</span></span>";
			"</td></tr>";
			$("#results_table").append(bhtml);
		},
		success: function(data)
		{
			var location = data.location;
			var prid = data.prid;
			thtml =
			"<tr><td class=\"diagnosticCell\">Your analysis with project ID <span style=\"color:#E90000; font-weight:bold\">" + prid + "</span>" +
			" has finished with success! You can download the results from <a href=\"" + location + "\" target=\"_blank\">here</a><br></td></tr>";
			$("#results_table").append(thtml);
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "json"
	});
}

function finishAndSave()
{
	var urlBase = initMe();
	
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { finish: "finish" },
		beforeSend: function()
		{
			bhtml =
			"<tr><td id=\"saveInfo\" class=\"diagnosticCell\">Saving... Please wait... " +
			"<span style=\"color:#E90000\">Do NOT close this window yet!</span></td></tr>";
			$("#results_table").append(bhtml);
		},
		complete: function()
		{
			if ($("#saveInfo").data("success"))
			{
				chtml =
				"<span style=\"font-weight:bold\">Analysis parameters and results saved! You can now close this window or " +
				" start a new <a href=\"../php/cinnamoned.php\">analysis</a></span>";
				$("#saveInfo").empty();
				$("#saveInfo").html(chtml);
			}
			window.onbeforeunload = null;
			window.onunload = null;
		},
		success: function(data)
		{
			$("#saveInfo").data("success",true);
			if (data.exit === 1)
			{
				ehtml =
				"Something bad happened during finalizing and database writing... Please contact the administrator using the above project ID...";
				$("#saveInfo").empty();
				$("#saveInfo").html(ehtml);
				$("#saveInfo").data("success",false);
			}
			else // Do not allow clicking "here" again, it will cause MySQL error
			{
				$("#run_saver").removeAttr("onclick");
				$("#run_discarder").removeAttr("onclick"); 
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
			$("#saveInfo").data("success",false);
			$("#run_saver").removeAttr("onclick");
			$("#run_discarder").removeAttr("onclick"); 
		},
		dataType: "json"
	});
}

function fetchTimeDiagnostics(mit,mat)
{
	var urlBase = initMe();
	var validators = [];

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { rt_diagnostic: "rt_diagnostic" },
		complete: function()
		{
			var html =
			"<tr><td class=\"innerCell\">&nbsp</td></tr>" +
			"<tr><td class=\"innerCell\" style=\"text-align:right; padding-top: 20px;\">" +
			"<div id=\"loading\" style=\"display:none; float:left;\"><img src=\"../images/loading_small.gif\"></div>" +
			"<button type=\"button\" id=\"resetFilter\" onclick=\"resetTimes()\">Reset</button>" +
			"<button type=\"button\" id=\"submit_to_norm\" onclick=\"sendTimeFilters()\">Next&raquo;</button>" +
			"</td></tr>";
			$("#rt_diagnostics").append(html);
		},
		success: function(data)
		{
			var thtml;
			var i = 0;
			var minTimeMsg = "Minimum filter time must be an integer greater than 0!";
			var maxTimeMsg = "Maximum filter time must be an integer greater than 0!";

			if (!$.isEmptyObject(data))
			{
				path = data["path"];
				fname = data["name"];
				prid = data["prid"];
				if (!$.isEmptyObject(path))
				{
					for (i=0; i<path.length; i++)
					{
						thtml = 
						//"<tr><td class=\"innerCell\" style=\"padding:2px\"><input type=\"text\" id=\"filename_" + data.fileCount + "\" name=\"filename\" value=" + fileObj.name + " style=\"width:90%; background-color:#F8DEBB\" readonly /></td>" +
						//"<td class=\"innerCell\" style=\"padding:2px\"><input type=\"text\" id=\"classname_" + data.fileCount + "\" name=\"classname\" style=\"width:90%\" onBlur=\"checkMe('classname')\"/></td></tr>";
						"<tr><td class=\"diagnosticCell\">" +
						"<div style=\"text-align:center; font-weight:bold; display:inline-block\">" + fname[i] + "</div><br/><br/>" +
						"<img src=\"" + path[i] + "\"><br/><br/>" +
						"<div style=\"float:left\">" +
						"<label for=\"rt_more_check_" + i + "\">Refine retention time limits&nbsp;</label>" +
						"<input type=\"checkbox\" id=\"rt_more_check_" + i + "\" name=\"rt_more_check\" onclick=\"checkMe('rt_refine')\"/>&nbsp;&nbsp;&nbsp;" +
						"<label for=\"rt_diag_min_" + i + "\">Min:&nbsp;</label>" +
						"<input type=\"text\" id=\"rt_diag_min_" + i + "\" name=\"rt_diag_min\" value=\"" + mit + "\" size=\"4\" disabled />" +
						"&nbsp;&nbsp;&nbsp;" +
						"<label for=\"rt_diag_max_" + i + "\">Max:&nbsp;</label>" +
						"<input type=\"text\" id=\"rt_diag_max_" + i + "\" name=\"rt_diag_max\" value=\"" + mat + "\"  size=\"4\" disabled />" +
						"&nbsp;&nbsp;&nbsp;" +
						"<div id=\"rtValMsg_" + i + "\" style=\"display:inline-block\"></div>" +
						"</div></td></tr>";
						$("#rt_diagnostics").append(thtml);

						validators.push(new LiveValidation("rt_diag_min_" + i, { validMessage: " ", insertAfterWhatNode: "rtValMsg_" + i, wait: 1000}));
						validators[2*i].add(Validate.Numericality, { minimum: 0, onlyInteger: true, notANumberMessage: minTimeMsg, notAnIntegerMessage: minTimeMsg, tooLowMessage: minTimeMsg });
						validators[2*i].add(Validate.Presence,{ failureMessage: "Lower time boundary cannot be empty!" });
						validators.push(new LiveValidation("rt_diag_max_" + i, { validMessage: " ", insertAfterWhatNode: "rtValMsg_" + i, wait: 1000}));
						validators[2*i+1].add(Validate.Numericality, { minimum: 0, onlyInteger: true, notANumberMessage: maxTimeMsg, notAnIntegerMessage: maxTimeMsg, tooLowMessage: maxTimeMsg });
						validators[2*i+1].add(Validate.Presence,{ failureMessage: "Upper time boundary cannot be empty!" });
					}
					$("#rt_diagnostics").data("number",path.length);
				}
				else
				{
					thtml =
					"<tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Something bad happened! Please contact administrator stating the following number:&nbsp;</span>" +
					"<span style=\"color:#E90000; font-weight:bold\">" + prid + "</span></td></tr>";
					$("#rt_diagnostics").append(thtml);
				}
			}
			else
			{
				thtml =
				"<tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Something really bad happened! Cannot report project ID... Please contact administrator...</span></td></tr>";
				$("#rt_diagnostics").append(thtml);
			}
			
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "json"
	});
}

function fetchNormDiagnostics(id)
{
	var urlBase = initMe();

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { norm_diagnostic: "norm_diagnostic" },
		success: function(data)
		{
			var thtml;
			var i = 0;
			if (!$.isEmptyObject(data))
			{
				path = data["path"];
				fname = data["name"];
				prid = data["prid"];
				if (!$.isEmptyObject(path))
				{
					for (i=0; i<fname.length; i++)
					{
						thtml = 
						"<tr><td colspan=3 class=\"diagnosticCell\">" +
						"<div style=\"text-align:center; font-weight:bold; display:inline-block\">" + fname[i] + "</div><br/><br/>" +
						"<table class=\"innerTable\" style=\"table-layout:fixed\">" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["alignment"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td class=\"diagnosticCell\" style=\"width:50%\"><img src=\"" + path["deviation"][i] + "\" height=100% width=100%></td>" +
						"<td class=\"diagnosticCell\" style=\"width:50%\"><img src=\"" + path["boxplot"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["rawint"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["normint"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["standards"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"</table>" +
						"</td></tr>";
						$("#norm_diagnostics").append(thtml);
					}
					
					xtra = path["other"];
					if (!isEmpty(xtra))
					{
						ehtml = "<tr><td colspan=2 class=\"diagnosticCell\">" +
						"<div style=\"text-align:center; font-weight:bold; display:inline-block\">Boxplots after (left) and before (right) normalization</div><br/><br/>" +
						"<table class=\"innerTable\" style=\"table-layout:fixed\"><tr>";
						for (i=0; i<xtra.length; i++)
						{
							ehtml += 
							"<td class=\"diagnosticCell\" style=\"width:50%\"><img src=\"" + xtra[i] + "\"></td>";
						}
						ehtml +=
						"</tr></table></td></tr>";
						$("#norm_diagnostics").append(ehtml);
					}
				}
				else
				{
					thtml =
					"<tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Something bad happened! Please contact administrator stating the following number:&nbsp;</span>" +
					"<span style=\"color:#E90000; font-weight:bold\">" + prid + "</span></td></tr>";
					$("#norm_diagnostics").append(thtml);
				}
			}
			else
			{
				thtml =
				"<tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Something really bad happened! Cannot report project ID... Please contact administrator...</span></td></tr>";
				$("#norm_diagnostics").append(thtml);
			}
			
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "json"
	});
}

function sendTimeFilters()
{
	var urlBase = initMe();
	var times = { low: [], high: [] };
	var i = 0;
	var n = $("#rt_diagnostics").data("number");
	
	for (i=0; i<n; i++)
	{
		times.low.push($("#rt_diag_min_"+i).val());
		times.high.push($("#rt_diag_max_"+i).val());
	}

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { time_filter: $.toJSON(times) },
		success: function(data)
		{
			window.location.href = urlBase+'php/normalization.php';
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);						
		},
		dataType: "json"
	});
}

function fetchRuns(type)
{
	var urlBase = initMe();
	var dates;
	var toSend;
	var i;
	var cls;
	var tmp;

	$("#runParameters").hide();
	$("#runParameters").html("");
	switch (type)
	{
		case 'all':
			toSend = { info_all: "info_all" };
			$("#calendarContainer").hide();
			$("#resultFilter").hide();
			break;
		case 'date':
			dates = { low: $("#from").val(), high: $("#to").val() };
			toSend = { info_date: $.toJSON(dates) };
			$("#resultFilter").hide();
			break;
	}
	$("#runInfo").data("type",type);
			
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: toSend,
		success: function(data)
		{
			if (!$.isEmptyObject(data))
			{
				html = "<span class=\"text\"><table class=\"resultTable\"><tr>" +
					   "<td class=\"resultHead\">Analysis ID</td>" +
					   "<td class=\"resultHead\">Project name</td>" +
					   "<td class=\"resultHead\">Date</td>" +
					   "<td class=\"resultHead\">&nbsp;</td></tr>";
				for (i=0; i<data.length; i++)
				{
					cls = i%2 === 1 ? "resultCellOdd" : "resultCellEven";
					tmp = "<tr>" +
						  "<td class=\"" + cls + "\" style=\"width:22%\"><span class=\"pseudolink\" onclick=\"fetchParameters('" + data[i]["id"] + "'); return false;\">" + data[i]["id"] + "</span></td>" +
						  "<td class=\"" + cls + "\" style=\"width:48%\">" + data[i]["name"] + "</td>" +
						  "<td class=\"" + cls + "\" style=\"width:26%\">" + data[i]["date"] + "</td>" +
						  "<td class=\"" + cls + "Del\" style=\"width:4%\" onclick=\"deleteRun('" + data[i]["id"] + "')\"><img src=\"../images/cancel.png\"></td>" +
						  "</tr>";
					html += tmp;
				}
				html += "</table></span>";
				$("#runInfo").html(html);
				$("#runInfo").show();
			}
			else
			{
				$("#runInfo").html("Sorry, nothing found!");
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);				
		},
		dataType: "json"
	});
}

function deleteRun(id)
{
	var urlBase = initMe();
	
	var del = confirm("You are about to delete analysis " + id + "\nAre you sure? (this action cannot be undone!)");
	if (del)
	{
		$.ajax(
		{
			type: 'POST',
			url: urlBase+'php/qdb.php',
			data: { delete_run: id },
			success: function(data)
			{
				alert(data);
				var t = $("#runInfo").data("type");
				if (!isEmpty(t)) { fetchRuns(t); } // Update the table
			},
			error: function(data,error)
			{												
				displayError('Ooops! ' + error + ' ' + data.responseText);						
			},
			dataType: "json"
		});
	}
	else { return; }
}

function discardRun()
{
	var urlBase = initMe();
	
	$.ajax(
	{
		type: 'POST',
		async: false,
		url: urlBase+'php/control.php',
		beforeSend: function()
		{
			bhtml =
			"<tr><td id=\"saveInfo\" class=\"diagnosticCell\">Discarding... Please wait... " +
			"<span style=\"color:#E90000\">Do NOT close this window yet!</span></td></tr>";
			$("#results_table").append(bhtml);
		},
		complete: function()
		{
			chtml =
			"<span style=\"font-weight:bold\">Analysis parameters and results discarded! You can now close this window or " +
			" start a new <a href=\"/php/cinnamoned.php\">analysis</a></span>";
			$("#saveInfo").empty();
			$("#saveInfo").html(chtml);
		},
		data: { discard: "discard" },
		success: function(data)
		{
			$("#saveInfo").data("success",false);
			window.onbeforeunload = null;
			window.onunload = null;
			//window.opener = 'x';
			//window.close();
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "json"
	});
}

function fetchParameters(id)
{
	var urlBase = initMe();
	var inner,outer;
	var cls,tmp;
	var c;
	
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: { run_params: id },
		success: function(data)
		{
			if (!$.isEmptyObject(data))
			{
				headers = { general_info: "General", xcms_info: "Peak detection", norm_info: "Normalization", result_info: "Analysis" };
				
				html = "<span class=\"text\"><table class=\"resultTable\">";
				for (outer in data["labels"])
				{
					c = 0;
					html += "<tr><td colspan=2 class=\"resultHead\">" + headers[outer] + "</td></tr>";
					for (inner in data["labels"][outer])
					{
						cls = c%2 === 1 ? "resultCellOdd" : "resultCellEven";
						tmp = "<tr>" +
							  "<td class=\"" + cls + "\" style=\"width:60%\">" + data["labels"][outer][inner] + "</td>" +
							  "<td class=\"" + cls + "\" style=\"width:40%\">" + data["values"][outer][inner] + "</td>" +
							  "</tr>";
						html += tmp;
						c++;
					}
				}
				html += "</table></span>";
				$("#runParameters").html(html);
				$("#runParameters").show();
			}
			else
			{
				$("#runParameters").html("Sorry, nothing found!");
				$("#runParameters").show();
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);				
		},
		dataType: "json"
	});
}

function createSampleInfo(id,type)
{
	var urlBase = initMe();

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: { class_on_the_fly: id },
		success: function(data)
		{						
			if ($.isEmptyObject(data))
			{
				displayError('Sorry, no sample information found for the selected analysis.');
			}
			else
			{
				if (type === "page")
				{
					html = gimmeSampleInfo(data,'page');
					//dWindow = window.open("","_blank");
					//dWindow.document.write(html);
				}
				else
				{
					html = gimmeSampleInfo(data,'popup');
					$("#class_popup").html(html);
				}
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "json"
	});
}

function normDiagnosticsOnTheFly(id)
{
	var urlBase = initMe();

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: { diagnostics_on_the_fly: id },
		success: function(data)
		{
			var html, htmlHeader, htmlFooter, htmlTitle, htmlMain;
			var i = 0;
			if (!$.isEmptyObject(data))
			{
				path = data["path"];
				fname = data["name"];
				if (!$.isEmptyObject(path))
				{
					htmlHeader =
					"<html>" +
					"<head>" +
					"<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">" + 
					"<link type=\"text/css\" rel=\"stylesheet\" href=\"../css/styles.css\"/>" +
					"<title\>Normalizaton diagnostics</title>" +
					"</head>" +
					"<body style=\"margin:50px 100px;\">";
					htmlFooter =
					"</body>" +
					"</html>";
					htmlTitle = "<span class=\"title\">Normalization diagnostics for Analysis " + id + "</span><br/><br/>";
					htmlMain = "<table class=\"innerTable\">";
					
					for (i=0; i<fname.length; i++)
					{
						htmlMain += 
						"<tr><td colspan=3 class=\"diagnosticCell\">" +
						"<div style=\"text-align:center; font-weight:bold; display:inline-block\">" + fname[i] + "</div><br/><br/>" +
						"<table class=\"innerTable\" style=\"table-layout:fixed\">" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["alignment"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td class=\"diagnosticCell\" style=\"width:50%\"><img src=\"" + path["deviation"][i] + "\" height=100% width=100%></td>" +
						"<td class=\"diagnosticCell\" style=\"width:50%\"><img src=\"" + path["boxplot"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["rawint"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["normint"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"<tr>" +
						"<td colspan=2 class=\"diagnosticCell\" style=\"width:100%\"><img src=\"" + path["standards"][i] + "\" height=100% width=100%></td>" +
						"</tr>" +
						"</table>" +
						"</td></tr>";
					}
					
					xtra = path["other"];
					if (!isEmpty(xtra))
					{
						htmlMain +=
						"<tr><td colspan=2 class=\"diagnosticCell\">" +
						"<div style=\"text-align:center; font-weight:bold; display:inline-block\">Boxplots after (left) and before (right) normalization</div><br/><br/>" +
						"<table class=\"innerTable\" style=\"table-layout:fixed\"><tr>";
						for (i=0; i<xtra.length; i++)
						{
							htmlMain += 
							"<td class=\"diagnosticCell\" style=\"width:50%\"><img src=\"" + xtra[i] + "\"></td>";
						}
						htmlMain +=
						"</tr></table></td></tr>";
					}

					htmlMain += "</table><br/>";
				}
				else
				{
					htmlMain =
					"<table class=\"innerTable\"><tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Something bad happened! Please contact administrator stating the following number and message:&nbsp;</span>" +
					"<span style=\"color:#E90000; font-weight:bold\">" + id + "&nbsp;<em>Error: Can't find image path!</em></span>. State that you tried to view past analysis diagnostics.</td></tr></table>";
				}
			}
			else
			{
				htmlMain =
				"<table class=\"innerTable\"><tr><td class=\"diagnosticCell\"><span style=\"font-weight:bold\">Something bad happened! Please contact administrator stating the following number and message:&nbsp;</span>" +
				"<span style=\"color:#E90000; font-weight:bold\">" + id + "&nbsp;<em>Error: Empty AJAX object!</em></span>. State that you tried to view past analysis diagnostics.</td></tr></table>";
			}

			// Show the created html in a new window
			html = htmlHeader + htmlTitle + htmlMain + htmlFooter;
			dWindow = window.open("","_blank");
			dWindow.document.write(html);
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "json"
	});
}

/*function normResultsOnTheFly(id)
{
	var urlBase = initMe();

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: { results_on_the_fly: id },
		success: function(data)
		{						
			if ($.isEmptyObject(data))
			{
				displayError('Sorry, no data found to download for the selected analysis.');
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);
		},
		dataType: "text"
	});
}*/

function fetchSpecificMetabolites(id)
{
	var urlBase = initMe();

	terms = $("#mz_id_filter").val().split(/\n|\r/);
	window.location = urlBase+'php/qdb.php?run_id='+id+'&metab_ids='+$.toJSON(terms);
}

function gimmeSampleInfo(data,type)
{
	var html;
	var key;
	var c = 0;
	switch(type)
	{
		case 'page':
			html = "";
			break;
		case 'popup':
			html = "<table class=\"popup\">";
			for (key in data)
			{
				if (c === 0)
				{
					html +=
					"<tr><td class=\"popup\" style=\"width:80%\"><strong>" + key + "</strong></td><td class=\"popup\" style=\"width:20%\"><strong>" + data[key] + "</strong></td></tr>";
				}
				else
				{
					html +=
					"<tr><td class=\"popup\" style=\"width:80%\">" + key + "</td><td class=\"popup\" style=\"width:20%\">" + data[key] + "</td></tr>";
				}
				c++;
			}
			html += "</table>";
			break;
	}
	return(html);
}

function searchMetabolites(by)
{
	var urlBase = initMe();
	var toSend;
	var low,high,terms,cls,tmp,i;

	$("#runParameters").html("").hide();
	switch(by)
	{
		case 'range':
			low =  $("#mz_low").val();
			high = $("#mz_high").val();
			if (low === '' || high === '')
			{
				alert("Both range values should be provided!");
				return;
			}
			else
			{
				toSend = { mz_low: low, mz_high: high };
			}
			break;
		case 'id':
			terms = $("#mz_id").val().split(/\n|\r/);
			toSend = { by_id: terms }
			break;
	}
			
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: toSend,
		success: function(data)
		{
			if (!$.isEmptyObject(data))
			{
				html = "<span class=\"text\"><table class=\"resultTable\"><tr>" +
					   "<td class=\"resultHead\">ID</td>" +
					   "<td class=\"resultHead\">m/z</td>" +
					   "<td class=\"resultHead\">Retention time</td></tr>";
				for (i=0; i<data.length; i++)
				{
					cls = i%2 === 1 ? "resultCellOdd" : "resultCellEven";
					tmp = "<tr>" +
						  "<td class=\"" + cls + "\" style=\"width:40%\"><span class=\"pseudolink\" onclick=\"fetchMetabolite('" + data[i]["id"] + "'); return false;\">" + data[i]["id"] + "</span></td>" +
						  "<td class=\"" + cls + "\" style=\"width:30%\">" + data[i]["mz"] + "</td>" +
						  "<td class=\"" + cls + "\" style=\"width:30%\">" + data[i]["rt"] + "</td>" +
						  "</tr>";
					html += tmp;
				}
				html += "</table></span>";
				$("#runInfo").html(html);
				$("#runInfo").show();
			}
			else
			{
				$("#runInfo").html("Sorry, nothing found!");
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);				
		},
		dataType: "json"
	});
}

function fetchMetabolite(id)
{
	var urlBase = initMe();
	var outer,inner,innermost;
	var cls,tmp,stmp;
	var c;
	
	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/qdb.php',
		data: { metab_id: id },
		success: function(data)
		{
			if (!$.isEmptyObject(data))
			{	
				html = "<span class=\"text\"><table class=\"resultTable\">";
				for (outer in data["labels"])
				{
					html += "<tr><td colspan=2 class=\"resultHead\">" + outer + "</td></tr>";
					if (outer === "Annotation")
					{
						c = 0;
						for (inner in data["labels"][outer])
						{
							cls = c%2 === 1 ? "resultCellOdd" : "resultCellEven";
							stmp = "<table class=\"innerTable\">" +
								   "<tr><td class=\"resultHead\">ID</td><td class=\"resultHead\">Formula</td><td class=\"resultHead\">Name</td></tr>";
							for (innermost in data["values"][outer][inner])
							{
								stmp += "<tr>" +
										"<td class=\"" + cls + "\">" + data["values"][outer][inner][innermost]["ID"] + "</td>" +
										"<td class=\"" + cls + "\">" + data["values"][outer][inner][innermost]["Formula"] + "</td>" +
										"<td class=\"" + cls + "\">" + data["values"][outer][inner][innermost]["Name"] + "</td>" +
										"</tr>";
							}
							stmp += "</table>";
							tmp = "<tr>" +
								  "<td class=\"" + cls + "\" style=\"width:30%\">" + data["labels"][outer][inner] + "</td>" +
								  "<td class=\"" + cls + "\" style=\"width:70%\">" + stmp + "</td>" +
								  "</tr>";
							html += tmp;
							c++;
						}
					}
					else
					{
						c = 0;
						for (inner in data["labels"][outer])
						{
							cls = c%2 === 1 ? "resultCellOdd" : "resultCellEven";
							tmp = "<tr>" +
								  "<td class=\"" + cls + "\" style=\"width:30%\">" + data["labels"][outer][inner] + "</td>" +
								  "<td class=\"" + cls + "\" style=\"width:70%\">" + data["values"][outer][inner] + "</td>" +
								  "</tr>";
							html += tmp;
							c++;
						}
					}
				}
				html += "</table></span>";
				$("#runParameters").html(html);
				$("#runParameters").show();
			}
			else
			{
				$("#runParameters").html("Sorry, nothing found!");
				$("#runParameters").show();
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);				
		},
		dataType: "json"
	});
}

function openMenu(which)
{
	switch(which)
	{
		case 'view':
			$("#runInfo").html("").hide();
			$("#runParameters").html("").hide();
			$("#resultFilter").html("").hide();
			$("#calendarContainer").hide();
			html = "<span class=\"text\"><a style=\"cursor:pointer;\" onclick=\"fetchRuns('all')\">All</a> or by " +
				   "<a style=\"cursor:pointer;\" onclick=\"toggleCalendar()\">date</a></li></span>";
			$("#localMenu").html("");
			$("#localMenu").html(html);
			break;
		case 'modify':
			$("#runInfo").html("").hide();
			$("#runParameters").html("").hide();
			$("#resultFilter").html("").hide();
			$("#calendarContainer").hide();
			break;
		case 'search':
			$("#runInfo").html("").hide();
			$("#runParameters").html("").hide();
			$("#resultFilter").html("").hide();
			$("#calendarContainer").hide();
			html = "<span class=\"text\"><strong>Search using:&nbsp;&nbsp;</strong>" + 
				   "<label for=\"mz_radio\">m/z range</label><input type=\"radio\" id=\"mz_radio\" name=\"search_radio\" onclick=\"checkMe('search_params')\" checked />" +
				   "<span style=\"font-weight:bold\">&nbsp;&nbsp;&nbsp;OR&nbsp;&nbsp;&nbsp;</span>" +
				   "<label for=\"id_radio\">Database ID</label><input type=\"radio\" id=\"id_radio\" name=\"search_radio\" onclick=\"checkMe('search_params')\"/><br/><br/>" +
				   "<div id=\"searchOptsContainer\" style=\"display:block;\">" +
				   "Please enter m/z range: " +
				   "<label for=\"mz_low\">From:&nbsp;</label><input type=\"text\" id=\"mz_low\" name=\"mz_low\" size=8 style=\"background-color:#FFEFCC\"/>&nbsp;&nbsp;" +
				   "<label for=\"mz_high\">To:&nbsp;</label><input type=\"text\" id=\"mz_high\" name=\"mz_high\" size=8 style=\"background-color:#FFEFCC\"/>&nbsp;&nbsp;" +
				   "<button type=\"button\" id=\"searchButton\" onclick=\"searchMetabolites('range')\">GO</button>" +
				   "</div>" +
				   "</span>";
			$("#localMenu").html("");
			$("#localMenu").html(html);
			
			var minMzMsg = "Minimum m/z must be a number greater than 0!";
            var minMz = new LiveValidation("mz_low", { validMessage: " ", wait: 1000});
            minMz.add(Validate.Numericality, { minimum: 0, notANumberMessage: minMzMsg, tooLowMessage: minMzMsg });
            //minMz.add(Validate.Presence, { failureMessage: minMzMsg });

            var maxMzMsg = "Maximum m/z must be a number greater than 0!";
            var maxMz = new LiveValidation("mz_high", { validMessage: " ", wait: 1000});
            maxMz.add(Validate.Numericality, { minimum: 0, notANumberMessage: maxMzMsg, tooLowMessage: maxMzMsg });
            //maxMz.add(Validate.Presence, { failureMessage: maxMzMsg });
			break;
	}
}

function openFilterArea(id)
{
	var html =
	"<table class=\"innerTable\" style=\"width:75%\">" +
	"<tr><td class=\"infoCell\" style=\"vertical-align:top; font-size:1em;\">Please enter metabolite ID(s): </td>" +
	"<td rowspan=2><textarea id=\"mz_id_filter\" name=\"mz_id_filter\" onkeyup=\"checkMe('search_fill_filter')\" style=\"font-size:1em;\"/></td>" +
	"<tr><td class=\"infoCell\" style=\"vertical-align:bottom; font-size:1.1em;\"><button type=\"button\" id=\"searchFilterButton\" onclick=\"fetchSpecificMetabolites('" + id + "')\" disabled>Comply!</button></td></tr>" +
	"</table>" +
	"<script>bindAutoComplete('mz_id_filter')";
	$("#resultFilter").html(html).show();
}

function beginUpload(id)
{
    $("#"+id).uploadifyUpload();
}

function initUploadify(what)
{   
	var urlBase = initMe();

	$.ajax(
	{
		type: 'POST',
		url: urlBase+'php/control.php',
		data: { sid: "sid" },
		success: function(data)
		{
			switch(what)
			{
				case 'cdf':
					initUploadifyCDF(data);
					break;
				case 'class':
					initUploadifyClass(data);
					break;
			}
		},
		error: function(data,error)
		{												
			displayError('Ooops! ' + error + ' ' + data.responseText);						
		},
		dataType: "json"
	});
}

function initUploadifyCDF(sid)
{   
	var urlBase = initMe();

    var initU = 
    {
        'uploader': urlBase + 'swf/uploadify.swf',
        'script': urlBase + 'php/uploadify.php',
        'scriptData': { 'session_id': sid },
        'cancelImg': urlBase + 'images/cancel.png',
        'expressInstall': urlBase + 'swf/expressInstall.swf',
        'fileDataName': 'netcdf_files',
        'fileExt': '*.cdf;*.ncdf;*.netcdf',
        'fileDesc': 'NetCDF Files (.cdf, .ncdf, .netcdf)',
        'simUploadLimit': 3,
        'sizeLimit': 1610612736,
        'multi': true,
        'auto': false,
        'buttonImg': urlBase + 'images/upload_button.png',
        'height': 25,
        'width': 105,
        'queueID': 'queueCDF',
        'removeCompleted': false,
        'onInit': function()
        {
			$("#file_class").data("number",0);
		},
        'onQueueFull': function(event,queueSizeLimit) 
		{
			alert("No more than 10 files allowed for upload!");
			return false;
		},
        'onComplete': function(event,ID,fileObj,response,data)
        {
			var thtml = 
			"<tr><td class=\"innerCell\" style=\"padding:2px\"><input type=\"text\" id=\"filename_" + data.fileCount + "\" name=\"filename\" value=" + fileObj.name + " style=\"width:90%; background-color:#F8DEBB\" readonly /></td>" +
			"<td class=\"innerCell\" style=\"padding:2px\"><input type=\"text\" id=\"classname_" + data.fileCount + "\" name=\"classname\" style=\"width:90%\" onblur=\"checkMe('classname')\" onkeyup=\"checkMe('classname')\"/></td></tr>";
			$("#file_class").append(thtml);

			var cn = $("#file_class").data("number");
			cn++;
			$("#file_class").data("number",cn);
		},
		'onAllComplete': function(event,data)
		{
			var i = 0;
			var validators = [];
			for (i=0; i<data.filesUploaded; i++)
			{
				validators.push(new LiveValidation("classname_" + i, { validMessage: " ", insertAfterWhatNode: "classNameValMsg", wait: 1000}));
				validators[i].add(Validate.Format,{ pattern: /^[^\\/@#$&*`\[\]!\(\)%\^,\.<>\?\\|';"]*$/, failureMessage: "Class name must not include the characters \ / @ # $ & * ` ! ( ) % ^ , . < > ? | ' ; [ ] \"" });
				validators[i].add(Validate.Presence,{ failureMessage: "Class name cannot be empty!" });
			}
            $("#file_class").data("validators",validators);
		},
        'onError': function (event,ID,fileObj,errorObj) 
		{
			alert(errorObj.type + ' Error: ' + errorObj.info);
		},
    };
    $('#upload_netcdf').uploadify(initU);
}

function initUploadifyClass(sid)
{
	var urlBase = initMe();

    var initU = 
    {
        'uploader': urlBase + 'swf/uploadify.swf',
        'script': urlBase + 'php/uploadify.php',
        'scriptData': { 'session_id' : sid },
        'cancelImg': urlBase + 'images/cancel.png',
        'expressInstall': urlBase + 'swf/expressInstall.swf',
        'fileDataName': 'class_file',
        'simUploadLimit': 1,
        'sizeLimit': 1048576,
        'multi': false,
        'auto': false,
        'buttonImg': urlBase + 'images/upload_button.png',
        'height': 25,
        'width': 105,
        'queueID': 'queueClass',
        'removeCompleted': false,
        'onAllComplete': function(event,data)
        {
			var i = 0;
			var names = [];
			var classes = [];
			var n = $("#file_class").data("number");
			for (i=0; i<n; i++)
			{
				names.push("filename_"+i);
				classes.push("classname_"+i);
			}
			disable(names);
			disable(classes);
			
			var validators = $("#file_class").data("validators");
			for (i=0; i<validators.length; i++)
			{
				validators[i].destroy();
			}
			$("#file_class").removeData("validators");

			enable(['submit_to_xcms']);
		},
		'onCancel': function (event,ID,fileObj,data)
		{
			disable(['submit_to_xcms']);
		},
        'onError': function (event,ID,fileObj,errorObj) 
		{
			alert(errorObj.type + ' Error: ' + errorObj.info);
			disable(['submit_to_xcms']);
		},
    };
    $('#upload_class').uploadify(initU);
}

function initValidators(caller)
{
    switch(caller)
    {
        case 'cinnamoned.php':
            var projectName = new LiveValidation("project_name", { validMessage: " ", insertAfterWhatNode: "projValMsg", wait: 1000});
            projectName.add(Validate.Format,{ pattern: /^[^\\/@#$&*`\[\]!\(\)%\^,\.<>\?\\|';"]*$/, failureMessage: "Project name must not include the characters \ / @ # $ & * ` ! ( ) % ^ , . < > ? | ' ; [ ] \"" });
            projectName.add(Validate.Length,{ minimum: 0, maximum: 100 });

            var minTimeMsg = "Minimum filter time must be an integer greater than 0!";
            var minTime = new LiveValidation("min_time_trunc", { validMessage: " ", insertAfterWhatNode: "projValMsg", wait: 1000});
            minTime.add(Validate.Numericality, { minimum: 0, onlyInteger: true, notANumberMessage: minTimeMsg, notAnIntegerMessage: minTimeMsg, tooLowMessage: minTimeMsg });
            minTime.add(Validate.Presence, { failureMessage: minTimeMsg });

            var maxTimeMsg = "Maximum filter time must be an integer greater than 0!";
            var maxTime = new LiveValidation("max_time_trunc", { validMessage: " ", insertAfterWhatNode: "projValMsg", wait: 1000});
            maxTime.add(Validate.Numericality, { minimum: 0, onlyInteger: true, notANumberMessage: maxTimeMsg, notAnIntegerMessage: maxTimeMsg, tooLowMessage: maxTimeMsg });
			maxTime.add(Validate.Presence, { failureMessage: maxTimeMsg });
			
            var minProfStepMsg = "Minimum profile step must be an integer greater than 0!";
            var maxProfStepMsg = "Maximum profile step must be an integer smaller than 10!";
            var profStep = new LiveValidation("profstep", { validMessage: " ", insertAfterWhatNode: "xcmsRunValMsg", wait: 1000});
            profStep.add(Validate.Numericality, { minimum: 0, maximum: 10, onlyInteger: true, notANumberMessage: minProfStepMsg, notAnIntegerMessage: minProfStepMsg, tooLowMessage: minProfStepMsg, tooHighMessage: maxProfStepMsg });
            profStep.add(Validate.Presence, { failureMessage: minProfStepMsg });

			var snrMsg = "Signal-to-noise ratio must be a number greater than 0!";
            var snThresh = new LiveValidation("snthresh", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
            snThresh.add(Validate.Numericality, { minimum: 0, notANumberMessage: snrMsg, tooLowMessage: snrMsg });
            snThresh.add(Validate.Presence, { failureMessage: snrMsg });

			var minStepMsg = "Minimum EIBPC step size must be a number greater than 0!";
            var maxStepMsg = "Maximum EIBPC step size must be smaller than 1!";
            var step = new LiveValidation("step", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
            step.add(Validate.Numericality, { minimum: 0, maximum: 1, notANumberMessage: minStepMsg, tooLowMessage: minStepMsg, tooHighMessage: maxStepMsg });
            step.add(Validate.Presence, { failureMessage: minStepMsg });

            var fwhmMsg = "FWHM must be a number greater than 0!";
            var fwhm = new LiveValidation("fwhm", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
            fwhm.add(Validate.Numericality, { minimum: 0, notANumberMessage: fwhmMsg, tooLowMessage: fwhmMsg });
            fwhm.add(Validate.Presence, { failureMessage: fwhmMsg });

			var sigmaMsg = "Standard deviation must be a number greater than 0!";
            var sigma = new LiveValidation("sigma", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
            sigma.add(Validate.Numericality, { minimum: 0, notANumberMessage: sigmaMsg, tooLowMessage: sigmaMsg });
            sigma.add(Validate.Presence, { failureMessage: sigmaMsg });

            var minStepsMsg = "EIBPC combine steps must be an integer greater than 0!";
            var maxStepsMsg = "EIBPC combine steps must be an integer smaller than 10!";
            var steps = new LiveValidation("steps", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
            steps.add(Validate.Numericality, { minimum: 0, maximum: 10, onlyInteger: true, notANumberMessage: minStepsMsg, notAnIntegerMessage: minStepsMsg, tooLowMessage: minStepsMsg, tooHighMessage: maxStepsMsg });
            steps.add(Validate.Presence, { failureMessage: minStepsMsg });

			var maxPeaksMsg = "Maximum number of peaks per EIBPC must be an integer greater than 0!";
            var maxPeaks = new LiveValidation("max", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
            maxPeaks.add(Validate.Numericality, { minimum: 0, onlyInteger: true, notANumberMessage: maxPeaksMsg, notAnIntegerMessage: maxPeaksMsg, tooLowMessage: maxPeaksMsg });
            maxPeaks.add(Validate.Presence, { failureMessage: maxPeaksMsg });

			var mzdiffMsg = "Minimum m/z difference must be a number greater than 0 or \"auto\"";
			var mzdiff = new LiveValidation("mzdiff", { validMessage: " ", insertAfterWhatNode: "xcmsPeakValMsg", wait: 1000});
			mzdiff.add(Validate.Presence, { failureMessage: "Minimum m/z difference must be supplied" });
			mzdiff.add(Validate.Custom, { against : function(value,args) {
					if ((value<0 || value !== "auto") && value !== "") {
						return false
					} else {
						return true;
					}
				}, failureMessage: mzdiffMsg });
			
            break;
		case 'normalization.php':
            var mzMsg = "m/z tolerance must be a number between 0 and 1!";
            var massTol = new LiveValidation("mztol", { validMessage: " ", insertAfterWhatNode: "normGenValMsg", wait: 1000});
            massTol.add(Validate.Numericality, { minimum: 0, maximum: 1, notANumberMessage: mzMsg, tooLowMessage: mzMsg, tooHighMessage: mzMsg });
            massTol.add(Validate.Presence, { failureMessage: mzMsg });

            var timeSpanMsg = "LOESS span for time alignment must be a number between 0 and 1, or 0 for adaptive span!";
            var timeSpan = new LiveValidation("tspan", { validMessage: " ", insertAfterWhatNode: "normTimeValMsg", wait: 1000});
            timeSpan.add(Validate.Numericality, { minimum: 0, maximum: 1, notANumberMessage: timeSpanMsg, tooLowMessage: timeSpanMsg, tooHighMessage: timeSpanMsg });
			timeSpan.add(Validate.Presence, { failureMessage: timeSpanMsg });

			var rtIterMsg = "Algorithm iterations must be an integer greater than 0!";
            var rtIter = new LiveValidation("it", { validMessage: " ", insertAfterWhatNode: "normTimeValMsg", wait: 1000});
            rtIter.add(Validate.Numericality, { minimum: 0, onlyInteger: true, notANumberMessage: rtIterMsg, notAnIntegerMessage: rtIterMsg, tooLowMessage: rtIterMsg });
			rtIter.add(Validate.Presence, { failureMessage: rtIterMsg });

			var corrFacMsg = "Correction factor must be a number greater than 0!";
            var corrFac = new LiveValidation("corrfac", { validMessage: " ", insertAfterWhatNode: "normTimeValMsg", wait: 1000});
            corrFac.add(Validate.Numericality, { minimum: 0, notANumberMessage: corrFacMsg, tooLowMessage: corrFacMsg });
			corrFac.add(Validate.Presence, { failureMessage: corrFacMsg });

			var timeExclMsg = "Exclusion quantile in time alignment deviation must be a number between 0 and 1!";
            var timeExcl = new LiveValidation("cutq", { validMessage: " ", insertAfterWhatNode: "normTimeValMsg", wait: 1000});
            timeExcl.add(Validate.Numericality, { minimum: 0, maximum: 1, notANumberMessage: timeExclMsg, tooLowMessage: timeExclMsg, tooHighMessage: timeExclMsg });
			timeExcl.add(Validate.Presence, { failureMessage: timeExclMsg });
			
			var cutFacMsg = "Non-standards correction factor must be a number greater than 0!";
            var cutFac = new LiveValidation("cutrat", { validMessage: " ", insertAfterWhatNode: "normTimeValMsg", wait: 1000});
            cutFac.add(Validate.Numericality, { minimum: 0, notANumberMessage: cutFacMsg, tooLowMessage: cutFacMsg });
			cutFac.add(Validate.Presence, { failureMessage: cutFacMsg });
			
			var intSpanMsg = "LOESS span for intensity correction must be a number between 0 and 1!";
            var intSpan = new LiveValidation("ispan", { validMessage: " ", insertAfterWhatNode: "normIntValMsg", wait: 1000});
            intSpan.add(Validate.Numericality, { minimum: 0, maximum: 1, notANumberMessage: intSpanMsg, tooLowMessage: intSpanMsg, tooHighMessage: intSpanMsg });
			intSpan.add(Validate.Presence, { failureMessage: intSpanMsg });
			
            break;
    }
}

function initCalendar()
{
	var dates = $("#from, #to").datepicker(
	{
		defaultDate: "+1w",
		changeMonth: true,
		numberOfMonths: 1,
		onSelect: function(selectedDate)
		{
			var option = this.id == "from" ? "minDate" : "maxDate",
			instance = $(this).data("datepicker"),
			date = $.datepicker.parseDate(
				instance.settings.dateFormat ||
				$.datepicker._defaults.dateFormat,
				selectedDate, instance.settings);
			dates.not(this).datepicker("option",option,date);
		},
		onClose: function(dateText,inst)
		{
			if ($("#from").val() === '' || $("#to").val() === '')
			{
				disable(['fetchRunsButton']);
			}
			else { enable(['fetchRunsButton']); }
		}
	});
}

function toggleCalendar()
{
	$("#calendarContainer").toggle();
	$("#runParameters").hide();
	$("#runParameters").html("");
}

function checkMe(me)
{
	var i = 0;

	switch(me)
	{
		case 'classname':
			var toEnable = true;
			var n = $("#file_class").data("number");
			for (i=0; i<n; i++)
			{
				if ($("#classname_"+i).val() === "")
				{
					disable(['submit_to_xcms']);
					toEnable = false;
					break;
				}
			}
			if (toEnable)
			{
				enable(['submit_to_xcms']);
			}
			break;
		case 'rt_trunc':
			if ($("#do_rt_filter").is(":checked"))
			{
				enable(['min_time_trunc','max_time_trunc']);
			}
			else
			{
				disable(['min_time_trunc','max_time_trunc']);
			}
			break;
		case 'xcms_params':
			if ($("#default_xcms").is(":checked"))
			{	
				var save =
				{
					profstep: $("#profstep").val(),
					profmethod: $("#profmethod").val(),
					snthresh: $("#snthresh").val(),
					step: $("#step").val(),
					fwhm: $("#fwhm").val(),
					sigma: $("#sigma").val(),
					steps: $("#steps").val(),
					max: $("#max").val(),
					mzdiff: $("#mzdiff").val()
				}
				$("#custom_xcms").data("save",save);
				$("#profstep").val(1);
				$("#profmethod").val("binlin");
				$("#snthresh").val(7);
				$("#step").val(0.1);
				$("#fwhm").val(30);
				$("#sigma").val(7);
				$("#steps").val(3);
				$("#max").val(5);
				$("#mzdiff").val("auto");
				disable(['profstep','profmethod','snthresh','step','fwhm','sigma','steps','max','mzdiff']);
			}
			else if ($("#custom_xcms").is(":checked"))
			{
				var restore = $("#custom_xcms").data("save");
				if (!isEmpty(restore))
				{
					$("#profstep").val(restore.profstep);
					$("#profmethod").val(restore.profmethod);
					$("#snthresh").val(restore.snthresh);
					$("#step").val(restore.step);
					$("#fwhm").val(restore.fwhm);
					$("#sigma").val(restore.sigma);
					$("#steps").val(restore.steps);
					$("#max").val(restore.max);
					$("#mzdiff").val(restore.mzdiff);
				}
				enable(['profstep','profmethod','snthresh','step','fwhm','sigma','steps','max','mzdiff']);
			}
			break;
		case 'rt_refine':
			var n = $("#rt_diagnostics").data("number");
			for (i=0; i<n; i++)
			{
				$("#rt_more_check_"+i).is(":checked") ?
				enable(['rt_diag_min_'+i,'rt_diag_max_'+i]) :
				disable(['rt_diag_min_'+i,'rt_diag_max_'+i]);
			}
			break;
		case 'norm_params':
			if ($("#default_norm").is(":checked"))
			{	
				var save =
				{
					method: $("#method").val(),
					correctfor: $("#correctfor").val(),
					export: $("#export").val(),
					mztol: $("#mztol").val(),
					diagplot: $("#diagplot").is(":checked") ? true : false,
					tspan: $("#tspan").val(),
					it: $("#it").val(),
					corrfac: $("#corrfac").val(),
					cutrat: $("#cutrat").val(),
					cutq: $("#cutq").val(),
					ispan: $("#ispan").val(),
					normalize: $("#normalize").val()
				}
				$("#custom_norm").data("save",save);
				$("#method").val("geom");
				$("#mztol").val(0.01);
				$("#export").val("none");
				$("#diagplot").attr("checked","checked");
				$("#tspan").val(0);
				$("#it").val(3);
				$("#corrfac").val(2);
				$("#cutrat").val(2);
				$("#cutq").val(0.98);
				$("#ispan").val(0);
				$("#normalize").val("rlm");
				disable(['method','correctfor','export','mztol','diagplot',
						 'tspan','it','corrfac','cutrat','ispan','normalize','cutq']);
			}
			else if ($("#custom_norm").is(":checked"))
			{
				var restore = $("#custom_norm").data("save");
				if (!isEmpty(restore))
				{
					$("#method").val(restore.method);
					$("#correctfor").val(restore.correctfor);
					$("#export").val(restore.export);
					$("#mztol").val(restore.mztol);
					$("#diagplot").attr("checked",restore.diagplot);
					$("#tspan").val(restore.tspan);
					$("#it").val(restore.it);
					$("#corrfac").val(restore.corrfac);
					$("#cutrat").val(restore.cutrat);
					$("#cutq").val(restore.cutq);
					$("#ispan").val(restore.ispan);
					$("#normalize").val(restore.normalize);
				}
				enable(['method','correctfor','export','mztol','diagplot',
						'tspan','it','corrfac','cutrat','ispan','normalize','cutq']);
			}
			break;
		case 'search_params':
			if ($("#mz_radio").is(":checked"))
			{
				html = 
				"Please enter m/z range: " +
				"<label for=\"mz_low\">From:&nbsp;</label><input type=\"text\" id=\"mz_low\" name=\"mz_low\" size=8 style=\"background-color:#FFEFCC\"/>&nbsp;&nbsp;" +
				"<label for=\"mz_high\">To:&nbsp;</label><input type=\"text\" id=\"mz_high\" name=\"mz_high\" size=8 style=\"background-color:#FFEFCC\"/>&nbsp;&nbsp;" +
				"<button type=\"button\" id=\"searchButton\" onclick=\"searchMetabolites('range')\">Comply!</button>";
			}
			else if ($("#id_radio").is(":checked"))
			{
				html =
				"<table class=\"innerTable\" style=\"width:50%\">" +
				"<tr><td class=\"infoCell\" style=\"vertical-align:top; font-size:1.25em;\">Please enter metabolite ID: </td>" +
				"<td rowspan=2><textarea id=\"mz_id\" name=\"mz_id\" onkeyup=\"checkMe('search_fill')\"/></td>" +
				"<tr><td class=\"infoCell\" style=\"vertical-align:bottom; font-size:1.2em;\"><button type=\"button\" id=\"searchButton\" onclick=\"searchMetabolites('id')\" disabled>Comply!</button></td></tr>" +
				"</table>" +
				"<script>bindAutoComplete('mz_id')";
			}
			$("#searchOptsContainer").html(html);
			break;
		case 'search_fill':
			if ($("#mz_id").val() === "")
			{
				disable(['searchButton']);
			}
			else
			{
				enable(['searchButton']);
			}
			break;
		case 'search_fill_filter':
			if ($("#mz_id_filter").val() === "")
			{
				disable(['searchFilterButton']);
			}
			else
			{
				enable(['searchFilterButton']);
			}
			break;
	}
}

function reset(caller,opts)
{
	switch(caller)
	{
		case 'cinnamoned.php':
			$("#project_name").val("");
			$("#do_rt_filter").attr("checked","checked");
			$("#min_time_trunc").val(300);
			$("#max_time_trunc").val(3000);
			$("#queueCDF").html("");
			$("#queueClass").html("");
			$("#profstep").val(1);
			$("#profmethod").val("binlin");
			$("#snthresh").val(7);
			$("#step").val(0.1);
			$("#fwhm").val(30);
			$("#sigma").val(7);
			$("#steps").val(3);
			$("#max").val(5);
			$("#mzdiff").val("auto");
			$("#file_class")
			.html("")
			.removeData("validators")
			.data("number",0);
			disable(['submit_to_xcms']);
			enable(['min_time_trunc','max_time_trunc']);
			$("#custom_xcms").removeData("save");
			$(".LV_validation_message").empty();
			break;
		case 'timefilter.php':
			var n = $("#rt_diagnostics").data("number");
			for (i=0; i<n; i++)
			{
				$("#rt_more_check_"+i).attr("checked",false)
				$("#rt_diag_min_"+i).val(opts.min);
				$("#rt_diag_max_"+i).val(opts.max);
				disable(['rt_diag_min_'+i,'rt_diag_max_'+i]);
			}
			$(".LV_validation_message").empty();
			break;
		case 'normalization.php':
			$("#method").val("geom");
			$("#mztol").val(0.1);
			$("#export").val("none");
			$("#diagplot").attr("checked","checked");
			$("#tspan").val(0);
			$("#it").val(3);
			$("#corrfac").val(2);
			$("#normalize").val("rlm");
			$("#cutq").val(0.98);
			$("#cutrat").val(2);
			$("#ispan").val(0);
			$("#normGenValMsg").html("");
			$("#normTimeValMsg").html("");
			$("#normIntValMsg").html("");
			$("#custom_norm").removeData("save");
			$(".LV_validation_message").empty();
			break;
	}
}

function disable(id)
{
    for (var item in id)
    {        
        $("#"+id[item]).attr("disabled",true);
    }
}

function enable(id)
{
    for (var item in id)
    {
        $("#"+id[item]).attr("disabled",false);
    }
}

function isEmpty(x)
{
	decision =  x === null || x === undefined || x === "" ? true : false;
	return(decision);
}

function startTimer(what,dir)
{
	var x = dir;
	var toSend = {};
	var interval = 0;
	if (T != null)
	{
		window.clearInterval(T);
		T = null;
	}
	if (what === 'detection')
	{
		toSend = { xcms_progress: x };
		interval = 5000;
	}
	else if (what === 'normalization')
	{
		toSend = { norm_progress: x };
		interval = 1000;
	}
	
	T = window.setInterval(function()
	{
		urlBase = initMe();
		$.ajax(
		{
			type: 'POST',
			url: urlBase+'php/getprogress.php',
			data: toSend,
			success: function(data)
			{
				if (!$.isEmptyObject(data))
				{
					$("#progressDisplay").html(data);
					$("#progressDisplay").scrollTop($("#progressDisplay").prop("scrollHeight"));
				}
				else
				{
					$("#progressDisplay").html("I don't receive data<br/>");
				}
			},
			error: function(data,error)
			{												
				displayError('Ooops! ' + error + ' ' + data.responseText);						
			},
			dataType: "json"
		});
	},interval);
}

function endTimer()
{
	if (T != null) 
	{
		window.clearInterval(T);
		T = null;
	}
}

function initMe()
{
	return "http://localhost:81/";
}

function displayError(error)
{				
	$('#errorContainer').html(error);
	$('#errorContainer').show("fast");
}

function hideError()
{
	$('#errorContainer').html("fast");
	$('#errorContainer').text('');
}

function bindAutoComplete(id)
{
	var urlBase = initMe(); 
		
	$('#'+id)
	.bind("keydown",function(event)
	{			
		if (event.keyCode === $.ui.keyCode.TAB && $(this).data("autocomplete").menu.active ) 
		{ event.preventDefault(); }
	})
	.autocomplete(
	{		
		source: function(request,response)
		{
			$.ajax(
			{
				type: 'POST',
				url: urlBase+'php/qdb.php',
				data: { suggest_term: extractLast(request.term) },
				success: function(data)
				{						
					var sugs = [];
					for (var key in data)
					{
						sugs.push(data[key]);
					}					
					response(sugs);
				},
				error: function(data,error)
				{												
					displayError('Ooops! ' + error + ' ' + data.responseText);						
				},
				dataType: "json"
			});
		},
		search: function() 
		{
			// custom minLength
			var term = extractLast(this.value);
			if (term.length < 2) 
			{
				return false;
			}
		},
		focus: function() { return false; },
		select: function(event,ui)
		{
			var terms = split(this.value);
			terms.pop();
			terms.push(ui.item.value);
			terms.push("");
			this.value = terms.join("\n");
			return false;
		}
	});
}

function bindUnload()
{
	window.onbeforeunload = function()
	{
		return("You are about to navigate away from this window without saving." +
			   "\nYour current analysis will be discarded!" +
			   "\nAre you sure? (this action cannot be undone!)" +
			   "\n(ignore this message if the analysis completed with errors)");
	}

	window.onunload = function()
	{
		if ($("#run_discarder").attr("onclick") != undefined && $("#run_discarder").attr("onclick") != null)
		{
			discardRun();
		}
	}
}

function split(val) { return val.split(/\n/); }

function extractLast(term) { return split(term).pop(); }

/*function getProgress(what,dir)
{
	var urlBase = initMe();

	switch(what)
	{
		case 'detection':
			$.ajax(
			{
				type: 'POST',
				url: urlBase+'php/getprogress.php',
				beforeSend: function() { $("#progressDisplay").show("slow"); },
				data: { xcms_progress: dir },
				success: function(data)
				{
					if (!$.isEmptyObject(data))
					{
						$("#progressDisplay").html(data);
					}
					else
					{
						$("#progressDisplay").html("I don't receive data<br/>");
					}
				},
				error: function(data,error)
				{												
					displayError('Ooops! ' + error + ' ' + data.responseText);						
				},
				dataType: "json"
			});
			break;
		case 'normalization':
			break;
	}
}

function bindUnload()
{
	window.onbeforeunload = function()
	{
		if (!$("#saveInfo").data("success"))
		{
			var del = confirm("You are about to navigate away from this window without saving." +
							  "\nYour current analysis will be discarded!" +
							  "\nAre you sure? (this action cannot be undone!)" +
							  "\n(ignore this message if the analysis completed with errors)");
			if (del) { discardRun(); }
			else { return false; }
		}
		else  { return false; }
	}
}*/
