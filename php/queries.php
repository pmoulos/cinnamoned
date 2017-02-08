<?php
$info_all = 'SELECT `run_id`,`project_name`,`date` '.
			'FROM `run_info` '.
			'ORDER BY `date`';

$info_dates_1 = 'SELECT `run_id`,`project_name`,`date` '.
				'FROM `run_info` '.
				'WHERE `date`>=';
$info_dates_2 = ' AND `date`<=';
$info_dates_3 = ' ORDER BY `date`';

$info_params = 'SELECT `ref_run_id`,`xcms_filter_do`,`xcms_filter_min`,`xcms_filter_max`,'.
			   '`xcms_read_profstep`,`xcms_read_profmethod`,`xcms_find_snthresh`,`xcms_find_step`,'.
			   '`xcms_find_fwhm`,`xcms_find_sigma`,`xcms_find_steps`,`xcms_find_max`,`xcms_find_mzdiff`,'.
			   '`norm_method`,`norm_tol`,`norm_correctfor`,`norm_export`,`norm_diagplot`,`norm_tspan`,`norm_tit`,'.
			   '`norm_corrfac`,`norm_cutq`,`norm_normalize`,`norm_ispan`,`norm_cutrat`,`norm_times` '.
			   'FROM `run_parameters` '.
			   'WHERE `ref_run_id`=';

$class_data = 'SELECT `project_name`,`project_path`,`class_file` '.
			  'FROM `run_info` '.
			  'WHERE `run_id`=';

$diag_data = 'SELECT `project_name`,`project_path`,`diagnostic_normalization_path`,`class_file` '.
			 'FROM `run_info` '.
			 'WHERE `run_id`=';

$res_data = 'SELECT `project_path` '.
			'FROM `run_info` '.
			'WHERE `run_id`=';

$auto_metab_1 = 'SELECT `id` '.
				'FROM `peak_info` '.
				'WHERE `id` LIKE ';
$auto_metab_2 = 'ORDER BY `id`';

$metab_by_range_1 = 'SELECT `id`,`mz`,`rt`,`real_mass` '.
					'FROM `peak_info` '.
					'WHERE `mz`>=';
$metab_by_range_2 = ' AND `mz`<=';
$metab_by_range_3 = ' ORDER BY `mz`';

$metab_by_id_1 = 'SELECT `id`,`mz`,`rt`,`real_mass` '.
				 'FROM `peak_info` '.
				 'WHERE `id` IN ';
$metab_by_id_2 = ' ORDER BY `mz`';
				  
$metab_info = 'SELECT `id`,`mz`,`rt`,`mzmin`,`mzmax`,`rtmin`,`rtmax`,`isotopes`,`adduct`,`real_mass`,`prop_formula`,`theor_mass`,'.
			  '`summarized_intensity_geom`,`summarized_intensity_rlm`,`summarized_intensity_both`,`is_geom`,`is_rlm`,`is_both` '.
			  'FROM `peak_info` '.
			  'WHERE `id`=';
$metab_an_1 = 'SELECT `hmdb_id`,`hmdb_formula`,`hmdb_name`,`kegg_id`,`kegg_formula`,`kegg_name`,`chebi_id`,`chebi_formula`,`chebi_name` '.
			  'FROM `meta_data` '.
			  'WHERE ABS(`real_mass`-';
$metab_an_2 = ')<=0.00000001';

$delete_run = 'DELETE FROM `run_info` '.
			  'WHERE `run_id`=';
?>
