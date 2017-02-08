<?php
/*
Uploadify v2.1.4
Release Date: November 8, 2010

Copyright (c) 2010 Ronnie Garcia, Travis Nickels

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

/* Several modifications made by Panagiotis Moulos in order to make the script
suitable for the cinnamoned application */

session_id($_POST['session_id']);
$id = session_id($_POST['session_id']);
session_start();
$_SESSION['session']['class_file'] = "";

if (!empty($_FILES))
{
    $targetPath = $_SESSION['session']['project_path'];
    if (!file_exists($targetPath)) 
    { 
        mkdir($targetPath,$mode=0777,$recursive=TRUE); 
    }
    
    if (!empty($_FILES['netcdf_files']))
    {
        $tempFile = $_FILES['netcdf_files']['tmp_name'];
        $dataPath = $_SESSION['session']['data_raw_path'];
        if (!file_exists($dataPath))
		{ 
			mkdir($dataPath,$mode=0777,$recursive=TRUE); 
		}
        $targetFile =  str_replace('//','/',$dataPath).$_FILES['netcdf_files']['name'];
    }
    if (!empty($_FILES['class_file']))
    {
        $tempFile = $_FILES['class_file']['tmp_name'];
        $targetFile =  str_replace('//','/',$targetPath).$_FILES['class_file']['name'];
        $_SESSION['session']['class_file'] = $targetFile;
    }
    
    move_uploaded_file($tempFile,$targetFile);
    //echo str_replace($_SERVER['DOCUMENT_ROOT'],'',$targetFile);
    echo $targetFile;

	/*$msg = $_SESSION['config']['base_path']."\n".
		   $_SESSION['session']['project_path']."\n".
		   $_SESSION['session']['data_raw_path']."\n".
		   $id."\n".
		   $targetFile;
	$fh = fopen("/media/HD3/cprocess/report.txt","w");
	fwrite($fh,$msg);
	fclose($fh);*/
		   
}
?>
