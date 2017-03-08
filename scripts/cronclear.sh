# Remove raw files, compress the project directory and remove the files in the archive

ROOT_PATH="/media/HD3/cprocess"
APP_PATH="/media/raid/www/cinnamoned"

cd $ROOT_PATH
for i in *
do
	if [ -d $i ]
	then
		TAR_NAME=$i".tar.gz"
		DATA_DIR=./data
		if [ -d $APP_PATH/tmp/$i ]
		then
			TMP_DIR=$APP_PATH/tmp/$i/
		else
			TMP_DIR=""
		fi
		cd $i
		if [ ! -f $TAR_NAME ]
		then
			echo "---"
			echo "tar name to be created = "$TAR_NAME
			echo "data directory = "$DATA_DIR
			echo "temp directory = "$TMP_DIR
			echo "---"
			RM_COMMAND="rm -r "$DATA_DIR" "$TMP_DIR
			TAR_COMMAND="tar -cvzf "$TAR_NAME" "./*" --remove-files"
			echo "rm command to be run = "$RM_COMMAND
			echo "tar command to be run = "$TAR_COMMAND
			echo "==="		
			$RM_COMMAND
			$TAR_COMMAND
		fi
		cd ..
	fi
done
