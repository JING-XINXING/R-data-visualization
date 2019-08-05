#!/usr/bin/bash
date
## nohup
Rscript volcano_v2.R --project_path "./" \
			--fc 1.2 \
## end
if ls | grep '*pdf'

then
	echo "Congratulations. The picture has been drawn"
else
	echo "Bad, please check the contents and format of the prepared file"

fi
