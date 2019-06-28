#!/bin/bash

pushd . >& /dev/null

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
declare -a OTHER_DIRS_TO_BACKUP=()

dat=`date +"%d.%m.%Y_%H.%M"`
if [ "$1" != "" ]; then
	dat+="_""$1"
fi
filn=${dat}".tgz"

outf=".BACKUP/${filn}"
CMD='tar cvz --exclude .BACKUP --exclude blib --exclude data --exclude states -f "${outf}" .'
eval $CMD > /dev/null
if [ $? -ne 0 ]; then echo "$0 : cal to tar command '$CMD' has failed"; exit 1; fi
popd >& /dev/null


for adir in "${OTHER_DIRS_TO_BACKUP[@]}"; do
	pushd . >& /dev/null
	echo "$0 : backing up in dir '${adir}'"
	cd "${adir}"
	bin/backup.bash $* > /dev/null
	if [ $? -ne 0 ]; then echo "$0 : could not backup in '${adir}', skipping this ..."; continue; fi
	popd >& /dev/null
done

echo "$0 : done backup in '${outf}'"

