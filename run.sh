#!/bin/sh

if [[ "$#" -ne 1 ]]; then
	echo "need to specify a dir to mount"
	exit 1
fi

docker run --rm -v $(pwd)/$1:/$1 -it cs381 /bin/bash
