#!/bin/sh

Args=$@
if [[ $Args == "" ]]
then
	Args="-nbt 4"
fi
ozc -x -o exe ozproject.oz && ./exe $Args
