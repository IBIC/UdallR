#!/bin/bash

if [[ ! $1 =~ ^([0-9]{2}_){2}[0-9]{2}$ ]]
then
    echo "Incorrect format: expected NN_NN_NN"
    exit 1
fi

sed -i -r "s/(panuc_multivis_20)([0-9]{2}_){2}[0-9]{2}/\1$1/g" \
    R/udallCleanREDCapDataWide.R getClosestVisits.R

echo "Now run: Rscript 4_getClosestVisits.R and upload"