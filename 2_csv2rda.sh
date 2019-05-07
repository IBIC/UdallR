#!/bin/bash

projecthome=~/UdallR

while getopts ":f" opt ; do
    case ${opt} in
        f)
            echo "Will overwrite multivis"
            shift 1
            FORCE=yes ;;
        \?)
            echo "Invalid option: -${OPTARG}" ;;
    esac
done

newdate=${1}

if [[ ${FORCE} == "yes" ]] ; then
    rm -f ${projecthome}/data/panuc_multivis_20${newdate}.rda
fi

# Check input for correct format
if [[ ! ${newdate} =~ ^([0-9]{2}_){2}[0-9]{2}$ ]] ; then
    echo "Error: Invalid date format: ${newdate}"
    echo "Expecting: YY_MM_DD"
    exit 1
fi

if [[ ! -e ${projecthome}/data/panuc_multivis_20${newdate}.csv ]] ; then
    # Check that the csv needed exists; exit if it doesn't.
    echo "Error: ${projecthome}/data/panuc_multivis_20${newdate}.csv" \
        "does not exist"
    exit 1
else
    if [[ -e ${projecthome}/data/panuc_multivis_20${newdate}.rda ]] ; then
        # Don't run the code if the output rda file aready exists.
        echo "${projecthome}/data/panuc_multivis_20${newdate}.rda" \
                    "already exists."
        echo "Run ${projecthome}/update-multivis ${newdate} to update."
    else
        # Use R to convert csv to rda
        R --no-save --slave <<-EOF

            panuc_multivis_20${newdate} <- \
                read.csv("data/panuc_multivis_20${newdate}.csv")

            cat("Loaded:", dim(panuc_multivis_20${newdate}), fill = TRUE)

            save(panuc_multivis_20${newdate}, \
                    file = "data/panuc_multivis_20${newdate}.rda")
EOF

    # The next step is to update the relevant files.
    echo "Now run:"
    echo "${projecthome}/3_update-multivis.sh ${newdate}"

    fi
fi

