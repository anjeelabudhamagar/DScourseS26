#!/bin/sh

# (a) Download the sample CSV from the URL
wget http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip

# (b) List files to see the downloaded zip
ls

# (c) Unzip the file
unzip -o FL_insurance_sample.csv.zip

# (d) Remove unnecessary MacOS folder and the zip file
rm -rf __MACOSX
rm -f FL_insurance_sample.csv.zip

# (e) Check the file size
ls -al --block-size=MB FL_insurance_sample.csv

# (f) View the first 5 lines of the CSV
head -5 FL_insurance_sample.csv

# (g) Count the number of lines (before EOL conversion)
wc -l FL_insurance_sample.csv

# (h) Fix end-of-line (Mac -> Linux)
dos2unix -c mac FL_insurance_sample.csv

# (i) View the first 5 lines again after conversion
head -5 FL_insurance_sample.csv

# (j) Count the number of lines again after conversion
wc -l FL_insurance_sample.csv








