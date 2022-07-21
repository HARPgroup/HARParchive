#!/bin/bash
echo -n 'land segment:'
read land_segment
echo -n 'land use:'
read landuse
echo -n 'scenario:'
read scenario_name
h5_file_path=/opt/model/p53/p532c-sova/output/hspf/land/out/$landuse/$scenario_name/$landuse$land_segment.h5
output_file_path=/opt/model/p53/p532c-sova/output/hspf/land/out/$landuse/$scenario_name/eos/$landuse$land_segment'_pwater.csv'
data_source_table=/RESULTS/PERLND_P001/PWATER/table
image_file_path=/media/model/p532/out/land/hsp2_2022/images

if [ ! -d /opt/model/p53/p532c-sova/output/hspf/land/out/$landuse/$scenario_name/eos ] 
 then mkdir /opt/model/p53/p532c-sova/output/hspf/land/out/$landuse/$scenario_name/eos ; fi

echo 'h5_file_path:' $h5_file_path
echo 'output_file_path:' $output_file_path
echo 'data_source_table:' $data_source_table
echo 'land_segment:' $land_segment
echo 'scenario_name:' $scenario_name
echo 'landuse:' $landuse
echo 'image_file_path:' $image_file_path

Rscript HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_file_path $data_source_table

Rscript HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_pwater.R $land_segment $scenario_name $landuse $output_file_path $image_file_path

Rscript HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_pwater_stats.R $land_segment $scenario_name $landuse $output_file_path 
$image_file_path
