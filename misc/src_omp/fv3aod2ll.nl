&record_input
 date="2018041406"
 input_grid_dir="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/fix_fv3/C96"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/fv3_sample"
 fname_fv3="20180417.120000.fv_aod.res.tile?.nc"
/
&record_interp
 dlon=0.5
 dlat=0.5
/
&record_output
 output_dir="."
 fname_aod_ll="fv3_test_aod.nc"
/
