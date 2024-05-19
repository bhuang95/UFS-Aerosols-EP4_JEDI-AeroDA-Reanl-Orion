#!/bin/bash

#SJOB=sbatch_glbus2niag_ret.sh
SJOB=sbatch_glbus2niag_diag.sh
TOPDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/AeroReanl_EP4_FreeRun_NoSPE_YesSfcanl_v15_0dz0dp_1M_C96_202007/dr-data-backup/HERA2HPSS

#TOPDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201801/dr-data/HERA2HPSS/
#TOPDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v15_0dz0dp_41M_C96_202007/dr-data/HERA2HPSS
#TOPDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201801/dr-data/HERA2HPSS

#SJOB=sbatch_glbus2niag_diag.sh
#TOPDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v15_0dz0dp_41M_C96_202007/dr-data-backup/HERA2HPSS
#TOPDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201801/dr-data-backup/HERA2HPSS/



RECDIR=${TOPDIR}/resubmit.record


CYCS="
2021030212
2021030218
2021030406
2021030600
2021030606
2021030706
2021030806
2021030906
2021030918
2021031012
2021031018
2021031100
"

[[ ! -d ${RECDIR} ]] && mkdir -p ${RECDIR}

for CYC in ${CYCS}; do
    if [ -f ${TOPDIR}/record.failed_GLBUS2NIAG-${CYC} ]; then
	echo ${CYC}
        BAKDIR=${TOPDIR}/${CYC}
        cd ${BAKDIR}
/opt/slurm/bin/sbatch ${SJOB}
        ERR=$?
        if [ ${ERR} -eq 0 ]; then
            mv ${TOPDIR}/record.failed_GLBUS2NIAG-${CYC} ${RECDIR}/
        fi
    fi
done
