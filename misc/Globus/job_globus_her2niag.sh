#!/bin/bash

set -x
RUNDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/
EXPS="
AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201801
AeroReanl_EP4_FreeRun_NoSPE_YesSfcanl_v14_0dz0dp_1M_C96_201801
"

FLDS="
dr-data
dr-data-backup
"

RCPRE=record.failed_GLBUS2NIAG
GLBUSJOB=sbatch_glbus2niag_ret.sh
for EXP in ${EXPS}; do
for FLD in ${FLDS}; do
    if [ ${FLD} = "dr-data" ]; then
        GLBUSJOB=sbatch_glbus2niag_ret.sh
    elif [ ${FLD} = "dr-data-backup" ]; then
        GLBUSJOB=sbatch_glbus2niag_diag.sh
    else
        echo "No globus script matches and exit now"
	exit 100
    fi
    H2NDIR=${RUNDIR}/${EXP}/${FLD}/HERA2HPSS
    cd ${H2NDIR}
    ls ${RCPRE}-?????????? | awk -F "-" '{print $2}' > ${H2NDIR}/GLBUS_FAILED.log
    for CDATE in $(cat ${H2NDIR}/GLBUS_FAILED.log); do
        if [ ! -f ${H2NDIR}/${RCPRE}-${CDATE}-HERCULES ]; then
	    mv ${H2NDIR}/${RCPRE}-${CDATE} ${H2NDIR}/${RCPRE}-${CDATE}-HERCULES
	    cd ${H2NDIR}/${CDATE}
/opt/slurm/bin/sbatch ${GLBUSJOB}
#echo "/opt/slurm/bin/sbatch ${GLBUSJOB}"
	else
	    echo "${CDATE}: Resubmiting failed hercules globus job was attempted or ongoing"
	fi
    done
done # FLD
done # EXP
exit 0
