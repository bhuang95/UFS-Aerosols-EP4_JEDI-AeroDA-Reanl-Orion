#!/bin/bash

set -x
RUNDIR=/work/noaa/gsd-fv3-dev/bhuang/expRuns/UFS-Aerosols_RETcyc/AeroReanl/
EXPS="
AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201801
AeroReanl_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v15_0dz0dp_41M_C96_202007
AeroReanl_EP4_FreeRun_NoSPE_YesSfcanl_v14_0dz0dp_1M_C96_201801
AeroReanl_EP4_FreeRun_NoSPE_YesSfcanl_v15_0dz0dp_1M_C96_202007
"

FLDS="
dr-data
dr-data-backup
"

RCPRE=record.failed_GLBUS2NIAG
tarstat="+ exit 0"
glbstat="+ exit 0"
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
    ls ${RCPRE}-?????????? | awk -F "-" '{print $2}' > ${H2NDIR}/GLBUS_FAILED_RESUBMIT.log
    echo ${RUNDIR}/${EXP}/${FLD}
    for CDATE in $(cat ${H2NDIR}/GLBUS_FAILED_RESUBMIT.log); do
        if [ -f ${H2NDIR}/${RCPRE}-${CDATE} -a -f ${H2NDIR}/${RCPRE}-${CDATE}-HERCULES ]; then
	    mkdir -p ${H2NDIR}/resubmit.record
	    cd ${H2NDIR}/${CDATE}
            tarstat_chk=$(tail -n 1 hera2hpss.out)
            glbstat_chk=$(tail -n 1 glbus2niag.out)
            if [ "${tarstat_chk}" = "${tarstat}" ]; then
	    if [ "${glbstat_chk}" != "${glbstat}" ]; then
	        mv ${H2NDIR}/${RCPRE}-${CDATE} ${H2NDIR}/resubmit.record/
	        echo "Resubmit- ${EXP}-${FLD}-${CDATE}"
/opt/slurm/bin/sbatch ${GLBUSJOB}
            fi
	    fi
	else
	    echo "${CDATE}: Resubmiting failed hercules globus job was attempted or ongoing"
	fi
    done
done # FLD
done # EXP
exit 0
