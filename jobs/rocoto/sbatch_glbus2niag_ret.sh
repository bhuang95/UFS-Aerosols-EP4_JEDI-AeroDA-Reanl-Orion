#!/bin/bash --login
#SBATCH -J hera2hpss
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 07:59:00
#SBATCH -p service
#SBATCH -D ./
#SBATCH -o ./glbus2niag.out
#SBATCH -e ./glbus2niag.out

set -x
# Back up cycled data to HPSS at ${CDATE}-6 cycle

module load python/3.7.5
source config_hera2hpss

NDATE=${NDATE:-"/home/bohuang/Workflow/UFS-Aerosols_NRTcyc/UFS-Aerosols-EP4_JEDI-AeroDA-Reanl-Orion/misc/ndate/ndate"}

NCP="/bin/cp -r"
NMV="/bin/mv -f"
NRM="/bin/rm -rf"
NLN="/bin/ln -sf"

GDATE=$(${NDATE} -${CYCINTHR} ${CDATE})
RMDATE=${GDATE}
RMDIR=${TMPDIR}/../${RMDATE}
RMREC=${RMDIR}/remove.record
#if ( grep YES ${RMREC} ); then
#	${NRM} ${RMDIR}
#fi

CY=${CDATE:0:4}
CM=${CDATE:4:2}
CD=${CDATE:6:2}
CH=${CDATE:8:2}
CYMD=${CDATE:0:8}

GY=${GDATE:0:4}
GM=${GDATE:4:2}
GD=${GDATE:6:2}
GH=${GDATE:8:2}
GYMD=${GDATE:0:8}


CNTLDIR=${ROTDIR}/gdas.${GYMD}/${GH}
ENKFDIR=${ROTDIR}/enkfgdas.${GYMD}/${GH}
DATAHPSSDIR=${ARCHHPSSDIR}/${PSLOT}/dr-data/${GDATE}
DATANIAGDIR=${ARCHNIAGDIR}/${PSLOT}/dr-data/${GDATE}

ICNT=0
GLBUSREC=Globus_o2n_${GDATE}.record
cd ${DATAHPSSDIR}
TARFILES=$(ls *.tar)
ERR=$?
ICNT=$((${ICNT}+${ERR}))

cd ${TMPDIR}
GINPUT=GlobusInput.out
GID=GlobusID_${GDATE}.out
GID2=GlobusID_${GDATE}_YES.out
[[ -f ${GINPUT} ]] && ${NRM} ${GINPUT}
for TARFILE in ${TARFILES};do
    echo "${TARFILE}    ${TARFILE}" >> ${GINPUT}
done

globus transfer --notify failed,inactive ${ORIONEP}:${DATAHPSSDIR} ${NIAGEP}:${DATANIAGDIR}  --batch ${GINPUT} >& ${GID}
ERR=$?
ICNT=$((${ICNT}+${ERR}))
GLBUSID=$(tail -n 1 ${GID} | awk '{print $3}')
globus task wait "${GLBUSID}"
ERR=$?
ICNT=$((${ICNT}+${ERR}))

if [ ${ICNT} -eq 0 ]; then
    echo "TAR and GLOBUS is successful at ${GDATE}"
    echo "SUCCESSFUL" > ${DATAHPSSDIR}/${GLBUSREC}
    globus transfer --notify failed,inactive ${ORIONEP}:${DATAHPSSDIR}/${GLBUSREC} ${NIAGEP}:${DATANIAGDIR}/${GLBUSREC} >& ${GID2}
    ERR=$?
    ICNT=$((${ICNT}+${ERR}))
    GLBUSID=$(tail -n 1 ${GID2} | awk '{print $3}')
    globus task wait "${GLBUSID}"
    ERR=$?
    ICNT=$((${ICNT}+${ERR}))

if [ ${ICNT} -eq 0 ]; then
    ${NRM} ${CNTLDIR}
    if [ ${AERODA} = "YES" -o ${ENSRUN} = "YES" ]; then
        ${NRM} ${ENKFDIR}
    fi
    echo "YES" > ${TMPDIR}/remove.record

    ${NRM} ${DATAHPSSDIR}
else
    echo "Globus failed at ${GDATE}" >> ${GLBUSRECORD}
    exit ${ICNT}
fi
else
    echo "Globus failed at ${GDATE}" >> ${GLBUSRECORD}
    exit ${ICNT}
fi

exit ${ICNT}
