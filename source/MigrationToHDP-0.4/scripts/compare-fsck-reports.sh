#!/bin/bash 
source ./upgrade-env.sh

#-Dprocess.name=upgrade-validation-`date +"%H-%M-%S"`
java $JAVA_OPTS -Dprocess.name=upgrade-validation  com.hortonworks.hdp.migration.HDFSFsckReportComparison $@