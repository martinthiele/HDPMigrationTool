#!/bin/bash 
source ./upgrade-env.sh
#-Dprocess.name=stop-services-`date +"%H-%M-%S"`
java  $JAVA_OPTS  -Dprocess.name=manage-services com.hortonworks.hdp.migration.ServiceManager stop $@