#!/bin/bash 
source ./upgrade-env.sh
#-Dprocess.name=pre-upgrade-`date +"%H-%M-%S"`
java $JAVA_OPTS   -Dprocess.name=pre-upgrade   com.hortonworks.hdp.migration.UpgradeProcessor pre-upgrade $@