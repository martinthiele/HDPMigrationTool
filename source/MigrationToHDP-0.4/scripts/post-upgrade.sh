#!/bin/bash 
source ./upgrade-env.sh
#-Dprocess.name=post-upgrade-`date +"%H-%M-%S"`
java $JAVA_OPTS -Dprocess.name=post-upgrade com.hortonworks.hdp.migration.UpgradeProcessor post-upgrade $@