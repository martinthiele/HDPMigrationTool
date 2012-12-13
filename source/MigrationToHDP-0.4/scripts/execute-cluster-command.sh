#!/bin/bash 
source ./upgrade-env.sh
#-Dprocess.name=post-upgrade-`date +"%H-%M-%S"`
echo $1
echo $2
echo $3
java $JAVA_OPTS  -Djsch.tracking=enabled -Djsch.max.unauthenticated.connections=100 -Dnumber.of.paraller.commands=100 -Dprocess.name=misc-services com.hortonworks.hdp.migration.ssh.CommandExecutor $1 "$2" $3