#!/bin/bash 
# Absolute path to this script, e.g. /home/user/bin/foo.sh
#SCRIPT=`readlink -f $0`
# Absolute path this script is in, thus /home/user/bin
SCRIPTPATH=`dirname $0`
#echo $SCRIPTPATH
cd $SCRIPTPATH/..


CLASSPATH=$CLASSPATH:./lib:

for f in ./*.jar
do 
	CLASSPATH=$CLASSPATH:$f
done


for f in ./lib/*.*
do 
	CLASSPATH=$CLASSPATH:$f
done


#echo $CLASSPATH

JAVA_OPTS="-Xms512m -Xmx4g -Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,address=1044,server=y,suspend=n -cp $CLASSPATH -Djsch.tracking=enabled -Djsch.max.unauthenticated.connections=100 -Dnumber.of.paraller.commands=10"

