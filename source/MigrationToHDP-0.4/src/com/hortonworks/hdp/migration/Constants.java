/*
 * 
 */
package com.hortonworks.hdp.migration;

// TODO: Auto-generated Javadoc
/**
 * The Class Constants.
 */
public class Constants {

	/** The Constant ACTION_RESTART. */
	public static final String ACTION_RESTART = "restart";

	/** The Constant ACTION_START. */
	public static final String ACTION_START = "start";

	/** The Constant ACTION_STOP. */
	public static final String ACTION_STOP = "stop";

	/** The Constant COMPONENT_FLUME. */
	public static final String COMPONENT_FLUME = "flume";

	/** The Constant COMPONENT_GANGLIA. */
	public static final String COMPONENT_GANGLIA = "ganglia";

	/** The Constant COMPONENT_HBASE. */
	public static final String COMPONENT_HBASE = "hbase";

	/** The Constant COMPONENT_HCATALOG. */
	public static final String COMPONENT_HCATALOG = "hcatalog";

	/** The Constant COMPONENT_HDFS. */
	public static final String COMPONENT_HDFS = "hdfs";

	/** The Constant COMPONENT_HIVE. */
	public static final String COMPONENT_HIVE = "hive";

	/** The Constant COMPONENT_MAP_REDUCE. */
	public static final String COMPONENT_MAP_REDUCE = "mapreduce";

	/** The Constant COMPONENT_NAGIOS. */
	public static final String COMPONENT_NAGIOS = "nagios";

	/** The Constant COMPONENT_OOZIE. */
	public static final String COMPONENT_OOZIE = "oozie";

	/** The Constant COMPONENT_PIG. */
	public static final String COMPONENT_PIG = "pig";

	/** The Constant COMPONENT_SQOOP. */
	public static final String COMPONENT_SQOOP = "sqoop";

	/** The Constant COMPONENT_TEMPLETON. */
	public static final String COMPONENT_TEMPLETON = "templeton";

	/** The Constant COMPONENT_ZOOKEEPER. */
	public static final String COMPONENT_ZOOKEEPER = "zookeeper";

	/** The Constant DEFAULT_DIR. */
	public static final String DEFAULT_DIR = "default";

	/** The Constant DEFAULT_THREAD_COUNT. */
	public static final String DEFAULT_NUMBER_OF_PARALLEL_COMMANDS = "10";

	/** The Constant HOST_NAME_PLACEHOLDER. */
	public static final String HOST_NAME_PLACEHOLDER = "HOSTNAME_PLACEHOLDER";

	/** The Constant MERGED_DIR. */
	public static final String MERGED_DIR = "merged";

	/** The Constant MIGRATION_PROPERTY_COMMON_BACKUP_CONFIG_FILES. */
	public static final String MIGRATION_PROPERTY_COMMON_BACKUP_CONFIG_FILES = "common.backup.config.files";

	/** The Constant MIGRATION_PROPERTY_COMMON_BACKUP_INSTALLATION_DIR. */
	public static final String MIGRATION_PROPERTY_COMMON_BACKUP_INSTALLATION_DIR = "common.backup.installation.dir";

	/** The Constant MIGRATION_PROPERTY_HBASE_BACK_UP_DATA_ON_HDFS. */
	public static final String MIGRATION_PROPERTY_HBASE_BACK_UP_DATA_ON_HDFS = "hbase.backup.data.on.hdfs";

	/** The Constant MIGRATION_PROPERTY_HBASE_COMPACT_TABLES. */
	public static final String MIGRATION_PROPERTY_HBASE_COMPACT_TABLES = "hbase.compact.tables";

	/** The Constant MIGRATION_PROPERTY_HBASE_EXPORT_DATA_ON_LOCAL_FS. */
	public static final String MIGRATION_PROPERTY_HBASE_EXPORT_DATA_ON_LOCAL_FS = "hbase.export.data.on.local.fs";

	/** The Constant MIGRATION_PROPERTY_HDFS_AUTO_UPGRADE_ENABLED. */
	public static final String MIGRATION_PROPERTY_HDFS_AUTO_UPGRADE_ENABLED = "hdfs.auto.upgrade.enabled";

	/** The Constant MIGRATION_PROPERTY_HDFS_BACK_UP_NN_IMAGE. */
	public static final String MIGRATION_PROPERTY_HDFS_BACK_UP_NN_IMAGE = "hdfs.backup.nn.image";

	/** The Constant MIGRATION_PROPERTY_HDFS_BACK_UP_SNN_IMAGE. */
	public static final String MIGRATION_PROPERTY_HDFS_BACK_UP_SNN_IMAGE = "hdfs.backup.snn.image";

	/** The Constant MIGRATION_PROPERTY_HDFS_CAPTURE_DFSADMIN_REPORT. */
	public static final String MIGRATION_PROPERTY_HDFS_CAPTURE_DFSADMIN_REPORT = "hdfs.capture.dfsadmin.report";

	/** The Constant MIGRATION_PROPERTY_HDFS_CAPTURE_FSCK_REPORT. */
	public static final String MIGRATION_PROPERTY_HDFS_CAPTURE_FSCK_REPORT = "hdfs.capture.fsck.report";

	/** The Constant MIGRATION_PROPERTY_HDFS_CAPTURE_LSR_REPORT. */
	public static final String MIGRATION_PROPERTY_HDFS_CAPTURE_LSR_REPORT = "hdfs.capture.lsr.report";
	
	public static final String MIGRATION_PROPERTY_COMMON_PROMPT_DURING_MULTIHOST_EXECUTION = "common.prompt.while.running.commands.on.multiple.nodes" ;

	/** The Constant NODE_TYPE_DASHBOARD_HOST. */
	public static final String NODE_TYPE_DASHBOARD_HOST = "dashboardhost";

	/** The Constant NODE_TYPE_DATA_NODES. */
	public static final String NODE_TYPE_DATA_NODES = "nodes";

	/** The Constant NODE_TYPE_GANGLIA_SERVER. */
	public static final String NODE_TYPE_GANGLIA_SERVER = "gangliaserver";

	/** The Constant NODE_TYPE_GATEWAY. */
	public static final String NODE_TYPE_GATEWAY = "gateway";

	/** The Constant NODE_TYPE_HBASE_MASTER. */
	public static final String NODE_TYPE_HBASE_MASTER = "hbasemaster";

	/** The Constant NODE_TYPE_HBASE_REGION_SERVERS. */
	public static final String NODE_TYPE_HBASE_REGION_SERVERS = "hbasenodes";

	/** The Constant NODE_TYPE_HIVE_METASTORE. */
	public static final String NODE_TYPE_HIVE_METASTORE = "hivemetastore";

	/** The Constant NODE_TYPE_JOB_TRACKER. */
	public static final String NODE_TYPE_JOB_TRACKER = "jobtracker";

	/** The Constant NODE_TYPE_NAGIOS_SERVER. */
	public static final String NODE_TYPE_NAGIOS_SERVER = "nagiosserver";

	/** The Constant NODE_TYPE_NAMENODE. */
	public static final String NODE_TYPE_NAMENODE = "namenode";

	/** The Constant NODE_TYPE_OOZIE_SERVER. */
	public static final String NODE_TYPE_OOZIE_SERVER = "oozieserver";

	/** The Constant NODE_TYPE_SECONDARY_NAMENODE. */
	public static final String NODE_TYPE_SECONDARY_NAMENODE = "snamenode";

	/** The Constant NODE_TYPE_TEMPLETON_NODE. */
	public static final String NODE_TYPE_TEMPLETON_NODE = "templetonnode";

	/** The Constant NODE_TYPE_ZOOKEEPER_NODES. */
	public static final String NODE_TYPE_ZOOKEEPER_NODES = "zknodes";

	/** The Constant THREAD_COUNT_PROPERTY_NAME. */
	public static final String NUMBER_OF_PARALLEL_COMMANDS = "number.of.paraller.commands";

	/** The Constant PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES. */
	// public static final int[] PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES = { 1,
	// 5, 10, 25, 50, 100 };
	public static final int[] PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES = { 1, 1000 };

	/** The Constant SERVICE_DATA_NODE. */
	public static final String SERVICE_DATA_NODE = "datanode";

	/** The Constant SERVICE_GMETAD. */
	public static final String SERVICE_GMETAD = "gmeta";

	/** The Constant SERVICE_GMOND. */
	public static final String SERVICE_GMOND = "gmod";

	/** The Constant SERVICE_HBASE_MASTER. */
	public static final String SERVICE_HBASE_MASTER = "hbasemaster";

	/** The Constant SERVICE_HBASE_REGION_SERVER. */
	public static final String SERVICE_HBASE_REGION_SERVER = "regionserver";

	/** The Constant SERVICE_HCATALOG. */
	public static final String SERVICE_HCATALOG = "hcatalog";

	/** The Constant SERVICE_HIVE_METASTORE. */
	public static final String SERVICE_HIVE_METASTORE = "hivemetastore";

	/** The Constant SERVICE_HIVE_SERVER. */
	public static final String SERVICE_HIVE_SERVER = "hiveserver";

	/** The Constant SERVICE_JOB_HISTORY_SERVER. */
	public static final String SERVICE_JOB_HISTORY_SERVER = "historyserver";

	/** The Constant SERVICE_JOB_TRACKER. */
	public static final String SERVICE_JOB_TRACKER = "jobtracker";

	/** The Constant SERVICE_NAGIOS. */
	public static final String SERVICE_NAGIOS = "nagios";

	/** The Constant SERVICE_NAME_NODE. */
	public static final String SERVICE_NAME_NODE = "namenode";

	/** The Constant SERVICE_OOZIE. */
	public static final String SERVICE_OOZIE = "oozie";

	/** The Constant SERVICE_PIG. */
	public static final String SERVICE_PIG = "pig";

	/** The Constant SERVICE_SECONDARY_NAME_NODE. */
	public static final String SERVICE_SECONDARY_NAME_NODE = "secondarynamenode";
	/** The Constant SERVICE_SQOOP. */
	public static final String SERVICE_SQOOP = "sqoop";

	/** The Constant SERVICE_TASK_TRACKER. */
	public static final String SERVICE_TASK_TRACKER = "tasktracker";
	/** The Constant SERVICE_TEMPLETON. */
	public static final String SERVICE_TEMPLETON = "templeton";
	/** The Constant SERVICE_TYPE_MASTER. */
	public static final int SERVICE_TYPE_MASTER = 1;
	/** The Constant SERVICE_TYPE_SLAVE. */
	public static final int SERVICE_TYPE_SLAVE = 2;
	/** The Constant SERVICE_ZOOKEEPER. */
	public static final String SERVICE_ZOOKEEPER = "zookeeper";
	/** The Constant UPGRADE_STAGE_POST. */
	public static final String UPGRADE_STAGE_POST = "post-upgrade";

	/** The Constant UPGRADE_STAGE_PRE. */
	public static final String UPGRADE_STAGE_PRE = "pre-upgrade";
	/** The Constant UPGRADE_STATUS_IN_PROGCESS. */
	public static final int UPGRADE_STATUS_IN_PROGCESS = 2;
	/** The Constant UPGRADE_STATUS_NOT_IN_PROGCESS. */
	public static final int UPGRADE_STATUS_NOT_IN_PROGCESS = 1;
}
