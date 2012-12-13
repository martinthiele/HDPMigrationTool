/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

//import java.io.File;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.file.util.FileComparer;
import com.hortonworks.hdp.migration.file.util.XMLUtil;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class HBase.
 */
public class HBase extends Component {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new h base.
	 * 
	 * @param distro
	 *            the distro
	 * @param installationType
	 *            the installation type
	 * @param user
	 *            the user
	 * @param homeLocation
	 *            the home location
	 * @param configLocation
	 *            the config location
	 * @param adminUser
	 *            the admin user
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	public HBase(String distro, String installationType, User user, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super("hbase", distro, installationType, user, homeLocation, configLocation, adminUser, upgradeStage);
		addService(new HBaseMasterService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);
		addService(new RegionServerService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_SLAVE);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Component#backupData(java.lang.String
	 * )
	 */
	@Override
	protected void backupData(String upgradeStage) throws Exception {
		exportTables(Constants.UPGRADE_STAGE_PRE);
		// TODO Do distcp backup through HDFS

	}

	/**
	 * Capture jh base ui information.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected void captureJHBaseUIInformation(String upgradeStage) throws Exception {
		log.info("Capturing the HBase UI information");

		String localDestinationDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "html");
		String remoteTarDir = Config.getRemoteStatsDir(upgradeStage, getName(), "html");
		String tarName = upgradeStage + "-hbase-ui-info.tgz";
		String remoteTar = remoteTarDir + tarName;

		// TODO Get the ports dynamically
		String hbaseMasterHost = getServiceByName(Constants.SERVICE_HBASE_MASTER).getHostsList().get(0);
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "mkdir -m 777 -p  " + remoteTarDir, getUser());
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "wget http://" + hbaseMasterHost + ":60010/master-status -O " + remoteTarDir
				+ "/rs-status.html", getUser());

		// CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(),
		// "tar -czpf " + remoteTar + " -C " + remoteTarDir + "/ ./",
		// getUser());
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), SSHUtils.getTarCommand(remoteTarDir, tarName, remoteTarDir), getUser());

		CommandExecutor.scpFromRemoteHost(hbaseMasterHost, getAdminUser(), remoteTar, localDestinationDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localDestinationDirectory + tarName + " -C " + localDestinationDirectory, null, (String) null);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Component#captureStats(java.lang
	 * .String)
	 */
	@Override
	protected void captureStats(String upgradeStage) throws Exception {
		log.info("Capturing HBase table list and table definitions");

		String localDestinationDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "schema");
		String remoteTarDir = Config.getRemoteStatsDir(upgradeStage, getName(), "schema");
		String tarName = upgradeStage + "-hbase-schema.tgz";
		String remoteTar = remoteTarDir + tarName;

		String listCommandsFile = remoteTarDir + "/listTables.hbase";
		String hbaseMasterHost = getServiceByName(Constants.SERVICE_HBASE_MASTER).getHostsList().get(0);
		String tablesListFileName = remoteTarDir + getTableListFileName(upgradeStage);

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "mkdir -m 777 -p  " + remoteTarDir, getUser());

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "echo -e \"list\\nexit\" >> " + listCommandsFile, getUser());

		String tablesListCommand = getHomeLocation() + "/bin/hbase --config " + getConfigLocation() + " shell " + listCommandsFile
				+ " | head -n -2 | tail -n +2 | sed \"s/ *$//g\" |  sed \"s/^ *//g\" | sort > " + tablesListFileName;

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), tablesListCommand, null);

		String describeCommandsFile = remoteTarDir + "/describeTables.hbase";

		String tableDescribeScriptGenerationCommand = "for table in $(cat " + tablesListFileName + ")  ; do echo describe \\'$table\\' >> "
				+ describeCommandsFile + "; done ;";
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), tableDescribeScriptGenerationCommand, null);
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "echo exit >> " + describeCommandsFile, getUser());

		String tablesDescribeCommand = getHomeLocation() + "/bin/hbase --config " + getConfigLocation() + " shell " + describeCommandsFile + " > "
				+ remoteTarDir + getTableDescriptionFileName(upgradeStage);
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), tablesDescribeCommand, getUser());
		// CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(),
		// "tar -czpf " + remoteTar + " -C " + remoteTarDir + "/ ./",
		// getUser());
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), SSHUtils.getTarCommand(remoteTarDir, tarName, remoteTarDir), getUser());

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localDestinationDirectory, null, hbaseMasterHost);

		CommandExecutor.scpFromRemoteHost(hbaseMasterHost, getAdminUser(), remoteTar, localDestinationDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localDestinationDirectory + tarName + " -C " + localDestinationDirectory, null, (String) null);

	}

	/**
	 * Compact tables.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	private void compactTables(String upgradeStage) throws Exception {

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HBASE_COMPACT_TABLES, Boolean.TRUE.toString()),
				Boolean.FALSE.toString())) {
			log.warn("Skipping compaction of HBase tables as specified in property '" + Constants.MIGRATION_PROPERTY_HBASE_COMPACT_TABLES
					+ "' You may face issues when upgrading to newer versions of HBase.");
			return;
		}

		log.info("Compacting all HBase tables.");

		String remoteTarDir = Config.getRemoteStatsDir(upgradeStage, getName(), "compactionlogs");
		String localDestinationDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "compactionlogs");
		String tarName = upgradeStage + "-compactionlogs.tgz";

		String compactCommandsFile = remoteTarDir + "/compactTables.hbase";
		String hbaseMasterHost = getServiceByName(Constants.SERVICE_HBASE_MASTER).getHostsList().get(0);
		String tablesListFileName = Config.getRemoteStatsDir(upgradeStage, getName(), "schema") + getTableListFileName(upgradeStage);
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "mkdir -m 777 -p  " + remoteTarDir, getUser());
		String compactionScriptGenerationCommand = "for table in $(cat " + tablesListFileName + ")  ; do echo major_compact \\'$table\\' >> "
				+ compactCommandsFile + "; done ; echo -e \"major_compact '.META.'\\nmajor_compact '-ROOT-'\\nexit\"  >> " + compactCommandsFile;

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), compactionScriptGenerationCommand, null);

		String tablesDescribeCommand = getHomeLocation() + "/bin/hbase --config " + getConfigLocation() + " shell " + compactCommandsFile + " > "
				+ remoteTarDir + getTableCompactionLogFileName(upgradeStage);

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), tablesDescribeCommand, getUser());

		// CommandExecutor
		// .executeRemoteCommand(hbaseMasterHost, getAdminUser(), "tar -czpf " +
		// remoteTarDir + tarName + " -C " + remoteTarDir + "/ ./", getUser());
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), SSHUtils.getTarCommand(remoteTarDir, tarName, remoteTarDir), getUser());

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localDestinationDirectory, null, hbaseMasterHost);

		CommandExecutor.scpFromRemoteHost(hbaseMasterHost, getAdminUser(), remoteTarDir + tarName, localDestinationDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localDestinationDirectory + tarName + " -C " + localDestinationDirectory, null, (String) null);

	}

	/**
	 * Export tables.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	private void exportTables(String upgradeStage) throws Exception {

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HBASE_EXPORT_DATA_ON_LOCAL_FS, Boolean.TRUE.toString()),
				Boolean.FALSE.toString())) {
			log.warn("Skipping export of HBase tables in local fs as specified in property '" + Constants.MIGRATION_PROPERTY_HBASE_EXPORT_DATA_ON_LOCAL_FS
					+ "'");
			return;
		}
		log.info("Exporting HBase tables");

		String remoteTarDir = Config.getRemoteStatsDir(upgradeStage, getName(), "exportlogs");
		String localDestinationDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "exportlogs");
		String tarName = upgradeStage + "-exportlogs.tgz";

		String hbaseMasterHost = getServiceByName(Constants.SERVICE_HBASE_MASTER).getHostsList().get(0);

		String hbaseRootDir = XMLUtil.getConfigParamValue(
				Config.getLocalServiceConfigDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, hbaseMasterHost) + "/hbase-site.xml",
				"hbase.rootdir");

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), "mkdir -m 777 -p  " + remoteTarDir, null);

		String tablesListFileName = Config.getRemoteStatsDir(upgradeStage, getName(), "schema") + getTableListFileName(upgradeStage);
		String exportTableCommand = "for table in $(cat " + tablesListFileName + ")  ; do " + getHomeLocation() + "/bin/hbase --config " + getConfigLocation()
				+ " org.apache.hadoop.hbase.mapreduce.Driver export $table " + hbaseRootDir + "/../tableExports/$table >> " + remoteTarDir
				+ "/exportTable.log ; done ; ";

		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), exportTableCommand, null);
		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localDestinationDirectory, null, hbaseMasterHost);
		// CommandExecutor
		// .executeRemoteCommand(hbaseMasterHost, getAdminUser(), "tar -czpf " +
		// remoteTarDir + tarName + " -C " + remoteTarDir + "/ ./", getUser());
		CommandExecutor.executeRemoteCommand(hbaseMasterHost, getAdminUser(), SSHUtils.getTarCommand(remoteTarDir, tarName, remoteTarDir), getUser());

		CommandExecutor.scpFromRemoteHost(hbaseMasterHost, getAdminUser(), remoteTarDir + tarName, localDestinationDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localDestinationDirectory + tarName + " -C " + localDestinationDirectory, null, (String) null);

	}

	/**
	 * Gets the table compaction log file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the table compaction log file name
	 */
	private String getTableCompactionLogFileName(String upgradeStage) {
		return upgradeStage + "-hbase-tables-compaction.log";
	}

	/**
	 * Gets the table description file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the table description file name
	 */
	private String getTableDescriptionFileName(String upgradeStage) {
		return upgradeStage + "-hbase-tables-definitions.txt";
	}

	/**
	 * Gets the table list file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the table list file name
	 */
	private String getTableListFileName(String upgradeStage) {
		return upgradeStage + "-hbase-tables-list.txt";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#getVersion()
	 */
	@Override
	public String getVersion() throws Exception {
		ExecutionResult exResult = CommandExecutor.executeRemoteCommand(getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0), getAdminUser(),
				getHomeLocation() + "/bin/hbase version 2>&1 1>&1 | grep ': HBase '  | grep -o ': HBase .*' | cut -d' ' -f3 |cut -d'.' -f1,2,3", getUser());
		return exResult.getOutput();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Component#performPostUpgradeActivities
	 * ()
	 */
	@Override
	public void performPostUpgradeActivities() throws Exception {
		super.performPostUpgradeActivities();
		log.info("Starting all HBase Services.");
		startAll();
		// Thread.sleep(60000);
		captureStats(Constants.UPGRADE_STAGE_POST);
		boolean success = validateStats();
		if (success) {
			log.info("\n\n*** " + getName() + " is now successfully upgraded. Please verify the pre/post  details available under "
					+ Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "*") + " ***\n\n");
		} else {
			log.error("\n\n*** " + getName() + " upgrade had ISSUES. Please verify the pre/post  details available under "
					+ Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "*") + " ***\n\n");
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Component#performPreUpgradeActivities
	 * ()
	 */
	@Override
	public void performPreUpgradeActivities() throws Exception {
		super.performPreUpgradeActivities();
		captureStats(Constants.UPGRADE_STAGE_PRE);
		compactTables(Constants.UPGRADE_STAGE_PRE);
		stopAll();
		backupData(Constants.UPGRADE_STAGE_PRE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#validateStats()
	 */
	@Override
	protected boolean validateStats() throws Exception {
		log.info("Verifying pre-upgrade and post upgrade stats.");
		boolean success = true;
		boolean flag = true;
		flag = FileComparer.compareContents(Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE, getName(), "schema")
				+ getTableListFileName(Constants.UPGRADE_STAGE_PRE), Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "schema")
				+ getTableListFileName(Constants.UPGRADE_STAGE_POST));
		;
		if (flag) {
			log.info("HBASE table list validation is SUCCESSFUL");
		} else {
			log.error("HIVE database list validation FAILED");
		}
		success &= flag;
		return success;

	}

}
