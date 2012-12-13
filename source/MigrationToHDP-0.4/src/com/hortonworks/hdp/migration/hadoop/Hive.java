/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.io.File;
import java.io.FilenameFilter;

import org.apache.commons.io.filefilter.SuffixFileFilter;
import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.file.util.FileComparer;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class Hive.
 */
public class Hive extends Component {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new hive.
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
	public Hive(String distro, String installationType, User user, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super("hive", distro, installationType, user, homeLocation, configLocation, adminUser, upgradeStage);
		addService(new HiveMetastoreService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);

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

		// TODO Automation to back up MySql and PostgreSql database from
		// Information provided on

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
		log.info("Capturing database and tables information");

		String localDestinationDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "schema");
		String remoteTarDir = Config.getRemoteStatsDir(upgradeStage, getName(), "schema");
		String tarName = upgradeStage + "-hive-schema.tgz";
		String remoteTar = remoteTarDir + tarName;

		Service metastoreService = getServiceByName(Constants.SERVICE_HIVE_METASTORE);
		String metastoreHost = metastoreService.getHostsList().get(0);
		CommandExecutor.executeRemoteCommand(metastoreHost, getAdminUser(), "mkdir -m 777 -p  " + remoteTarDir, getUser());
		String command = getHomeLocation() + "/bin/hive -e \"show databases\" | sed \"s/ *$//g\" |  sed \"s/^ *//g\" | sort 1> " + remoteTarDir
				+ getDataBaseListFileName(upgradeStage);
		CommandExecutor.executeRemoteCommand(metastoreHost, getAdminUser(), command, getUser());
		command = "for dbname in $(cat " + remoteTarDir + getDataBaseListFileName(upgradeStage) + ")  ; do " + getHomeLocation()
				+ "/bin/hive -e \"use ${dbname} ; show tables ; \" | sed \"s/ *$//g\" |  sed \"s/^ *//g\" | sort  1> " + remoteTarDir
				+ getTableListFileName(upgradeStage) + "; done ; ";
		CommandExecutor.executeRemoteCommand(metastoreHost, getAdminUser(), command, getUser());

		// CommandExecutor.executeRemoteCommand(metastoreHost, getAdminUser(),
		// "tar -czpf " + remoteTar + " -C " + remoteTarDir + "/ ./",
		// getUser());
		CommandExecutor.executeRemoteCommand(metastoreHost, getAdminUser(), SSHUtils.getTarCommand(remoteTarDir, tarName, remoteTarDir), getUser());

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localDestinationDirectory, null, metastoreHost);

		CommandExecutor.scpFromRemoteHost(metastoreHost, getAdminUser(), remoteTar, localDestinationDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localDestinationDirectory + tarName + " -C " + localDestinationDirectory, null, metastoreHost);

	}

	/**
	 * Gets the data base list file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the data base list file name
	 */
	private String getDataBaseListFileName(String upgradeStage) {
		return "hive-database-list-" + upgradeStage + ".log";
	}

	/**
	 * Gets the table list file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the table list file name
	 */
	private String getTableListFileName(String upgradeStage) {
		return "hive-tables-in-${dbname}-" + upgradeStage + ".log";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#getVersion()
	 */
	@Override
	public String getVersion() throws Exception {
		ExecutionResult exResult = CommandExecutor.executeRemoteCommand(getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0), getAdminUser(),
				"ls " + getHomeLocation() + "/lib/hive-exec-*.jar | grep -o '/hive-exec-.*.jar' | cut -d'-' -f3 |cut -d'.' -f1,2,3", getUser());
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

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_COMMON_BACKUP_INSTALLATION_DIR, Boolean.TRUE.toString()),
				Boolean.TRUE.toString())) {
			String masterHost = getMastersServiceList().get(0).getHostsList().get(0);
			String tarLocalDir = Config.getLocalDataDir(Constants.UPGRADE_STAGE_PRE, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, masterHost);
			String tarName = Config.getInstallationTarFileName(Constants.UPGRADE_STAGE_PRE, getName());
			CommandExecutor.executeLocalCommand("tar -xzpf " + tarLocalDir + tarName + " -C " + tarLocalDir, null, masterHost);

			File file = new File(tarLocalDir + "/bin/.hiverc");
			if (file.exists()) {
				log.info("Pushing .hiverc file");

				for (Service masterService : getMastersServiceList()) {
					for (String hostName : masterService.getHostsList()) {
						CommandExecutor.scpToRemoteHost(hostName, getAdminUser(), tarLocalDir + "/bin/.hiverc", getHomeLocation() + "/bin/", getUser());
					}
				}
			}

			// TODO Also distribute all app specific jars.
		}

		startAll();
		captureStats(Constants.UPGRADE_STAGE_POST);
		boolean success = validateStats();

		if (success) {
			log.info("\n\n*** " + getName() + " is now successfully upgraded. Please verify the pre/post  details available under "
					+ Config.getLocalStatsDir("*", getName(), "*") + " ***\n\n");
		} else {
			log.error("\n\n*** " + getName() + " upgrade had ISSUES. Please verify the pre/post  details available under "
					+ Config.getLocalStatsDir("*", getName(), "*") + " ***\n\n");

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
		stopAll();
		log.info("\n\n*** " + getName() + " is ready to upgrade. PLEASE BACK UP THE METASTORE DATABASE AND EXECUTE UPGRADE SCRIPTS ***\n\n");

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#validateStats()
	 */
	@Override
	protected boolean validateStats() throws Exception {
		log.info("Validating  pre and post upgrade stats");

		boolean success = true;
		boolean flag = true;
		flag = FileComparer.compareContents(Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE, getName(), "schema")
				+ getDataBaseListFileName(Constants.UPGRADE_STAGE_PRE), Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "schema")
				+ getDataBaseListFileName(Constants.UPGRADE_STAGE_POST));

		if (flag) {
			log.info("HIVE database list validation is SUCCESSFUL");
		} else {
			log.error("HIVE database list validation FAILED");
		}

		success &= flag;
		File tableNamesFiles = new File(Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE, getName(), "schema"));

		for (File file : tableNamesFiles.listFiles(((FilenameFilter) new SuffixFileFilter(new String[] { ".log", ".txt" })))) {
			flag = FileComparer.compareContents(
					Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE, getName(), "schema") + file.getName(),
					Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "schema")
							+ file.getName().replace(Constants.UPGRADE_STAGE_PRE, Constants.UPGRADE_STAGE_POST));

			if (flag) {
				log.info("HIVE table list validation for database (" + file.getName() + ") is SUCCESSFUL");
			} else {
				log.error("HIVE database list validation for database (" + file.getName() + ") FAILED");
			}
			success &= flag;

		}
		// success &=
		// FileComparer.compareFileListings(Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE,
		// getName(), "log")
		// + getPartitionListFileName(Constants.UPGRADE_STAGE_PRE),
		// Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(),
		// "log")
		// + getPartitionListFileName(Constants.UPGRADE_STAGE_POST));
		return success;

	}

}
