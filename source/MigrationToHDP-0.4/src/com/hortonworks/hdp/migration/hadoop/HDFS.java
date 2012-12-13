/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.io.File;

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
 * The Class HDFS.
 */
public class HDFS extends Component {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new hDFS.
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
	public HDFS(String distro, String installationType, User user, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super("hdfs", distro, installationType, user, homeLocation, configLocation, adminUser, upgradeStage);
		addService(new NameNodeService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);
		addService(new SecondaryNameNodeService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);
		addService(new DataNodeService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_SLAVE);
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
		backupFSImage(upgradeStage);

	}

	/**
	 * Backup fs image.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected void backupFSImage(String upgradeStage) throws Exception {

		int mountNumber = 1;
		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HDFS_BACK_UP_NN_IMAGE, Boolean.TRUE.toString()),
				Boolean.FALSE.toString())) {
			log.warn("ALARM: Skipping back up of nn image for " + getName() + " as specified in property '"
					+ Constants.MIGRATION_PROPERTY_HDFS_BACK_UP_NN_IMAGE
					+ "'. Please not onus is on you to revert back to old version of nn image in case of any possible failures.");
		} else {

			log.info("Backing up nn image");

			NameNodeService nnService = (NameNodeService) getServiceByName(Constants.SERVICE_NAME_NODE);
			// TODO error handling should be able to copy at least from one
			// location. Ignore some locations in case they could not be
			// accessed.

			// TODO handle various configs such as defaults and also different
			// configs for fs.checkpoint.edits.dir and dfs.name.edits.dir
			String[] nameNodeDataLocations = StringUtils.split(
					XMLUtil.getConfigParamValue(
							Config.getLocalServiceConfigDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
									nnService.getHostsList().get(0))
									+ "/hdfs-site.xml", "dfs.name.dir"), ",");
			if (nameNodeDataLocations == null) {
				nameNodeDataLocations = StringUtils.split(
						XMLUtil.getConfigParamValue(
								Config.getLocalServiceConfigDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
										nnService.getHostsList().get(0))
										+ "/hdfs-site.xml", "dfs.namenode.name.dir"), ",");

			}

			mountNumber = 1;
			if (nameNodeDataLocations == null) {
				log.error("No dfs.name.dir found in hdfs xml. It will not be backed up automatically. Please make sure fsImage directory is backed up.");
			} else {

				for (String nameNodeDataLocation : nameNodeDataLocations) {
					for (String hostName : nnService.getHostsList()) {
						String tarName = getName() + "-fsimage-mount" + mountNumber + "-" + upgradeStage + ".tgz";
						String remoteDataDir = Config.getRemoteDataDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostName);
						String tarFullyQualifiedRemotePath = remoteDataDir + tarName;
						String localDataDir = Config.getLocalDataDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostName);
						CommandExecutor.executeRemoteCommand(hostName, getAdminUser(), "mkdir -m 777 -p  " + remoteDataDir, getUser());
						// CommandExecutor.executeRemoteCommand(hostName,
						// getAdminUser(), "tar -czpf " +
						// tarFullyQualifiedRemotePath + " -C " +
						// nameNodeDataLocation
						// + "/ ./", getUser());
						CommandExecutor.executeRemoteCommand(hostName, getAdminUser(), SSHUtils.getTarCommand(remoteDataDir, tarName, nameNodeDataLocation),
								null);

						CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localDataDir, null, hostName);
						CommandExecutor.scpFromRemoteHost(hostName, getAdminUser(), tarFullyQualifiedRemotePath, localDataDir, getUser());
						// CommandExecutor.executeCommand("tar -xzpf " +
						// localDataDir + tarName + " -C " + localDataDir,
						// null);
					}
					mountNumber++;
				}
			}
		}

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HDFS_BACK_UP_SNN_IMAGE, Boolean.TRUE.toString()),
				Boolean.FALSE.toString())) {
			log.warn("ALARM: Skipping back up of snn image for " + getName() + " as specified in property '"
					+ Constants.MIGRATION_PROPERTY_HDFS_BACK_UP_SNN_IMAGE + "'");

		} else {
			log.info("Backing up snn image");

			SecondaryNameNodeService snnService = (SecondaryNameNodeService) getServiceByName(Constants.SERVICE_SECONDARY_NAME_NODE);
			String[] secondaryNameNodeDataLocations = StringUtils.split(
					XMLUtil.getConfigParamValue(
							Config.getLocalServiceConfigDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
									snnService.getHostsList().get(0))
									+ "/core-site.xml", "fs.checkpoint.dir"), ",");
			mountNumber = 1;
			if (secondaryNameNodeDataLocations == null) {
				log.error("No fs.checkpoint.dir found in core xml. It will not be backed up automatically. Please make sure check point directory is backed up");
			} else {
				for (String secondaryNameNodeDataLocation : secondaryNameNodeDataLocations) {
					for (String hostName : snnService.getHostsList()) {
						String tarName = getName() + "-checkpoint-image-mount" + mountNumber + "-" + upgradeStage + ".tgz";

						String remoteDataDir = Config.getRemoteDataDir(upgradeStage, getName());
						String tarFullyQualifiedRemotePath = remoteDataDir + tarName;
						String localDataDir = Config.getLocalDataDir(upgradeStage, getName());

						CommandExecutor.executeRemoteCommand(hostName, getAdminUser(), "mkdir -m 777 -p  " + remoteDataDir, getUser());

						// CommandExecutor.executeRemoteCommand(hostName,
						// getAdminUser(), "tar -czpf " +
						// tarFullyQualifiedRemotePath + " -C "
						// + secondaryNameNodeDataLocation + "/ ./", getUser());
						CommandExecutor.executeRemoteCommand(hostName, getAdminUser(),
								SSHUtils.getTarCommand(remoteDataDir, tarName, secondaryNameNodeDataLocation), getUser());

						CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localDataDir, null, hostName);
						CommandExecutor.scpFromRemoteHost(hostName, getAdminUser(), tarFullyQualifiedRemotePath, localDataDir, getUser());
						// CommandExecutor.executeLocalCommand("tar -xzpf " +
						// localDataDir + tarName + " -C " + localDataDir,
						// null);

					}
					mountNumber++;
				}
			}
		}
	}

	/**
	 * Capture file and block information.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected void captureFileAndBlockInformation(String upgradeStage) throws Exception {
		log.info("Capturing file list and block information of HDFS");
		String localStatsDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "log");
		String remoteStatsDirectory = Config.getRemoteStatsDir(upgradeStage, getName(), "log");
		String tarName = upgradeStage + "-file-block-info.tgz";
		String remoteTar = remoteStatsDirectory + "/" + tarName;

		String nameNodeHost = getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0);

		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), "mkdir -m 777 -p  " + remoteStatsDirectory, getUser());

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HDFS_CAPTURE_FSCK_REPORT, Boolean.TRUE.toString()),
				Boolean.TRUE.toString())) {
			log.info("Running fsck report on entire filesystem. This may take a while.");
			CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), getHomeLocation() + "/bin/hadoop --config " + getConfigLocation()
					+ "  fsck / > " + remoteStatsDirectory + getFsckReportFileName(upgradeStage), getUser());
		} else {
			log.warn("Skipping fsck report for "
					+ getName()
					+ " as specified in property '"
					+ Constants.MIGRATION_PROPERTY_HDFS_CAPTURE_FSCK_REPORT
					+ "'. Please note that pre and post validation will not be performed for fsck report on HDFS and you should ensure by other means that HDFS upgrade is successfull.");

		}

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HDFS_CAPTURE_LSR_REPORT, Boolean.TRUE.toString()),
				Boolean.TRUE.toString())) {
			log.info("Running lsr report on entire filesystem. This may take a while.");
			CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), getHomeLocation() + "/bin/hadoop --config " + getConfigLocation()
					+ "  fs -lsr / > " + remoteStatsDirectory + getLsrReportFileName(upgradeStage), getUser());
		} else {
			log.warn("Skipping lsr report for "
					+ getName()
					+ " as specified in property '"
					+ Constants.MIGRATION_PROPERTY_HDFS_CAPTURE_LSR_REPORT
					+ "'. Please note that pre and post validation will not be performed for lsr report on HDFS and you should ensure by other means that HDFS upgrade is successfull.");

		}

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HDFS_CAPTURE_DFSADMIN_REPORT, Boolean.TRUE.toString()),
				Boolean.TRUE.toString())) {
			log.info("Running dfsadmin report");
			CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), getHomeLocation() + "/bin/hadoop --config " + getConfigLocation()
					+ "  dfsadmin -report  > " + remoteStatsDirectory + getAdminReportFileName(upgradeStage), getUser());
		} else {
			log.warn("Skipping dfsadmin report for " + getName() + " as specified in property '" + Constants.MIGRATION_PROPERTY_HDFS_CAPTURE_DFSADMIN_REPORT
					+ "'");

		}

		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), SSHUtils.getTarCommand(remoteStatsDirectory, tarName, remoteStatsDirectory),
				getUser());

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localStatsDirectory, null, nameNodeHost);
		CommandExecutor.scpFromRemoteHost(nameNodeHost, getAdminUser(), remoteTar, localStatsDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localStatsDirectory + tarName + " -C " + localStatsDirectory, null, nameNodeHost);

	}

	/**
	 * Capture name node ui information.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected void captureNameNodeUIInformation(String upgradeStage) throws Exception {
		log.info("Capturing namenode UI information");

		String localStatsDirectory = Config.getLocalStatsDir(upgradeStage, getName(), "html");
		String remoteStatsDirectory = Config.getRemoteStatsDir(upgradeStage, getName(), "html");

		String tarName = upgradeStage + "-nn-ui-info.tgz";
		String remoteTar = remoteStatsDirectory + tarName;
		String nameNodeHost = getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0);

		// TODO Get the ports dynamically
		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), "mkdir -m 777 -p  " + remoteStatsDirectory, getUser());
		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), "wget http://" + nameNodeHost + ":50070/dfshealth.jsp -O " + remoteStatsDirectory
				+ "/dfshealth.html", getUser());
		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), "wget http://" + nameNodeHost + ":50070/dfsnodelist.jsp?whatNodes=LIVE -O "
				+ remoteStatsDirectory + "/dfs-live-nodes.html", getUser());
		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), "wget http://" + nameNodeHost + ":50070/dfsnodelist.jsp?whatNodes=DEAD -O "
				+ remoteStatsDirectory + "/dfs-dead-nodes.html", getUser());

		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), "wget http://" + nameNodeHost
				+ ":50070/dfsnodelist.jsp?whatNodes=DECOMMISSIONING -O " + remoteStatsDirectory + "/dfs-decommissioning-nodes.html", getUser());

		// CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(),
		// "tar -czpf " + remoteTar + " -C " + remoteStatsDirectory + "/ ./",
		// getUser());
		CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), SSHUtils.getTarCommand(remoteStatsDirectory, tarName, remoteStatsDirectory),
				getUser());

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localStatsDirectory, null, nameNodeHost);
		CommandExecutor.scpFromRemoteHost(nameNodeHost, getAdminUser(), remoteTar, localStatsDirectory, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localStatsDirectory + tarName + " -C " + localStatsDirectory, null, nameNodeHost);

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
		captureFileAndBlockInformation(upgradeStage);
		captureNameNodeUIInformation(upgradeStage);

	}

	/**
	 * Enter safe mode.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	protected void enterSafeMode() throws Exception {
		((NameNodeService) getServiceByName(Constants.SERVICE_NAME_NODE)).enterSafeMode();
	}

	/**
	 * Gets the admin report file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the admin report file name
	 */
	private String getAdminReportFileName(String upgradeStage) {
		return "hdfs-report-" + upgradeStage + ".log";
	}

	/**
	 * Gets the fsck report file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the fsck report file name
	 */
	private String getFsckReportFileName(String upgradeStage) {
		return "hdfs-fsck-" + upgradeStage + ".log";
	}

	/**
	 * Gets the lsr report file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the lsr report file name
	 */
	private String getLsrReportFileName(String upgradeStage) {
		return "hdfs-lsr-" + upgradeStage + ".log";
	}

	/**
	 * Gets the upgrade status.
	 * 
	 * @return the upgrade status
	 * @throws Exception
	 *             the exception
	 */
	public int getUpgradeStatus() throws Exception {
		NameNodeService nnService = (NameNodeService) getServiceByName(Constants.SERVICE_NAME_NODE);
		String nameNodeHost = nnService.getHostsList().get(0);
		ExecutionResult result = CommandExecutor.executeRemoteCommand(nameNodeHost, getAdminUser(), getHomeLocation() + "/bin/hadoop --config "
				+ getConfigLocation() + "  dfsadmin -upgradeProgress status", getUser());
		if (StringUtils.containsIgnoreCase(result.getOutput(), "There are no upgrades in progress")) {
			return Constants.UPGRADE_STATUS_NOT_IN_PROGCESS;
		} else {
			return Constants.UPGRADE_STATUS_IN_PROGCESS;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#getVersion()
	 */
	@Override
	public String getVersion() throws Exception {
		ExecutionResult exResult = CommandExecutor.executeRemoteCommand(getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0), getAdminUser(),
				getHomeLocation() + "/bin/hadoop version | grep '^Hadoop' | cut -d' ' -f2 | cut -d'.' -f1,2,3", getUser());
		return exResult.getOutput();
	}

	/**
	 * Leave safe mode.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	protected void leaveSafeMode() throws Exception {
		CommandExecutor.executeRemoteCommand(getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0), getAdminUser(), getHomeLocation()
				+ "/bin/hadoop --config " + getConfigLocation() + "  dfsadmin -safemode leave", getUser());
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
		NameNodeService namenode = (NameNodeService) getServiceByName(Constants.SERVICE_NAME_NODE);

		/*
		 * String nameDirs = StringUtils.replace(
		 * XMLUtil.getConfigParamValue(Config
		 * .getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName(),
		 * namenode.getHostsList().get(0)) + "/hdfs-site.xml", "dfs.name.dir"),
		 * ",", " "); if (nameDirs != null) {
		 * CommandExecutor.executeRemoteCommand(namenode.getHostsList(),
		 * getAdminUser(), "chmod 755 " + nameDirs, null); }
		 * 
		 * String dataDirs = StringUtils.replace(
		 * XMLUtil.getConfigParamValue(Config
		 * .getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName(),
		 * namenode.getHostsList().get(0)) + "/hdfs-site.xml", "dfs.data.dir"),
		 * ",", " "); if (dataDirs != null) {
		 * CommandExecutor.executeRemoteCommand
		 * (getServiceByName(Constants.SERVICE_DATA_NODE).getHostsList(),
		 * getAdminUser(), "chmod 750 " + dataDirs, null); }
		 * 
		 * String checkpointDirs = StringUtils.replace(
		 * XMLUtil.getConfigParamValue
		 * (Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE,
		 * getName(), namenode.getHostsList().get(0)) + "/core-site.xml",
		 * "fs.checkpoint.dir"), ",", " "); if (checkpointDirs != null) {
		 * CommandExecutor.executeRemoteCommand(getServiceByName(Constants.
		 * SERVICE_SECONDARY_NAME_NODE).getHostsList(), getAdminUser(),
		 * "chmod 755 " + checkpointDirs, null); }
		 */

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_HDFS_AUTO_UPGRADE_ENABLED, Boolean.TRUE.toString()),
				Boolean.TRUE.toString())) {
			namenode.startInUpgradeMode();
			while (!SSHUtils
					.confirmAction("Started NameNode in upgrade mode. Please verify the NameNode UI http://"
							+ getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0)
							+ ":50070/dfshealth.jsp ; If NameNode is not running, then wait for it to start (If it does not start for long time, check the logs and bring it up in upgrade mode manually if needed). Is it running ?")) {
				;
			}
			enterSafeMode();
			getServiceByName(Constants.SERVICE_SECONDARY_NAME_NODE).startService();
			startSlaves();
		} else {
			log.warn("Skipping the auto upgrade of HDFS as specified in the property '" + Constants.MIGRATION_PROPERTY_HDFS_AUTO_UPGRADE_ENABLED
					+ "'. Please make sure that you perform following activities before proceeding." + "\n 1. upgrade/downgrade namenode and start it"
					+ "\n 2. Push name node in safe mode. (DO NOT MISS THIS STEP)" + "\n 3. Start secondary namenode"
					+ "\n 4. Start all data nodes (downgrade/upgrade them as needed before starting)");
			SSHUtils.readUserInput("Hit enter when you have done above activities.", null, true, null);

		}

		while (!SSHUtils
				.confirmAction("Please verify the NameNode UI http://"
						+ getServiceByName(Constants.SERVICE_NAME_NODE).getHostsList().get(0)
						+ ":50070/dfshealth.jsp ; Wait for all data nodes to report to NameNode  (If some data nodes do not report for long time, check the logs and bring them up manually if needed). Have all "
						+ getServiceByName(Constants.SERVICE_DATA_NODE).getHostsList().size()
						+ " data nodes reported to namenode and also is the block count matching ?")) {
			;

		}
		captureStats(Constants.UPGRADE_STAGE_POST);
		boolean success = validateStats();

		if (success) {
			log.info("\n\n*** " + getName() + " is now successfully upgraded.  ***\n\n");

		} else {
			log.error("\n\n*** "
					+ getName()
					+ " upgrade had ISSUES. Either pre and post reports do not match or were not available for comparision. Please verify the pre/post  details available under "
					+ Config.getLocalStatsDir("*", getName(), "*") + " ***\n\n");
		}

		if (SSHUtils
				.confirmAction("NOTE: NameNode is in safe mode. Subsequent components may need the name node to be out of safe mode in order to perform post upgrade activities. Before taking the name node out of safe mode, you may want to verify the pre/post upgrade details available under "
						+ Config.getLocalStatsDir("*", getName(), "*") + "\nDo you want NameNode to leave safe mode?")) {
			leaveSafeMode();
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

		if (Constants.UPGRADE_STATUS_IN_PROGCESS == getUpgradeStatus()) {
			if (!SSHUtils.confirmAction("There is already a upgrade in process. Are you sure you want to continue without finalizing it?")) {
				throw new Exception("Exiting as there is alreday a upgrade in progress");
			}
		}
		enterSafeMode();
		saveNamespace();
		captureStats(Constants.UPGRADE_STAGE_PRE);
		stopAll();
		backupData(Constants.UPGRADE_STAGE_PRE);
	}

	/**
	 * Save namespace.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	protected void saveNamespace() throws Exception {

		((NameNodeService) getServiceByName(Constants.SERVICE_NAME_NODE)).saveNamespace();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#validateStats()
	 */
	@Override
	protected boolean validateStats() throws Exception {
		boolean success = true;
		boolean flag = true;
		String preUpgradeLsrReport = Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE, getName(), "log") + getLsrReportFileName(Constants.UPGRADE_STAGE_PRE);
		String postUpgradeLsrReport = Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "log")
				+ getLsrReportFileName(Constants.UPGRADE_STAGE_POST);

		if ((new File(preUpgradeLsrReport)).exists() && (new File(postUpgradeLsrReport)).exists()) {
			log.info("Comparing lsr reports");
			flag = FileComparer.compareFileListings(preUpgradeLsrReport, postUpgradeLsrReport);

			if (flag) {
				log.info("Great News! HDFS lsr reports validation is SUCCESSFUL");
			} else {
				log.error("HDFS lsr reports validation FAILED");
			}

		} else {
			log.warn("Skipping pre and post lsr report verification becuase either " + preUpgradeLsrReport + " or " + postUpgradeLsrReport
					+ " is missing. Please perform manual verification of HDFS");
			flag = false;

		}
		success &= flag;

		String preUpgradeFsckReport = Config.getLocalStatsDir(Constants.UPGRADE_STAGE_PRE, getName(), "log")
				+ getFsckReportFileName(Constants.UPGRADE_STAGE_PRE);
		String postUpgradeFsckReport = Config.getLocalStatsDir(Constants.UPGRADE_STAGE_POST, getName(), "log")
				+ getFsckReportFileName(Constants.UPGRADE_STAGE_POST);

		if ((new File(preUpgradeFsckReport)).exists() && (new File(postUpgradeFsckReport)).exists()) {

			log.info("Comparing fsck reports");

			flag = FileComparer.compareFsckReport(preUpgradeFsckReport, postUpgradeFsckReport);

			if (flag) {
				log.info("Great News! HDFS fsck reports validation is SUCCESSFUL");
			} else {
				log.error("HDFS fsck reports validation FAILED");
			}
		} else {
			log.warn("Skipping pre and post fsck report verification becuase either " + preUpgradeFsckReport + " or " + postUpgradeFsckReport
					+ " is missing. Please perform manual verification of HDFS");
			flag = false;

		}

		success &= flag;
		return success;

	}

}
