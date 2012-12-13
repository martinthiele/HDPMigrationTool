/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class MapReduce.
 */
public class MapReduce extends Component {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new map reduce.
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
	public MapReduce(String distro, String installationType, User user, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super("mapreduce", distro, installationType, user, homeLocation, configLocation, adminUser, upgradeStage);
		addService(new JobTrackerService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);
		addService(new JobHistoryService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);
		addService(new TaskTrackerService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_SLAVE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Component#backupData(java.lang.String
	 * )
	 */
	@Override
	public void backupData(String upgradeStage) throws Exception {
		// TODO Back up history data
	}

	/**
	 * Capture job tracker ui information.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	public void captureJobTrackerUIInformation(String upgradeStage) throws Exception {
		log.info("Capturing JobTracker UI snapshots");

		String localStatsDir = Config.getLocalStatsDir(upgradeStage, getName(), "html");
		String remoteStatsDir = Config.getRemoteStatsDir(upgradeStage, getName(), "html");
		String tarName = upgradeStage + "-jt-ui-info.tgz";
		String remoteTar = remoteStatsDir + tarName;

		// TODO Get the ports dynamically
		String jobTrackerHost = getServiceByName(Constants.SERVICE_JOB_TRACKER).getHostsList().get(0);
		CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(), "mkdir -m 777 -p  " + remoteStatsDir, getUser());
		CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(), "wget http://" + jobTrackerHost + ":50030/jobtracker.jsp -O " + remoteStatsDir
				+ "/jobtracker.html", getUser());
		CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(), "wget http://" + jobTrackerHost + ":50030/machines.jsp?type=active -O "
				+ remoteStatsDir + "/mapred-live-nodes.html", getUser());
		CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(), "wget http://" + jobTrackerHost + ":50030/machines.jsp?type=blacklisted -O "
				+ remoteStatsDir + "/mapred-blacklisted-nodes.html", getUser());

		CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(), "wget http://" + jobTrackerHost + ":50030/machines.jsp?type=excluded -O "
				+ remoteStatsDir + "/mapred-excluded-nodes.html", getUser());

		// CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(),
		// "tar -czpf " + remoteTar + " -C " + remoteStatsDir + "/ ./",
		// getUser());
		CommandExecutor.executeRemoteCommand(jobTrackerHost, getAdminUser(), SSHUtils.getTarCommand(remoteStatsDir, tarName, remoteStatsDir), getUser());

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + localStatsDir, null, jobTrackerHost);
		CommandExecutor.scpFromRemoteHost(jobTrackerHost, getAdminUser(), remoteTar, localStatsDir, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + localStatsDir + tarName + " -C " + localStatsDir, null, jobTrackerHost);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Component#captureStats(java.lang
	 * .String)
	 */
	@Override
	public void captureStats(String upgradeStage) throws Exception {
		captureJobTrackerUIInformation(upgradeStage);

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

		/*
		 * String mapredDirs = StringUtils
		 * .replace(XMLUtil.getConfigParamValue(Config
		 * .getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName(),
		 * getServiceByName
		 * (Constants.SERVICE_JOB_TRACKER).getHostsList().get(0)) +
		 * "/mapred-site.xml", "mapred.local.dir"), ",", " "); if ( mapredDirs
		 * != null ) {
		 * CommandExecutor.executeRemoteCommand(getServiceByName(Constants
		 * .SERVICE_JOB_TRACKER).getHostsList(), getAdminUser(), "chmod 755 " +
		 * mapredDirs, null) ;
		 * CommandExecutor.executeRemoteCommand(getServiceByName
		 * (Constants.SERVICE_JOB_HISTORY_SERVER).getHostsList(),
		 * getAdminUser(), "chmod 755 " + mapredDirs, null) ;
		 * CommandExecutor.executeRemoteCommand
		 * (getServiceByName(Constants.SERVICE_TASK_TRACKER).getHostsList(),
		 * getAdminUser(), "chmod 755 " + mapredDirs, null) ; }
		 */

		startAll();
		Thread.currentThread();
		// Thread.sleep(60000);
		captureStats(Constants.UPGRADE_STAGE_POST);

		boolean success = validateStats();
		if (success) {
			log.info("\n\n*** " + getName() + " is now successfully upgraded. Please verify the pre/post  details available under "
					+ Config.getLocalStatsDir("*", getName(), "*") + " ***\n\n");
		} else {
			log.error("\n\n*** " + getName() + " upgrade was NOT successful. Please verify the pre/post  details available under "
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
		backupData(Constants.UPGRADE_STAGE_PRE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#validateStats()
	 */
	@Override
	public boolean validateStats() throws Exception {
		boolean success = true;
		// TODO What else can be added for validations?
		return success;

	}
}
