/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.io.FileInputStream;
import java.util.List;
import java.util.Properties;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class ZooKeeper.
 */
public class ZooKeeper extends Component {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new zoo keeper.
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
	public ZooKeeper(String distro, String installationType, User user, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super("zookeeper", distro, installationType, user, homeLocation, configLocation, adminUser, upgradeStage);
		addService(new ZooKeeperService(user, homeLocation, configLocation, adminUser, upgradeStage), Constants.SERVICE_TYPE_MASTER);

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
		Properties prop = new Properties();
		FileInputStream in = null;
		String zkDataDir = null;
		List<Service> zkServices = getServiceList();

		for (Service zkService : zkServices) {
			for (String hostName : zkService.getHostsList()) {
				prop.clear();
				in = new FileInputStream(Config.getLocalServiceConfigDir(upgradeStage, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostName)
						+ "/zoo.cfg");
				prop.load(in);
				zkDataDir = prop.getProperty("dataDir");
				String tarRemoteDir = Config.getRemoteDataDir(upgradeStage, getName());
				String tarLocalDir = Config.getLocalDataDir(upgradeStage, getName());
				String tarName = "zookeeper-data-dir-" + upgradeStage + ".tgz";
				String tarFullyQualifiedRemotePath = tarRemoteDir + tarName;

				CommandExecutor.executeRemoteCommand(hostName, getAdminUser(), "mkdir -m 777 -p  " + tarRemoteDir, getUser());
				// CommandExecutor.executeRemoteCommand(hostName,
				// getAdminUser(), "tar -czpf " + tarFullyQualifiedRemotePath +
				// " -C " + zkDataDir + " ./",
				// getUser());
				CommandExecutor.executeRemoteCommand(hostName, getAdminUser(), SSHUtils.getTarCommand(tarRemoteDir, tarName, zkDataDir), null);

				CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + tarLocalDir, null, hostName);
				CommandExecutor.scpFromRemoteHost(hostName, getAdminUser(), tarFullyQualifiedRemotePath, tarLocalDir, getUser());
			}
		}

		in.close();

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
		// TODO Nothing to capture as of now. Find out ways to capture stats

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#getVersion()
	 */
	@Override
	public String getVersion() {
		// TODO currently ZK does not have good way to identify version. Fix is
		// later.
		return null;
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
		startAll();
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
		backupData(Constants.UPGRADE_STAGE_PRE);
		stopAll();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Component#validateStats()
	 */
	@Override
	protected boolean validateStats() throws Exception {
		// TODO Nothing to validate as of now. Find out ways to validate upgrade
		return true;
	}

}
