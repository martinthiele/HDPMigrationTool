/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.util.List;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class HiveMetastoreService.
 */
public class HiveMetastoreService extends Service {

	/** The Constant configFiles. */
	private static final String[] configFiles = { "hive-site.xml" };

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new hive metastore service.
	 * 
	 * @param serviceOwnerUser
	 *            the service owner user
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
	public HiveMetastoreService(User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
		super("hivemetastore", serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getConfigFileNames()
	 */
	@Override
	public String[] getConfigFileNames() {
		return configFiles;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getProcessPattern()
	 */
	@Override
	public String[] getProcessPattern() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Service#getStartServiceCommand()
	 */
	@Override
	public String getStartServiceCommand() {
		return "nohup " + getHomeLocation() + "/bin/hive --service metastore 1> /tmp/hive.out 2> /tmp/hive.log & ";

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getStopServiceCommand()
	 */
	@Override
	public String getStopServiceCommand() {
		return "ps aux | awk '{print $1,$2}' | grep hive | awk '{print $2}' | xargs -r kill >/dev/null 2>&1";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#stopService()
	 */
	@Override
	public List<ExecutionResult> stopService() throws Exception {
		List<ExecutionResult> result = CommandExecutor.executeRemoteCommand(getHostsList(), getAdminUser(), getStopServiceCommand(), null);
		return result;
	}
}
