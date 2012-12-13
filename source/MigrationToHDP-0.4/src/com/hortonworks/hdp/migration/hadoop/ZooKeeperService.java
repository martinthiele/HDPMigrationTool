/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class ZooKeeperService.
 */
public class ZooKeeperService extends Service {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant processPattern. */
	private static final String[] processPattern = { "zookeeper", "org.apache.zookeeper.server.quorum.QuorumPeerMain" };

	/**
	 * Instantiates a new zoo keeper service.
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
	public ZooKeeperService(User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
		super("zookeeper", serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getConfigFileNames()
	 */
	@Override
	public String[] getConfigFileNames() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getProcessPattern()
	 */
	@Override
	public String[] getProcessPattern() {

		return processPattern;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Service#getStartServiceCommand()
	 */
	@Override
	public String getStartServiceCommand() {
		return getHomeLocation() + "/bin/zkServer.sh start ";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getStopServiceCommand()
	 */
	@Override
	public String getStopServiceCommand() {
		return getHomeLocation() + "/bin/zkServer.sh stop ";
	}
}
