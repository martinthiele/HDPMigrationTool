/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class HBaseService.
 */
public abstract class HBaseService extends Service {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new h base service.
	 * 
	 * @param name
	 *            the name
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
	public HBaseService(String name, User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
		super(name, serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Service#getStartServiceCommand()
	 */
	@Override
	public String getStartServiceCommand() {
		String serviceName = getName();
		if (Constants.SERVICE_HBASE_MASTER.equalsIgnoreCase(serviceName)) {
			serviceName = "master";
		}
		// return getHomeLocation() + "/bin/hbase-daemon.sh --config " +
		// getConfigLocation() + " start " + serviceName + " ; sleep 25 ";
		return getHomeLocation() + "/bin/hbase-daemon.sh --config " + getConfigLocation() + " start " + serviceName;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getStopServiceCommand()
	 */
	@Override
	public String getStopServiceCommand() {
		String serviceName = getName();
		if (Constants.SERVICE_HBASE_MASTER.equalsIgnoreCase(serviceName)) {
			serviceName = "master";
		}
		// return getHomeLocation() + "/bin/hbase-daemon.sh --config " +
		// getConfigLocation() + " stop " + serviceName + " ; sleep 25 ";
		return getHomeLocation() + "/bin/hbase-daemon.sh --config " + getConfigLocation() + " stop " + serviceName;
	}

}
