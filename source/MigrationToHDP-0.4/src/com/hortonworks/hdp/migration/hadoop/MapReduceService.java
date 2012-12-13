/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.User;

// TODO: Auto-generated Javadoc
/**
 * The Class MapReduceService.
 */
public abstract class MapReduceService extends CoreHadoopService {

	/** The Constant configFiles. */
	private static final String[] configFiles = { "mapred-site.xml" };

	/**
	 * Instantiates a new map reduce service.
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
	public MapReduceService(String name, User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super(name, serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
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

}
