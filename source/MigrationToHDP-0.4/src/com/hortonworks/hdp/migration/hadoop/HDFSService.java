/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class HDFSService.
 */
public abstract class HDFSService extends CoreHadoopService {

	/** The Constant configFiles. */
	private static final String[] configFiles = { "hdfs-site.xml", "core-site.xml" };

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new hDFS service.
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
	public HDFSService(String name, User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
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
