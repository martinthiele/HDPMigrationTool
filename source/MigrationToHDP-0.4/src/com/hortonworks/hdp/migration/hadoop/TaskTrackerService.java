/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class TaskTrackerService.
 */
public class TaskTrackerService extends MapReduceService {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant processPattern. */
	private static final String[] processPattern = { "Dproc_tasktracker", "org.apache.hadoop.mapred.TaskTracker" };

	/**
	 * Instantiates a new task tracker service.
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
	public TaskTrackerService(User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {

		super("tasktracker", serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
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
}
