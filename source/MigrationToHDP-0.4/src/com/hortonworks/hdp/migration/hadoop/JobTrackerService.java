/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class JobTrackerService.
 */
public class JobTrackerService extends MapReduceService {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant processPattern. */
	private static final String[] processPattern = { "Dproc_jobtracker", "org.apache.hadoop.mapred.JobTracker" };

	/**
	 * Instantiates a new job tracker service.
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
	public JobTrackerService(User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
		super("jobtracker", serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
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
	 * com.hortonworks.hdp.migration.hadoop.CoreHadoopService#getStartServiceCommand
	 * ()
	 */
	@Override
	public String getStartServiceCommand() {
		return getStartServiceAndWaitCommand(15);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.CoreHadoopService#getStopServiceCommand
	 * ()
	 */
	@Override
	public String getStopServiceCommand() {
		return getStopServiceAndWaitCommand(15);
	}

}
