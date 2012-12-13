/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class CoreHadoopService.
 */
public abstract class CoreHadoopService extends Service {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Instantiates a new core hadoop service.
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
	public CoreHadoopService(String name, User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage)
			throws Exception {
		super(name, serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
	}

	/**
	 * Gets the start service and wait command.
	 * 
	 * @param secondsToWait
	 *            the seconds to wait
	 * @return the start service and wait command
	 */
	protected String getStartServiceAndWaitCommand(int secondsToWait) {
		// return getHomeLocation() + "/bin/hadoop-daemon.sh --config " +
		// getConfigLocation() + " start " + getName() + " ; sleep " +
		// secondsToWait;
		return getHomeLocation() + "/bin/hadoop-daemon.sh --config " + getConfigLocation() + " start " + getName();
	}

	/**
	 * Gets the start service and wait command.
	 * 
	 * @param additionalSwitches
	 *            the additional switches
	 * @param secondsToWait
	 *            the seconds to wait
	 * @return the start service and wait command
	 */
	protected String getStartServiceAndWaitCommand(String additionalSwitches, int secondsToWait) {

		String startCommand = getHomeLocation() + "/bin/hadoop-daemon.sh --config " + getConfigLocation() + " start " + getName();
		if (StringUtils.isNotBlank(additionalSwitches)) {
			startCommand += " " + additionalSwitches;
		}
		// return startCommand + " ; sleep " + secondsToWait;
		return startCommand;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.Service#getStartServiceCommand()
	 */
	@Override
	public String getStartServiceCommand() {
		return getStartServiceAndWaitCommand(1);
	}

	/**
	 * Gets the stop service and wait command.
	 * 
	 * @param secondsToWait
	 *            the seconds to wait
	 * @param processPatternsToSearchAndKill
	 *            the process patterns to search and kill
	 * @return the stop service and wait command
	 */
	protected String getStopServiceAndWaitCommand(int secondsToWait, String... processPatternsToSearchAndKill) {
		// return getHomeLocation() + "/bin/hadoop-daemon.sh --config " +
		// getConfigLocation() + " stop " + getName() + " ; sleep " +
		// secondsToWait;
		return getHomeLocation() + "/bin/hadoop-daemon.sh --config " + getConfigLocation() + " stop " + getName();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getStopServiceCommand()
	 */
	@Override
	public String getStopServiceCommand() {
		return getStopServiceAndWaitCommand(5);
	}

}
