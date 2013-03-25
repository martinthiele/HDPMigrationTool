/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class Service.
 */
public abstract class Service {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The admin user. */
	private User adminUser;

	/** The config location. */
	private String configLocation;

	/** The home location. */
	private String homeLocation;

	/** The hosts list. */
	private List<String> hostsList;

	/** The name. */
	private String name;

	/** The user. */
	private User user;

	/**
	 * Instantiates a new service.
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
	public Service(String name, User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
		this.name = name;
		this.adminUser = adminUser;
		this.configLocation = configLocation;
		this.homeLocation = homeLocation;
		this.user = serviceOwnerUser;
		hostsList = ServiceUtils.readHostNamesFromFile(Config.getLocalClusterInfoDir(upgradeStage) + name, Config.getLocalClusterInfoDir(Constants.DEFAULT_DIR)
				+ name);
		if (hostsList == null || hostsList.size() == 0) {
			throw new Exception("The hosts file for " + name + " service is not created. Please create a file " + Config.getLocalClusterInfoDir(upgradeStage)
					+ name + " or " + Config.getLocalClusterInfoDir(Constants.DEFAULT_DIR) + name + " and specify fully qualified host(s) that run the " + name
					+ " service");
		}
	}

	/**
	 * Gets the admin user.
	 * 
	 * @return the admin user
	 */
	public User getAdminUser() {
		return adminUser;
	}

	/**
	 * Gets the config file names.
	 * 
	 * @return the config file names
	 */
	public abstract String[] getConfigFileNames();

	/**
	 * Gets the config location.
	 * 
	 * @return the config location
	 */
	public String getConfigLocation() {
		return configLocation;
	}

	/**
	 * Gets the home location.
	 * 
	 * @return the home location
	 */
	public String getHomeLocation() {
		return homeLocation;
	}

	/**
	 * Gets the hosts list.
	 * 
	 * @return the hosts list
	 */
	public List<String> getHostsList() {
		return hostsList;
	}

	/**
	 * Gets the kill process command.
	 * 
	 * @return the kill process command
	 * @throws Exception
	 *             the exception
	 */
	public String getKillProcessCommand() throws Exception {
		String eGrepPattern = "";
		if (getProcessPattern() != null) {
			for (String pattern : getProcessPattern()) {

				eGrepPattern += "[" + pattern.charAt(0) + "]" + pattern.substring(1) + "|";
			}
			eGrepPattern = eGrepPattern.substring(0, (eGrepPattern.length() - 1));
			String command = "ps aux | egrep '" + eGrepPattern + "' | awk '{print \\$2}' |  xargs -r kill";
			return command;
		} else {
			return null;
		}
	}

	/**
	 * Gets the name.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the primary configuration source.
	 * 
	 * @return the primary configuration source
	 */
	public String getPrimaryConfigurationSource() {
		return Constants.UPGRADE_STAGE_POST;
	}

	/**
	 * Gets the process pattern.
	 * 
	 * @return the process pattern
	 */
	public abstract String[] getProcessPattern();

	/**
	 * Specified configuration parameters will NOT be merged. If left null (not
	 * blank) no parameters will be excluded. This takes precedence over
	 * getPropertiesToIncludeDuringMerge.
	 * 
	 * @param serviceConfigFileName
	 *            the service config file name
	 * @return the properties to exclude during merge
	 */
	public List<String> getPropertiesToExcludeDuringMerge(String serviceConfigFileName) {
		return getPropertyNamesFromFile(serviceConfigFileName + ".exclude");
	}

	/**
	 * Only specified configuration parameters will be merged. If left null (not
	 * blank) all parameters will be merged.
	 * 
	 * @param serviceConfigFileName
	 *            the service config file name
	 * @return the properties to include during merge
	 */
	public List<String> getPropertiesToIncludeDuringMerge(String serviceConfigFileName) {
		return getPropertyNamesFromFile(serviceConfigFileName + ".include");
	}

	/**
	 * Gets the property names from file.
	 * 
	 * @param mergeInclusionFileName
	 *            the merge inclusion file name
	 * @return the property names from file
	 */
	private List<String> getPropertyNamesFromFile(String mergeInclusionFileName) {

		return ServiceUtils.readUniqueLinesFromFile(true, Config.getLocalClusterInfoDir(Constants.DEFAULT_DIR) + "/migration-config/merge-properties/"
				+ mergeInclusionFileName);
	}

	/**
	 * Gets the start service command.
	 * 
	 * @return the start service command
	 */
	public abstract String getStartServiceCommand();

	/**
	 * Gets the stop service command.
	 * 
	 * @return the stop service command
	 */
	public abstract String getStopServiceCommand();

	/**
	 * Gets the user.
	 * 
	 * @return the user
	 */
	public User getUser() {
		return user;
	}

	/**
	 * Kill process.
	 * 
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public List<ExecutionResult> killProcess() throws Exception {
		String command = getKillProcessCommand();
		if (StringUtils.isNotBlank(command)) {
			return CommandExecutor.executeRemoteCommand(hostsList, adminUser, command, null);
		} else {
			return null;
		}
	}

	/**
	 * Sets the admin user.
	 * 
	 * @param adminUser
	 *            the new admin user
	 */
	public void setAdminUser(User adminUser) {
		this.adminUser = adminUser;
	}

	/**
	 * Sets the config location.
	 * 
	 * @param configLocation
	 *            the new config location
	 */
	public void setConfigLocation(String configLocation) {
		this.configLocation = configLocation;
	}

	/**
	 * Sets the home location.
	 * 
	 * @param homeLocation
	 *            the new home location
	 */
	public void setHomeLocation(String homeLocation) {
		this.homeLocation = homeLocation;
	}

	/**
	 * Sets the hosts list.
	 * 
	 * @param hostsList
	 *            the new hosts list
	 */
	protected void setHostsList(List<String> hostsList) {
		this.hostsList = hostsList;
	}

	/**
	 * Sets the name.
	 * 
	 * @param name
	 *            the new name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Sets the user.
	 * 
	 * @param user
	 *            the new user
	 */
	public void setUser(User user) {
		this.user = user;
	}

	/**
	 * Start service.
	 * 
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public List<ExecutionResult> startService() throws Exception {
		return CommandExecutor.executeRemoteCommand(getHostsList(), getAdminUser(), getStartServiceCommand(), getUser(),
				Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Stop service.
	 * 
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public List<ExecutionResult> stopService() throws Exception {
		List<ExecutionResult> result = CommandExecutor.executeRemoteCommand(getHostsList(), getAdminUser(), new String[] { getStopServiceCommand(),
				getKillProcessCommand() }, getUser());
		return result;
	}

}
