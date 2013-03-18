/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.collections.ListUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.file.util.XMLUtil;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class Component.
 */
public abstract class Component {

	/** The Constant CONFIG_TYPE_MERGED. */
	private static final String CONFIG_TYPE_MERGED = "3";

	/** The Constant CONFIG_TYPE_OTHER. */
	private static final String CONFIG_TYPE_OTHER = "4";

	/** The Constant CONFIG_TYPE_POST_UPGRADE. */
	private static final String CONFIG_TYPE_POST_UPGRADE = "2";

	/** The Constant CONFIG_TYPE_PRE_UPGRADE. */
	private static final String CONFIG_TYPE_PRE_UPGRADE = "1";

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The admin user. */
	private User adminUser;

	/** The config location. */
	private String configLocation;

	/** The distro. */
	private String distro;

	/** The home location. */
	private String homeLocation;

	/** The installation type. */
	private String installationType;

	/** The masters service list. */
	private List<Service> mastersServiceList = new ArrayList<Service>();

	/** The name. */
	private String name;

	/** The service list. */
	private List<Service> serviceList = new ArrayList<Service>();

	/** The slaves service list. */
	private List<Service> slavesServiceList = new ArrayList<Service>();

	/** The user. */
	private User user;

	/**
	 * Instantiates a new component.
	 * 
	 * @param name
	 *            the name
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
	 */
	public Component(String name, String distro, String installationType, User user, String homeLocation, String configLocation, User adminUser,
			String upgradeStage) {
		this.name = name;
		this.distro = distro;
		this.installationType = installationType;
		this.user = user;
		this.homeLocation = homeLocation;
		this.configLocation = configLocation;
		this.adminUser = adminUser;

	}

	/**
	 * Adds the service.
	 * 
	 * @param service
	 *            the service
	 * @param serviceType
	 *            the service type
	 */
	public void addService(Service service, int serviceType) {
		this.serviceList.add(service);
		if (Constants.SERVICE_TYPE_MASTER == serviceType) {
			this.mastersServiceList.add(service);
		} else if (Constants.SERVICE_TYPE_SLAVE == serviceType) {
			this.slavesServiceList.add(service);
		}
	}

	/**
	 * Backup data.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected abstract void backupData(String upgradeStage) throws Exception;

	/**
	 * Backup installation.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void backupInstallation() throws Exception {

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_COMMON_BACKUP_INSTALLATION_DIR, Boolean.TRUE.toString()),
				Boolean.FALSE.toString())) {
			log.warn("Skipping back up of installation directory of " + getName() + " as specified in property '"
					+ Constants.MIGRATION_PROPERTY_COMMON_BACKUP_INSTALLATION_DIR
					+ "'. Please note that now the onus is on you to track pre and post installation binaries.");
			return;
		}

		Service service = mastersServiceList.get(0);
		String hostName = service.getHostsList().get(0);

		log.info("Backing up the installation home directory (" + getHomeLocation() + ") for " + getName() + " from " + hostName);

		String tarName = Config.getInstallationTarFileName(Constants.UPGRADE_STAGE_PRE, getName());
		String tarRemoteDir = Config.getRemoteDataDir(Constants.UPGRADE_STAGE_PRE, getName());
		String tarFullyQualifiedRemotePath = tarRemoteDir + tarName;
		String tarLocalDir = Config.getLocalDataDir(Constants.UPGRADE_STAGE_PRE, getName());

		CommandExecutor.executeRemoteCommand(hostName, adminUser, "mkdir -m 777 -p  " + tarRemoteDir, getUser());
		// CommandExecutor.executeRemoteCommand(hostName, adminUser,
		// "tar -czpf " + tarFullyQualifiedRemotePath + " -C " +
		// getHomeLocation() + " ./", getUser());
		CommandExecutor.executeRemoteCommand(hostName, adminUser, SSHUtils.getTarCommand(tarRemoteDir, tarName, getHomeLocation()), null);

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + tarLocalDir, null, hostName);
		CommandExecutor.scpFromRemoteHost(hostName, adminUser, tarFullyQualifiedRemotePath, tarLocalDir, getUser());

	}

	/**
	 * Capture stats.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected abstract void captureStats(String upgradeStage) throws Exception;

	/**
	 * Gets the admin user.
	 * 
	 * @return the admin user
	 */
	public User getAdminUser() {
		return adminUser;
	}

	/**
	 * Gets the config location.
	 * 
	 * @return the config location
	 */
	public String getConfigLocation() {
		return configLocation;
	}

	/**
	 * Gets the distro.
	 * 
	 * @return the distro
	 */
	public String getDistro() {
		return distro;
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
	 * Gets the installation type.
	 * 
	 * @return the installation type
	 */
	public String getInstallationType() {
		return installationType;
	}

	/**
	 * Gets the masters service list.
	 * 
	 * @return the masters service list
	 */
	public List<Service> getMastersServiceList() {
		return mastersServiceList;
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
	 * Gets the service by name.
	 * 
	 * @param serviceName
	 *            the service name
	 * @return the service by name
	 */
	public Service getServiceByName(String serviceName) {
		Service service = null;
		for (Service serv : serviceList) {
			if (serv.getName().equals(serviceName)) {
				service = serv;
				break;
			}
		}
		return service;
	}

	/**
	 * Gets the service list.
	 * 
	 * @return the service list
	 */
	public List<Service> getServiceList() {
		return serviceList;
	}

	/**
	 * Gets the slaves service list.
	 * 
	 * @return the slaves service list
	 */
	public List<Service> getSlavesServiceList() {
		return slavesServiceList;
	}

	/**
	 * Gets the user.
	 * 
	 * @return the user
	 */
	public User getUser() {
		return user;
	}

	/**
	 * Gets the version.
	 * 
	 * @return the version
	 * @throws Exception
	 *             the exception
	 */
	public abstract String getVersion() throws Exception;

	/**
	 * Merge configurations.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void mergeConfigurations() throws Exception {
		log.info("Merging pre and post upgrade configurations for " + getName());

		for (Service service : serviceList) {
			for (String hostName : service.getHostsList()) {
				if (service.getConfigFileNames() != null) {
					for (String configFileName : service.getConfigFileNames()) {
						log.info("Config file " + configFileName);

						String configXmlString = null;
						if (Constants.UPGRADE_STAGE_PRE.equals(service.getPrimaryConfigurationSource())) {
							configXmlString = XMLUtil.mergePostInstallXMLIntoPreInstallXML(
									Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
											hostName)
											+ configFileName,
									Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_POST, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
											hostName)
											+ configFileName, service.getPropertiesToIncludeDuringMerge(configFileName),
									service.getPropertiesToExcludeDuringMerge(configFileName));
						} else if (Constants.UPGRADE_STAGE_POST.equals(service.getPrimaryConfigurationSource())) {
							configXmlString = XMLUtil.mergePreInstallXMLIntoPostInstallXML(
									Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
											hostName)
											+ configFileName,
									Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_POST, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
											hostName)
											+ configFileName, service.getPropertiesToIncludeDuringMerge(configFileName),
									service.getPropertiesToExcludeDuringMerge(configFileName));
						}

						File file = new File(Config.getLocalServiceConfigDir(Constants.MERGED_DIR, getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER,
								hostName)
								+ configFileName);
						FileUtils.writeStringToFile(file, configXmlString);
					}
				}
			}

		}
	}

	/**
	 * Perform post upgrade activities.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void performPostUpgradeActivities() throws Exception {
		log.info("Performing post-upgrade activities for " + getName());
		pullConfigurationsFromAllHosts(Constants.UPGRADE_STAGE_POST);
		String configType = CONFIG_TYPE_POST_UPGRADE;
		String configLocation = Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName());

		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_COMMON_BACKUP_CONFIG_FILES, Boolean.TRUE.toString()),
				Boolean.TRUE.toString())) {

			configType = SSHUtils.readUserInput("Which configurations do you want to use for " + getName() + " ? " + CONFIG_TYPE_PRE_UPGRADE + "=pre-upgrade, "
					+ CONFIG_TYPE_POST_UPGRADE + "=post-upgrade, " + CONFIG_TYPE_MERGED + "=merged, " + CONFIG_TYPE_OTHER + "=other", new String[] {
					CONFIG_TYPE_PRE_UPGRADE, CONFIG_TYPE_POST_UPGRADE, CONFIG_TYPE_MERGED, CONFIG_TYPE_OTHER }, false, CONFIG_TYPE_MERGED);


			if (StringUtils.equalsIgnoreCase(CONFIG_TYPE_PRE_UPGRADE, configType)) {
				configLocation = Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_PRE, getName());
			} else if (StringUtils.equalsIgnoreCase(CONFIG_TYPE_POST_UPGRADE, configType)) {
				configLocation = Config.getLocalServiceConfigDir(Constants.UPGRADE_STAGE_POST, getName());
			} else if (StringUtils.equalsIgnoreCase(CONFIG_TYPE_MERGED, configType)) {
				configLocation = Config.getLocalServiceConfigDir(Constants.MERGED_DIR, getName());
				mergeConfigurations();
			} else if (StringUtils.equalsIgnoreCase(CONFIG_TYPE_OTHER, configType)) {
				boolean validPath = false;
				do {
					configLocation = SSHUtils.readUserInput("Please specify the directory location of configuration files to be used.", null, false, null)
							.trim();
					File confDir = new File(configLocation);
					validPath = confDir.exists() && confDir.isDirectory();
					if (validPath) {
						Collection<File> XMLFiles = FileUtils.listFiles(confDir, new WildcardFileFilter("*-site.xml"), null);
						if (XMLFiles == null || XMLFiles.size() == 0) {
							validPath = SSHUtils.confirmAction("The config location you specified '" + configLocation
									+ "' does not contain any site.xml. Do you still want to proceed?");
						}
					} else {
						log.error("The path '" + configLocation
								+ "' does not exist or is not a directory. Please provide valid directory path that has config files for " + getName());
					}
				} while (!validPath);

			}
		} else {
			configType = CONFIG_TYPE_POST_UPGRADE;
		}
		if (StringUtils.equalsIgnoreCase(CONFIG_TYPE_POST_UPGRADE, configType)) {
			log.info("Keeping the post installation configurations as is for " + getName());
		} else {
			pushConfigurationsToAllHosts(configLocation);
		}
	}

	/**
	 * Perform pre upgrade activities.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void performPreUpgradeActivities() throws Exception {
		log.info("Performing pre-upgrade activities for " + getName());
		pullConfigurationsFromAllHosts(Constants.UPGRADE_STAGE_PRE);
		backupInstallation();
	}

	/**
	 * Pull configurations from all hosts.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	public void pullConfigurationsFromAllHosts(String upgradeStage) throws Exception {
		List<String> hosts = new ArrayList<String>();
		if (StringUtils.equalsIgnoreCase(System.getProperty(Constants.MIGRATION_PROPERTY_COMMON_BACKUP_CONFIG_FILES, Boolean.TRUE.toString()),
				Boolean.FALSE.toString())) {
			log.info("Pulling down all configurations for " + getName() + " from master hosts");
			for (Service service : getMastersServiceList()) {
				hosts = ListUtils.sum(hosts, service.getHostsList());
			}
			log.warn("Skipping back up of configurations for " + getName() + " from all other hosts as specified in property '"
					+ Constants.MIGRATION_PROPERTY_COMMON_BACKUP_CONFIG_FILES
					+ "'. Please note that now the onus is on you to track pre and post install configurations of each host.");
		} else {
			log.info("Pulling down all configurations for " + getName() + " from all hosts");
			for (Service service : serviceList) {
				hosts = ListUtils.sum(hosts, service.getHostsList());
			}
		}
		String tarName = Config.getConfTarFileName(upgradeStage, getName());
		String tarRemoteDir = Config.getRemoteServiceConfigDir(upgradeStage, getName());
		String tarFullyQualifiedRemotePath = tarRemoteDir + tarName;
		String tarLocalDir = Config.getLocalServiceConfigDir(upgradeStage, getName());

		CommandExecutor.executeRemoteCommand(hosts, adminUser, "mkdir -m 777 -p  " + tarRemoteDir, getUser());
		CommandExecutor.executeRemoteCommand(hosts, adminUser, SSHUtils.getTarCommand(tarRemoteDir, tarName, getConfigLocation()), null);

		CommandExecutor.executeLocalCommand("mkdir -m 777 -p  " + tarLocalDir, null, hosts);
		CommandExecutor.scpFromRemoteHost(hosts, adminUser, tarFullyQualifiedRemotePath, tarLocalDir, getUser());
		CommandExecutor.executeLocalCommand("tar -xzpf " + tarLocalDir + tarName + " -C " + tarLocalDir, null, hosts);

	}

	/**
	 * Push configurations to all hosts.
	 * 
	 * @param configLocation
	 *            the config location
	 * @throws Exception
	 *             the exception
	 */
	public void pushConfigurationsToAllHosts(String configLocation) throws Exception {
		log.info("Pushing all configurations for " + getName() + " to all hosts");

		for (Service service : serviceList) {
			log.info("For service " + service.getName());
			if (service.getConfigFileNames() != null) {
				for (String configFileName : service.getConfigFileNames()) {
					String configFileFullyQualifiedPath = configLocation + File.separator + configFileName;
					log.info("configFileFullyQualifiedPath " + configFileFullyQualifiedPath);

					CommandExecutor.scpToRemoteHost(service.getHostsList(), adminUser, configFileFullyQualifiedPath, getConfigLocation() + "/", getUser());
				}
				CommandExecutor.executeRemoteCommand(service.getHostsList(), adminUser, "chmod 775 " + getConfigLocation() + "/* ; chown "
						+ getUser().getUserId() + " " + getConfigLocation() + "/*", null);

			}
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
	 * Sets the user.
	 * 
	 * @param user
	 *            the new user
	 */
	public void setUser(User user) {
		this.user = user;
	}

	/**
	 * Start all.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void startAll() throws Exception {
		startMasters();
		startSlaves();
	}

	/**
	 * Start masters.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void startMasters() throws Exception {
		log.info("Starting all master services for " + getName());
		List<Service> masterServices = getMastersServiceList();
		if (masterServices != null) {
			for (Service masterService : masterServices) {
				log.debug(masterService.getName() + " service ");
				masterService.startService();
			}
		}

		try {
			long waitPeriodInMillis = 30000;
			log.debug("Issued start command to all " + getName() + " masters. Waiting for " + DurationFormatUtils.formatDuration(waitPeriodInMillis, "mm:ss")
					+ " minutes:seconds");
			Thread.sleep(waitPeriodInMillis);
		} catch (InterruptedException e) {
			// Ignore
		}
	}

	/**
	 * Start slaves.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void startSlaves() throws Exception {
		log.info("Starting all slave services for " + getName());
		List<Service> slaveServices = getSlavesServiceList();
		if (slaveServices != null) {
			for (Service slaveService : slaveServices) {
				log.debug(slaveService.getName() + " service ");

				slaveService.startService();
			}
		}

		try {
			long waitPeriodInMillis = 30000;
			log.debug("Issued start command to all " + getName() + " slaves. Waiting for " + DurationFormatUtils.formatDuration(waitPeriodInMillis, "mm:ss")
					+ " minutes:seconds");
			Thread.sleep(waitPeriodInMillis);
		} catch (InterruptedException e) {
			// Ignore
		}

	}

	/**
	 * Stop all.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void stopAll() throws Exception {
		stopMasters();
		stopSlaves();

		try {
			long waitPeriodInMillis = 30000;
			log.debug("Issued stop command to all " + getName() + " hosts. Waiting for " + DurationFormatUtils.formatDuration(waitPeriodInMillis, "mm:ss")
					+ " minutes:seconds");
			Thread.sleep(waitPeriodInMillis);
		} catch (InterruptedException e) {
			// Ignore
			e.printStackTrace();
		}
	}

	/**
	 * Stop masters.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void stopMasters() throws Exception {
		log.info("Stopping all master services for " + getName());
		List<Service> masterServices = getMastersServiceList();
		if (masterServices != null) {
			for (Service masterService : masterServices) {
				log.debug(masterService.getName() + " service ");

				masterService.stopService();
			}
		}

	}

	/**
	 * Stop slaves.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void stopSlaves() throws Exception {
		log.info("Stopping all slave services for " + getName());
		List<Service> slaveServices = getSlavesServiceList();
		if (slaveServices != null) {
			for (Service slaveService : slaveServices) {
				log.debug(slaveService.getName() + " service ");

				slaveService.stopService();
			}
		}
	}

	/**
	 * Validate stats.
	 * 
	 * @return true, if successful
	 * @throws Exception
	 *             the exception
	 */
	protected abstract boolean validateStats() throws Exception;
}
