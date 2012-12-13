/*
 * 
 */
package com.hortonworks.hdp.migration;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.hadoop.Component;
import com.hortonworks.hdp.migration.hadoop.ComponentFactory;
import com.hortonworks.hdp.migration.hadoop.Service;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class UpgradeProcessor.
 */
public class UpgradeProcessor {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * @throws Exception
	 *             the exception
	 */
	public static void main(String[] args) throws Exception {
		/*
		 * if (args.length < 2 ||
		 * !(Constants.UPGRADE_STAGE_PRE.equalsIgnoreCase(args[0]) ||
		 * Constants.UPGRADE_STAGE_POST.equalsIgnoreCase(args[0]))) { usage();
		 * System.exit(1); }
		 */
		if (Constants.UPGRADE_STAGE_PRE.equalsIgnoreCase(args[0])) {
			// PreUpgradeProcessor processor = new PreUpgradeProcessor(args[1],
			// args[2]);
			PreUpgradeProcessor processor = new PreUpgradeProcessor(null, null);
			processor.process();
		} else if (Constants.UPGRADE_STAGE_POST.equalsIgnoreCase(args[0])) {

			// PostUpgradeProcessor processor = new
			// PostUpgradeProcessor(args[1], args[2]);
			PostUpgradeProcessor processor = new PostUpgradeProcessor(null, null);
			processor.process();
		}
	}

	/** The admin user. */
	protected User adminUser = null;

	/** The component hierarchy. */
	protected final String[] componentHierarchy = { Constants.COMPONENT_HDFS, Constants.COMPONENT_MAP_REDUCE, Constants.COMPONENT_ZOOKEEPER,
			Constants.COMPONENT_HBASE, Constants.COMPONENT_HCATALOG, Constants.COMPONENT_HIVE, Constants.COMPONENT_PIG, Constants.COMPONENT_SQOOP,
			Constants.COMPONENT_OOZIE, Constants.COMPONENT_TEMPLETON, Constants.COMPONENT_FLUME, Constants.COMPONENT_GANGLIA, Constants.COMPONENT_NAGIOS };

	/** The distro. */
	protected String distro;

	/** The installation type. */
	protected String installationType;

	/** The installed components. */
	protected List<Component> installedComponents;

	/** The upgrade stage. */
	protected String upgradeStage;

	/**
	 * Instantiates a new upgrade processor.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param distro
	 *            the distro
	 * @param installationType
	 *            the installation type
	 * @throws Exception
	 *             the exception
	 */
	public UpgradeProcessor(String upgradeStage, String distro, String installationType) throws Exception {
		this.upgradeStage = upgradeStage;
		this.distro = distro;
		this.installationType = installationType;
	}

	/**
	 * Ask for admin id password.
	 */
	protected void askForAdminIdPassword() {
		adminUser = SSHUtils.readUserIdPassword(
				"\n\nPlease provide user id to be used for ssh into the cluster nodes. This should be a power user who has access to su to all hadoop users",
				"root");
	}

	/**
	 * Check radiness.
	 */
	public void checkRadiness() {
		// TODO
		/*
		 * put checks such as hostname correctness ability to access and log in
		 * ability (uid/pwd) permissions on necessary files and directories user
		 * availability and ability to su to different users on all the nodes
		 */
	}

	/**
	 * Gets the component by name.
	 * 
	 * @param componentName
	 *            the component name
	 * @return the component by name
	 */
	protected Component getComponentByName(String componentName) {
		Component component = null;
		for (Component comp : installedComponents) {
			if (comp.getName().equalsIgnoreCase(componentName.trim())) {
				component = comp;
				break;
			}
		}
		return component;

	}

	/**
	 * Pull configurations from nodes.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	protected void pullConfigurationsFromNodes(String upgradeStage) throws Exception {
		for (Component component : installedComponents) {
			component.pullConfigurationsFromAllHosts(upgradeStage);
		}
	}

	/**
	 * Read migration properties.
	 */
	private void readMigrationProperties()

	{
		Properties props = new Properties();
		String propertiesFile = "./hadoop-upgrade/default/migration-config/execution-control.properties";
		try {
			props.load(new FileInputStream(propertiesFile));
			log.debug("Execution properties are: " + props);
			System.getProperties().putAll(props);
		} catch (Exception e) {
			log.error("Unable to read properties file " + propertiesFile + ".  All default properties will be used.", e);
		}
	}

	// protected void prepareEnv() throws IOException {
	//
	// String[] configVariations = new String[] { "pre-upgrade", "post-upgrade",
	// "common", "default", "merged" };
	// String[] configLocations = { Constants.CLUSTER_INFO_DIR,
	// Constants.UPGRADE_CONFIG_DIR, Constants.UPGRADE_STATS_DIR };
	//
	// for (String configVariation : configVariations)
	// for (String configLocation : configLocations) {
	// File file = new File(configLocation + "/" + configVariation);
	// file.mkdirs();
	// }
	// }

	// TODO - Cleanup process that will reset all the files/configs back to
	// original

	/**
	 * Start components.
	 * 
	 * @param componentNamesTobeStarted
	 *            the component names tobe started
	 * @throws Exception
	 *             the exception
	 */
	public void startComponents(String[] componentNamesTobeStarted) throws Exception {

		for (String componentName : componentHierarchy) {
			if (ArrayUtils.contains(componentNamesTobeStarted, componentName)) {
				Component component = getComponentByName(componentName);
				if (component != null) {
					component.startAll();
				}
			}
		}
	}

	/**
	 * Stop components.
	 * 
	 * @param componentNamesTobeStopped
	 *            the component names tobe stopped
	 * @throws Exception
	 *             the exception
	 */
	public void stopComponents(String[] componentNamesTobeStopped) throws Exception {

		for (int i = componentHierarchy.length; i > 0; i--) {
			if (ArrayUtils.contains(componentNamesTobeStopped, componentHierarchy[i - 1])) {
				Component component = getComponentByName(componentHierarchy[i - 1]);
				if (component != null) {
					component.stopAll();
				}
			}
		}
	}

	/**
	 * Validate and initialize.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param cleanup
	 *            the cleanup
	 * @throws Exception
	 *             the exception
	 */
	public void validateAndInitialize(String upgradeStage, boolean cleanup) throws Exception {
		log.info("####################################################################################");
		log.info("###     BEGINING THE " + StringUtils.upperCase(upgradeStage) + " PROCESS AT " + new Date() + "     ###");
		log.info("####################################################################################");

		readMigrationProperties();

		askForAdminIdPassword();
		installedComponents = ComponentFactory.createComponents(upgradeStage, distro, installationType, adminUser);
		List<ExecutionResult> resultsList = new ArrayList<ExecutionResult>();
		
		/*
		if (cleanup) {
			log.info("Checking all the hosts and cleaning up old upgrade data.");

			for (Component component : installedComponents) {
				for (Service service : component.getServiceList()) {

					resultsList.addAll(CommandExecutor.executeRemoteCommand(service.getHostsList(), adminUser,
							" rm -rf " + Config.getRemoteDataDir(upgradeStage, component.getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, "*"), null));
					resultsList.addAll(CommandExecutor.executeRemoteCommand(service.getHostsList(), adminUser,
							" rm -rf " + Config.getRemoteServiceConfigDir(upgradeStage, component.getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, "*"),
							null));
					resultsList.addAll(CommandExecutor.executeRemoteCommand(service.getHostsList(), adminUser,
							" rm -rf " + Config.getRemoteStatsDir(upgradeStage, component.getName(), null), null));
				}
				CommandExecutor.executeLocalCommand(
						" rm -rf " + Config.getLocalDataDir(upgradeStage, component.getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, "*"), null,
						(String) null);
				CommandExecutor.executeLocalCommand(
						" rm -rf " + Config.getLocalServiceConfigDir(upgradeStage, component.getName()).replaceAll(Constants.HOST_NAME_PLACEHOLDER, "*"), null,
						(String) null);
				CommandExecutor.executeLocalCommand(" rm -rf " + Config.getLocalStatsDir(upgradeStage, component.getName(), null), null, (String) null);

			}
		}
*/
		HashSet<String> failedHostsSet = new HashSet<String>();
		for (ExecutionResult er : resultsList) {
			if (er != null && er.getExitCode() != 0) {
				failedHostsSet.add(er.getHostname());
			}
		}
		if (failedHostsSet.size() > 0) {
			log.error("Could not successfully connect to following hosts : " + failedHostsSet);
			if (!SSHUtils.confirmAction("Do you still want to continue?")) {
				if (SSHUtils.confirmAction("Do you want to re-enter user info and try?")) {
					validateAndInitialize(upgradeStage, cleanup);
				} else {
					log.info("Existing as user did not want to continue");
					System.exit(1);
				}
			}
		}
	}

}
