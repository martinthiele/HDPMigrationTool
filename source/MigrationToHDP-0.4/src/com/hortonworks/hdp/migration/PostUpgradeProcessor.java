/*
 * 
 */
package com.hortonworks.hdp.migration;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.hadoop.Component;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class PostUpgradeProcessor.
 */
public class PostUpgradeProcessor extends UpgradeProcessor {

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
		 * if (args != null && args.length < 2) { usage(); System.exit(1) ; }
		 * PostUpgradeProcessor processor = new PostUpgradeProcessor(args[0],
		 * args[1]);
		 */
		PostUpgradeProcessor processor = new PostUpgradeProcessor(null, null);
		processor.process();
	}

	/**
	 * Usage.
	 */
	public static void usage() {
		log.error("Usage: java " + PostUpgradeProcessor.class.getName()
				+ " <current distro: apache|cdh3u0|cdh3u1|cdh3u2|cdh3u3|cdh3u4> <current installation type: rpm|tar>");
		log.error("Example: java " + PostUpgradeProcessor.class.getName() + " cdh3u0 rpm");
	}

	/**
	 * Instantiates a new post upgrade processor.
	 * 
	 * @param distro
	 *            the distro
	 * @param installationType
	 *            the installation type
	 * @throws Exception
	 *             the exception
	 */
	public PostUpgradeProcessor(String distro, String installationType) throws Exception {
		super(Constants.UPGRADE_STAGE_POST, distro, installationType);
	}

	/**
	 * Process.
	 */
	public void process() {
		try {
			String additionalMessage = "";
			validateAndInitialize(Constants.UPGRADE_STAGE_POST, true);
			for (String componentName : componentHierarchy) {
				Component component = getComponentByName(componentName);
				if (component != null) {
					if (StringUtils.equalsIgnoreCase(Constants.COMPONENT_HIVE, component.getName())) {
						additionalMessage = "Please make sure that hive upgrade script is executed and JDBC driver jar is copied to "
								+ component.getHomeLocation() + "/lib/ .";
					} else {
						additionalMessage = "";
					}
					while (!SSHUtils.confirmAction("Performing upgrade activities for " + component.getName() + ". " + additionalMessage
							+ "Are you ready to proceed ?")) {
						;
					}
					component.performPostUpgradeActivities();

				}
			}
			log.info("");
			log.info("");
			log.info("----------------------------------------------------------------------------------------------------------------------------------------------");
			log.info("*** Cluster has been upgraded. Please verify the results. Finalize when all apps are validated. ***");
			log.info("----------------------------------------------------------------------------------------------------------------------------------------------");
			log.info("");
			log.info("");

		} catch (Exception e) {
			log.error("Failed to perform post upgrade process.", e);
		}
	}

}
