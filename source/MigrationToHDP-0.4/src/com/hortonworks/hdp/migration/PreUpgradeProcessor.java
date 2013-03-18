/*
 * 
 */
package com.hortonworks.hdp.migration;

import com.hortonworks.hdp.migration.hadoop.Component;
import com.hortonworks.hdp.migration.ssh.SSHUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class PreUpgradeProcessor.
 */
public class PreUpgradeProcessor extends UpgradeProcessor {

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
		 * if (args != null && args.length < 2) { usage(); System.exit(1); }
		 * 
		 * PreUpgradeProcessor processor = new PreUpgradeProcessor(args[0],
		 * args[1]);
		 */
		PreUpgradeProcessor processor = new PreUpgradeProcessor(null, null);
		processor.process();
	}

	/**
	 * Usage.
	 */
	public static void usage() {
		log.error("Usage: java " + PreUpgradeProcessor.class.getName()
				+ " <current distro: apache|cdh3u0|cdh3u1|cdh3u2|cdh3u3|cdh3u4> <current installation type: rpm|tar>");
		log.error("Example: java " + PreUpgradeProcessor.class.getName() + " cdh3u0 rpm");
	}

	/**
	 * Instantiates a new pre upgrade processor.
	 * 
	 * @param distro
	 *            the distro
	 * @param installationType
	 *            the installation type
	 * @throws Exception
	 *             the exception
	 */
	public PreUpgradeProcessor(String distro, String installationType) throws Exception {
		super(Constants.UPGRADE_STAGE_PRE, distro, installationType);
	}

	/**
	 * Process.
	 */
	public void process() {
		try {
			validateAndInitialize(Constants.UPGRADE_STAGE_PRE, true);
			for (int i = componentHierarchy.length; i > 0; i--) {

				Component component = getComponentByName(componentHierarchy[i - 1]);
				if (component != null) {
					if (Constants.COMPONENT_MAP_REDUCE.equalsIgnoreCase(component.getName())) {
						while (!SSHUtils
								.confirmAction("We are about to stop the MapReduce. Please finish all your MapReduce activities, if any, such as distcp etc that you want to perform before upgrade. Are you ready to stop MapReduce ?")) {
							;
						}
					}
					if (Constants.COMPONENT_HDFS.equalsIgnoreCase(component.getName())) {
						while (!SSHUtils
								.confirmAction("We are about to stop the HDFS. Please finish all your back up activities, if any, that you want to perform before upgrade. Are you ready to stop HDFS ?")) {
							;
						}
					}
					component.performPreUpgradeActivities();
				}
			}
			log.info("");
			log.info("");
			log.info("-----------------------------------------------------------------------------------------------------------------------------------------");
			log.info("******* Cluster is now ready for upgrade. Please uninstall current  hadoop installation, install HDP and then perform post upgrade activities *******");
			log.info("-----------------------------------------------------------------------------------------------------------------------------------------");
			log.info("");
			log.info("");
		} catch (Exception e) {
			log.error("Failed to perform pre upgrade process.", e);
		}
	}

}
