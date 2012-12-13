/*
 * 
 */
package com.hortonworks.hdp.migration;

import java.util.ArrayList;
import java.util.List;

import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class ServiceManager.
 */
public class ServiceManager {

	/** The all components. */
	private static List<String> allComponents = new ArrayList<String>();

	/** The Constant log. */
	private static final Logger log = new Logger();

	static {
		allComponents.add(Constants.COMPONENT_FLUME);
		allComponents.add(Constants.COMPONENT_GANGLIA);
		allComponents.add(Constants.COMPONENT_HBASE);
		allComponents.add(Constants.COMPONENT_HCATALOG);
		allComponents.add(Constants.COMPONENT_HDFS);
		allComponents.add(Constants.COMPONENT_HIVE);
		allComponents.add(Constants.COMPONENT_MAP_REDUCE);
		allComponents.add(Constants.COMPONENT_NAGIOS);
		allComponents.add(Constants.COMPONENT_OOZIE);
		allComponents.add(Constants.COMPONENT_PIG);
		allComponents.add(Constants.COMPONENT_SQOOP);
		allComponents.add(Constants.COMPONENT_TEMPLETON);
		allComponents.add(Constants.COMPONENT_ZOOKEEPER);
	};

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * @throws Exception
	 *             the exception
	 */
	public static void main(String[] args) throws Exception {

		if (args == null
				|| args.length < 3
				|| (!Constants.UPGRADE_STAGE_PRE.equalsIgnoreCase(args[1]) && !Constants.UPGRADE_STAGE_POST.equalsIgnoreCase(args[1]))
				|| (!Constants.ACTION_START.equalsIgnoreCase(args[0]) && !Constants.ACTION_STOP.equalsIgnoreCase(args[0]) && !Constants.ACTION_RESTART
						.equalsIgnoreCase(args[0]))) {
			usage();
			System.exit(1);

		}

		List<String> specifiedComponentNamesList = new ArrayList<String>();
		UpgradeProcessor processor = new UpgradeProcessor(args[1], "apache", "tar");

		for (int i = 2; i < args.length; i++) {
			if ("all".equalsIgnoreCase(args[i])) {
				specifiedComponentNamesList = allComponents;
				break;
			} else {
				if (allComponents.contains(args[i])) {
					if (!specifiedComponentNamesList.contains(args[i])) {
						specifiedComponentNamesList.add(args[i]);
					}
				} else {
					log.error("Could not find component name '" + args[i] + "' Ignoring it.");
				}
			}
		}

		processor.validateAndInitialize(args[1], false);

		if (Constants.ACTION_STOP.equalsIgnoreCase(args[0]) || Constants.ACTION_RESTART.equalsIgnoreCase(args[0])) {
			processor.stopComponents(specifiedComponentNamesList.toArray(new String[specifiedComponentNamesList.size()]));
		}
		if (Constants.ACTION_START.equalsIgnoreCase(args[0]) || Constants.ACTION_RESTART.equalsIgnoreCase(args[0])) {
			processor.startComponents(specifiedComponentNamesList.toArray(new String[specifiedComponentNamesList.size()]));
		}

	}

	/**
	 * Usage.
	 */
	private static void usage() {
		System.out.println("Usage: java " + ServiceManager.class.getName()
				+ " <service action: start|stop> <upgrade stage: pre-upgrade|post-upgrade> <component names: all|" + Constants.COMPONENT_HBASE + "|"
				+ Constants.COMPONENT_HDFS + "|" + Constants.COMPONENT_HIVE + "|" + Constants.COMPONENT_MAP_REDUCE + "|" + Constants.COMPONENT_ZOOKEEPER);
	}
}
