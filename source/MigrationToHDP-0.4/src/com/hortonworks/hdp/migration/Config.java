/*
 * 
 */
package com.hortonworks.hdp.migration;

import java.io.File;

import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class Config.
 */
public class Config {

	/** The Constant CLUSTER_INFO_DIR. */
	private static final String CLUSTER_INFO_DIR = "cluster-info";

	/** The Constant DATA_DIR. */
	private static final String DATA_DIR = "data";

	/** The Constant LOCAL_WORK_AREA. */
	private static final String LOCAL_WORK_AREA = "./";

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant REMOTE_WORK_AREA. */
	private static final String REMOTE_WORK_AREA = "/tmp/";

	/** The Constant SERVICE_CONFIG_DIR. */
	private static final String SERVICE_CONFIG_DIR = "service-config";

	/** The Constant STATS_DIR. */
	private static final String STATS_DIR = "stats";

	/** The Constant UPGRADE_HOME. */
	private static final String UPGRADE_HOME = "./hadoop-upgrade";

	/**
	 * Gets the conf tar file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @return the conf tar file name
	 */
	public static String getConfTarFileName(String upgradeStage, String componentName) {
		return componentName + "-conf-" + upgradeStage + ".tgz";
	}

	/**
	 * Gets the data dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param baseDir
	 *            the base dir
	 * @param componentName
	 *            the component name
	 * @return the data dir
	 */
	private static String getDataDir(String upgradeStage, String baseDir, String componentName) {
		return baseDir + File.separator + UPGRADE_HOME + File.separator + upgradeStage + File.separator + DATA_DIR + File.separator + componentName
				+ File.separator + Constants.HOST_NAME_PLACEHOLDER + File.separator;
	}

	/**
	 * Gets the installation tar file name.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @return the installation tar file name
	 */
	public static String getInstallationTarFileName(String upgradeStage, String componentName) {
		return componentName + "-installation-" + upgradeStage + ".tgz";
	}

	/**
	 * Gets the local cluster info dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the local cluster info dir
	 */
	public static String getLocalClusterInfoDir(String upgradeStage) {
		return LOCAL_WORK_AREA + File.separator + UPGRADE_HOME + File.separator + upgradeStage + File.separator + CLUSTER_INFO_DIR + File.separator;
	}

	/**
	 * Gets the local data dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @return the local data dir
	 */
	public static String getLocalDataDir(String upgradeStage, String componentName) {
		return getDataDir(upgradeStage, LOCAL_WORK_AREA, componentName);
	}

	/**
	 * Gets the local service config dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @return the local service config dir
	 */
	public static String getLocalServiceConfigDir(String upgradeStage, String componentName) {
		return getServiceConfigDir(upgradeStage, LOCAL_WORK_AREA, componentName);
	}

	/**
	 * Gets the local stats dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @param childDirs
	 *            the child dirs
	 * @return the local stats dir
	 */
	public static String getLocalStatsDir(String upgradeStage, String componentName, String childDirs) {
		if (childDirs != null) {
			return getStatsDir(upgradeStage, LOCAL_WORK_AREA, componentName, childDirs);
		} else {
			return getStatsDir(upgradeStage, LOCAL_WORK_AREA, componentName, "");
		}
	}

	/**
	 * Gets the remote data dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @return the remote data dir
	 */
	public static String getRemoteDataDir(String upgradeStage, String componentName) {
		return getDataDir(upgradeStage, REMOTE_WORK_AREA, componentName);
	}

	/**
	 * Gets the remote service config dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @return the remote service config dir
	 */
	public static String getRemoteServiceConfigDir(String upgradeStage, String componentName) {
		return getServiceConfigDir(upgradeStage, REMOTE_WORK_AREA, componentName);
	}

	/**
	 * Gets the remote stats dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param componentName
	 *            the component name
	 * @param childDirs
	 *            the child dirs
	 * @return the remote stats dir
	 */
	public static String getRemoteStatsDir(String upgradeStage, String componentName, String childDirs) {
		if (childDirs != null) {
			return getStatsDir(upgradeStage, REMOTE_WORK_AREA, componentName, childDirs);
		} else {
			return getStatsDir(upgradeStage, REMOTE_WORK_AREA, componentName, "");
		}
	}

	/**
	 * Gets the service config dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param baseDir
	 *            the base dir
	 * @param componentName
	 *            the component name
	 * @return the service config dir
	 */
	private static String getServiceConfigDir(String upgradeStage, String baseDir, String componentName) {
		return baseDir + File.separator + UPGRADE_HOME + File.separator + upgradeStage + File.separator + SERVICE_CONFIG_DIR + File.separator + componentName
				+ File.separator + Constants.HOST_NAME_PLACEHOLDER + File.separator + "conf" + File.separator;
	}

	/**
	 * Gets the stats dir.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param baseDir
	 *            the base dir
	 * @param componentName
	 *            the component name
	 * @param childDirs
	 *            the child dirs
	 * @return the stats dir
	 */
	private static String getStatsDir(String upgradeStage, String baseDir, String componentName, String childDirs) {
		return baseDir + File.separator + UPGRADE_HOME + File.separator + upgradeStage + File.separator + STATS_DIR + File.separator + componentName
				+ File.separator + childDirs + File.separator;
	}

}
