/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class ServiceUtils.
 */
public class ServiceUtils {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant serviceToComponentMap. */
	private static final Map<String, String> serviceToComponentMap = new HashMap<String, String>();

	static {
		serviceToComponentMap.put(Constants.SERVICE_DATA_NODE, Constants.COMPONENT_HDFS);
		serviceToComponentMap.put(Constants.SERVICE_GMOND, Constants.COMPONENT_GANGLIA);
		serviceToComponentMap.put(Constants.SERVICE_GMETAD, Constants.COMPONENT_GANGLIA);
		serviceToComponentMap.put(Constants.SERVICE_HBASE_MASTER, Constants.COMPONENT_HBASE);
		serviceToComponentMap.put(Constants.SERVICE_HBASE_REGION_SERVER, Constants.COMPONENT_HBASE);
		serviceToComponentMap.put(Constants.SERVICE_HCATALOG, Constants.COMPONENT_HCATALOG);
		serviceToComponentMap.put(Constants.SERVICE_HIVE_METASTORE, Constants.COMPONENT_HIVE);
		serviceToComponentMap.put(Constants.SERVICE_JOB_HISTORY_SERVER, Constants.COMPONENT_MAP_REDUCE);
		serviceToComponentMap.put(Constants.SERVICE_JOB_TRACKER, Constants.COMPONENT_MAP_REDUCE);
		serviceToComponentMap.put(Constants.SERVICE_TASK_TRACKER, Constants.COMPONENT_MAP_REDUCE);
		serviceToComponentMap.put(Constants.SERVICE_NAGIOS, Constants.COMPONENT_NAGIOS);
		serviceToComponentMap.put(Constants.SERVICE_NAME_NODE, Constants.COMPONENT_HDFS);
		serviceToComponentMap.put(Constants.SERVICE_OOZIE, Constants.COMPONENT_OOZIE);
		serviceToComponentMap.put(Constants.SERVICE_PIG, Constants.COMPONENT_PIG);
		serviceToComponentMap.put(Constants.SERVICE_SECONDARY_NAME_NODE, Constants.COMPONENT_HDFS);
		serviceToComponentMap.put(Constants.SERVICE_SQOOP, Constants.COMPONENT_SQOOP);
		serviceToComponentMap.put(Constants.SERVICE_TEMPLETON, Constants.COMPONENT_TEMPLETON);
		serviceToComponentMap.put(Constants.SERVICE_ZOOKEEPER, Constants.COMPONENT_ZOOKEEPER);
	}

	/**
	 * Gets the component name by service name.
	 * 
	 * @param serviceName
	 *            the service name
	 * @return the component name by service name
	 */
	public static String getComponentNameByServiceName(String serviceName) {
		return serviceToComponentMap.get(StringUtils.lowerCase(serviceName));
	};

	/**
	 * Gets the service names by component.
	 * 
	 * @param serviceName
	 *            the service name
	 * @return the service names by component
	 */
	public static List<String> getServiceNamesByComponent(String serviceName) {

		List<String> serviceNames = new ArrayList<String>();
		Iterator<Map.Entry<String, String>> itr = serviceToComponentMap.entrySet().iterator();
		Map.Entry<String, String> entry = null;
		while (itr.hasNext()) {
			entry = itr.next();
			if (StringUtils.equalsIgnoreCase(serviceName, entry.getValue())) {
				serviceNames.add(serviceName);
			}
		}
		return serviceNames;
	}

	/**
	 * Read host names from file.
	 * 
	 * @param filePaths
	 *            the file paths
	 * @return the list
	 */
	public static List<String> readHostNamesFromFile(String... filePaths) {
		return readUniqueLinesFromFile(true, filePaths);
	}

	/**
	 * Read unique lines from file.
	 * 
	 * @param ignoreCase
	 *            the ignore case
	 * @param filePaths
	 *            the file paths
	 * @return the list
	 */
	public static List<String> readUniqueLinesFromFile(boolean ignoreCase, String... filePaths) {
		List<String> lines = new ArrayList<String>();

		for (String fileName : filePaths) {
			log.debug("Reading file " + fileName);
			try {
				File f = new File(fileName);
				if (f.exists()) {
					FileInputStream fstream = new FileInputStream(fileName);
					DataInputStream in = new DataInputStream(fstream);
					BufferedReader br = new BufferedReader(new InputStreamReader(in));
					String line;
					while ((line = br.readLine()) != null) {

						line = line.trim();
						if (ignoreCase) {
							line = line.toLowerCase();
						}

						if (StringUtils.isBlank(line) || line.startsWith("#") || lines.contains(line)) {
							continue;
						} else {
							lines.add(line);
						}
					}
					in.close();
				}
			} catch (Exception e) {
				log.error("Error: ", e);
			}
		}
		return lines;
	}

	/**
	 * Instantiates a new service utils.
	 */
	private ServiceUtils() {
	}
}
