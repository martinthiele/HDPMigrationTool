/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.jdom2.Element;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.file.util.XMLUtil;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * A factory for creating Component objects.
 */
public class ComponentFactory {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Creates a new Component object.
	 * 
	 * @param upgradeStage
	 *            the upgrade stage
	 * @param distro
	 *            the distro
	 * @param installationType
	 *            the installation type
	 * @param adminUser
	 *            the admin user
	 * @return the list< component>
	 * @throws Exception
	 *             the exception
	 */
	public static List<Component> createComponents(String upgradeStage, String distro, String installationType, User adminUser) throws Exception {
		Element root = XMLUtil.getMigrationConfigXmlRoot("components.xml", upgradeStage);
		List<Element> componentElements = root.getChildren();
		List<Component> components = new ArrayList<Component>();
		Component comp = null;
		for (Element e : componentElements) {

			if (Constants.COMPONENT_HDFS.equalsIgnoreCase(StringUtils.trim(e.getAttributeValue("name")))) {
				comp = new HDFS(distro, installationType, new User(e.getChildTextTrim("user"), null, null), e.getChildTextTrim("home"),
						e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			} else if (Constants.COMPONENT_MAP_REDUCE.equalsIgnoreCase(StringUtils.trim(e.getAttributeValue("name")))) {
				comp = new MapReduce(distro, installationType, new User(e.getChildTextTrim("user"), null, null), e.getChildTextTrim("home"),
						e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			} else if (Constants.COMPONENT_HBASE.equalsIgnoreCase(StringUtils.trim(e.getAttributeValue("name")))) {
				comp = new HBase(distro, installationType, new User(e.getChildTextTrim("user"), null, null), e.getChildTextTrim("home"),
						e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			} else if (Constants.COMPONENT_HIVE.equalsIgnoreCase(StringUtils.trim(e.getAttributeValue("name")))) {
				comp = new Hive(distro, installationType, new User(e.getChildTextTrim("user"), null, null), e.getChildTextTrim("home"),
						e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			} else if (Constants.COMPONENT_ZOOKEEPER.equalsIgnoreCase(StringUtils.trim(e.getAttributeValue("name")))) {
				comp = new ZooKeeper(distro, installationType, new User(e.getChildTextTrim("user"), null, null), e.getChildTextTrim("home"),
						e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			}
			/*
			 * else if
			 * (Constants.COMPONENT_OOZIE.equalsIgnoreCase(StringUtils.trim
			 * (e.getAttributeValue("name")))) { comp = new Oozie(distro,
			 * installationType, e.getChildTextTrim("user"),
			 * e.getChildTextTrim("home"),
			 * e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			 * } else if
			 * (Constants.COMPONENT_PIG.equalsIgnoreCase(StringUtils.trim
			 * (e.getAttributeValue("name")))) { comp = new Pig(distro,
			 * installationType, e.getChildTextTrim("user"),
			 * e.getChildTextTrim("home"),
			 * e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			 * } else if
			 * (Constants.COMPONENT_SQOOP.equalsIgnoreCase(StringUtils.
			 * trim(e.getAttributeValue("name")))) { comp = new Sqoop(distro,
			 * installationType, e.getChildTextTrim("user"),
			 * e.getChildTextTrim("home"),
			 * e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			 * } else if
			 * (Constants.COMPONENT_TEMPLETON.equalsIgnoreCase(StringUtils
			 * .trim(e.getAttributeValue("name")))) { comp = new
			 * Templeton(distro, installationType, e.getChildTextTrim("user"),
			 * e.getChildTextTrim("home"),
			 * e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			 * } else if
			 * (Constants.COMPONENT_HCATALOG.equalsIgnoreCase(StringUtils
			 * .trim(e.getAttributeValue("name")))) { comp = new
			 * HCatalog(distro, installationType, e.getChildTextTrim("user"),
			 * e.getChildTextTrim("home"),
			 * e.getChildTextTrim("config-location"), adminUser, upgradeStage);
			 * }
			 */

			// TODO Make sure that all components and services are initialized
			// properly.

			if (comp != null) {
				components.add(comp);
			}
		}
		return components;
	}

	/**
	 * Instantiates a new component factory.
	 */
	private ComponentFactory() {
	}
}
