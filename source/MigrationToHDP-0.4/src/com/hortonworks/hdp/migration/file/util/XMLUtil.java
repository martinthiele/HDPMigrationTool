/*
 * 
 */
package com.hortonworks.hdp.migration.file.util;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.jdom2.Comment;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;
import org.jdom2.output.Format;
import org.jdom2.output.XMLOutputter;

import com.hortonworks.hdp.migration.Config;
import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class XMLUtil.
 */
public class XMLUtil {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Gets the config param value.
	 * 
	 * @param xmlPath
	 *            the xml path
	 * @param paramName
	 *            the param name
	 * @return the config param value
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	public static String getConfigParamValue(String xmlPath, String paramName) throws IOException, JDOMException {
		Element root = getRootElement(xmlPath);
		String value = null;
		List<Element> properties = root.getChildren("property");
		for (Element property : properties) {
			if (StringUtils.equals(property.getChild("name").getValue(), paramName)) {
				value = property.getChild("value").getValue();
				break;
			}
		}

		return value;
	}

	/**
	 * Gets the migration config xml root.
	 * 
	 * @param xmlFileName
	 *            the xml file name
	 * @param upgradeStage
	 *            the upgrade stage
	 * @return the migration config xml root
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	public static Element getMigrationConfigXmlRoot(String xmlFileName, String upgradeStage) throws IOException, JDOMException {
		String xmlLocation = null;

		xmlLocation = Config.getLocalClusterInfoDir(upgradeStage) + "/" + xmlFileName;

		File configXML = new File(xmlLocation);

		if (!configXML.exists())

		{
			configXML = new File(Config.getLocalClusterInfoDir(Constants.DEFAULT_DIR) + "/" + xmlFileName);
		}
		if (!configXML.exists()) {
			throw new IOException("Unable to read the configuration file. Please make sure that config file '" + (new File(".")).getAbsolutePath() + "/"
					+ xmlLocation + "' or '" + configXML.getAbsolutePath() + "' exists.");
		}
		return getRootElement(configXML.getAbsolutePath());
	}

	/**
	 * Gets the property element by property name.
	 * 
	 * @param rootElement
	 *            the root element
	 * @param propertyName
	 *            the property name
	 * @return the property element by property name
	 */
	public static Element getPropertyElementByPropertyName(Element rootElement, String propertyName) {
		List<Element> properties = rootElement.getChildren("property");
		Element matchingProperty = null;
		for (Element property : properties) {
			if (StringUtils.equals(StringUtils.trim(propertyName), StringUtils.trim(property.getChild("name").getValue()))) {
				matchingProperty = property;
				break;
			}
		}
		return matchingProperty;

	}

	/**
	 * Gets the root element.
	 * 
	 * @param xmlFilePath
	 *            the xml file path
	 * @return the root element
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	public static Element getRootElement(String xmlFilePath) throws IOException, JDOMException {
		return getXmlDocument(xmlFilePath).getRootElement();

	}

	/**
	 * Gets the xml document.
	 * 
	 * @param xmlFilePath
	 *            the xml file path
	 * @return the xml document
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	public static Document getXmlDocument(String xmlFilePath) throws IOException, JDOMException {
		SAXBuilder sb = new SAXBuilder();
		Document doc = sb.build(xmlFilePath);
		return doc;

	}

	/**
	 * Merge configurations.
	 * 
	 * @param targetXMLPath
	 *            the target xml path
	 * @param sourceXMLPath
	 *            the source xml path
	 * @param propertiesToMerge
	 *            the properties to merge
	 * @param propertiesToExclude
	 *            the properties to exclude
	 * @param source
	 *            the source
	 * @return the string
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	private static String mergeConfigurations(String targetXMLPath, String sourceXMLPath, List<String> propertiesToMerge, List<String> propertiesToExclude,
			String source) throws IOException, JDOMException {
		Document targetXMLDocument = getXmlDocument(targetXMLPath);
		Document sourceXMLDocument = getXmlDocument(sourceXMLPath);
		Document mergedXmlDocument = targetXMLDocument.clone();
		Element mergedRootElement = mergedXmlDocument.getRootElement();

		List<Element> newProperties = sourceXMLDocument.getRootElement().getChildren("property");

		Element existingProperty = null;
		for (Element newProperty : newProperties) {
			if (propertiesToExclude != null && propertiesToExclude.contains(newProperty.getChild("name").getValue()) || ( propertiesToMerge != null && propertiesToMerge.size() > 0
					&& !propertiesToMerge.contains(newProperty.getChild("name").getValue()))) {
				continue;
			}
			existingProperty = getPropertyElementByPropertyName(mergedRootElement, newProperty.getChild("name").getValue());
			if (existingProperty != null) {
				existingProperty.detach();
			}
			newProperty = newProperty.clone();
			newProperty.detach();
			newProperty.addContent(new Comment("This property is taken from the " + source + " configurations"));
			mergedRootElement.addContent(newProperty);

		}

		String xmlString = (new XMLOutputter(Format.getPrettyFormat().setLineSeparator(System.getProperty("line.separator")))).outputString(mergedXmlDocument);

		return xmlString;
	}

	/**
	 * Merge post install xml into pre install xml.
	 * 
	 * @param preUpgradeConfigurationXmlPath
	 *            the pre upgrade configuration xml path
	 * @param postUpgradeConfigurationXmlPath
	 *            the post upgrade configuration xml path
	 * @param propertiesToMerge
	 *            the properties to merge
	 * @param propertiesToExclude
	 *            the properties to exclude
	 * @return the string
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	public static String mergePostInstallXMLIntoPreInstallXML(String preUpgradeConfigurationXmlPath, String postUpgradeConfigurationXmlPath,
			List<String> propertiesToMerge, List<String> propertiesToExclude) throws IOException, JDOMException {
		return mergeConfigurations(preUpgradeConfigurationXmlPath, postUpgradeConfigurationXmlPath, propertiesToMerge, propertiesToExclude, "HDP");
	}

	/**
	 * Merge pre install xml into post install xml.
	 * 
	 * @param preUpgradeConfigurationXmlPath
	 *            the pre upgrade configuration xml path
	 * @param postUpgradeConfigurationXmlPath
	 *            the post upgrade configuration xml path
	 * @param propertiesToMerge
	 *            the properties to merge
	 * @param propertiesToExclude
	 *            the properties to exclude
	 * @return the string
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws JDOMException
	 *             the jDOM exception
	 */
	public static String mergePreInstallXMLIntoPostInstallXML(String preUpgradeConfigurationXmlPath, String postUpgradeConfigurationXmlPath,
			List<String> propertiesToMerge, List<String> propertiesToExclude) throws IOException, JDOMException {
		return mergeConfigurations(postUpgradeConfigurationXmlPath, preUpgradeConfigurationXmlPath, propertiesToMerge, propertiesToExclude, "OLD");
	}

}
