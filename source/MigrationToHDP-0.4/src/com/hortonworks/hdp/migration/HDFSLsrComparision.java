package com.hortonworks.hdp.migration;

import java.io.IOException;

import com.hortonworks.hdp.migration.file.util.FileComparer;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class HDFSFileListComparision.
 */
public class HDFSLsrComparision {

	/** The log. */
	static Logger log = new Logger();

	/**
	 * Compare hdfs file lists.
	 * 
	 * @param preUpgradeLsrReport
	 *            the pre upgrade lsr report
	 * @param postUpgradeLsrReport
	 *            the post upgrade lsr report
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareHDFSFileLists(String preUpgradeLsrReport, String postUpgradeLsrReport) throws IOException {
		boolean result = FileComparer.compareFileListings(preUpgradeLsrReport, postUpgradeLsrReport);

		if (result) {
			log.info("Great News! HDFS lsr reports validation is SUCCESSFUL");
		} else {
			log.error("HDFS lsr reports validation FAILED");
		}
		return result;
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static void main(String[] args) throws IOException {

		if (args == null || args.length < 2) {
			System.out.println("Usage: " + HDFSLsrComparision.class.getName() + "<path to pre-upgrade lsr report> <path to post-upgrade lsr report>");
			System.exit(1);
		}
		compareHDFSFileLists(args[0], args[1]);

	}

}
