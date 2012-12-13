package com.hortonworks.hdp.migration;

import com.hortonworks.hdp.migration.file.util.FileComparer;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class BlockReportComparison.
 */
public class HDFSFsckReportComparison {

	/** The log. */
	static Logger log = new Logger();

	/**
	 * Compare hdfsfsck report.
	 * 
	 * @param preUpgradeFsckReport
	 *            the pre upgrade fsck report
	 * @param postUpgradeFsckReport
	 *            the post upgrade fsck report
	 * @return true, if successful
	 * @throws Exception
	 *             the exception
	 */
	public static boolean compareHDFSFSCKReport(String preUpgradeFsckReport, String postUpgradeFsckReport) throws Exception {

		boolean result = FileComparer.compareFsckReport(preUpgradeFsckReport, postUpgradeFsckReport);

		if (result) {
			log.info("Great News! HDFS fsck reports validation is SUCCESSFUL");
		} else {
			log.error("HDFS fsck reports validation FAILED");
		}
		return result;
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * @throws Exception
	 *             the exception
	 */
	public static void main(String[] args) throws Exception {

		if (args == null || args.length < 2) {
			System.out.println("Usage: " + HDFSFsckReportComparison.class.getName() + "<path to pre-upgrade fsck report> <path to post-upgrade fsck report>");
			System.exit(1);
		}
		compareHDFSFSCKReport(args[0], args[1]);

	}

}
