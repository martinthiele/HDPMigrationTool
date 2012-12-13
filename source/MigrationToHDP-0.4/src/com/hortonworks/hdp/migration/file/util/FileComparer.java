/*
 * 
 */
package com.hortonworks.hdp.migration.file.util;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.ListUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.hadoop.FsckResult;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class FileComparer.
 */
public class FileComparer {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Compare block report.
	 * 
	 * @param preUpgradeBlockReport
	 *            the pre upgrade block report
	 * @param postUpgradeBlockReport
	 *            the post upgrade block report
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareBlockReport(File preUpgradeBlockReport, File postUpgradeBlockReport) throws IOException {
		return compareBlockReport(new FileInputStream(preUpgradeBlockReport), new FileInputStream(postUpgradeBlockReport));
	}

	/**
	 * Compare block report.
	 * 
	 * @param inputStreamA
	 *            the input stream a
	 * @param inputStreamB
	 *            the input stream b
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	private static boolean compareBlockReport(FileInputStream inputStreamA, FileInputStream inputStreamB) throws IOException {
		long lineNumber = 1;

		DataInputStream preUpgradeReportInputStream = null;
		DataInputStream postUpgradeReportInputStream = null;

		BufferedReader preUpgradeReportReader = null;
		BufferedReader postUpgradeReportReader = null;

		try {
			preUpgradeReportInputStream = new DataInputStream(inputStreamA);
			postUpgradeReportInputStream = new DataInputStream(inputStreamB);

			preUpgradeReportReader = new BufferedReader(new InputStreamReader(preUpgradeReportInputStream));
			postUpgradeReportReader = new BufferedReader(new InputStreamReader(postUpgradeReportInputStream));

			String lineA;
			String lineB;
			String blockInfoA;
			String blockInfoB;
			List<String> preUpgradeBlockLocationList;
			List<String> postUpgradeBlockLocationList;
			// Read Files Line By Line
			while (((lineA = preUpgradeReportReader.readLine()) != null) && ((lineB = postUpgradeReportReader.readLine()) != null)) {

				if (StringUtils.contains(lineA, "blk_") && StringUtils.contains(lineA, "[") && StringUtils.contains(lineA, "]")) {
					blockInfoA = StringUtils.substring(lineA, 0, StringUtils.lastIndexOf(lineA, '['));
					blockInfoB = StringUtils.substring(lineB, 0, StringUtils.lastIndexOf(lineB, '['));
					preUpgradeBlockLocationList = getSortedBlockLocations(lineA);
					postUpgradeBlockLocationList = getSortedBlockLocations(lineB);

					if (!StringUtils.equals(blockInfoA, blockInfoB)) {
						log.error("Line # " + lineNumber + " does not match.");
						log.error("Line # " + lineNumber + " in preUpgradeFile  is : " + lineA);
						log.error("Line # " + lineNumber + " in postUpgradeFile is : " + lineB);
						return false;
					} else if (!ListUtils.isEqualList(preUpgradeBlockLocationList, postUpgradeBlockLocationList)) {
						log.error("The block location for block " + blockInfoA + " do not match");
						log.error("The pre-upgrade  block locations are : " + preUpgradeBlockLocationList);
						log.error("The post-upgrade block locations are : " + postUpgradeBlockLocationList);
						return false;
					}

				} else if (!StringUtils.equals(lineA, lineB)) {
					log.error("Line # " + lineNumber + " does not match.");
					log.error("Line # " + lineNumber + " in preUpgradeFile  is : " + lineA);
					log.error("Line # " + lineNumber + " in postUpgradeFile is : " + lineB);
					return false;
				}

				lineNumber++;
			}
		} finally {
			try {
				if (preUpgradeReportReader != null) {
					preUpgradeReportReader.close();
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (preUpgradeReportInputStream != null) {
					preUpgradeReportInputStream.close();
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (postUpgradeReportReader != null) {
					postUpgradeReportReader.close();
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (postUpgradeReportInputStream != null) {
					postUpgradeReportInputStream.close();
				}
			} catch (Exception e) {
				// ignore
			}

		}
		log.debug("Both the block reports match with exact files, blocks and locations.");
		return true;
	}

	/**
	 * Compare block report.
	 * 
	 * @param preUpgradeBlockReportPath
	 *            the pre upgrade block report path
	 * @param postUpgradeBlockReportPath
	 *            the post upgrade block report path
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareBlockReport(String preUpgradeBlockReportPath, String postUpgradeBlockReportPath) throws IOException {
		return compareBlockReport(new FileInputStream(preUpgradeBlockReportPath), new FileInputStream(postUpgradeBlockReportPath));
	}

	/**
	 * Compare columns.
	 * 
	 * @param inputStreamA
	 *            the input stream a
	 * @param inputStreamB
	 *            the input stream b
	 * @param delimeter
	 *            the delimeter
	 * @param ignoreNullTokens
	 *            the ignore null tokens
	 * @param columnsToCompare
	 *            the columns to compare
	 * @param columnsToIgnore
	 *            the columns to ignore
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	private static boolean compareColumns(FileInputStream inputStreamA, FileInputStream inputStreamB, char delimeter, boolean ignoreNullTokens,
			int[] columnsToCompare, int[] columnsToIgnore) throws IOException {
		long lineNumber = 1;

		DataInputStream preUpgradeReportInputStream = null;
		DataInputStream postUpgradeReportInputStream = null;

		BufferedReader preUpgradeReportReader = null;
		BufferedReader postUpgradeReportReader = null;

		try {
			preUpgradeReportInputStream = new DataInputStream(inputStreamA);
			postUpgradeReportInputStream = new DataInputStream(inputStreamB);

			preUpgradeReportReader = new BufferedReader(new InputStreamReader(preUpgradeReportInputStream));
			postUpgradeReportReader = new BufferedReader(new InputStreamReader(postUpgradeReportInputStream));

			String lineA;
			String lineB;
			String[] lineAColumns;
			String[] lineBColumns;
			// Read File Line By Line
			while (((lineA = preUpgradeReportReader.readLine()) != null) && ((lineB = postUpgradeReportReader.readLine()) != null)) {
				log.trace("Comparing line # " + lineNumber);
				if (ignoreNullTokens) {
					lineAColumns = StringUtils.split(lineA, delimeter);
					lineBColumns = StringUtils.split(lineB, delimeter);
				} else {
					lineAColumns = StringUtils.splitPreserveAllTokens(lineA, delimeter);
					lineBColumns = StringUtils.splitPreserveAllTokens(lineB, delimeter);

				}

				if (lineAColumns.length != lineBColumns.length) {
					log.error("The lines are not matching. The line # " + lineNumber + " in pre-upgrade file has " + lineAColumns
							+ " tokens where as post-upgrade file has " + lineBColumns);
					log.error("Line # " + lineNumber + " from pre-upgrade  file : " + lineA);
					log.error("Line # " + lineNumber + " from post-upgrade file : " + lineB);
					return false;
				} else {
					for (int i = 0; i < lineAColumns.length; i++) {
						if ((columnsToIgnore == null || !ArrayUtils.contains(columnsToIgnore, i + 1))
								&& (columnsToCompare == null || ArrayUtils.contains(columnsToCompare, i + 1))) {
							if (!StringUtils.equals(lineAColumns[i], lineBColumns[i])) {
								log.error("Column # " + (i + 1) + " on line # " + lineNumber + " do not match.");
								log.error("Line # " + lineNumber + " from pre-upgrade  file : " + lineA);
								log.error("Line # " + lineNumber + " from post-upgrade file : " + lineB);
								log.error("Column # " + (i + 1) + " from pre-upgrade  file : " + lineAColumns[i]);
								log.error("Column # " + (i + 1) + " from post-upgrade file : " + lineBColumns[i]);

								return false;
							}
						} else {
							log.trace("Ignoring the Column # " + (i + 1));
						}
					}
				}

				lineNumber++;
			}
		} finally {
			try {
				if (preUpgradeReportReader != null) {
					preUpgradeReportReader.close();
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (preUpgradeReportInputStream != null) {
					preUpgradeReportInputStream.close();
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (postUpgradeReportReader != null) {
					postUpgradeReportReader.close();
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (postUpgradeReportInputStream != null) {
					postUpgradeReportInputStream.close();
				}
			} catch (Exception e) {
				// ignore
			}

		}
		log.debug("All lines in both files match specified columns.");
		return true;
	}

	/**
	 * Compare contents.
	 * 
	 * @param preUpgradeFile
	 *            the pre upgrade file
	 * @param postUpgradeFile
	 *            the post upgrade file
	 * @return true, if successful
	 */
	public static boolean compareContents(File preUpgradeFile, File postUpgradeFile) {
		try {

			if (FileUtils.checksumCRC32(preUpgradeFile) == FileUtils.checksumCRC32(postUpgradeFile)) {
				log.debug("File checksum matches.");
				return true;
			}
		} catch (Exception e) {
			log.error("Failed to compare files " + preUpgradeFile.getAbsolutePath() + " and " + postUpgradeFile.getAbsolutePath(), e);
		}
		log.error("Checksum for files " + preUpgradeFile.getName() + " and " + postUpgradeFile.getName() + " does not match.");
		return false;
	}

	/**
	 * Compare contents.
	 * 
	 * @param preUpgradeFilePath
	 *            the pre upgrade file path
	 * @param postUpgradeFilePath
	 *            the post upgrade file path
	 * @return true, if successful
	 */
	public static boolean compareContents(String preUpgradeFilePath, String postUpgradeFilePath) {
		return compareContents(new File(preUpgradeFilePath), new File(postUpgradeFilePath));

	}

	/**
	 * Compare excluding specified columns.
	 * 
	 * @param preUpgradeFile
	 *            the pre upgrade file
	 * @param postUpgradeFile
	 *            the post upgrade file
	 * @param delimeter
	 *            the delimeter
	 * @param ignoreWhiteSpaces
	 *            the ignore white spaces
	 * @param columnsToExclude
	 *            the columns to exclude
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareExcludingSpecifiedColumns(File preUpgradeFile, File postUpgradeFile, char delimeter, boolean ignoreWhiteSpaces,
			int[] columnsToExclude) throws IOException {
		return compareColumns(new FileInputStream(preUpgradeFile), new FileInputStream(postUpgradeFile), delimeter, ignoreWhiteSpaces, null, columnsToExclude);
	}

	/**
	 * Compare excluding specified columns.
	 * 
	 * @param preUpgradeFilePath
	 *            the pre upgrade file path
	 * @param postUpgradeFilePath
	 *            the post upgrade file path
	 * @param delimeter
	 *            the delimeter
	 * @param ignoreWhiteSpaces
	 *            the ignore white spaces
	 * @param columnsToExclude
	 *            the columns to exclude
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareExcludingSpecifiedColumns(String preUpgradeFilePath, String postUpgradeFilePath, char delimeter, boolean ignoreWhiteSpaces,
			int[] columnsToExclude) throws IOException {
		return compareColumns(new FileInputStream(preUpgradeFilePath), new FileInputStream(postUpgradeFilePath), delimeter, ignoreWhiteSpaces, null,
				columnsToExclude);
	}

	/**
	 * Compare file listings.
	 * 
	 * @param preUpgradeFile
	 *            the pre upgrade file
	 * @param postUpgradeFile
	 *            the post upgrade file
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareFileListings(File preUpgradeFile, File postUpgradeFile) throws IOException {
		return compareExcludingSpecifiedColumns(preUpgradeFile, postUpgradeFile, ' ', true, new int[] { 6, 7 });
	}

	/**
	 * Compare file listings.
	 * 
	 * @param preUpgradeFilePath
	 *            the pre upgrade file path
	 * @param postUpgradeFilePath
	 *            the post upgrade file path
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareFileListings(String preUpgradeFilePath, String postUpgradeFilePath) throws IOException {
		return compareExcludingSpecifiedColumns(preUpgradeFilePath, postUpgradeFilePath, ' ', true, new int[] { 6, 7 });
	}

	/**
	 * Compare fsck report.
	 * 
	 * @param preUpgradeFilePath
	 *            the pre upgrade file path
	 * @param postUpgradeFilePath
	 *            the post upgrade file path
	 * @return true, if successful
	 * @throws Exception
	 *             the exception
	 */
	public static boolean compareFsckReport(String preUpgradeFilePath, String postUpgradeFilePath) throws Exception {
		FsckResult preFsckResult = new FsckResult(new File(preUpgradeFilePath));
		FsckResult postFsckResult = new FsckResult(new File(postUpgradeFilePath));
		return preFsckResult.equals(postFsckResult);
	}

	/**
	 * Compare specified columns.
	 * 
	 * @param preUpgradeFile
	 *            the pre upgrade file
	 * @param postUpgradeFile
	 *            the post upgrade file
	 * @param delimeter
	 *            the delimeter
	 * @param ignoreWhiteSpaces
	 *            the ignore white spaces
	 * @param columnsToCompare
	 *            the columns to compare
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareSpecifiedColumns(File preUpgradeFile, File postUpgradeFile, char delimeter, boolean ignoreWhiteSpaces, int[] columnsToCompare)
			throws IOException {
		return compareColumns(new FileInputStream(preUpgradeFile), new FileInputStream(postUpgradeFile), delimeter, ignoreWhiteSpaces, columnsToCompare, null);
	}

	/**
	 * Compare specified columns.
	 * 
	 * @param preUpgradeFilePath
	 *            the pre upgrade file path
	 * @param postUpgradeFilePath
	 *            the post upgrade file path
	 * @param delimeter
	 *            the delimeter
	 * @param ignoreWhiteSpaces
	 *            the ignore white spaces
	 * @param columnsToCompare
	 *            the columns to compare
	 * @return true, if successful
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static boolean compareSpecifiedColumns(String preUpgradeFilePath, String postUpgradeFilePath, char delimeter, boolean ignoreWhiteSpaces,
			int[] columnsToCompare) throws IOException {
		return compareColumns(new FileInputStream(preUpgradeFilePath), new FileInputStream(postUpgradeFilePath), delimeter, ignoreWhiteSpaces,
				columnsToCompare, null);
	}

	/**
	 * Gets the sorted block locations.
	 * 
	 * @param blockDetailsLine
	 *            the block details line
	 * @return the sorted block locations
	 */
	private static List<String> getSortedBlockLocations(String blockDetailsLine) {

		List<String> sortedBlockLocationsList = null;
		String blockLocationsString = StringUtils.substring(blockDetailsLine, StringUtils.lastIndexOf(blockDetailsLine, '[') + 1,
				StringUtils.lastIndexOf(blockDetailsLine, ']'));
		String[] blockLocationsArray = StringUtils.split(blockLocationsString, ", ");
		if (blockLocationsArray.length > 0) {
			sortedBlockLocationsList = Arrays.asList(blockLocationsArray);
			Collections.sort(sortedBlockLocationsList);
		}
		return sortedBlockLocationsList;
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
		testBlockReportVerification();
	}

	/**
	 * Test block report verification.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public static void testBlockReportVerification() throws Exception {
		compareBlockReport("/Volumes/Macintosh HD OLD/Documents (from old)/preUpgradeBlockReport.txt",
				"/Volumes/Macintosh HD OLD/Documents (from old)/postUpgradeBlockReport.txt");
	}

	/**
	 * Test file verification.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public static void testFileVerification() throws Exception {
		long t1 = System.currentTimeMillis();
		log.debug("File content verification "
				+ compareContents("/Volumes/Macintosh HD OLD/Documents (from old)/preUpgradeFile.txt",
						"/Volumes/Macintosh HD OLD/Documents (from old)/postUpgradeFile.txt"));
		long t2 = System.currentTimeMillis();
		log.debug("Time to verify content :  " + ((t2 - t1) / 60000) + " mins");
		log.debug("All column verification "
				+ compareSpecifiedColumns("/Volumes/Macintosh HD OLD/Documents (from old)/preUpgradeFile.txt",
						"/Volumes/Macintosh HD OLD/Documents (from old)/postUpgradeFile.txt", ' ', true, null));
		long t3 = System.currentTimeMillis();
		log.debug("Time to verify content :  " + ((t3 - t2) / 60000) + " mins");
		log.debug("Specified column verification "
				+ compareSpecifiedColumns("/Volumes/Macintosh HD OLD/Documents (from old)/preUpgradeFile.txt",
						"/Volumes/Macintosh HD OLD/Documents (from old)/postUpgradeFile.txt", ' ', true, new int[] { 1, 2, 3, 4, 5, 8 }));
		long t4 = System.currentTimeMillis();
		log.debug("Time to verify content :  " + ((t4 - t3) / 60000) + " mins");
		log.debug("Exclusion column verification "
				+ compareExcludingSpecifiedColumns("/Volumes/Macintosh HD OLD/Documents (from old)/preUpgradeFile.txt",
						"/Volumes/Macintosh HD OLD/Documents (from old)/postUpgradeFile.txt", ' ', true, new int[] { 6, 7 }));
		long t5 = System.currentTimeMillis();
		log.debug("Time to verify content :  " + ((t5 - t4) / 60000) + " mins");
	}

}
