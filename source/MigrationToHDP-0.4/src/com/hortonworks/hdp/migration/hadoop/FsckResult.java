/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.StringReader;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class FsckResult.
 */
public class FsckResult {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The avarage block replication. */
	private float avarageBlockReplication;

	/** The corrupt blocks. */
	private long corruptBlocks;

	/** The default replication factor. */
	private int defaultReplicationFactor;

	/** The minimally replicated blocks. */
	private long minimallyReplicatedBlocks;

	/** The mis replicated blocks. */
	private long misReplicatedBlocks;

	/** The missing replicas. */
	private long missingReplicas;

	/** The number of data nodes. */
	private int numberOfDataNodes;

	/** The number of racks. */
	private int numberOfRacks;

	/** The over replicated blocks. */
	private long overReplicatedBlocks;

	/** The status. */
	private String status;

	/** The total blocks. */
	private long totalBlocks;

	/** The total dirs. */
	private long totalDirs;

	/** The total files. */
	private long totalFiles;

	/** The total size. */
	private long totalSize;

	/** The under replicated blocks. */
	private long underReplicatedBlocks;

	/**
	 * Instantiates a new fsck result.
	 * 
	 * @param fsckResultFile
	 *            the fsck result file
	 * @throws Exception
	 *             the exception
	 */
	public FsckResult(File fsckResultFile) throws Exception {
		BufferedReader reader = new BufferedReader(new FileReader(fsckResultFile));
		parseFsckResults(reader);
	}

	/**
	 * Instantiates a new fsck result.
	 * 
	 * @param fsckResult
	 *            the fsck result
	 * @throws Exception
	 *             the exception
	 */
	public FsckResult(String fsckResult) throws Exception {
		BufferedReader reader = new BufferedReader(new StringReader(fsckResult));
		parseFsckResults(reader);
	}

	/**
	 * Equals.
	 * 
	 * @param fsckResult
	 *            the fsck result
	 * @return true, if successful
	 */
	public boolean equals(FsckResult fsckResult) {
		boolean eqls = true;
		if (!StringUtils.equalsIgnoreCase(this.status, fsckResult.status)) {
			eqls = false;
		} else if (this.totalSize != fsckResult.totalSize) {
			log.error("fsck report mismatch. pre-upgrade total size is " + this.totalSize + " and post-upgrade total size is " + fsckResult.totalSize);

			eqls = false;
		} else if (this.totalDirs != fsckResult.totalDirs) {
			log.error("fsck report mismatch. pre-upgrade total directories are " + this.totalDirs + " and post-upgrade total directories are "
					+ fsckResult.totalDirs);
			eqls = false;
		} else if (this.totalFiles != fsckResult.totalFiles) {
			log.error("fsck report mismatch. pre-upgrade total files are " + this.totalFiles + " and post-upgrade total files are " + fsckResult.totalFiles);

			eqls = false;
		} else if (this.totalBlocks != fsckResult.totalBlocks) {
			log.error("fsck report mismatch. pre-upgrade total blocks are " + this.totalBlocks + " and post-upgrade total blocks are " + fsckResult.totalBlocks);

			eqls = false;
		} else if (this.minimallyReplicatedBlocks != fsckResult.minimallyReplicatedBlocks) {
			log.error("fsck report mismatch. pre-upgrade minimallyReplicatedBlocks are " + this.minimallyReplicatedBlocks
					+ " and post-upgrade minimallyReplicatedBlocks are " + fsckResult.minimallyReplicatedBlocks);

			eqls = false;
		} else if (this.overReplicatedBlocks != fsckResult.overReplicatedBlocks) {
			log.error("fsck report mismatch. pre-upgrade overReplicatedBlocks are " + this.overReplicatedBlocks + " and post-upgrade overReplicatedBlocks are "
					+ fsckResult.overReplicatedBlocks);

			eqls = false;
		} else if (this.underReplicatedBlocks != fsckResult.underReplicatedBlocks) {
			log.error("fsck report mismatch. pre-upgrade underReplicatedBlocks are " + this.underReplicatedBlocks
					+ " and post-upgrade underReplicatedBlocks are " + fsckResult.underReplicatedBlocks);

			eqls = false;
		} else if (this.misReplicatedBlocks != fsckResult.misReplicatedBlocks) {
			log.error("fsck report mismatch. pre-upgrade misReplicatedBlocks are " + this.misReplicatedBlocks + " and post-upgrade misReplicatedBlocks are "
					+ fsckResult.misReplicatedBlocks);

			eqls = false;
		} else if (this.defaultReplicationFactor != fsckResult.defaultReplicationFactor) {
			log.error("fsck report mismatch. pre-upgrade defaultReplicationFactor are " + this.defaultReplicationFactor
					+ " and post-upgrade defaultReplicationFactor are " + fsckResult.defaultReplicationFactor);

			eqls = false;
		} else if (this.avarageBlockReplication != fsckResult.avarageBlockReplication) {
			log.error("fsck report mismatch. pre-upgrade avarageBlockReplication are " + this.avarageBlockReplication
					+ " and post-upgrade avarageBlockReplication are " + fsckResult.avarageBlockReplication);

			eqls = false;
		} else if (this.corruptBlocks != fsckResult.corruptBlocks) {
			log.error("fsck report mismatch. pre-upgrade corruptBlocks are " + this.corruptBlocks + " and post-upgrade corruptBlocks are "
					+ fsckResult.corruptBlocks);

			eqls = false;
		} else if (this.missingReplicas != fsckResult.missingReplicas) {
			log.error("fsck report mismatch. pre-upgrade missingReplicas are " + this.missingReplicas + " and post-upgrade missingReplicas are "
					+ fsckResult.missingReplicas);

			eqls = false;
		} else if (this.numberOfDataNodes != fsckResult.numberOfDataNodes) {
			log.error("fsck report mismatch. pre-upgrade numberOfDataNodes are " + this.numberOfDataNodes + " and post-upgrade numberOfDataNodes are "
					+ fsckResult.numberOfDataNodes);

			eqls = false;
		} else if (this.numberOfRacks != fsckResult.numberOfRacks) {
			log.error("fsck report mismatch. pre-upgrade numberOfRacks are " + this.numberOfRacks + " and post-upgrade numberOfRacks are "
					+ fsckResult.numberOfRacks);

			eqls = false;
		}

		if (eqls) {
			log.info("GREAT NEWS! : FSCK Reports match");
		} else {
			log.error("FSCK Reports DO NOT match");
		}

		return eqls;
	}

	/**
	 * Gets the avarage block replication.
	 * 
	 * @return the avarage block replication
	 */
	public float getAvarageBlockReplication() {
		return avarageBlockReplication;
	}

	/**
	 * Gets the corrupt blocks.
	 * 
	 * @return the corrupt blocks
	 */
	public long getCorruptBlocks() {
		return corruptBlocks;
	}

	/**
	 * Gets the default replication factor.
	 * 
	 * @return the default replication factor
	 */
	public int getDefaultReplicationFactor() {
		return defaultReplicationFactor;
	}

	/**
	 * Gets the float value.
	 * 
	 * @param line
	 *            the line
	 * @return the float value
	 */
	private float getFloatValue(String line) {
		return Float.parseFloat(parseValue(line));
	}

	/**
	 * Gets the int value.
	 * 
	 * @param line
	 *            the line
	 * @return the int value
	 */
	private int getIntValue(String line) {
		return Integer.parseInt(parseValue(line));
	}

	/**
	 * Gets the long value.
	 * 
	 * @param line
	 *            the line
	 * @return the long value
	 */
	private long getLongValue(String line) {
		return Long.parseLong(parseValue(line));
	}

	/**
	 * Gets the minimally replicated blocks.
	 * 
	 * @return the minimally replicated blocks
	 */
	public long getMinimallyReplicatedBlocks() {
		return minimallyReplicatedBlocks;
	}

	/**
	 * Gets the mis replicated blocks.
	 * 
	 * @return the mis replicated blocks
	 */
	public long getMisReplicatedBlocks() {
		return misReplicatedBlocks;
	}

	/**
	 * Gets the missing replicas.
	 * 
	 * @return the missing replicas
	 */
	public long getMissingReplicas() {
		return missingReplicas;
	}

	/**
	 * Gets the number of data nodes.
	 * 
	 * @return the number of data nodes
	 */
	public int getNumberOfDataNodes() {
		return numberOfDataNodes;
	}

	/**
	 * Gets the number of racks.
	 * 
	 * @return the number of racks
	 */
	public int getNumberOfRacks() {
		return numberOfRacks;
	}

	/**
	 * Gets the over replicated blocks.
	 * 
	 * @return the over replicated blocks
	 */
	public long getOverReplicatedBlocks() {
		return overReplicatedBlocks;
	}

	/**
	 * Gets the status.
	 * 
	 * @return the status
	 */
	public String getStatus() {
		return status;
	}

	/**
	 * Gets the total blocks.
	 * 
	 * @return the total blocks
	 */
	public long getTotalBlocks() {
		return totalBlocks;
	}

	/**
	 * Gets the total dirs.
	 * 
	 * @return the total dirs
	 */
	public long getTotalDirs() {
		return totalDirs;
	}

	/**
	 * Gets the total files.
	 * 
	 * @return the total files
	 */
	public long getTotalFiles() {
		return totalFiles;
	}

	/**
	 * Gets the total size.
	 * 
	 * @return the total size
	 */
	public long getTotalSize() {
		return totalSize;
	}

	/**
	 * Gets the under replicated blocks.
	 * 
	 * @return the under replicated blocks
	 */
	public long getUnderReplicatedBlocks() {
		return underReplicatedBlocks;
	}

	/**
	 * Parses the fsck results.
	 * 
	 * @param reader
	 *            the reader
	 * @throws Exception
	 *             the exception
	 */
	public void parseFsckResults(BufferedReader reader) throws Exception {

		String line;
		while ((line = reader.readLine()) != null) {
			line = line.trim();
			if (StringUtils.startsWithIgnoreCase(line, "Status")) {
				this.status = line.split(":")[1].trim();
				break;
			}
		}

		while ((line = reader.readLine()) != null) {
			line = line.trim();
			if (StringUtils.startsWithIgnoreCase(line, "FSCK ended")) {
				break;
			} else if (StringUtils.startsWithIgnoreCase(line, "Total size")) {
				this.totalSize = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Total dirs")) {
				this.totalDirs = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Total files")) {
				this.totalFiles = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Total blocks (validated)")) {
				this.totalBlocks = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Minimally replicated blocks")) {
				this.minimallyReplicatedBlocks = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Over-replicated blocks")) {
				this.overReplicatedBlocks = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, " Under-replicated blocks")) {
				this.underReplicatedBlocks = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Mis-replicated blocks")) {
				this.misReplicatedBlocks = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Default replication factor")) {
				this.defaultReplicationFactor = getIntValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Average block replication")) {
				this.avarageBlockReplication = getFloatValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Corrupt blocks")) {
				this.corruptBlocks = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Missing replicas")) {
				this.missingReplicas = getLongValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Number of data-nodes")) {
				this.numberOfDataNodes = getIntValue(line);
			} else if (StringUtils.startsWithIgnoreCase(line, "Number of racks")) {
				this.numberOfRacks = getIntValue(line);
			}
		}
	}

	/**
	 * Parses the value.
	 * 
	 * @param line
	 *            the line
	 * @return the string
	 */
	private String parseValue(String line) {
		String value = line.split(":")[1].trim();
		value = StringUtils.split(value, " ")[0];
		return value;
	}
}
