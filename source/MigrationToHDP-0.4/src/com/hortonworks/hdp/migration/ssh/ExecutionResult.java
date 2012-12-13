/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class ExecutionResult.
 */
public class ExecutionResult {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The command. */
	private String command;

	/** The error. */
	private String error;

	/** The exit code. */
	private int exitCode = 0;

	/** The hostname. */
	private String hostname;

	/** The output. */
	private String output;

	/**
	 * Instantiates a new execution result.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param command
	 *            the command
	 * @param exitCode
	 *            the exit code
	 * @param output
	 *            the output
	 * @param error
	 *            the error
	 */
	public ExecutionResult(String hostname, String command, int exitCode, String output, String error) {
		this.hostname = hostname;
		this.command = command;
		this.exitCode = exitCode;
		this.output = output;
		this.error = error;
	}

	/**
	 * Gets the command.
	 * 
	 * @return the command
	 */
	public String getCommand() {
		return command;
	}

	/**
	 * Gets the error.
	 * 
	 * @return the error
	 */
	public String getError() {
		return error;
	}

	/**
	 * Gets the exit code.
	 * 
	 * @return the exit code
	 */
	public int getExitCode() {
		return exitCode;
	}

	/**
	 * Gets the hostname.
	 * 
	 * @return the hostname
	 */
	public String getHostname() {
		return hostname;
	}

	/**
	 * Gets the output.
	 * 
	 * @return the output
	 */
	public String getOutput() {
		return output;
	}

	/**
	 * Prints the.
	 */
	public void print() {
		if (exitCode <= 0) {
			if (StringUtils.isBlank(error)) {
				log.debug(toString());
			} else {
				log.warn(toString());
			}
		} else {
			log.error(toString());
			if (!SSHUtils
					.confirmAction("Above command execution failed. Please fix the problem and re-execute above command before continuing. Do you want to continue?  Answering n/N will exit the process")) {
				log.info("Exiting as user did not want to continue due to above failure.");
				System.exit(1);
			}
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String str = "\nhostname=" + hostname;
		str += "\ncommand=" + command;
		str += "\nexitCode=" + exitCode;
		str += "\noutput=" + StringUtils.trim(output);
		str += "\nerror=" + StringUtils.trim(error);
		return str;
	}
}
