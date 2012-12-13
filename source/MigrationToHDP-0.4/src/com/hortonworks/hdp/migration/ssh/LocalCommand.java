/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class LocalCommand.
 */
class LocalCommand extends RunnableCommand {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The commands. */
	private String[] commands;

	/** The for hostname. */
	protected String forHostname = null;

	/** The hostname. */
	protected String hostname = "localhost";

	/**
	 * Instantiates a new local command.
	 * 
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 */
	public LocalCommand(String command, User runAsUser) {
		this.runAsUser = runAsUser;
		this.commands = new String[] { command };
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Instantiates a new local command.
	 * 
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param results
	 *            the results
	 */
	public LocalCommand(String command, User runAsUser, List<ExecutionResult> results) {
		this.commands = new String[] { command };
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Instantiates a new local command.
	 * 
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 */
	public LocalCommand(String[] commands, User runAsUser) {
		this.runAsUser = runAsUser;
		this.commands = commands;
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Instantiates a new local command.
	 * 
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @param results
	 *            the results
	 */
	public LocalCommand(String[] commands, User runAsUser, List<ExecutionResult> results) {
		this.commands = commands;
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Execute local command.
	 * 
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public List<ExecutionResult> executeLocalCommand() throws Exception {
		List<ExecutionResult> results = new ArrayList<ExecutionResult>();

		for (String command : commands) {

			if (getForHostname() != null) {
				command = command.replaceAll(Constants.HOST_NAME_PLACEHOLDER, getForHostname());
			}

			StringBuffer output = new StringBuffer();
			StringBuffer error = new StringBuffer();

			Process p = Runtime.getRuntime().exec(new String[] { "bash", "-c", SSHUtils.substituteUser(command, adminUser, runAsUser) });
			p.waitFor();

			InputStream outIS = p.getInputStream();
			InputStreamReader isr = new InputStreamReader(outIS);
			BufferedReader br = new BufferedReader(isr);
			String line = null;
			while ((line = br.readLine()) != null) {
				output.append(line);

			}
			if (br != null) {
				br.close();
			}
			if (isr != null) {
				isr.close();
			}
			if (outIS != null) {
				outIS.close();
			}

			InputStream err = p.getErrorStream();
			isr = new InputStreamReader(err);
			br = new BufferedReader(isr);
			while ((line = br.readLine()) != null) {
				error.append(line);

			}

			if (err != null) {
				br.close();
			}
			if (isr != null) {
				isr.close();
			}
			if (outIS != null) {
				outIS.close();
			}
			results.add(new ExecutionResult("localhost", command, p.exitValue(), output.toString(), error.toString()));
		}
		return results;
	}

	/**
	 * Gets the for hostname.
	 * 
	 * @return the for hostname
	 */
	public String getForHostname() {
		return forHostname;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		try {
			results.addAll(executeLocalCommand());
		} catch (Exception e) {
			log.error("Failed to execute command", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.ssh.RunnableCommand#setHostname(java.lang
	 * .String)
	 */
	@Override
	public void setHostname(String hostName) {
		// Intentionally assigned to forHostname as hostname for local command
		// is always localhost
		this.forHostname = hostName;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.ssh.RunnableCommand#toString()
	 */
	@Override
	public String toString() {
		String str = "hostname=localhost";
		str += "\nfor hostname=" + getForHostname();
		if (getAdminUser() == null) {
			str += "\nadminUser=" + getAdminUser();
		} else {
			str += "\nadminUser=" + getAdminUser().getUserId();
		}
		if (getRunAsUser() == null) {
			str += "\nrunAsUser=" + getRunAsUser();
		} else {
			str += "\nrunAsUser=" + getRunAsUser().getUserId();
		}
		str += "\ncommand=" + getPrintableCommand();
		return str;
	}
}
