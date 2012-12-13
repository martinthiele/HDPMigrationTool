/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.util.List;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;

// TODO: Auto-generated Javadoc
/**
 * The Class RunnableCommand.
 */
public abstract class RunnableCommand implements Runnable, Cloneable {

	/** The admin user. */
	protected User adminUser;

	/** The hostname. */
	protected String hostname = Constants.HOST_NAME_PLACEHOLDER;

	/** The printable command. */
	protected String printableCommand;

	/** The results. */
	protected List<ExecutionResult> results;

	/** The run as user. */
	protected User runAsUser;

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	/**
	 * Gets the admin user.
	 * 
	 * @return the admin user
	 */
	public User getAdminUser() {
		return adminUser;
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
	 * Gets the printable command.
	 * 
	 * @return the printable command
	 */
	public String getPrintableCommand() {
		return printableCommand;
	}

	/**
	 * Gets the results.
	 * 
	 * @return the results
	 */
	public List<ExecutionResult> getResults() {
		return results;
	}

	/**
	 * Gets the run as user.
	 * 
	 * @return the run as user
	 */
	public User getRunAsUser() {
		return runAsUser;
	}

	/**
	 * Sets the admin user.
	 * 
	 * @param adminUser
	 *            the new admin user
	 */
	public void setAdminUser(User adminUser) {
		this.adminUser = adminUser;
	}

	/**
	 * Sets the hostname.
	 * 
	 * @param hostname
	 *            the new hostname
	 */
	public void setHostname(String hostname) {
		this.hostname = hostname;
	}

	/**
	 * Sets the results.
	 * 
	 * @param results
	 *            the new results
	 */
	public void setResults(List<ExecutionResult> results) {
		this.results = results;
	}

	/**
	 * Sets the run as user.
	 * 
	 * @param runAsUser
	 *            the new run as user
	 */
	public void setRunAsUser(User runAsUser) {
		this.runAsUser = runAsUser;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String str = "hostname=" + getHostname();
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
