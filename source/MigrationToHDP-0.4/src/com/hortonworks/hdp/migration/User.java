/*
 * 
 */
package com.hortonworks.hdp.migration;

import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class User.
 */
public class User {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The password. */
	private String password = null;

	/** The private key file. */
	private String privateKeyFile = null;

	/** The user id. */
	private String userId;

	/** The work area. */
	private String workArea = "~/";

	/**
	 * Instantiates a new user.
	 * 
	 * @param id
	 *            the id
	 * @param pwd
	 *            the pwd
	 * @param privateKeyFile
	 *            the private key file
	 */
	public User(String id, String pwd, String privateKeyFile) {
		this.userId = id;
		this.password = pwd;
		this.privateKeyFile = privateKeyFile;
	}

	/**
	 * Gets the password.
	 * 
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * Gets the private key file.
	 * 
	 * @return the private key file
	 */
	public String getPrivateKeyFile() {
		return privateKeyFile;
	}

	/**
	 * Gets the user id.
	 * 
	 * @return the user id
	 */
	public String getUserId() {
		return userId;
	}

	/**
	 * Gets the work area.
	 * 
	 * @return the work area
	 */
	public String getWorkArea() {
		return workArea;
	}

	// TODO ask user for work area and create the necessary directories on hosts
	/**
	 * Sets the work area.
	 * 
	 * @param workArea
	 *            the new work area
	 */
	public void setWorkArea(String workArea) {
		this.workArea = workArea;
	}

}
