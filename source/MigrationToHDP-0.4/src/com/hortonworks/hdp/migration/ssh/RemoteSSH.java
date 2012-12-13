/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Session;

// TODO: Auto-generated Javadoc
/**
 * The Class RemoteSSH.
 */
class RemoteSSH extends RunnableCommand {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The commands. */
	private String[] commands;

	/**
	 * Instantiates a new remote ssh.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param results
	 *            the results
	 */
	public RemoteSSH(String hostname, User adminUser, String command, User runAsUser, List<ExecutionResult> results) {
		this.hostname = hostname;
		this.adminUser = adminUser;
		this.commands = new String[] { command };
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Instantiates a new remote ssh.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @param results
	 *            the results
	 */
	public RemoteSSH(String hostname, User adminUser, String[] commands, User runAsUser, List<ExecutionResult> results) {
		this.hostname = hostname;
		this.adminUser = adminUser;
		this.commands = commands;
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Instantiates a new remote ssh.
	 * 
	 * @param adminUser
	 *            the admin user
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 */
	public RemoteSSH(User adminUser, String command, User runAsUser) {
		this.adminUser = adminUser;
		this.runAsUser = runAsUser;
		this.commands = new String[] { command };
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");

	}

	/**
	 * Instantiates a new remote ssh.
	 * 
	 * @param adminUser
	 *            the admin user
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 */
	public RemoteSSH(User adminUser, String[] commands, User runAsUser) {
		this.adminUser = adminUser;
		this.runAsUser = runAsUser;
		this.commands = commands;
		this.printableCommand = StringUtils.replace(ArrayUtils.toString(commands), ",", " ; ");
	}

	/**
	 * Execute remote command.
	 * 
	 * @return the list
	 */
	private List<ExecutionResult> executeRemoteCommand() {
		int exitCode = -1;
		ChannelExec channel = null;
		Session session = null;
		StringBuffer output = new StringBuffer();
		StringBuffer error = new StringBuffer();
		List<ExecutionResult> results = new ArrayList<ExecutionResult>();

		JSch jsch = null;
		try {
			jsch = new JSch();
			if (adminUser.getPrivateKeyFile() != null) {
				jsch.addIdentity(adminUser.getPrivateKeyFile());
			}
			session = jsch.getSession(adminUser.getUserId(), hostname, 22);
			session.setConfig("StrictHostKeyChecking", "no");
			if (StringUtils.isNotBlank(adminUser.getPassword())) {
				session.setPassword(adminUser.getPassword());
			} else if (StringUtils.isNotBlank(adminUser.getPrivateKeyFile())) {
				jsch.addIdentity(adminUser.getPrivateKeyFile());
			}

			session.connect();

			for (String command : commands) {
				if (StringUtils.isBlank(command)) {
					continue;
				}

				command = command.replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostname);
				command = SSHUtils.substituteUser(command, adminUser, runAsUser);

				log.debug("Executing command: " + command);
				output.delete(0, output.length());
				error.delete(0, error.length());
				exitCode = -1;

				channel = (ChannelExec) session.openChannel("exec");
				channel.setCommand(command);
				channel.setInputStream(null);
				channel.setErrStream(System.err);
				InputStream in = channel.getInputStream();
				InputStream err = channel.getErrStream();

				channel.connect();
				log.debug("Connected to host " + hostname);
				byte[] tmp = new byte[1024];
				while (true) {
					while (in.available() > 0) {
						int i = in.read(tmp, 0, 1024);
						if (i < 0) {
							break;
						}
						output.append(new String(tmp, 0, i));
					}
					while (err.available() > 0) {
						int i = err.read(tmp, 0, 1024);
						if (i < 0) {
							break;
						}
						error.append(new String(tmp, 0, i));
					}

					if (channel.isClosed()) {
						exitCode = channel.getExitStatus();

						break;
					}
					try {
						Thread.sleep(1000);
					} catch (Exception e) {
						log.info("", e);
					}
				}

				exitCode = channel.getExitStatus();
				ExecutionResult result = new ExecutionResult(hostname, command, exitCode, output.toString(), error.toString());
				results.add(result);
			}
		} catch (Exception e) {

			ExecutionResult result = new ExecutionResult(hostname, commands[0], 1, output.toString(), "Failed to execute command by connecting to host "
					+ hostname + " using user id " + adminUser.getUserId() + "\n" + ExceptionUtils.getStackTrace(e));
			results.add(result);

		} finally {
			if (channel != null) {
				channel.disconnect();
			}
			if (session != null) {
				session.disconnect();
			}

		}

		return results;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		results.addAll(executeRemoteCommand());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return ArrayUtils.toString(commands);
	}

}
