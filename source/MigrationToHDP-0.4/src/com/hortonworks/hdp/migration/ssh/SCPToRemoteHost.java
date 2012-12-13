/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

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
 * The Class SCPToRemoteHost.
 */
public class SCPToRemoteHost extends RunnableCommand {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The destination directory path. */
	private String destinationDirectoryPath;

	/** The source files. */
	private String sourceFiles;

	/**
	 * Instantiates a new sCP to remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFiles
	 *            the source files
	 * @param destinationDirectoryPath
	 *            the destination directory path
	 * @param runAsUser
	 *            the run as user
	 * @param results
	 *            the results
	 */
	public SCPToRemoteHost(String hostname, User adminUser, String sourceFiles, String destinationDirectoryPath, User runAsUser, List<ExecutionResult> results) {
		this.hostname = hostname;
		this.adminUser = adminUser;
		this.sourceFiles = sourceFiles;
		this.destinationDirectoryPath = destinationDirectoryPath;
		this.results = results;
		this.runAsUser = runAsUser;
		this.printableCommand = "scp " + sourceFiles + " " + adminUser.getUserId() + "@" + hostname + ":" + destinationDirectoryPath;
	}

	/**
	 * Instantiates a new sCP to remote host.
	 * 
	 * @param adminUser
	 *            the admin user
	 * @param sourceFiles
	 *            the source files
	 * @param destinationDirectoryPath
	 *            the destination directory path
	 * @param runAsUser
	 *            the run as user
	 */
	public SCPToRemoteHost(User adminUser, String sourceFiles, String destinationDirectoryPath, User runAsUser) {
		this.adminUser = adminUser;
		this.sourceFiles = sourceFiles;
		this.destinationDirectoryPath = destinationDirectoryPath;
		this.runAsUser = runAsUser;
		this.printableCommand = "scp " + sourceFiles + " " + adminUser.getUserId() + "@" + hostname + ":" + destinationDirectoryPath;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		results.add(scpToRemoteHost());
	}

	/**
	 * Scp to remote host.
	 * 
	 * @return the execution result
	 */
	private ExecutionResult scpToRemoteHost() {

		sourceFiles = sourceFiles.replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostname);
		destinationDirectoryPath = destinationDirectoryPath.replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostname);

		printableCommand = "scp " + sourceFiles + " " + adminUser.getUserId() + "@" + hostname + ":" + destinationDirectoryPath;
		log.debug("scp command in standard form will be: " + printableCommand);

		FileInputStream fis = null;
		int exitCode = -1;
		StringBuffer output = new StringBuffer();
		StringBuffer error = new StringBuffer();

		JSch jsch = new JSch();
		ChannelExec channel = null;
		Session session = null;
		ExecutionResult result = null;

		try {
			session = jsch.getSession(adminUser.getUserId(), hostname, 22);
			session.setConfig("StrictHostKeyChecking", "no");
			if (StringUtils.isNotBlank(adminUser.getPassword())) {
				session.setPassword(adminUser.getPassword());
			} else if (StringUtils.isNotBlank(adminUser.getPrivateKeyFile())) {
				jsch.addIdentity(adminUser.getPrivateKeyFile());
			}

			session.connect();

			// exec 'scp -t rfile' remotely
			String command = "scp -p -t " + destinationDirectoryPath;
			channel = (ChannelExec) session.openChannel("exec");
			channel.setCommand(command);

			// get I/O streams for remote scp
			OutputStream out = channel.getOutputStream();
			InputStream in = channel.getInputStream();
			InputStream err = channel.getErrStream();

			channel.connect();

			if (SSHUtils.checkAck(in) != 0) {
				byte[] tmp = new byte[1024];

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
				exitCode = channel.getExitStatus();

				result = new ExecutionResult(hostname, printableCommand, exitCode, output.toString(), error.toString());
				return result;
			}

			File _lfile = new File(sourceFiles);

			if (true) {
				command = "T " + (_lfile.lastModified() / 1000) + " 0";
				// The access time should be sent here,
				// but it is not accessible with JavaAPI ;-<
				command += (" " + (_lfile.lastModified() / 1000) + " 0\n");
				out.write(command.getBytes());
				out.flush();
				if (SSHUtils.checkAck(in) != 0) {
					// System.exit(0);
					log.error("Failed to execute the command successfully");
					return null;
				}
			}

			// send "C0644 filesize filename", where filename should not include
			// '/'
			long filesize = _lfile.length();
			command = "C0644 " + filesize + " ";
			if (sourceFiles.lastIndexOf('/') > 0) {
				command += sourceFiles.substring(sourceFiles.lastIndexOf('/') + 1);
			} else {
				command += sourceFiles;
			}
			command += "\n";
			log.debug("Actual scp command is " + command);
			out.write(command.getBytes());
			out.flush();
			if (SSHUtils.checkAck(in) != 0) {
				byte[] tmp = new byte[1024];

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
				exitCode = channel.getExitStatus();
				result = new ExecutionResult(hostname, printableCommand, exitCode, output.toString(), error.toString());
				return result;
			}

			// send a content of lfile
			fis = new FileInputStream(sourceFiles);
			byte[] buf = new byte[1024];
			while (true) {
				int len = fis.read(buf, 0, buf.length);
				if (len <= 0) {
					break;
				}
				out.write(buf, 0, len); // out.flush();
			}
			fis.close();
			fis = null;
			// send '\0'
			buf[0] = 0;
			out.write(buf, 0, 1);
			out.flush();

			if (SSHUtils.checkAck(in) != 0) {
				byte[] tmp = new byte[1024];

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
				exitCode = channel.getExitStatus();
				result = new ExecutionResult(hostname, printableCommand, exitCode, output.toString(), error.toString());
				return result;
			}
			byte[] tmp = new byte[1024];

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

			out.close();
			in.close();
			err.close();

			exitCode = channel.getExitStatus();
			result = new ExecutionResult(hostname, printableCommand, exitCode, output.toString(), error.toString());

		} catch (Exception e) {
			result = new ExecutionResult(hostname, printableCommand, 1, output.toString(), "Error while scp from remote host" + "\n"
					+ ExceptionUtils.getStackTrace(e));
		} finally {
			if (channel != null) {
				try {
					channel.disconnect();
				} catch (Exception e) {
					// Ignore exception
				}
			}
			if (session != null) {
				try {
					session.disconnect();
				} catch (Exception e) {
					// Ignore exception
				}
			}

		}
		return result;
	}

}
