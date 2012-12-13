/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.io.File;
import java.io.FileOutputStream;
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
 * The Class SCPFromRemoteHost.
 */
class SCPFromRemoteHost extends RunnableCommand {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The destination directory path. */
	private String destinationDirectoryPath;

	/** The source files. */
	private String sourceFiles;

	/**
	 * Instantiates a new sCP from remote host.
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
	public SCPFromRemoteHost(String hostname, User adminUser, String sourceFiles, String destinationDirectoryPath, User runAsUser, List<ExecutionResult> results) {
		this.hostname = hostname;
		this.adminUser = adminUser;
		this.sourceFiles = sourceFiles;
		this.destinationDirectoryPath = destinationDirectoryPath;
		this.results = results;
		this.runAsUser = runAsUser;
		this.printableCommand = "scp " + adminUser.getUserId() + "@" + hostname + ":" + destinationDirectoryPath + " " + sourceFiles;

	}

	/**
	 * Instantiates a new sCP from remote host.
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
	public SCPFromRemoteHost(User adminUser, String sourceFiles, String destinationDirectoryPath, User runAsUser) {
		this.adminUser = adminUser;
		this.sourceFiles = sourceFiles;
		this.destinationDirectoryPath = destinationDirectoryPath;
		this.runAsUser = runAsUser;
		this.printableCommand = "scp " + adminUser.getUserId() + "@" + hostname + ":" + destinationDirectoryPath + " " + sourceFiles;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		results.add(scpFromRemoteHost());
	}

	/**
	 * Scp from remote host.
	 * 
	 * @return the execution result
	 */
	private ExecutionResult scpFromRemoteHost() {

		sourceFiles = sourceFiles.replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostname);
		destinationDirectoryPath = destinationDirectoryPath.replaceAll(Constants.HOST_NAME_PLACEHOLDER, hostname);

		int exitCode = -1;
		StringBuffer output = new StringBuffer();
		StringBuffer error = new StringBuffer();
		ExecutionResult result = null;

		destinationDirectoryPath = destinationDirectoryPath + "/";
		printableCommand = " scp " + adminUser.getUserId() + "@" + hostname + ":" + sourceFiles + " " + destinationDirectoryPath;

		log.debug("scp command in standard form will be:" + printableCommand);

		FileOutputStream fos = null;
		ChannelExec channel = null;
		Session session = null;
		try {
			File dest = new File(destinationDirectoryPath);
			dest.mkdirs();

			JSch jsch = new JSch();
			session = jsch.getSession(adminUser.getUserId(), hostname, 22);
			session.setConfig("StrictHostKeyChecking", "no");
			if (StringUtils.isNotBlank(adminUser.getPassword())) {
				session.setPassword(adminUser.getPassword());
			} else if (StringUtils.isNotBlank(adminUser.getPrivateKeyFile())) {
				jsch.addIdentity(adminUser.getPrivateKeyFile());
			}
			session.connect();

			// exec 'scp -f rfile' remotely
			String command = "scp -p -f " + sourceFiles;
			// log.debug("command=" + command);
			channel = (ChannelExec) session.openChannel("exec");
			channel.setCommand(command);

			// get I/O streams for remote scp
			OutputStream out = channel.getOutputStream();
			InputStream in = channel.getInputStream();
			InputStream err = channel.getErrStream();

			channel.connect();

			byte[] buf = new byte[1024];

			// send '\0'
			buf[0] = 0;
			out.write(buf, 0, 1);
			out.flush();

			while (true) {

				int c = SSHUtils.checkAck(in);
				if (c != 'T') {
					break;
				}

				long modtime = 0L;
				while (true) {
					if (in.read(buf, 0, 1) < 0) {
						// error
						break;
					}
					if (buf[0] == ' ') {
						break;
					}
					modtime = modtime * 10L + (buf[0] - '0');
				}

				in.read(buf, 0, 2);
				long acctime = 0L;
				while (true) {
					if (in.read(buf, 0, 1) < 0) {
						// error
						break;
					}
					if (buf[0] == ' ') {
						break;
					}
					acctime = acctime * 10L + (buf[0] - '0');
				}

				buf[0] = 0;
				out.write(buf, 0, 1);
				out.flush();

				while (true) {
					c = SSHUtils.checkAck(in);
					if (c == 'C') {
						break;
					}
				}
				in.read(buf, 0, 5);
				long filesize = 0L;
				while (true) {
					if (in.read(buf, 0, 1) < 0) {
						// error
						break;
					}
					if (buf[0] == ' ') {
						break;
					}
					filesize = filesize * 10L + (buf[0] - '0');
				}

				String fileName = null;
				for (int i = 0;; i++) {
					in.read(buf, i, 1);
					if (buf[i] == (byte) 0x0a) {
						fileName = new String(buf, 0, i);
						break;
					}
				}

				log.debug("filesize=" + filesize + ", fileName=" + fileName);

				// send '\0'
				buf[0] = 0;
				out.write(buf, 0, 1);
				out.flush();

				String destinationFileName = (dest.isDirectory() || destinationDirectoryPath.endsWith("/")) ? destinationDirectoryPath + fileName
						: destinationDirectoryPath;
				File destinationFile = new File(destinationFileName);
				if (!destinationFile.getParentFile().exists()) {
					destinationFile.getParentFile().mkdirs();
				}
				fos = new FileOutputStream(destinationFileName);
				int foo;
				while (true) {
					if (buf.length < filesize) {
						foo = buf.length;
					} else {
						foo = (int) filesize;
					}
					foo = in.read(buf, 0, foo);
					if (foo < 0) {
						// error
						break;
					}
					fos.write(buf, 0, foo);
					filesize -= foo;
					if (filesize == 0L) {
						break;
					}
				}
				fos.close();
				fos = null;

				destinationFile.setLastModified(modtime * 1000);

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

				// send '\0'
				buf[0] = 0;
				out.write(buf, 0, 1);
				out.flush();

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
			if (fos != null) {
				try {
					fos.close();
				} catch (Exception e) {
					// Ignore exception
				}
			}
		}

		return result;
	}
}
