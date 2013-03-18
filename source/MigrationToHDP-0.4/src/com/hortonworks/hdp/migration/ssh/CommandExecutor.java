/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.StringUtils;

import com.hortonworks.hdp.migration.Constants;
import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.hadoop.ServiceUtils;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandExecutor.
 */
public class CommandExecutor {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/**
	 * Execute in parallel.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param command
	 *            the command
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	private static List<ExecutionResult> executeInParallel(List<String> hostsList, RunnableCommand command, int[] batchSizes) throws Exception {

		if (batchSizes == null || batchSizes.length == 0) {
			batchSizes = new int[] { 100 };
		}

		List<ExecutionResult> allResults = Collections.synchronizedList(new ArrayList<ExecutionResult>());

		log.debug("Executing command " + command + " on host(s) " + hostsList);

		int threads = Integer.valueOf(System.getProperty(Constants.NUMBER_OF_PARALLEL_COMMANDS, Constants.DEFAULT_NUMBER_OF_PARALLEL_COMMANDS));
		ScheduledThreadPoolExecutor threadPoolExecutor = new ScheduledThreadPoolExecutor(threads);

		int currentHostNumber = 0;
		int iterationNumber = 0;
		int currentBatchSize = 0;
		int nextBatchSize = 0;
		List<String> hostsInAction = new ArrayList<String>();

		for (currentHostNumber = 0; currentHostNumber < hostsList.size(); iterationNumber++) {
			if (iterationNumber < batchSizes.length) {
				currentBatchSize = batchSizes[iterationNumber];
			} else {
				currentBatchSize = batchSizes[batchSizes.length - 1];
			}
			if (currentHostNumber + currentBatchSize >= hostsList.size()) {
				currentBatchSize = hostsList.size() - currentHostNumber;
			}
			hostsInAction.clear();
			RunnableCommand myCommand = command;
			for (int i = 0; i < currentBatchSize; i++, currentHostNumber++) {
				myCommand = (RunnableCommand) (command.clone());
				myCommand.setHostname(hostsList.get(currentHostNumber));
				myCommand.setResults(allResults);
				threadPoolExecutor.schedule(myCommand, 0, TimeUnit.SECONDS);
				hostsInAction.add(hostsList.get(currentHostNumber));
			}
			log.debug(command.getClass().getSimpleName() + " command : " + command.getPrintableCommand());
			Collections.sort(hostsInAction);
			log.debug("Executing batch # " + (iterationNumber + 1) + " with " + currentBatchSize + " host(s) " + hostsInAction);
			while (threadPoolExecutor.getActiveCount() > 0) {
				try {
					Thread.sleep(5000);
				} catch (Exception e) {
					// ignore
				}
			}
			try {
				printResults(allResults.subList(currentHostNumber - currentBatchSize, currentHostNumber));
			} catch (Exception e) {
				// Ignore exception
			}
			log.debug("Executed batch # " + (iterationNumber + 1) + " with " + currentBatchSize + " host(s) " + hostsInAction);

			if ((iterationNumber + 1) < batchSizes.length) {
				nextBatchSize = batchSizes[iterationNumber + 1];
			} else {
				nextBatchSize = batchSizes[batchSizes.length - 1];
			}
			if (currentHostNumber + nextBatchSize >= hostsList.size()) {
				nextBatchSize = hostsList.size() - currentHostNumber;
			}

			while (nextBatchSize > 0
					&& StringUtils.equalsIgnoreCase(
							System.getProperty(Constants.MIGRATION_PROPERTY_COMMON_PROMPT_DURING_MULTIHOST_EXECUTION, Boolean.FALSE.toString()),
							Boolean.TRUE.toString())
					&& !SSHUtils.confirmAction("Executed " + command.getClass().getSimpleName() + " command : " + command.getPrintableCommand() + " on "
							+ hostsInAction.size() + " host(s) " + hostsInAction
							+ ". Have you verified the results and do you want to continue with next batch of " + nextBatchSize + " host(s)?")) {
				// Keep asking the question
			}

		}
		log.debug("Waiting on all commands to finish execution");
		threadPoolExecutor.shutdown();
		threadPoolExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);

		return allResults;

	}

	/**
	 * Execute local command.
	 * 
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param hostsList
	 *            the hosts list
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeLocalCommand(String command, User runAsUser, List<String> hostsList) throws Exception {
		return executeInParallel(hostsList, new LocalCommand(command, runAsUser), Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Execute local command.
	 * 
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param hostsList
	 *            the hosts list
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeLocalCommand(String command, User runAsUser, List<String> hostsList, int[] batchSizes) throws Exception {
		return executeInParallel(hostsList, new LocalCommand(command, runAsUser), batchSizes);
	}

	/**
	 * Execute local command.
	 * 
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param host
	 *            the host
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult executeLocalCommand(String command, User runAsUser, String host) throws Exception {
		return executeLocalCommand(command, runAsUser, host, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Execute local command.
	 * 
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param host
	 *            the host
	 * @param batchSizes
	 *            the batch sizes
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult executeLocalCommand(String command, User runAsUser, String host, int[] batchSizes) throws Exception {
		List<String> hostsList = new ArrayList<String>();
		if (StringUtils.isBlank(host)) {
			hostsList.add("localhost");
		} else {
			hostsList.add(host);
		}
		List<ExecutionResult> results = executeInParallel(hostsList, new LocalCommand(command, runAsUser), batchSizes);
		if (results != null && results.size() > 0) {
			return results.get(0);
		} else {
			return null;
		}
	}

	/**
	 * Execute local command.
	 * 
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @param hostsList
	 *            the hosts list
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeLocalCommand(String[] commands, User runAsUser, List<String> hostsList) throws Exception {
		return executeInParallel(hostsList, new LocalCommand(commands, runAsUser), Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Execute local command.
	 * 
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @param hostsList
	 *            the hosts list
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeLocalCommand(String[] commands, User runAsUser, List<String> hostsList, int[] batchSizes) throws Exception {
		return executeInParallel(hostsList, new LocalCommand(commands, runAsUser), batchSizes);
	}

	/**
	 * Execute local command.
	 * 
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @param host
	 *            the host
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeLocalCommand(String[] commands, User runAsUser, String host) throws Exception {
		List<String> hostsList = new ArrayList<String>();
		hostsList.add(host);
		return executeInParallel(hostsList, new LocalCommand(commands, runAsUser), Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	};

	/**
	 * Execute local command.
	 * 
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @param host
	 *            the host
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeLocalCommand(String[] commands, User runAsUser, String host, int[] batchSizes) throws Exception {
		List<String> hostsList = new ArrayList<String>();
		hostsList.add(host);
		return executeInParallel(hostsList, new LocalCommand(commands, runAsUser), batchSizes);
	};

	/**
	 * Execute remote command.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @return number of command execution failures. If the return value is non
	 *         zero it means at least one command on at least one host failed.
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeRemoteCommand(List<String> hostsList, User adminUser, String command, User runAsUser) throws Exception {
		return executeInParallel(hostsList, new RemoteSSH(adminUser, command, runAsUser), Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	};

	/**
	 * Execute remote command.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeRemoteCommand(List<String> hostsList, User adminUser, String command, User runAsUser, int[] batchSizes)
			throws Exception {
		return executeInParallel(hostsList, new RemoteSSH(adminUser, command, runAsUser), batchSizes);
	};

	/**
	 * Execute remote command.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param commands
	 *            the commands
	 * @param runAsUser
	 *            the run as user
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> executeRemoteCommand(List<String> hostsList, User adminUser, String[] commands, User runAsUser) throws Exception {
		return executeInParallel(hostsList, new RemoteSSH(adminUser, commands, runAsUser), Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	};

	/**
	 * Execute remote command.
	 * 
	 * @param host
	 *            the host
	 * @param adminUser
	 *            the admin user
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult executeRemoteCommand(String host, User adminUser, String command, User runAsUser) throws Exception {
		return executeRemoteCommand(host, adminUser, command, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);

	}

	/**
	 * Execute remote command.
	 * 
	 * @param host
	 *            the host
	 * @param adminUser
	 *            the admin user
	 * @param command
	 *            the command
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult executeRemoteCommand(String host, User adminUser, String command, User runAsUser, int[] batchSizes) throws Exception {
		List<String> hostsList = new ArrayList<String>();
		hostsList.add(host);
		List<ExecutionResult> results = executeInParallel(hostsList, new RemoteSSH(adminUser, command, runAsUser), batchSizes);
		if (results != null && results.size() > 0) {
			return results.get(0);
		} else {
			return null;
		}

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
			System.out.println("Usage: " + CommandExecutor.class.getName()
					+ " <path of file with the host names> <command to be executed on all hosts> [run as userid :optional]");
			System.exit(1);
		}
		List<String> hostsList = ServiceUtils.readHostNamesFromFile(args[0]);
		User runAsUser = null;
		User adminUser = null;
		if (args.length >= 3 && StringUtils.isNotBlank(args[2])) {
			runAsUser = new User(args[2], null, null);
		}
		adminUser = SSHUtils.readUserIdPassword("Please enter user id to be used to login to remote hosts", "root");
		List<ExecutionResult> results = executeRemoteCommand(hostsList, adminUser, args[1], runAsUser);

		printResults(results);

	}

	/**
	 * Prints the.
	 * 
	 * @param results
	 *            the results
	 */
	public static void printResults(List<ExecutionResult> results) {

		List<String> failedHosts = new ArrayList<String>();
		List<String> successfulHosts = new ArrayList<String>();

		for (ExecutionResult result : results) {

			if (result.getExitCode() <= 0) {
				if (StringUtils.isBlank(result.getError())) {
					log.debug(result.toString());
				} else {
					log.warn(result.toString());
				}
				successfulHosts.add(result.getHostname());
			} else {
				log.error(result.toString());
				failedHosts.add(result.getHostname());
			}
		}
		Collections.sort(failedHosts);
		if (failedHosts.size() > 0
				&& !SSHUtils
						.confirmAction("Above command execution failed on "
								+ failedHosts.size()
								+ " host(s) "
								+ failedHosts
								+ ". Please fix the problem and re-execute above command before continuing. Do you want to continue? Answering n/N will exit the process")) {
			log.info("Exiting as user did not want to continue due to above failure.");
			System.exit(1);
		}

	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> scpFromRemoteHost(List<String> hostsList, User adminUser, String sourceFilePath, String destinationPath, User runAsUser)
			throws Exception {
		return scpFromRemoteHost(hostsList, adminUser, sourceFilePath, destinationPath, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> scpFromRemoteHost(List<String> hostsList, User adminUser, String sourceFilePath, String destinationPath,
			User runAsUser, int[] batchSizes) throws Exception {
		return executeInParallel(hostsList,
				new SCPFromRemoteHost(adminUser, StringUtils.replace(sourceFilePath, " ", "\\ "), StringUtils.replace(destinationPath, " ", "\\ "), runAsUser),
				batchSizes);
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePaths
	 *            the source file paths
	 * @param destinationDirectory
	 *            the destination directory
	 * @param runAsUser
	 *            the run as user
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> scpFromRemoteHost(List<String> hostsList, User adminUser, String[] sourceFilePaths, String destinationDirectory,
			User runAsUser) throws Exception {
		return scpFromRemoteHost(hostsList, adminUser, sourceFilePaths, destinationDirectory, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePaths
	 *            the source file paths
	 * @param destinationDirectory
	 *            the destination directory
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> scpFromRemoteHost(List<String> hostsList, User adminUser, String[] sourceFilePaths, String destinationDirectory,
			User runAsUser, int[] batchSizes) throws Exception {
		String files = "";
		for (String file : sourceFilePaths) {
			files += StringUtils.replace(file, " ", "\\ ") + " ";
		}
		return executeInParallel(hostsList, new SCPFromRemoteHost(adminUser, files, StringUtils.replace(destinationDirectory, " ", "\\ "), runAsUser),
				batchSizes);
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult scpFromRemoteHost(String hostname, User adminUser, String sourceFilePath, String destinationPath, User runAsUser)
			throws Exception {

		return scpFromRemoteHost(hostname, adminUser, sourceFilePath, destinationPath, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult scpFromRemoteHost(String hostname, User adminUser, String sourceFilePath, String destinationPath, User runAsUser,
			int[] batchSizes) throws Exception {

		List<String> hostsList = new ArrayList<String>();
		hostsList.add(hostname);
		List<ExecutionResult> results = scpFromRemoteHost(hostsList, adminUser, sourceFilePath, destinationPath, runAsUser, batchSizes);
		if (results != null && results.size() > 0) {
			return results.get(0);
		} else {
			return null;
		}
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePaths
	 *            the source file paths
	 * @param destinationDirectory
	 *            the destination directory
	 * @param runAsUser
	 *            the run as user
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult scpFromRemoteHost(String hostname, User adminUser, String[] sourceFilePaths, String destinationDirectory, User runAsUser)
			throws Exception {
		return scpFromRemoteHost(hostname, adminUser, sourceFilePaths, destinationDirectory, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Scp from remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePaths
	 *            the source file paths
	 * @param destinationDirectory
	 *            the destination directory
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult scpFromRemoteHost(String hostname, User adminUser, String[] sourceFilePaths, String destinationDirectory, User runAsUser,
			int[] batchSizes) throws Exception {
		List<String> hostsList = new ArrayList<String>();
		hostsList.add(hostname);

		List<ExecutionResult> results = scpFromRemoteHost(hostsList, adminUser, sourceFilePaths, destinationDirectory, runAsUser, batchSizes);
		if (results != null && results.size() > 0) {
			return results.get(0);
		} else {
			return null;
		}
	}

	/**
	 * Scp to remote host.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> scpToRemoteHost(List<String> hostsList, User adminUser, String sourceFilePath, String destinationPath, User runAsUser)
			throws Exception {
		return scpToRemoteHost(hostsList, adminUser, sourceFilePath, destinationPath, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Scp to remote host.
	 * 
	 * @param hostsList
	 *            the hosts list
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public static List<ExecutionResult> scpToRemoteHost(List<String> hostsList, User adminUser, String sourceFilePath, String destinationPath, User runAsUser,
			int[] batchSizes) throws Exception {
		return executeInParallel(hostsList,
				new SCPToRemoteHost(adminUser, StringUtils.replace(sourceFilePath, " ", "\\ "), StringUtils.replace(destinationPath, " ", "\\ "), runAsUser),
				batchSizes);
	}

	/**
	 * Scp to remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult scpToRemoteHost(String hostname, User adminUser, String sourceFilePath, String destinationPath, User runAsUser)
			throws Exception {
		return scpToRemoteHost(hostname, adminUser, sourceFilePath, destinationPath, runAsUser, Constants.PREFERRED_PARALLEL_EXECUTION_BATCH_SIZES);
	}

	/**
	 * Scp to remote host.
	 * 
	 * @param hostname
	 *            the hostname
	 * @param adminUser
	 *            the admin user
	 * @param sourceFilePath
	 *            the source file path
	 * @param destinationPath
	 *            the destination path
	 * @param runAsUser
	 *            the run as user
	 * @param batchSizes
	 *            the batch sizes
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	public static ExecutionResult scpToRemoteHost(String hostname, User adminUser, String sourceFilePath, String destinationPath, User runAsUser,
			int[] batchSizes) throws Exception {
		List<String> hostsList = new ArrayList<String>();
		hostsList.add(hostname);

		List<ExecutionResult> results = scpToRemoteHost(hostsList, adminUser, sourceFilePath, destinationPath, runAsUser, batchSizes);
		if (results != null && results.size() > 0) {
			return results.get(0);
		} else {
			return null;
		}

	}
}
