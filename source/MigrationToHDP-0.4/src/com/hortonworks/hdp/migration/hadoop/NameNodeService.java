/*
 * 
 */
package com.hortonworks.hdp.migration.hadoop;

import java.util.List;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.ssh.CommandExecutor;
import com.hortonworks.hdp.migration.ssh.ExecutionResult;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class NameNodeService.
 */
public class NameNodeService extends HDFSService {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant processPattern. */
	private static final String[] processPattern = { "Dproc_namenode", "org.apache.hadoop.hdfs.server.namenode.NameNode" };

	/**
	 * Instantiates a new name node service.
	 * 
	 * @param serviceOwnerUser
	 *            the service owner user
	 * @param homeLocation
	 *            the home location
	 * @param configLocation
	 *            the config location
	 * @param adminUser
	 *            the admin user
	 * @param upgradeStage
	 *            the upgrade stage
	 * @throws Exception
	 *             the exception
	 */
	public NameNodeService(User serviceOwnerUser, String homeLocation, String configLocation, User adminUser, String upgradeStage) throws Exception {
		super("namenode", serviceOwnerUser, homeLocation, configLocation, adminUser, upgradeStage);
		// TODO load installation specific configurations.
	}

	/**
	 * Enter safe mode.
	 * 
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	protected ExecutionResult enterSafeMode() throws Exception {
		return CommandExecutor.executeRemoteCommand(getHostsList().get(0), getAdminUser(), getHomeLocation() + "/bin/hadoop --config " + getConfigLocation()
				+ "  dfsadmin -safemode enter", getUser());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#getProcessPattern()
	 */
	@Override
	public String[] getProcessPattern() {
		return processPattern;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.CoreHadoopService#getStartServiceCommand
	 * ()
	 */
	@Override
	public String getStartServiceCommand() {
		return getStartServiceAndWaitCommand(15);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.hortonworks.hdp.migration.hadoop.CoreHadoopService#getStopServiceCommand
	 * ()
	 */
	@Override
	public String getStopServiceCommand() {
		return getStopServiceAndWaitCommand(15);
	}

	/**
	 * Save namespace.
	 * 
	 * @return the execution result
	 * @throws Exception
	 *             the exception
	 */
	protected ExecutionResult saveNamespace() throws Exception {
		enterSafeMode();
		return CommandExecutor.executeRemoteCommand(getHostsList().get(0), getAdminUser(), getHomeLocation() + "/bin/hadoop --config " + getConfigLocation()
				+ " dfsadmin -saveNamespace", getUser());
	}

	/**
	 * Start in upgrade mode.
	 * 
	 * @return the list
	 * @throws Exception
	 *             the exception
	 */
	public List<ExecutionResult> startInUpgradeMode() throws Exception {
		log.info("Starting HDFS in upgrade mode");
		return CommandExecutor.executeRemoteCommand(getHostsList(), getAdminUser(), getStartServiceAndWaitCommand(" -upgrade ", 60), getUser());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.hortonworks.hdp.migration.hadoop.Service#stopService()
	 */
	@Override
	public List<ExecutionResult> stopService() throws Exception {
		ExecutionResult saveNamespaceResult = saveNamespace();
		List<ExecutionResult> results = super.stopService();
		results.add(0, saveNamespaceResult);
		return results;
	}
}
