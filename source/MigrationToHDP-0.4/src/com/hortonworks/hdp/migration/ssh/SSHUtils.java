/*
 * 
 */
package com.hortonworks.hdp.migration.ssh;

import java.io.Console;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;

import com.hortonworks.hdp.migration.User;
import com.hortonworks.hdp.migration.util.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class SSHUtils.
 */
public class SSHUtils {

	/** The Constant log. */
	private static final Logger log = new Logger();

	/** The Constant MESSAGE_DEFAULT_PATTERN. */
	private static final String MESSAGE_DEFAULT_PATTERN = "\u001B[0m";

	/** The Constant MESSAGE_HIGHLIGHT_PATTERN. */
	private static final String MESSAGE_HIGHLIGHT_PATTERN = "\u001B[1;32m";

	/**
	 * Check ack.
	 * 
	 * @param in
	 *            the in
	 * @return the int
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	static int checkAck(InputStream in) throws IOException {
		int b = in.read();
		// b may be 0 for success,
		// 1 for error,
		// 2 for fatal error,
		// -1
		if (b == 0) {
			return b;
		}
		if (b == -1) {
			return b;
		}

		if (b == 1 || b == 2) {
			StringBuffer sb = new StringBuffer();
			int c;
			do {
				c = in.read();
				sb.append((char) c);
			} while (c != '\n');
			if (b == 1) { // error
				System.out.print(sb.toString());
			}
			if (b == 2) { // fatal error
				System.out.print(sb.toString());
			}
		}
		return b;
	}

	/**
	 * Confirm action.
	 * 
	 * @param question
	 *            the question
	 * @return true, if successful
	 */
	public static boolean confirmAction(String question) {
		String input = readUserInput(question, new String[] { "Y", "y", "N", "n" }, false, null);
		if ("Y".equalsIgnoreCase(input) || "Yes".equalsIgnoreCase(input)) {
			return true;
		}
		return false;
	};

	/**
	 * Gets the tar command.
	 * 
	 * @param tarDirectory
	 *            the tar directory
	 * @param tarFileName
	 *            the tar file name
	 * @param sourceDirectory
	 *            the source directory
	 * @return the tar command
	 */
	public static String getTarCommand(String tarDirectory, String tarFileName, String sourceDirectory) {
		if (tarDirectory.equals(sourceDirectory)) {
			return "touch " + tarDirectory + tarFileName + " ; tar -czpf " + tarDirectory + tarFileName + " --exclude " + tarFileName + " -C "
					+ sourceDirectory + "/ ./";
		} else {
			return " tar -czpf " + tarDirectory + tarFileName + " -C " + sourceDirectory + "/ ./";

		}
	}

	/**
	 * Checks if is valid user input.
	 * 
	 * @param permissibleInputs
	 *            the permissible inputs
	 * @param allowBlank
	 *            the allow blank
	 * @param defaultValue
	 *            the default value
	 * @param userInput
	 *            the user input
	 * @return true, if is valid user input
	 */
	private static boolean isValidUserInput(String[] permissibleInputs, boolean allowBlank, String defaultValue, String userInput) {
		boolean isValid = true;

		if (defaultValue != null) {
			allowBlank = true;
		}
		if (!allowBlank && StringUtils.isBlank(userInput) && defaultValue == null) {
			isValid = false;
		}

		if (permissibleInputs != null && permissibleInputs.length > 0 && StringUtils.isNotBlank(userInput)
				&& !ArrayUtils.contains(permissibleInputs, userInput)) {
			isValid = false;
		}
		return isValid;
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args) {
		log.debug(readUserIdPassword(null, "root"));
	}

	/**
	 * Read password.
	 * 
	 * @param message
	 *            the message
	 * @return the string
	 */
	public static String readPassword(String message) {

		Console c = System.console();
		if (c == null) {
			log.error("No console. Please execute from command line.");
			System.exit(1);
		}
		char[] pwd = c.readPassword(MESSAGE_HIGHLIGHT_PATTERN + message + " : " + MESSAGE_DEFAULT_PATTERN);

		return new String(pwd).trim();
	}

	/**
	 * Read user id password.
	 * 
	 * @param message
	 *            the message
	 * @param defaultId
	 *            the default id
	 * @return the user
	 */
	public static User readUserIdPassword(String message, String defaultId) {
		String userId = null;
		String password = null;
		String privateKeyFile = null;
		if (StringUtils.isBlank(message)) {
			userId = readUserInput("Please enter the user id", null, false, defaultId);
		} else {
			userId = readUserInput(message, null, false, defaultId);
		}
		if (SSHUtils.confirmAction("Do you want to use private key based authentication (assumes passwordless ssh is set up for user " + userId
				+ ". If not, you will need to provide the password for " + userId + ") ?")) {
			privateKeyFile = readUserInput("Specify fully qualified path of private key file for root", null, false, null);
		} else {
			password = readPassword("Enter password for " + userId);
		}
		User user = new User(userId, password, privateKeyFile);
		return user;

	}

	/**
	 * Read user input.
	 * 
	 * @param message
	 *            the message
	 * @param permissibleInputs
	 *            the permissible inputs
	 * @param allowBlank
	 *            the allow blank
	 * @param defaultValue
	 *            the default value
	 * @return the string
	 */
	public static String readUserInput(String message, String[] permissibleInputs, boolean allowBlank, String defaultValue) {
		log.debug("Waiting for user input on " + message);
		long startTime = System.currentTimeMillis();
		System.out.println(MESSAGE_HIGHLIGHT_PATTERN + "\n" + MESSAGE_DEFAULT_PATTERN);

		String input = null;
		Console c = System.console();
		System.out.println("\n");
		if (c == null) {
			log.error("No console. Please execute from command line.");
			System.exit(1);
		}
		if (permissibleInputs != null && permissibleInputs.length > 0) {
			message += " (";
			for (String permissibleInput : permissibleInputs) {
				message += permissibleInput + ", ";
			}
			message = message.substring(0, message.length() - 2) + (")");
		}
		if (defaultValue != null) {
			message += " [" + defaultValue + "]";
		}

		do {
			input = c.readLine(MESSAGE_HIGHLIGHT_PATTERN + message + ": " + MESSAGE_DEFAULT_PATTERN);
		} while (!isValidUserInput(permissibleInputs, allowBlank, defaultValue, input));

		if (defaultValue != null && (StringUtils.isBlank(input))) {
			input = defaultValue;
		}
		long endTime = System.currentTimeMillis();

		log.debug("Received user input. Time taken by user is (hh:mm:ss) " + DurationFormatUtils.formatPeriod(startTime, endTime, "HH:mm:ss"));
		System.out.println("\n");
		return input;
	}

	/**
	 * Substitute user.
	 * 
	 * @param command
	 *            the command
	 * @param adminUser
	 *            the admin user
	 * @param runAsUser
	 *            the run as user
	 * @return the string
	 */
	public static String substituteUser(String command, User adminUser, User runAsUser) {
		if (StringUtils.isNotBlank(command)) {
			String quote = "\"";

			if (command.contains(quote)) {
				quote = "'";
			}

			command = "umask 000; " + command;
			if (runAsUser != null && StringUtils.isNotBlank(runAsUser.getUserId())
					&& !StringUtils.equalsIgnoreCase(adminUser.getUserId(), runAsUser.getUserId())) {
				return "su -l " + runAsUser.getUserId() + " -c " + quote + command + quote;
			}
		}
		return command;
	}

	/**
	 * Instantiates a new sSH utils.
	 */
	private SSHUtils() {
	}
}
