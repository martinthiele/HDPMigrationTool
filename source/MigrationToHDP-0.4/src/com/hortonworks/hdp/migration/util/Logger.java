/*
 * 
 */
package com.hortonworks.hdp.migration.util;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

// TODO: Auto-generated Javadoc
/**
 * The Class Logger.
 */
public class Logger {

	/** The Constant MESSAGE_DEFAULT_PATTERN. */
	private static final String MESSAGE_DEFAULT_PATTERN = "\u001B[0m";

	/** The Constant MESSAGE_ERROR_PATTERN. */
	private static final String MESSAGE_ERROR_PATTERN = "\u001B[1;31mERROR : ";

	/** The Constant MESSAGE_INFO_PATTERN. */
	private static final String MESSAGE_INFO_PATTERN = "\u001B[0;34mINFO : ";

	/** The Constant MESSAGE_WARN_PATTERN. */
	private static final String MESSAGE_WARN_PATTERN = "\u001B[1;35mWARN : ";

	/** The log. */
	Log log = null;

	/**
	 * Instantiates a new logger.
	 */
	public Logger() {
		log = LogFactory.getLog(new Throwable().getStackTrace()[1].getClassName());
	}

	/**
	 * Debug.
	 * 
	 * @param message
	 *            the message
	 */
	public void debug(Object message) {
		log.debug(getMessageQualifiers() + message);
	}

	/**
	 * Debug.
	 * 
	 * @param message
	 *            the message
	 * @param throwable
	 *            the throwable
	 */
	public void debug(Object message, Throwable throwable) {
		log.debug(getMessageQualifiers() + message, throwable);
	}

	/**
	 * Error.
	 * 
	 * @param message
	 *            the message
	 */
	public void error(Object message) {
		print(message, null, MESSAGE_ERROR_PATTERN);
		log.error(getMessageQualifiers() + message);
	}

	/**
	 * Error.
	 * 
	 * @param message
	 *            the message
	 * @param throwable
	 *            the throwable
	 */
	public void error(Object message, Throwable throwable) {
		print(message, throwable, MESSAGE_ERROR_PATTERN);
		log.error(getMessageQualifiers() + message, throwable);
	}

	/**
	 * Gets the message qualifiers.
	 * 
	 * @return the message qualifiers
	 */
	private String getMessageQualifiers() {
		Throwable t = new Throwable();
		return t.getStackTrace()[2].getClassName().substring(t.getStackTrace()[2].getClassName().lastIndexOf('.') + 1) + "::"
				+ t.getStackTrace()[2].getMethodName() + ":" + t.getStackTrace()[2].getLineNumber() + " - ";
	}

	/**
	 * Info.
	 * 
	 * @param message
	 *            the message
	 */
	public void info(Object message)

	{
		print(message, null, MESSAGE_INFO_PATTERN);
		log.info(getMessageQualifiers() + message);
	}

	/**
	 * Info.
	 * 
	 * @param message
	 *            the message
	 * @param throwable
	 *            the throwable
	 */
	public void info(Object message, Throwable throwable) {
		print(message, null, MESSAGE_INFO_PATTERN);

		log.info(getMessageQualifiers() + message, throwable);
	}

	/**
	 * Prints the.
	 * 
	 * @param obj
	 *            the obj
	 * @param t
	 *            the t
	 * @param messagePattern
	 *            the message pattern
	 */
	private void print(Object obj, Throwable t, String messagePattern) {
		System.out.println(messagePattern + obj + MESSAGE_DEFAULT_PATTERN);
		if (t != null) {
			System.out.println(messagePattern + ExceptionUtils.getStackTrace(t) + MESSAGE_DEFAULT_PATTERN);
		}
	}

	/**
	 * Trace.
	 * 
	 * @param message
	 *            the message
	 */
	public void trace(Object message) {
		log.trace(getMessageQualifiers() + message);
	}

	/**
	 * Trace.
	 * 
	 * @param message
	 *            the message
	 * @param throwable
	 *            the throwable
	 */
	public void trace(Object message, Throwable throwable) {
		log.trace(getMessageQualifiers() + message, throwable);
	}

	/**
	 * Warn.
	 * 
	 * @param message
	 *            the message
	 */
	public void warn(Object message) {
		print(message, null, MESSAGE_WARN_PATTERN);
		log.warn(getMessageQualifiers() + message);
	}

	/**
	 * Warn.
	 * 
	 * @param message
	 *            the message
	 * @param throwable
	 *            the throwable
	 */
	public void warn(Object message, Throwable throwable) {
		print(message, throwable, MESSAGE_WARN_PATTERN);
		log.warn(getMessageQualifiers() + message, throwable);
	}
}
