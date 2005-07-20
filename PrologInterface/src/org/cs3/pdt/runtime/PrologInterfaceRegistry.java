package org.cs3.pdt.runtime;

import java.util.Map;

import org.cs3.pl.prolog.PrologInterface;

/**
 * manages a set of prolog interface instances. <br>
 * This class manages a relation consisting of tupels that are of the form
 * (PrologInterface pif,Object client,String use)
 * 
 * @author lukas
 */
public interface PrologInterfaceRegistry {
	/**
	 * register a client with a given prolog interface. If the specified
	 * (pif,client,use)-tupel is already registered, no action is taken.
	 * 
	 * @param pif
	 *            The prolog interface to register with. If it is not managed by
	 *            this registry, it will be registered first. May not be null-
	 *            Be carefull not to register a prolog interface with more than
	 *            one registry. This is not checked ATM.
	 * @param client
	 *            The client that should be associated with this prolog
	 *            interface. Can be an arbitrary object or even null, but
	 *            remember that the (pif,client,use) tuple has to be unique.
	 * @param use
	 *            A short string key describing the use the client makes of the
	 *            pif. Can be empty or even null,but remember that the
	 *            (pif,client,use) tuple has to be unique.
	 */
	public void register(PrologInterface pif, Object client, String use);

	/**
	 * unregister a tupel. If the given tupel is not registered with registry,
	 * no action is taken.
	 * 
	 * @param pif
	 * @param client
	 * @param use
	 */
	public void unregister(PrologInterface pif, Object client, String use);

	/**
	 * Retrieve all managed PrologInterfaces
	 * 
	 * @return An array containing all PrologInterfaces managed by the registry.
	 *         This includes those that currently do not have any assoziated
	 *         clients.
	 */
	public PrologInterface[] getPrologInterfaces();

	
	
	/**
	 * retrieve all client objects associated to a given PIF
	 * @param pif
	 * @return A Map containing the use strings as keys as and  java.util.Set instances
	 * containing the client objects that are associated with the PIF using the  
	 * respective key.
	 */
	public Map getClients(PrologInterface pif);
	
}
