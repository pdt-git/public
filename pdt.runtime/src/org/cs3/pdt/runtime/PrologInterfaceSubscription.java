package org.cs3.pdt.runtime;


/**
 * Simple data structure for storing information on a particular use of 
 * a PrologInterface instance.
 * 
 * Currently, only the pif's registry key and an informal description 
 * is stored.  
 * 
 * If the need should arise, we may add arbitrary other "tags". 
 *
 */
public class PrologInterfaceSubscription {
	/**
	 * A symbolic identifier for the PrologInterface that is to be used.
	 * The PDTRuntime guarantees to return the same PrologInterface instance
	 * for any subscription that uses this key.   
	 */
	public String key;
	
	/**
	 * A short, human readable description of what the pif is used for.
	 * This will be displayed in tooltips or info dialogs by the ui, e.g. when the user manualy switches
	 * to another pif in the console. It should help the user to 
	 * distinguish between all the different pifs that may be active at the 
	 * same time. 
	 */
	public String descritpion;
	
	
	
	public PrologInterfaceSubscription(String pifID,  String descritpion) {
		this.key = pifID;
	
		this.descritpion = descritpion;
	}
	
}
