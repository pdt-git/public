/*
 */
package org.cs3.pdt;

import java.util.List;

/**
 * describes a contribution to the set of bootstrap files consulted during the
 * start of PrologInterface objects created by the PDT.
 */
public interface BootstrapContribution {

    /**
     * Contribute to the list of bootstrap files.
     * 
     * @param bootstrapList
     *                    the List of bootstrap files
     * @param key
     *                    the key for which the prolog interface was requested. Typicaly
     *                    a project name or null.
     * @return true if the bootstrap list was modified, false otherwise.
     */
    public boolean contributeToBootstrapList(String key, List bootstrapList);
}
