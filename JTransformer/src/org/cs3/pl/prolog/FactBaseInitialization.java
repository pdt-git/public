/*
 * Created on 15.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.prolog;

import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.builders.InitialFactBaseBuilder;
import org.cs3.pl.fileops.MetaDataManager;
import org.cs3.pl.fileops.MetaDataManagerFactory;
import org.cs3.pl.fileops.PrologMetaDataManager;
import org.eclipse.core.runtime.CoreException;

/**
 * @author xproot
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class FactBaseInitialization implements StartupHook {

	private static final String FACTFILEPOSTFIX = "_factbase_initialization.pl";
	private static final String JAVALANGFACTFILE = "javalang" + FACTFILEPOSTFIX;
	private static final String COMPLETEFACTFILE = "complete" + FACTFILEPOSTFIX;

	IPrologClient client;
	PrologMetaDataManager meta;
	
	private static final String[] FQNS = {
		"java.lang.Object",
		"java.lang.Class",
		"java.lang.ClassLoader",
		"java.lang.Compiler",
		"java.lang.String",
		"java.lang.InheritableThreadLocal",
		"java.lang.Integer",
		"java.lang.Long",
		"java.lang.Double",
		"java.lang.Float",
		"java.lang.Number",
		"java.lang.Byte",
		"java.lang.Boolean",
		"java.lang.Character",
		"java.lang.Short",
		"java.lang.StringBuffer",
		"java.lang.Void",
		"java.lang.Thread",
		"java.lang.ThreadLocal",
		"java.lang.ThreadGroup",
		"java.lang.Throwable",
		"java.lang.Package",
		"java.lang.System",
		"java.lang.Math",
		"java.lang.StrictMath",
		"java.lang.Runtime",
		"java.lang.RuntimePermission",
		"java.lang.Process",
		
		"java.lang.CharSequence",
		"java.lang.Runnable",
		"java.lang.Cloneable",
		"java.lang.Comparable",
		
		"java.lang.ArithmeticException",
		"java.lang.ArrayIndexOutOfBoundsException",
		"java.lang.ArrayStoreException",
		"java.lang.ClassCastException",
		"java.lang.ClassNotFoundException",
		"java.lang.CloneNotSupportedException",
		"java.lang.Exception",
		"java.lang.IllegalAccessException",
		"java.lang.IllegalArgumentException",
		"java.lang.IllegalMonitorStateException",
		"java.lang.IllegalStateException",
		"java.lang.IllegalThreadStateException",
		"java.lang.IndexOutOfBoundsException",
		"java.lang.InstantiationException",
		"java.lang.InterruptedException",
		"java.lang.NegativeArraySizeException",
		"java.lang.NoSuchFieldException",
		"java.lang.NoSuchMethodException",
		"java.lang.NullPointerException",
		"java.lang.NumberFormatException",
		"java.lang.RuntimeException",
		"java.lang.SecurityException",
		"java.lang.StringIndexOutOfBoundsException",
		"java.lang.UnsupportedOperationException",
		"java.lang.AbstractMethodError",
		"java.lang.AssertionError",
		"java.lang.ClassCircularityError",
		"java.lang.ClassFormatError",
		"java.lang.Error",
		"java.lang.ExceptionInInitializerError",
		"java.lang.IllegalAccessError",
		"java.lang.IncompatibleClassChangeError",
		"java.lang.InstantiationError",
		"java.lang.InternalError",
		"java.lang.LinkageError",
		"java.lang.NoClassDefFoundError",
		"java.lang.NoSuchFieldError",
		"java.lang.NoSuchMethodError",
		"java.lang.OutOfMemoryError",
		"java.lang.StackOverflowError",
		"java.lang.ThreadDeath",
		"java.lang.UnknownError",
		"java.lang.UnsatisfiedLinkError",
		"java.lang.UnsupportedClassVersionError",
		"java.lang.VerifyError",
		"java.lang.VirtualMachineError"
	};
    private boolean completeFactsLoaded = false;
    private PDTPlugin plugin;

	/**
	 * @param baseClient
	 * @throws IOException
	 */
	public FactBaseInitialization(IPrologClient baseClient) throws IOException {
		this.client = baseClient;
		meta = MetaDataManagerFactory.getPrologMetaDataManager(client,PrologMetaDataManager.EXT);
		plugin =PDTPlugin.getDefault();
	}

	public void onStartup() {
		try {
			String filename = null;
			if (plugin.isCompleteCachingEnabled() && doProjectFactsExist()) {
                filename = PrologHelper.makeFilenameSWIConform(meta.getFullPath(COMPLETEFACTFILE));
                completeFactsLoaded = true;
			} else {
			    if (plugin.isJavaLangCachingEnabled()) {
                    filename = PrologHelper.makeFilenameSWIConform(meta.getFullPath(JAVALANGFACTFILE));
                    if (!meta.exists(JAVALANGFACTFILE)) {
                        //PrologManager manager = PrologManager.getInstance();
                        for (int i = 0; i < FQNS.length; i++) {
                            client.query("new_id(ID), assert(globalIds('" + FQNS[i] + "',ID))");
                        }
                        InitialFactBaseBuilder builder = new InitialFactBaseBuilder(client);
                        builder.initFactbase();
                        client.query("writeTreeFacts('" + filename + "')");
                    }
                }
            }
			// We MUST use query instead of "meta.consult(FACTFILE)" here,
			// since meta.consult uses the hidden client!
			if(filename != null)
			    client.query("consult('" + filename + "')");
			
			deleteProjectFactFile();

		} catch (IOException e) {
			Debug.error(e.getLocalizedMessage());
		} catch (InterruptedException e) {
			Debug.error(e.getLocalizedMessage());
		} catch (CoreException e) {
			Debug.error(e.getLocalizedMessage());
		}
	}

	/**
     * 
     */
    public void deleteProjectFactFile() {
        if (doProjectFactsExist())
            meta.delete(COMPLETEFACTFILE);
    }

    /**
     * @return
     */
    private boolean doProjectFactsExist() {
        return meta.exists(COMPLETEFACTFILE);
    }

    public void onPluginStop() {
        if (plugin.isCompleteCachingEnabled()) {
            String filename = PrologHelper.makeFilenameSWIConform(meta.getFullPath(COMPLETEFACTFILE));
            client.query("writeTreeFacts('" + filename + "')");
        }
	}


    public boolean isCompleteFactbaseLoaded() {
        return completeFactsLoaded;
    }
}
