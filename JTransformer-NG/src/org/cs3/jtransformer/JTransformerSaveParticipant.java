package org.cs3.jtransformer;

import java.io.File;
import java.util.Set;

import org.cs3.jtransformer.internal.natures.JTransformerSubscription;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public class JTransformerSaveParticipant implements ISaveParticipant {

	public void doneSaving(ISaveContext context) {
		// TODO Auto-generated method stub

	}

	public void prepareToSave(ISaveContext context) throws CoreException {
		// TODO Auto-generated method stub

	}

	public void rollback(ISaveContext context) {
		// TODO Auto-generated method stub

	}

	public void saving(ISaveContext context) throws CoreException {
		PrologInterfaceRegistry pir = PrologRuntimePlugin.getDefault()
				.getPrologInterfaceRegistry();

		//	 Alle existierenden Faktenbasis "keys" anfragen:
		Set<String> keys = pir.getAllKeys();
		for (String key : keys) {
			//	 Alle zughörigen Subscriptions anfragen:
			Set<Subscription> subscriptions = pir.getSubscriptionsForPif(key);
			//	JTransformer definiert die Subscrition JTransformerSubscription:
			for (Subscription subscription : subscriptions) {
				if (subscription instanceof JTransformerSubscription) {
					PrologInterface pif = pir.getPrologInterface(key);
					if (pif == null) {
						continue;
					}
					IPath init = JTUtils.getPersistantFactbaseFileForPif(key);
					if (pif.isDown() || init.toFile().isFile()) {
						continue;
					}
					File directory = init.removeLastSegments(1).toFile();
					if (!directory.isDirectory()) {
						directory.mkdirs();
					}
					String fileName = JTUtils.iPathToPrologFilename(init);
					PrologSession session = null;
					try {
						session = pif.getSession();
						//	        	            submon.beginTask("load persistant factbase", 90);
						session.queryOnce("rollback, writeTreeFacts('" + fileName+ "')");
						//	        	            submon.worked(90);
					} catch (Exception ex) {
						JTDebug.report(ex);
					} finally {
						if (session != null) {
							session.dispose();
						}
					}

					// your code goes here: Jetzt hast du alle Projekte und den zugehörigen Key
				}
			}
		}
	}

}
