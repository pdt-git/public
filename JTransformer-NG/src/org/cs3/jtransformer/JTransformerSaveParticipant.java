package org.cs3.jtransformer;

import java.io.File;
import java.util.Set;

import javax.swing.Icon;

import org.cs3.jtransformer.internal.natures.JTransformerSubscription;
import org.cs3.jtransformer.internal.natures.TimeMeasurement;
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
import org.eclipse.jdt.internal.corext.refactoring.typeconstraints.IContext;

public class JTransformerSaveParticipant implements ISaveParticipant {

	public void doneSaving(ISaveContext context) {
	}

	public void prepareToSave(ISaveContext context) throws CoreException {
	}

	public void rollback(ISaveContext context) {
	}

	public void saving(ISaveContext context) throws CoreException {
		if(context.getKind() == ISaveContext.FULL_SAVE) {
		JTDebug.info("full save triggered, saving all open factbases have not been made persistent, yet.");
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
					
					if (pif.isDown() || JTUtils.persistentFactbaseFileExistsForPifKey(key)) {
						continue;
					}
					IPath init = JTUtils.getPersistentFactbaseFileForPif(key);
					File directory = init.removeLastSegments(1).toFile();
					if (!directory.isDirectory()) {
						directory.mkdirs();
					}
					String fileName = JTUtils.iPathToPrologFilename(init);
					PrologSession session = null;
					try {
						TimeMeasurement time = new TimeMeasurement("Save factbase " + fileName,JTDebug.LEVEL_INFO);

						session = pif.getSession();
						//	        	            submon.beginTask("load persistent factbase", 90);
						session.queryOnce(
								JTPrologFacade.ROLLBACK + ","+
								JTPrologFacade.WRITE_TREE_FACTS +"('" + fileName+ "')");
						
						time.logTimeDiff();

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

}
