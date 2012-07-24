/* $LICENSE_MSG$(tr) */

package pdt.y.focusview;

import java.util.ArrayList;
import java.util.List;

import org.cs3.prolog.connector.DefaultSubscription;
import org.cs3.prolog.lifecycle.LifeCycleHook;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

import pdt.y.main.PluginActivator;


public class FocusViewSubscription extends DefaultSubscription implements
			LifeCycleHook {

		private static final String YWORKS_BOOTSTRAPCONTRIBUTION_KEY = "yworks.contribution.key";
		private static List<String> demandedBootstrapContributionsKeys = new ArrayList<String>();
		
		static {
			demandedBootstrapContributionsKeys.add(YWORKS_BOOTSTRAPCONTRIBUTION_KEY);
		}
		
		private FocusViewSubscription(String id, String pifID,
				String descritpion, String name) {
			super(id, pifID, descritpion, name, PluginActivator.PLUGIN_ID, true);
			setPersistent(false);

		}

		/**
		 * Use this method to instantiate a YWorksSubscription. 
		 * @param project
		 * @param pifID
		 * @return
		 */
		public static FocusViewSubscription newInstance(String pifID) {
			FocusViewSubscription subscription = new FocusViewSubscription(
					"FocusViewSubscription",
					pifID,
					"Subscription of FocusView", 
					"FocusView"
					);
			return subscription;
		}

		
		@Override
		public List<String> getBootstrapConstributionKeys() {
			return demandedBootstrapContributionsKeys;
		}

		@Override
		public void lateInit(PrologInterface pif) {
		}

		@Override
		public void onError(PrologInterface pif) {
		}

		@Override
		public void setData(Object data) {
		}

		@Override
		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
		}

		@Override
		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologInterfaceException {
		}

		@Override
		public void onInit(PrologInterface pif, PrologSession initSession)
				throws PrologInterfaceException {
		}
		
		@Override
		public boolean isVisible() {
			return false;
		}
	}