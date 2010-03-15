package org.cs3.pdt.internal.views;

import java.io.File;

import junit.framework.TestCase;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.FileSearchPathConfigurator;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;

public class CTermContentProviderTest extends TestCase {
	final static String testdata = "testdata/testdata_00.pl";

	private File file;

	private PrologInterface pif;

	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);

		pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface("test");
		PrologSession s = pif.getSession();
		PrologRuntimeUIPlugin.getDefault().ensureInstalled(testdata, CTermContentProviderTest.class);
		file = PrologRuntimeUIPlugin.getDefault().getResourceLocator().resolve(testdata);
		
		PrologLibraryManager mgr = PrologRuntimeUIPlugin.getDefault().getLibraryManager();
		FileSearchPathConfigurator.configureFileSearchPath(mgr, s, new String[] { PDTCore.ENGINE_ID });
		s.queryOnce("use_module(library('/org/cs3/pdt/annotate/pdt_annotator')),"
						+ "use_module(library('/org/cs3/pdt/core/pdt_meta_info')),"
						+ "use_module(library('/org/cs3/pdt/model/predicate_definition_factory')),"
						+ "use_module(library('/org/cs3/pdt/model/builtin_predicate_factory')),"
						+ "use_module(library('/org/cs3/pdt/model/pdt_index')),"
						+ "use_module(library('/org/cs3/pdt/model/pdt_handle'))");
		s.queryOnce("register_annotator(library('/org/cs3/pdt/annotate/op_annotator')),"
						+ "register_annotator(library('/org/cs3/pdt/annotate/fileref_annotator')),"
						+ "register_annotator(library('/org/cs3/pdt/annotate/export_annotator')),"
						+ "register_annotator(library('/org/cs3/pdt/annotate/member_annotator')),"
						+ "register_annotator(library('/org/cs3/pdt/annotate/indexer'))");
		s.queryOnce("win_window_pos([show(true)])");
		s.dispose();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testMalte() throws Exception {
		PrologFileContentModel backend = new ContentModel() {

			public File getFile() {
				return (File) getInput();
			}

		};
		backend.setInput(file);
		backend.setPif(pif,null);
		PrologSession s = pif.getSession();
		s.queryOnce("pdt_ensure_annotated('" + Util.prologFileName(file)
						+ "')");
	}
}
