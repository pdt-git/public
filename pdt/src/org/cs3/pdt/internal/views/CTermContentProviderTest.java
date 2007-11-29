package org.cs3.pdt.internal.views;

import java.io.File;
import java.util.Map;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;

import junit.framework.TestCase;

public class CTermContentProviderTest extends TestCase {
	final static String testdata = "testdata/testdata_00.pl";

	private File file;

	private PrologInterface pif;

	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);

		pif = PrologRuntimePlugin.getDefault().getPrologInterface("test");
		PrologInterfaceFactory factory = pif.getFactory();
		PrologSession s = pif.getSession();
		factory.ensureInstalled(testdata, CTermContentProviderTest.class);
		file = factory.getResourceLocator().resolve(testdata);
		PrologLibraryManager mgr = PrologRuntimePlugin.getDefault()
				.getLibraryManager();
		PLUtil.configureFileSearchPath(mgr, s,
				new String[] { PDTCore.ENGINE_ID });
		s
				.queryOnce("use_module(library('/org/cs3/pdt/annotate/pdt_annotator')),"
						+ "use_module(library('/org/cs3/pdt/core/pdt_meta_info')),"
						+ "use_module(library('/org/cs3/pdt/model/predicate_definition_factory')),"
						+ "use_module(library('/org/cs3/pdt/model/builtin_predicate_factory')),"
						+ "use_module(library('/org/cs3/pdt/model/pdt_index')),"
						+ "use_module(library('/org/cs3/pdt/model/pdt_handle'))");
		s
				.queryOnce("register_annotator(library('/org/cs3/pdt/annotate/op_annotator')),"
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
		s
				.queryOnce("pdt_ensure_annotated('" + Util.prologFileName(file)
						+ "')");

		// FIXME
		/*
		 * Object[] data = backend.getData(); assertNotNull(data);
		 * assertTrue(data.length>0);
		 */
	}
}
