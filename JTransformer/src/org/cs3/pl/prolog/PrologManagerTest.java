/*
 * Created on 11.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.prolog;

import java.io.IOException;

import org.cs3.pl.regenerator.IAffectedFile;
import org.cs3.pl.regenerator.ISourceRegenerator;
import org.cs3.pl.regenerator.SourceCodeRegenerator;

import junit.framework.TestCase;

/**
 * @author windeln
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class PrologManagerTest extends TestCase {

	/**
	 * @throws IOException
	 * 
	 */
	
	public void testRegeneration() throws IOException {
		PrologManager manager = PrologManager.getInstance();
		CacheOutputPrologListener cache = new CacheOutputPrologListener();
		manager.addPrologListener(cache);
		IPrologClient client = manager.getHiddenClient();
		String pred ="assert(projectLocationT(tlid,'project','')),"+
			"action(add(modifierT(class,public)))," +
				"action(add(class(class,null,cname))),"+
	    "action(add(method(method1,class,name1,[],type(basic,int,0),[],null))),"+
	    "action(add(method(method2,class,name2,[],type(basic,int,0),[],null))),"+
//	    "action(add(dirty_tree(method1))),"+
//	    "action(add(dirty_tree(method2))),"+

		"assert(toplevel(tl,null,filename,[class2])),"+
		"assert(class(class2,null,cname2,[method3])),"+
	    "action(add(method(method3,class2,name3,[],type(basic,int,0),[],null)))";
//	    "action(add(dirty_tree(method3)))";
		
		client.query(pred);
		client.query("prolog_server(1080,[])");
		
		ISourceRegenerator regen = new SourceCodeRegenerator();
		IAffectedFile[] files= regen.getAffectedFiles();
		assertEquals(2,files.length);
		manager.halt();
	}
}
