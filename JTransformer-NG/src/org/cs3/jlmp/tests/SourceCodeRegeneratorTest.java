/*
 * Created on 11.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.jlmp.tests;

import java.io.IOException;

import junit.framework.TestCase;

import org.cs3.jlmp.regenerator.IAffectedFile;
import org.cs3.jlmp.regenerator.ISourceRegenerator;
import org.cs3.jlmp.regenerator.SourceCodeRegenerator;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 * @author windeln
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class SourceCodeRegeneratorTest extends FactGenerationTest {

	/**
     * @param name
     */
    public SourceCodeRegeneratorTest(String name) {
        super(name);        
    }

    /**
	 * @throws IOException
	 * 
	 */
	public void testRegeneration() throws IOException {
		PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		try{
	    String pred ="assert(projectLocationT(tlid,'project','')),"+
		
		"assert(toplevelT(t3,null,filename3,[class3])),"+
		"assert(modifierT(class3,public)),"+
		"assert(classDefT(class3,null,cname3,[method4])),"+
	    "assert(methodDefT(method4,class2,name4,[],type(basic,int,0),[],null)),"+
		"action(delete(class(class3,null,cname3))),"+
		
		
			"action(add(modifierT(class,public)))," +
				"action(add(class(class,null,cname))),"+
	    "action(add(method(method1,class,name1,[],type(basic,int,0),[],null))),"+
	    "action(add(method(method2,class,name2,[],type(basic,int,0),[],null))),"+
//	    "action(add(dirty_tree(method1))),"+
//	    "action(add(dirty_tree(method2))),"+

		"assert(toplevelT(tl,null,filename2,[class2])),"+
		"assert(classDefT(class2,null,cname2,[])),"+
	    "action(add(method(method3,class2,name3,[],type(basic,int,0),[],null))).";

		
//	    "action(add(dirty_tree(method3)))";
		
		session.queryOnce(pred);
		//session.queryOnce("prolog_server(1080,[])");
		
		ISourceRegenerator regen = getTestJLMPProject().getSourceRegenerator();
		IAffectedFile[] files= regen.getAffectedFiles();
		assertEquals(3,files.length);
		assertEquals(IAffectedFile.REMOVED,files[0].getStatus());
		assertEquals(IAffectedFile.CREATED,files[1].getStatus());
		assertEquals(IAffectedFile.CHANGED,files[2].getStatus());
		
		session.queryOnce("rollback");
		IAffectedFile[] files2= regen.getAffectedFiles();
		assertEquals(3,files2.length);
		}
		finally{
		    session.dispose();
		}
				
	}
	
}
