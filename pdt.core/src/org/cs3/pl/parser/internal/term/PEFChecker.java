/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pl.parser.internal.term;

import java.util.HashMap;

import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;

public class PEFChecker extends DefaultPrologTermParserVisitor {
	private final ProblemCollector problemCollecter;
	private HashMap pefs= new HashMap();

	public PEFChecker(ProblemCollector problemCollecter) {
		this.problemCollecter = problemCollecter;
		initPEFs();
	}
	
	public Object visit(ASTCompilationUnit node, Object data) {
		SimpleNode canonical = node.toCanonicalTerm(true,true);
		return super.visit(canonical, canonical);
	}
	
	public Object visit(ASTCompoundTerm node, Object data) {
		String label = node.getLabel();
		if(pefs.containsKey(label)){
			Integer expected = (Integer) pefs.get(label);
			int arity = node.getOriginal().getArity();
			if(arity!=expected.intValue()){
				Problem p = TermParserUtils.createProblem(node.getOriginal(),"Expected arity for '" + label
	                    + "' is " + expected.intValue() + ", arity found: " + arity
	                    + ".",Problem.WARNING);
				problemCollecter.reportProblem(p);
			}
		}
		return super.visit(node, data);
	}
	
	
	private void initPEFs() {
        pefs.put("fieldDefT", new Integer(5));
        pefs.put("paramDefT", new Integer(4));
        pefs.put("localDefT", new Integer(6));
        pefs.put("methodDefT", new Integer(7));
        pefs.put("classDefT", new Integer(4));
        pefs.put("getFieldT", new Integer(6));
        pefs.put("identT", new Integer(5));
        pefs.put("literalT", new Integer(5));
        pefs.put("execT", new Integer(4));
        pefs.put("operationT", new Integer(6));
        pefs.put("applyT", new Integer(7));
        pefs.put("blockT", new Integer(4));
        pefs.put("selectT", new Integer(6));
        pefs.put("conditionalT", new Integer(6));
        pefs.put("ifT", new Integer(6));
        pefs.put("assignT", new Integer(5));
        pefs.put("importT", new Integer(3));
        pefs.put("newArrayT", new Integer(6));
        pefs.put("toplevelT", new Integer(4));
        pefs.put("newClassT", new Integer(8));
        pefs.put("returnT", new Integer(4));
        pefs.put("switchT", new Integer(5));
        pefs.put("typeCastT", new Integer(5));
        pefs.put("tryT", new Integer(6));
        pefs.put("whileLoopT", new Integer(5));
        pefs.put("continueT", new Integer(5));
        pefs.put("doLoopT", new Integer(5));
        pefs.put("indexedT", new Integer(5));
        pefs.put("throwT", new Integer(4));
        pefs.put("forLoopT", new Integer(7));
        pefs.put("synchronizedT", new Integer(5));
        pefs.put("labelT", new Integer(5));
        pefs.put("breakT", new Integer(5));
        pefs.put("typeTestT", new Integer(5));
        pefs.put("assignopT", new Integer(6));
        pefs.put("caseT", new Integer(4));
        pefs.put("catchT", new Integer(5));
        pefs.put("assertT", new Integer(5));
        pefs.put("modifierT", new Integer(2));
        pefs.put("externT", new Integer(1));
        pefs.put("interfaceT", new Integer(1));
        pefs.put("lastID", new Integer(1));
        pefs.put("packageT", new Integer(2));
        pefs.put("implementsT", new Integer(2));
        pefs.put("extendsT", new Integer(2));
        pefs.put("precedenceT", new Integer(4));
        pefs.put("nopT", new Integer(3));
    }
}
