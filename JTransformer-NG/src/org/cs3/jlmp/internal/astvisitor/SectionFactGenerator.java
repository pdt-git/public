package org.cs3.jlmp.internal.astvisitor;

import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jface.text.ITextSelection;

/**
 * Provides FactGeneratro functionality for source Ranges,
 * that is, parts of an actual CompilationUnit. This is useful
 * for creating Prolog facts for source snippets. 
 * 
 * @author xproot
 * @inheritDoc
 */
public class SectionFactGenerator extends FactGenerator {

		private int start;
		private int end;

	/**
	 * create a new SectionFactGenerator, using the default
	 * FQN handling.
	 * 
	 * @param icu an ICompilationUnit Object
	 * @param name the name of the source file
	 * @param resolver an IIDResolver
	 * @param typeResolver an ITypeResolver
	 * @param writer A PrologWriter instance
	 * @param selection the selected Text.
	 */
	public SectionFactGenerator(ICompilationUnit icu, String name, IIDResolver resolver, ITypeResolver typeResolver, IPrologWriter writer,ITextSelection selection) {
		super(icu, name, resolver, typeResolver, writer, new IdentityFQNTranslator());
		start = selection.getOffset();
		end = start + selection.getLength();
	}
	
	/**
	 * create a new SectionFactGenerator, using the specified
	 * 
	 * @param icu an ICompilationUnit Object
	 * @param name the name of the source file
	 * @param resolver an IIDResolver
	 * @param typeResolver an ITypeResolver
	 * @param writer A PrologWriter instance
	 * @param fqn An FQNTranslator
	 * @param selection the selected Text.
	 */
	
	public SectionFactGenerator(ICompilationUnit icu, String name, IIDResolver resolver, ITypeResolver typeResolver, IPrologWriter writer, FQNTranslator fqn, ITextSelection selection) {
		super(icu, name, resolver, typeResolver, writer, fqn);
		start = selection.getOffset();
		end = start + selection.getLength();
	}
	
	private boolean inRange(ASTNode node) {
		if(node == null)
			return false;
		int nodeEnd = node.getStartPosition() +node.getLength();
		if(start <= node.getStartPosition() && end >= nodeEnd)
			return true;
		return false;
	}
	
	public boolean visit(AnonymousClassDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(ArrayAccess node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(ArrayCreation node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(ArrayInitializer node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(ArrayType node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(AssertStatement node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(Assignment node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(Block node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(BlockComment node) {
		if (inRange(node))
			return super.visit(node);
		else 
			return true;
	}
	
	public boolean visit(BooleanLiteral node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	
	public boolean visit(BreakStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(CastExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(CatchClause node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(CharacterLiteral node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ClassInstanceCreation node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(CompilationUnit node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ConditionalExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ConstructorInvocation node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ContinueStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(DoStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(EmptyStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ExpressionStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(FieldAccess node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(FieldDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ForStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(IfStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ImportDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(InfixExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(InstanceofExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(Initializer node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(LabeledStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(LineComment node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(MemberRef node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(MethodRef node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(MethodRefParameter node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(MethodDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(MethodInvocation node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(NullLiteral node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(NumberLiteral node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(PackageDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ParenthesizedExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(PostfixExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(PrefixExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(PrimitiveType node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(QualifiedName node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ReturnStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SimpleName node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SimpleType node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(StringLiteral node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SuperConstructorInvocation node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SuperFieldAccess node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SuperMethodInvocation node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SwitchCase node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SwitchStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SynchronizedStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(TagElement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(TextElement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ThisExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(ThrowStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(TryStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(TypeDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(TypeDeclarationStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(TypeLiteral node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(SingleVariableDeclaration node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(VariableDeclarationExpression node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(VariableDeclarationStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(VariableDeclarationFragment node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public boolean visit(WhileStatement node) {
		if (inRange(node))
			return super.visit(node);
		return true;
	}
	public void endVisit(CompilationUnit cu) {
	}
	public void endVisit(TypeDeclaration t) {
		if (inRange(t))
			writer.reduceIndention();
	}
	public void endVisit(Block b) {
		if (inRange(b))
			writer.reduceIndention();
	}

}
