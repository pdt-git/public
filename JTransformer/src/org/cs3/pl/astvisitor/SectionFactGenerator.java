package org.cs3.pl.astvisitor;

import org.cs3.pl.fileops.IPrologWriter;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.*;
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
