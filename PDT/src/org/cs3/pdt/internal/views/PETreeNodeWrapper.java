/*
 */
package org.cs3.pdt.internal.views;

import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.model.IAtom;
import org.cs3.pl.model.IClause;
import org.cs3.pl.model.IModule;
import org.cs3.pl.model.IPredicate;
import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.IString;
import org.cs3.pl.model.ITerm;
import org.cs3.pl.model.IVariable;
import org.eclipse.swt.graphics.Image;

/**
 */
public class PETreeNodeWrapper implements IPrologElement {
    IPrologElement element;

    IPrologElement parent;

    private Image image;

    private HashMap cache = new HashMap();

    public PETreeNodeWrapper(IPrologElement element, IPrologElement p) {
        super();
        this.element = element;
        this.parent = p;
    }

    public IPrologElement getParent() {
        return parent;
    }

    public IPrologElement[] getChildren() {
        final Vector v = new Vector();
        accept(new IPrologElementVisitor() {
            /*
             * (non-Javadoc)
             * 
             * @see org.cs3.pl.model.IPrologElementVisitor#visit(org.cs3.pl.model.IPrologElement,
             *           java.util.List, java.lang.Object)
             */
            public boolean visit(IPrologElement node, List path, Object role) {
                if (node.equals(PETreeNodeWrapper.this.element)) {
                    return true;
                }
                if (node.equals(parent)) {
                    return false;
                }
                //...otherwise:
                v.add(new PETreeNodeWrapper(node,
                        PETreeNodeWrapper.this.element));
                return false;
            }
        });
        return (IPrologElement[]) v.toArray(new IPrologElement[0]);
    }

    public void accept(IPrologElementVisitor visitor) {
        element.accept(visitor);
    }

    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        element.accept(visitor, path, role);
    }

    public boolean equals(Object obj) {
        return element.equals(obj);
    }

    public String getLabel() {
        return element.getLabel();
    }

    public ISource getSource() {
        return element.getSource();
    }

    public int hashCode() {
        return element.hashCode();
    }

    public boolean isSynthetic() {
        return element.isSynthetic();
    }

    public String toString() {
        return element.toString();
    }

    /**
     * @return
     */
    public boolean hasChildren() {
        return !(element instanceof IAtom || element instanceof IString||element instanceof IVariable );
    }

    /**
     * @return
     */
    public Image getImage() {
        if (image == null) {
            if (element instanceof IModule) {
                image = ImageRepository.getImage(ImageRepository.PE_MODULE);
            } else if (element instanceof IClause) {
                image = ImageRepository.getImage(ImageRepository.PE_CLAUSE);
            }  else if (element instanceof IAtom) {
                image = ImageRepository.getImage(ImageRepository.PE_ATOM);
            }  else if (element instanceof IVariable) {
                image = ImageRepository.getImage(ImageRepository.PE_VARIABLE);
            }else if (element instanceof ITerm) {
                image = ImageRepository.getImage(ImageRepository.PE_TERM);
            } else if (element instanceof IPredicate) {
                image = ImageRepository.getImage(((IPredicate) element)
                        .isExported() ? ImageRepository.PE_PUBLIC
                        : ImageRepository.PE_HIDDEN);
            }
        }
        return image;
    }
}
