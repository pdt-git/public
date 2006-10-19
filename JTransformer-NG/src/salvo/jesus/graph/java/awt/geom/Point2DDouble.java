package salvo.jesus.graph.java.awt.geom;

import java.awt.geom.*;
import java.io.*;

/**
 * A <tt>java.awt.geom.Point2D.Double</tt> that is serializable.
 * There are no additional methods nor are there overriden methods in this class.
 * The only thing added is the <tt>Serializable</tt> interface.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class Point2DDouble extends Point2D.Double implements Serializable {

    public Point2DDouble() {
        super();
    }

    public Point2DDouble( double x, double y ) {
        super( x, y );
    }

}