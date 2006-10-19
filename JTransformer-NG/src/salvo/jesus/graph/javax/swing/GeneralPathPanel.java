package salvo.jesus.graph.javax.swing;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;


/**
 * A <tt>JPanel</tt> that draws each segment of a <tt>GeneralPath</tt> as
 * <tt>GeneralPath</tt>s themselves.
 *
 * @author  Jesus M. Salvo Jr.
 */
class GeneralPathPanel extends JPanel {

    private JScrollPane     scroller;
    private GeneralPath     gPath;

    private Color           outlineColor;
    private Color           selectedColor;
    private Vector          segmentVector = new Vector();

    private ArrayList       generalPathList;

    private int             selectedSegment;
    private boolean         firstPaint = true;

    public static final int        MINIMUM_WIDTH = 320;
    public static final int        MINIMUM_HEIGHT = 120;

    /**
     * Creates an instance of <tt>GeneralPathPanel</tt>.
     *
     * @param   gPath   The <tt>GeneralPath</tt> that we want to get the segments from
     * @param   outlineColor    The <tt>Color</tt> to use to draw the segments.
     */
    public GeneralPathPanel( GeneralPath gPath, Color outlineColor ) {
        super();

        this.gPath = gPath;
        this.setPreferredSize( this.getGeneralPathDimension());
        this.setMinimumSize( new Dimension( MINIMUM_WIDTH, MINIMUM_HEIGHT ));
        this.generalPathList = this.createGeneralPaths( gPath );
        this.setBackground( Color.white );
        this.setVisible( true );

        this.outlineColor = outlineColor;
        this.selectedColor =  outlineColor.darker();
    }

    /**
     * Changes the <tt>GeneralPath</tt> that this panel is drawing and
     * getting segments from. This also changes the preferredSize of the panel.
     */
    public void setGeneralPath( GeneralPath newGPath ) {
        this.gPath = newGPath;
        this.setPreferredSize( this.getGeneralPathDimension());
        this.generalPathList = this.createGeneralPaths( this.gPath );
        this.firstPaint = true;
        this.revalidate();
        this.repaint();
    }

    /**
     * Changes the outline color for drawing the segments of the
     * <tt>GeneralPath</tt>
     */
    public void setOutlineColor( Color newOutlineColor ) {
        this.outlineColor = newOutlineColor;
        this.selectedColor = this.outlineColor.darker();
    }

    /**
     * Set the scrollpane that is viewing this panel. This is used internally
     * to determine the translation of the coordinates for the GeneralPath.
     */
    public void setScrollPane( JScrollPane scroller ) {
        this.scroller = scroller;
    }

    /**
     * Creates an <tt>ArrayList</tt> of <tt>GeneralPath</tt>s from a
     * <tt>GeneralPath</tt>'s segment via a <tt>PathIterator</tt>.
     */
    private ArrayList createGeneralPaths( GeneralPath gPath ) {
        ArrayList   listOfPaths = new ArrayList();
        GeneralPath segmentPath = null;

        PathIterator iterator = gPath.getPathIterator( null );
        float segments[] = new float[6];
        float   x1 = 0, y1 = 0;
        float   firstX = 0, firstY = 0;
        boolean firstPoint = true;

        this.segmentVector = new Vector();

        while( !iterator.isDone() ) {
            int currSegment = iterator.currentSegment( segments );
            if( firstPoint ) {
                firstX = segments[0]; firstY = segments[1];
                firstPoint = false;
            }
            switch ( currSegment ) {
                case  PathIterator.SEG_MOVETO:
                    // Create a GeneralPath consisting only of a single dot
                    segmentPath = new GeneralPath();
                    segmentPath.moveTo( segments[0], segments[1] );
                    segmentVector.add( "SEG_MOVETO" );
                    // End point will be the starting point in next loop
                    x1 = segments[0];
                    y1 = segments[1];
                    break;
                case  PathIterator.SEG_LINETO:
                    // Create a GeneralPath consisting only of a straight line
                    segmentPath = new GeneralPath();
                    segmentPath.moveTo( x1, y1 );
                    segmentPath.lineTo( segments[0], segments[1] );
                    segmentVector.add( "SEG_LINETO" );
                    // End point will be the starting point in next loop
                    x1 = segments[0];
                    y1 = segments[1];
                    break;
                case  PathIterator.SEG_QUADTO:
                    // Create a GeneralPath consisting only of a quadratic curve
                    segmentPath = new GeneralPath();
                    segmentPath.moveTo( x1, y1 );
                    segmentPath.quadTo( segments[0], segments[1],
                        segments[2], segments[3] );
                    segmentVector.add( "SEG_QUADTO" );
                    // End point will be the starting point in next loop
                    x1 = segments[2];
                    y1 = segments[3];
                    break;
                case  PathIterator.SEG_CUBICTO:
                    // Create a GeneralPath consisting only of a Bezier curve
                    segmentPath = new GeneralPath();
                    segmentPath.moveTo( x1, y1 );
                    segmentPath.curveTo( segments[0], segments[1],
                        segments[2], segments[3], segments[4], segments[5] );
                    segmentVector.add( "SEG_CUBICTO" );
                    // End point will be the starting point in next loop
                    x1 = segments[4];
                    y1 = segments[5];
                    break;
                case  PathIterator.SEG_CLOSE:
                    // Create a GeneralPath consisting only of a straight line
                    segmentPath = new GeneralPath();
                    segmentPath.moveTo( x1, y1 );
                    segmentPath.lineTo( firstX, firstY );
                    segmentVector.add( "SEG_CLOSE" );
                    // End point will be the starting point in next loop
                    x1 = firstX;
                    y1 = firstY;
                    break;
            }
            // Add the created GeneralPath to our ArrayList of GeneralPaths.
            listOfPaths.add( segmentPath );
            iterator.next();
        }

        return listOfPaths;
    }

    /**
     * Return a <tt>Vector</tt> of <tt>String</tt>s describing each segment
     * in the order that they are returned via a <tt>PathIterator</tt>
     * of the <tt>GeneralPath</tt>
     */
    public Vector getSegmentVector() {
        return this.segmentVector;
    }

    /**
     * Sets the selected segment
     */
    public void setSelectedSegment( int segmentIndex ) {
        this.selectedSegment = segmentIndex;
    }

    /**
     * Returns the size the GeneralPath will consume
     */
    public Dimension getGeneralPathDimension() {
        Rectangle2D bounds = this.gPath.getBounds2D();
        return new Dimension( (int) bounds.getWidth() + 10, (int) bounds.getHeight() + 10);
    }

    /**
     * Paint each of the segments as separate <tt>GeneralPath</tt>s.
     */
    public void paint( Graphics g ){
        AffineTransform transform = new AffineTransform();
        Rectangle2D bounds = this.gPath.getBounds2D();
        int         width = 0, height = 0;

        if( firstPaint ) {
            // Create a translatation for the the coordinates of the GeneralPath
            // so that most if not all are visible
            double      newX, newY;
            double      xTransform, yTransform;

            JComponent  componentToAlignWith;
            if( this.scroller == null )
                componentToAlignWith = this;
            else
                componentToAlignWith = this.scroller;

            if( componentToAlignWith.getWidth() > bounds.getWidth() )
                newX = ( componentToAlignWith.getWidth() - bounds.getWidth() ) / 2;
            else
                newX = 5;
            xTransform = newX - bounds.getX();

            if ( componentToAlignWith.getHeight() > bounds.getHeight() )
                newY = ( componentToAlignWith.getHeight() - bounds.getHeight() ) / 2;
            else
                newY = 5;
            yTransform = newY - bounds.getY();

            transform.setToTranslation( xTransform, yTransform );
        }

        // Call super.paint(). Otherwise, painting will not work properly.
        super.paint( g );

        Graphics2D g2d = (Graphics2D) g;
        g2d.setColor( this.outlineColor );

        // Paint each segment as a GeneralPath
        GeneralPath segmentPath;
        Stroke      origStroke = g2d.getStroke();
        for( int i = 0; i < this.generalPathList.size(); i++ ) {
            segmentPath = (GeneralPath) this.generalPathList.get( i );
            // Transform the GeneralPath only once
            if( firstPaint )
                segmentPath.transform( transform );
            // If we are paintin the selected "segment",
            // use a different stroke and color
            if( i == this.selectedSegment ) {
                Stroke newStroke = new BasicStroke( (new BasicStroke()).getLineWidth() * 2);
                g2d.setStroke( newStroke );
                g2d.setColor( this.selectedColor );
            }
            // Draw the segment as a GeneralPath
            g2d.draw( segmentPath );
            // If we are paintin the selected "segment",
            // reset the stroke and color
            if( i == this.selectedSegment ) {
                g2d.setStroke( origStroke );
                g2d.setColor( this.outlineColor );
            }
        }

        firstPaint = false;

    }


}