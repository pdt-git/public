package salvo.jesus.graph.java.awt.geom;

import java.awt.geom.*;
import java.io.*;

/**
 * An implementation of PathIterator that is also Serializable
 * to workaround the fact that all of the classes in java.awt.geom
 * are non-serliaizable, and GeneralPath is declared final.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class SerializablePathIterator implements PathIterator, Serializable {

    /**
     * Reference to the PathIterator delegate
     */
    private transient PathIterator    pIterator;

    /**
     * Indicate the end of the path
     */
    private static final int    SEG_END = -1;

    /**
     * Creates an instance of a SerializablePathIterator that wraps
     * around a PathItereator.
     */
    public SerializablePathIterator( PathIterator pIterator ) {
        this.pIterator = pIterator;
    }

    /**
     * Delegates call to PathIterator.getWindingRule()
     */
    public int getWindingRule() {
        return this.pIterator.getWindingRule();
    }

    /**
     * Delegates call to PathIterator.isDone()
     */
    public boolean isDone() {
        return this.pIterator.isDone();
    }

    /**
     * Delegates call to PathIterator.next()
     */
    public void next() {
        this.pIterator.next();
    }

    /**
     * Delegates call to PathIterator.currentSegment()
     */
    public int currentSegment(float[] segment ) {
        return this.pIterator.currentSegment( segment );
    }

    /**
     * Delegates call to PathIterator.currentSegment()
     */
    public int currentSegment(double[] segment) {
        return this.pIterator.currentSegment( segment );
    }

    /**
     * Writes out the segments of the path of the PathIterator.
     */
    private void writeObject( ObjectOutputStream out ) throws IOException {
        SerializablePathIterator    iterator = new SerializablePathIterator( this.pIterator );
        float  segment[] = new float[6];
        int     segmentType;

        // Call default even if there are no serializable fields
        out.defaultWriteObject();

        // Write out the winding rule
        out.writeInt( iterator.getWindingRule() );

        // Now write out the segments of the path
        while( !iterator.isDone() ) {

            // Write out the result of calling currentSegment()
            segmentType = iterator.currentSegment( segment );
            out.writeInt( segmentType );

            // Write out the segment itself
            switch( segmentType ) {
            // One point
            case SEG_MOVETO:
                out.writeFloat( segment[0] );
                out.writeFloat( segment[1] );
                break;
            case SEG_LINETO:
                out.writeFloat( segment[0] );
                out.writeFloat( segment[1] );
                break;
            // Two points
            case SEG_QUADTO:
                out.writeFloat( segment[0] );
                out.writeFloat( segment[1] );
                out.writeFloat( segment[2] );
                out.writeFloat( segment[3] );
                break;
            // Three points
            case SEG_CUBICTO:
                out.writeFloat( segment[0] );
                out.writeFloat( segment[1] );
                out.writeFloat( segment[2] );
                out.writeFloat( segment[3] );
                out.writeFloat( segment[4] );
                out.writeFloat( segment[5] );
                break;
            case SEG_CLOSE:
                break;
            }

            iterator.next();
        }

        // Write out that we have reached the end of the path
        // SEG_CLOSE may not be the end of a path, so we write our own
        // indicator
        out.writeInt( SEG_END );
    }


    /**
     * Reads in the segments of the path of the PathIterator
     */
    private void readObject( ObjectInputStream in )
            throws IOException, ClassNotFoundException
    {
        GeneralPath gPath = new GeneralPath();
        int     segmentType;

        // Call default first even if there are no serializable fields
        in.defaultReadObject();

        // Get the winding rule
        gPath.setWindingRule( in.readInt());

        // Read the segment Type
        segmentType = in.readInt();

        // Get the segments of the path
        while( segmentType != SEG_END ) {
            // Read the segment itself
            switch( segmentType ) {
            // One point
            case SEG_MOVETO:
                gPath.moveTo( in.readFloat(), in.readFloat() );
                break;
            // One point
            case SEG_LINETO:
                gPath.lineTo( in.readFloat(), in.readFloat() );
                break;
            // Two points
            case SEG_QUADTO:
                gPath.quadTo(
                    in.readFloat(), in.readFloat(),
                    in.readFloat(), in.readFloat() );
                break;
            // Three points
            case SEG_CUBICTO:
                gPath.curveTo(
                    in.readFloat(), in.readFloat(),
                    in.readFloat(), in.readFloat(),
                    in.readFloat(), in.readFloat() );
                break;
            case SEG_CLOSE:
                break;
            }

            // Read the next segment Type
            segmentType = in.readInt();
        }


        // Finally, assign the complete path to the instance variable
        this.pIterator = gPath.getPathIterator( new AffineTransform() );
    }
}
