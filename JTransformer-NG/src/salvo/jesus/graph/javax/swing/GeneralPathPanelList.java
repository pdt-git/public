package salvo.jesus.graph.javax.swing;

import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import java.awt.Color;
import java.awt.geom.GeneralPath;
import java.awt.Dimension;
import java.awt.BorderLayout;

/**
 * A <tt>JPanel</tt> that contains a <tt>JSplitPane</tt> which in turn
 * splits a <tt>GeneralPathPanel</tT> to the right and a
 * <tt>JList</tt> to the left. The <tt>JList</tt> contains a descriptive text
 * of each of the segments of the <tt>GeneralPath<tt> being represented
 * in the <tt>GeneralPathPanel</tt>.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class GeneralPathPanelList extends JPanel implements ListSelectionListener {

    private GeneralPathPanel    pathPanel;
    private JScrollPane         pathPanelScroller;

    private JList               segmentList;
    private JScrollPane         listScroller;

    private JSplitPane          splitter;

    public GeneralPathPanelList( GeneralPath gPath, Color outlineColor ) {
        super();

        this.setLayout( new BorderLayout());

        // Initialise the GeneralPathPanel
        this.pathPanel = new GeneralPathPanel( gPath, outlineColor );
        this.pathPanelScroller = new JScrollPane( this.pathPanel );
        JViewport viewport = new JViewport( );
        viewport.setView( this.pathPanel );
        this.pathPanelScroller.setViewport( viewport );
        this.pathPanelScroller.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
        this.pathPanelScroller.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
        this.pathPanelScroller.setMinimumSize( new Dimension(
            GeneralPathPanel.MINIMUM_WIDTH, GeneralPathPanel.MINIMUM_HEIGHT ));
        this.pathPanel.setScrollPane( this.pathPanelScroller );

        // Initialise the JList, and add this class as a listener
        // to listen for selection changes
        this.segmentList = new JList( this.pathPanel.getSegmentVector() );
        this.segmentList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
        this.segmentList.addListSelectionListener( this );
        this.listScroller = new JScrollPane( this.segmentList );
        this.listScroller.setPreferredSize( new Dimension(
            130, GeneralPathPanel.MINIMUM_HEIGHT ));

        // Initialise the JSplitPane
        this.splitter = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT,
            this.listScroller, this.pathPanelScroller );
        this.splitter.setOneTouchExpandable( true );
        this.splitter.setPreferredSize( new Dimension(
            440, GeneralPathPanel.MINIMUM_HEIGHT ));

        this.add( this.splitter, BorderLayout.CENTER );
        this.setVisible( true );
    }

    /**
     * Implementation for the <tt>ListSelectionListener</tt> interface's
     * <tt>valueChanged()</tt> method. This basically informs the
     * encapsulated <tt>GeneralPathPanel</tt> of the segment selected.
     */
    public void valueChanged( ListSelectionEvent e ) {
        this.pathPanel.setSelectedSegment( this.segmentList.getSelectedIndex());
        this.pathPanel.revalidate();
        this.pathPanel.repaint();
    }

    /**
     * Calls <tt>GeneralPathPanel.setGeneralPath()</tt> and updates
     * the internal <tt>JList</tt> with segments from the new
     * <tt>GeneralPath</tt>.
     */
    public void setGeneralPath( GeneralPath newGPath ) {
        this.pathPanel.setGeneralPath( newGPath );
        this.segmentList.setListData( this.pathPanel.getSegmentVector());
    }

    /**
     * Simply calls <tt>GeneralPathPanel.setOutlineColor()</tt>
     */
    public void setOutlineColor( Color newOutlineColor ) {
        this.pathPanel.setOutlineColor( newOutlineColor );
    }
}