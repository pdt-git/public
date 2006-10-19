package salvo.jesus.util;

import java.awt.*;
import javax.swing.*;
import java.awt.print.*;
import org.apache.log4j.Category;

/**
 * Generic printing utility.
 * <p>
 * The original source code for this class only was obtained from
 * http://www.apl.jhu.edu/~hall/java/Swing-Tutorial/Swing-Tutorial-Printing.html
 * without restrictions as specified on their web site. To quote the link above:
 * <p>
 * <i>All source code freely available for unrestricted use.</i>
 * <p>
 * However, additional changes have been made to the original.
 */


public class PrintUtilities implements Printable {

    private Component componentToBePrinted;

    /**
     * Log4J Category. The name of the category is the fully qualified name of the
     * enclosing class.
     */
    static        Category    logger;

    static {

        logger = Category.getInstance( PrintUtilities.class.getName());
    }

    public static void printComponent(Component c) {
        new PrintUtilities(c).print();
    }

    public PrintUtilities(Component componentToBePrinted) {
        this.componentToBePrinted = componentToBePrinted;
    }

    public void print() {
        PrinterJob printJob = PrinterJob.getPrinterJob();
        printJob.setPrintable(this);
        if (printJob.printDialog())
        try {
            printJob.print();
        } catch(PrinterException pe) {
            logger.error( "Printing Error", pe );
        }
    }

    public int print(Graphics g, PageFormat pageFormat, int pageIndex) {

        logger.info( "Printing page " + pageIndex );

        if (pageIndex > 0) {
            return(NO_SUCH_PAGE);
        } else {
            Graphics2D g2d = (Graphics2D)g;
            g2d.translate(pageFormat.getImageableX(), pageFormat.getImageableY());
            disableDoubleBuffering(componentToBePrinted);
            componentToBePrinted.paint(g2d);
            enableDoubleBuffering(componentToBePrinted);
            return(PAGE_EXISTS);
        }
    }

    public static void disableDoubleBuffering(Component c) {
        RepaintManager currentManager = RepaintManager.currentManager(c);
        currentManager.setDoubleBufferingEnabled(false);
    }

    public static void enableDoubleBuffering(Component c) {
        RepaintManager currentManager = RepaintManager.currentManager(c);
        currentManager.setDoubleBufferingEnabled(true);
    }
}
