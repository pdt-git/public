package salvo.jesus.graph.javax.swing;

import java.awt.*;
import javax.swing.*;
import java.util.*;
import salvo.jesus.graph.java.awt.*;

/**
 * A font chooser object. Currently, there is no dialog box option for
 * this class such as the one implemented by JColorChooser, but that can be
 * added in the future. There is also currently no preview option.
 *
 * @author  Jesus M. Salvo Jr.
 */
public class JFontChooser extends JPanel {
  JComboBox   fontscombo;
  JComboBox   sizescombo;
  JComboBox   stylescombo;

  private String  fontsString[];
  private static final String  sizesString[] = {"8","9","10","11","12","13","14","15","16"};
  private static final String  stylesString[] = {"Plain","Bold","Italic","Bold & Italic"};

  /**
   * Creates a JFontChooser object. The object created is intended to be
   * placed on a container.
   */
  public JFontChooser() {
    initJFontChooser();
  }

  /**
   * Creates a JFontChooser object with a preselected font.
   *  The object created is intended to be placed on a container.
   *
   * @param   initialfont   Font object that is to be initially selected.
   */
  public JFontChooser( Font initialfont ) {
    Integer   initialfontsize = new Integer( initialfont.getSize());
    int       sizeindex;
    int       fontIndex;

    initJFontChooser();

    if( initialfont == null )
      initialfont = new Font( "Lucida Sans", Font.PLAIN, 10 );

    // Select initial font
    fontIndex = Arrays.binarySearch( fontsString, initialfont.getName(),
        new salvo.jesus.util.StringComparator() );
    // If Lucida Sans is not in the list, get the first font.
    if( fontIndex < 0 )
        fontIndex = 0;

    fontscombo.setSelectedIndex( fontIndex );
    stylescombo.setSelectedIndex( initialfont.getStyle() );
    sizeindex = Arrays.binarySearch( sizesString, initialfontsize.toString(),
      new Comparator() {
        public int compare( Object obj1, Object obj2 ) {
          Integer i1 = new Integer( (String) obj1 );
          Integer i2 = new Integer( (String) obj2 );

          return i1.intValue() - i2.intValue();
        }
      });
    sizescombo.setSelectedIndex( sizeindex );
  }

  /**
   * Creates the components for font selection.
   */
  private void initJFontChooser() {
    Cursor  originalcursor = this.getCursor();

    this.setCursor( new Cursor( Cursor.WAIT_CURSOR ) );
    fontsString = Fonts.instance().getFonts();
    this.setCursor( originalcursor );

    Arrays.sort( fontsString );
    fontscombo = new JComboBox( fontsString );

    sizescombo = new JComboBox( sizesString );
    stylescombo = new JComboBox( stylesString );

    JPanel  fontspanel = new JPanel();
    fontspanel.setLayout( new GridLayout( 0, 1 ));
    fontspanel.add( new JLabel( "Fonts" ));
    fontspanel.add( fontscombo );

    JPanel  sizescombopanel = new JPanel();
    sizescombopanel.setLayout( new GridLayout( 0, 1 ));
    sizescombopanel.add( new JLabel( "Size" ));
    sizescombopanel.add( sizescombo );

    JPanel  stylescombopanel = new JPanel();
    stylescombopanel.setLayout( new GridLayout( 0, 1 ));
    stylescombopanel.add( new JLabel( "Style" ));
    stylescombopanel.add( stylescombo );

    JPanel  sizescombotylepanel = new JPanel();
    sizescombotylepanel.setLayout( new BorderLayout() );
    sizescombotylepanel.add( sizescombopanel, BorderLayout.WEST );
    sizescombotylepanel.add( stylescombopanel, BorderLayout.CENTER );

    this.setLayout( new BorderLayout() );
    this.add( fontspanel, BorderLayout.WEST );
    this.add( sizescombotylepanel, BorderLayout.CENTER );
  }

  /**
   * Returns a Font object representing the currently selected font, size, and style.
   * This will always return a new instance of a Font, even though there were no
   * changes in the selection between getFont() calls.
   *
   * @return  A new Font object with the font, size, and style based on the font selection.
   */
  public Font getSelectedFont() {
    Integer size = new Integer((String) sizescombo.getSelectedItem());

    return new Font( (String) fontscombo.getSelectedItem(), stylescombo.getSelectedIndex(),
      size.intValue() );
  }

}

