package salvo.jesus.graph.java.awt;

import java.awt.*;

/**
 * A singleton class that gets all available fonts.
 *
 * @author  Jesus M. Salvo Jr. 
 */
public final class Fonts {
  /**
   * A reference to the single instance of Fonts
   */
  private static Fonts fontsInstance;

  /**
   * An array of String that holds the names of available fonts.
   */
  private String  fontsString[];

  /**
   * Private constructor so that no instance can be created externally from this class.
   */
  private Fonts(){
    GraphicsEnvironment gEnv = GraphicsEnvironment.getLocalGraphicsEnvironment();
    this.fontsString = gEnv.getAvailableFontFamilyNames();
  }

  /**
   * Returns a single instance of Fonts. If no instance has been created,
   * creates a new instance of Fonts. If an instance already exists,
   * simply returns the existing instance of Fonts.
   *
   * @return    Singleton Fonts object.
   */
  public static Fonts instance() {
    if( fontsInstance == null )
      fontsInstance = new Fonts();
    return fontsInstance;
  }

  /**
   * Returns the available Fonts in the system.
   *
   * @return  An array of Strings of the available fonts in the system.
   */
  public String[] getFonts() {
    return this.fontsString;
  }
}

