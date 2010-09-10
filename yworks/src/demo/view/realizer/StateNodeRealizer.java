/****************************************************************************
 **
 ** This file is part of yFiles-2.7.0.1. 
 ** 
 ** yWorks proprietary/confidential. Use is subject to license terms.
 **
 ** Redistribution of this file or of an unauthorized byte-code version
 ** of this file is strictly forbidden.
 **
 ** Copyright (c) 2000-2010 by yWorks GmbH, Vor dem Kreuzberg 28, 
 ** 72070 Tuebingen, Germany. All rights reserved.
 **
 ***************************************************************************/
package demo.view.realizer;


import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.geom.GeneralPath;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import y.io.graphml.graph2d.ShapeNodeRealizerSerializer;
import y.io.graphml.input.GraphMLParseContext;
import y.io.graphml.input.GraphMLParseException;
import y.io.graphml.output.GraphMLWriteContext;
import y.io.graphml.output.XmlWriter;
import y.util.YVersion;
import y.view.LineType;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;

/**
 * This class represents a custom NodeRealizer with its own paint,
 * copy and serialisation routines.
 * <br>
 * This realizer will be used in the demo
 *  {@link demo.view.realizer.StateNodeRealizerDemo}. 
 */
public class StateNodeRealizer extends ShapeNodeRealizer
{
  /**
   * State specifier constant. A node with this state will be drawn
   * like an ordinary ShapeNodeRealizer.
   */
  public static final byte INITIAL_STATE     = 0;
  
  /**
   * State specifier constant. A node with this state will be drawn
   * with an additional dashed line.
   */
  public static final byte TRANSITION_STATE  = 1;

  /**
   * State specifier constant. A node with this state will be drawn
   * with an additional solid line.
   */
  public static final byte FINAL_STATE       = 2;
  
  
  private byte state;
  
  /**
   * Instantiates a new StateNodeRealizer with the given state.
   * @see #setState(byte)
   */
  public StateNodeRealizer(byte state)
  {
    super();
    this.state = state;
    this.setShapeType(ELLIPSE);
  }
  
  /**
   * Instantiates a new StateNodeRealizer.
   */
  public StateNodeRealizer()
  {
    this(INITIAL_STATE);
  }
  
  /**
   * Instantiates a new StateNodeRealizer as a copy of 
   * the given NodeRealizer.
   */
  public StateNodeRealizer(NodeRealizer r)
  {
    super(r);
    if(r instanceof StateNodeRealizer)
    {
      StateNodeRealizer sr = (StateNodeRealizer)r;
      state = sr.state;
    }
    else
    {
      state = INITIAL_STATE;
    }
  }
  
  /**
   * Sets the state of this realizer.
   * @param state on of {@link #INITIAL_STATE}, {@link #TRANSITION_STATE}
   * or {@link #FINAL_STATE}.
   */
  public void setState(byte state)
  {
    this.state = state;
  }
  
  /**
   * Returns the state of this realizer.
   * By default {@link #INITIAL_STATE} will be returned.
   * @see #setState(byte)
   */
  public byte getState()
  {
    return this.state;
  }
  
  
  /**
   * Paints the node on the given graphics context.
   * Depending on the state of the realizer the node will be
   * drawn differently
   */
  public void paintNode(Graphics2D gfx)
  {
    //first paint the shape node realzier
    super.paintNode(gfx);
    
    if(getState() != INITIAL_STATE)
    {
      //then draw an additional state marker
      //on top of the node.
      Stroke oldStroke = gfx.getStroke();
      Color oldColor   = gfx.getColor();
      if(state == TRANSITION_STATE) {
        gfx.setStroke(LineType.DASHED_1);
      } else if(getState() == FINAL_STATE) {
        gfx.setStroke(LineType.LINE_1);
      }
      double oldWidth  = getWidth();
      double oldHeight = getHeight();
      if(oldWidth > 10 && oldHeight > 10)
      {
        setSize(oldWidth-10,oldHeight-10);
        gfx.draw(shape);
        setSize(oldWidth,oldHeight);
      }
      
      gfx.setColor(oldColor);
      gfx.setStroke(oldStroke);
    }
  }
  
  /**
   * Creates a copy of the given realizer that has type
   * StateNodeRealizer. It is important to implement this method properly
   * if the realizer should be able to act as a template for 
   * other realizers, e.g. if it is used as default ndoe realizer
   * in a Graph2D.
   * The canonic way to implement this method is to return the result
   * of a copy constructor.
   * @see y.view.Graph2D#setDefaultNodeRealizer(y.view.NodeRealizer)
   * @see #StateNodeRealizer(NodeRealizer) 
   */
  public NodeRealizer createCopy(NodeRealizer r)
  {
    return new StateNodeRealizer(r);
  }

  /**
   * A custom shape type specifier.
   */
  public static final byte CUSTOM_SHAPE = -1;

  /**
   * Demonstrates how to define custom shapes.
   */
  public void setShapeType(byte type)
  {
    if(type == CUSTOM_SHAPE)
    {
      updateCustomShape();
    }
    super.setShapeType(type);
  }
  
  void updateCustomShape()
  {
    GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD,5);
    float x = (float)getX();
    float y = (float)getY();
    float w = (float)getWidth();
    float h = (float)getHeight();
    path.moveTo(x, y + h);
    path.lineTo(x, y);
    float dx = Math.min(h*0.2f,w);
    path.lineTo(x+w-dx,y);
    path.lineTo(x+w,y+0.5f*h);
    path.lineTo(x+w-dx,y+h);
    path.closePath();
    shape = path;      
  }
  
  /**
   * adjust custom shape when size changes. Overrides default scaling operation.
   */
  public void setSize(double x, double y)
  {
    if(getShapeType() == CUSTOM_SHAPE)
    {
      shape = null;
      super.setSize(x,y);
      updateCustomShape();
    }
    else
    {
      super.setSize(x,y);
    }
  }
      
  
  /**
   * Writes out this realizer in a serialized form. This method 
   * will be used by YGFIOHandler to serialize this NodeRealizer.
   */
  public void write(ObjectOutputStream out) throws IOException 
  {
    //write out a version tag. version tags help to provide future
    //serialization compatibility when node realizer features
    //change.    
    out.writeByte(YVersion.VERSION_1);
    //write out the shape node realzier features
    super.write(out);
    //write out the state variable
    out.writeByte(state);
  }
  
  /**
   * Reads in the serialized form of this realizer. The realizer must have been
   * written out before by it's {@link #write(ObjectOutputStream)} method.
   * This method will be used by YGFIOHandler to deserialize this NodeRealizer.
   */
  public void read(ObjectInputStream in) throws IOException, 
    ClassNotFoundException 
  {
    switch(in.readByte()) {
    case YVersion.VERSION_1:
      super.read(in);
      state = in.readByte();
      break;
    default:
      //trouble
    }
  }

  /**
   * RealizerSerializer that can be used to serialize instances of StateNodeRealizer to/from GraphML
   */
  static class StateNodeRealizerSerializer extends ShapeNodeRealizerSerializer {
    public String getName() {
      return "StateNode";
    }

    public String getNamespaceURI() {
      return "demo.view.realizer";
    }

    public Class getRealizerClass() {
      return StateNodeRealizer.class;
    }

    public void parse(NodeRealizer realizer, Node domNode, GraphMLParseContext context) throws GraphMLParseException {
      super.parse(realizer, domNode, context);
      StateNodeRealizer snr = (StateNodeRealizer) realizer;
      String state = ((Element) domNode).getAttribute("state");

      if("initial".equals(state)) {
        snr.setState(INITIAL_STATE);
      }
      else if("transition".equals(state)) {
        snr.setState(TRANSITION_STATE);
      }
      else if("final".equals(state)) {
        snr.setState(FINAL_STATE);
      }
    }


    protected byte decodeShapeType(String s, GraphMLParseContext context) {
      if("custom".equals(s)) {
        return CUSTOM_SHAPE;
      }
      return super.decodeShapeType(s, context);
    }

    protected String encodeShapeType(ShapeNodeRealizer snr, GraphMLWriteContext context) {
      if(snr.getShapeType() == CUSTOM_SHAPE) {
        return "custom";
      }
      return super.encodeShapeType(snr, context);
    }

    public void writeAttributes(NodeRealizer nr, XmlWriter writer, GraphMLWriteContext context) {
      super.writeAttributes(nr, writer, context);
      StateNodeRealizer snr = (StateNodeRealizer) nr;
      switch(snr.getState()) {
        case INITIAL_STATE:
          writer.writeAttribute("state", "initial");
          break;
        case TRANSITION_STATE:
          writer.writeAttribute("state", "transition");
          break;
        case FINAL_STATE:
          writer.writeAttribute("state", "final");
          break;
      }
    }
  }
}

