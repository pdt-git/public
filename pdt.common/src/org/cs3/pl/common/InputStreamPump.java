package org.cs3.pl.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
 /**
  * A thread that exhaustes InputStream objects.
  * <p/>
  * Actualy not very exiting, but it was needed on several places, so 
  * i put it here as a public class.  
  */
 public class InputStreamPump extends Thread {
     BufferedReader in = null;

     public InputStreamPump(InputStream s) {
         super("InputStreamPump");
         this.in = new BufferedReader(new InputStreamReader(s));
     }

     public void run() {
         char[] buf = new char[256];
         int read = 0;
         try {
             //while ((read = in.read(buf)) > -1) {
             while(true){
                
                 read = in.read(buf);
                 if(read>0){;
                     dataAvailable(buf,read);
                 }
                 if(read==-1){
                     break;
                 }
             }
         } catch (IOException e) {
             e.printStackTrace();
             Debug.report(e);
         }
         finally{
             Debug.warning("InputStreamPump stops working!");
         }
     }

     /**
      * called when data is available on the stream.
      * <p>
      * The default implementation does nothing. Subclasses that
      * actualy want to use the read data may overwrite this method.
      * @param buffer a char array containing the read data.
      * @param length the number of <b>new</b> chars available in the buffer. The
      * newly avaible data will always be at the begining of the buffer.  
      */
     protected void dataAvailable(char[] buffer, int length) {            
         ;
     }
 }