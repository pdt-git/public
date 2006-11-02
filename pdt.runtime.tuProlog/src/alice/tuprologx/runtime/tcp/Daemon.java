package alice.tuprologx.runtime.tcp;
import java.util.*;
import java.net.*;
import java.io.*;
import java.lang.reflect.*;

import alice.tuprolog.*;

class Acceptor implements Runnable {
    ObjectOutputStream outStream;
    ObjectInputStream  inStream;
    Socket socket;
    PrologImpl core;
    boolean  initOk=true;

    Acceptor(Socket s,   PrologImpl core_){
        core=core_;
        socket=s;
        try {
            outStream=new ObjectOutputStream(socket.getOutputStream());
            inStream=new ObjectInputStream(socket.getInputStream());
        } catch (Exception ex){
            //ex.printStackTrace();
            initOk=false;
        }
    }

    public void run() {
        if (!initOk)
            return;
        System.out.println("[ User "+Thread.currentThread()+ " ] start.");
        while (true){
            try {
                NetMsg msg=(NetMsg)inStream.readObject();
                Method m=core.getClass().getMethod(msg.methodName,new Class[]{ inStream.getClass(), outStream.getClass() });
                m.invoke(core,new Object[]{ inStream,  outStream });
            } catch (Exception e){
                //e.printStackTrace();
                break;
            }
        }
        try { socket.close(); } catch (IOException ex){};
        System.out.println("[ User "+Thread.currentThread()+ "] shutdown.");
    }
}

public class Daemon implements Runnable {
    public static int DEFAULT_PORT = 3203;
    ServerSocket s;
    alice.tuprolog.Prolog   core;
    PrologImpl coreTCP;

    public Daemon(alice.tuprolog.Prolog core_) throws IOException {
        core=core_;
        coreTCP=new PrologImpl(core);
        initCore(DEFAULT_PORT);
    }

    public Daemon(alice.tuprolog.Prolog core_,int port) throws IOException {
        core=core_;
        coreTCP=new PrologImpl(core);
        initCore(port);
    }

    private void initCore(int port) throws IOException {
        s=new ServerSocket(port);
    }

    public void run() {
        System.out.println("prolog TCP daemon start.");
        while (true){
            try {
                Socket socket=s.accept();
                System.out.println("[ UserAcceptor ] accept new user.");
                Thread th=new Thread(new Acceptor(socket, coreTCP));
                th.start();
            } catch (Exception e){
                e.printStackTrace();
                break;
            }
        }
        System.out.println("[ UserAcceptor ] end.");
    }


    public static void main(String args[]){
        try {
            if (args.length!=0 && args.length!=1){
                System.err.println("args: { port }.");
                System.exit(-1);
            }
            alice.tuprolog.Prolog core=new alice.tuprolog.Prolog();
            if (args.length>0){
                int port=-1;
                try {
                    port=Integer.parseInt(args[0]);
                } catch (Exception ex){
                    System.err.println("args: { port }.");
                    System.exit(-1);
                }
                new Thread(new Daemon(core,port)).start();
            } else
            new Thread(new Daemon(core)).start();
        } catch (Exception ex){
            ex.printStackTrace();
        }
    }
}

