/*
 */
import java.io.IOException;
import java.util.Map;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.ConsoleView;
import org.cs3.pl.console.DefaultConsoleController;
import org.cs3.pl.metadata.MetadataEngineInstaller;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 */
public class ConsoleViewTest {
    public static void main(String[] args) throws IOException {
        System.out.println(System.getProperty("java.library.path"));
        Shell shell = new Shell();
        shell.setLayout(new FillLayout());
        shell.setSize(400, 500);
        Display display = shell.getDisplay();
        ConsoleView view = new ConsoleView();
        view.createPartControl(shell);
        final PrologInterface pif = PrologInterfaceFactory.newInstance().create();
      
        final PrologSocketConsoleModel consoleModel = new PrologSocketConsoleModel(false);
        int consolePort= Integer.getInteger(PDT.PREF_CONSOLE_PORT,4711).intValue();
    	if(consolePort==-1){
    		throw new NullPointerException("Required property \""+PDT.PREF_CONSOLE_PORT+"\" was not specified.");
    	}
        consoleModel.setPort(consolePort);
        MetadataEngineInstaller.install(pif);
        pif.addLifeCycleHook(new ConsoleServerHook(),ConsoleServerHook.HOOK_ID,null);        
        pif.addLifeCycleHook(new LifeCycleHook(){

            public void onInit(PrologInterface pif,PrologSession initSession) {
                // TODO Auto-generated method stub
                
            }

            public void afterInit(PrologInterface pif) {
                consoleModel.connect();
                
            }

            public void beforeShutdown(PrologInterface pif,PrologSession session) {
                consoleModel.disconnect();
                
            }
        },"Console",new String[]{ConsoleServerHook.HOOK_ID});
        pif.start();
        view.setModel(consoleModel);
        PrologCompletionProvider completionProvider = new PrologCompletionProvider();
        completionProvider.setPrologInterface(pif);
        DefaultConsoleController controller = new DefaultConsoleController(){
            /* (non-Javadoc)
             * @see org.cs3.pl.console.DefaultConsoleController#keyStrokeIntercepted(int, char)
             */
            public boolean keyStrokeIntercepted(int keyCode, char keyChar) {
                if (keyCode==SWT.F9){                    
                    try {
                        PrologSession session = pif.getSession();
                        Map r = session.query("current_thread(A,B)");
                        while(r!=null){
                            Debug.info(r.get("A")+"-->"+r.get("B"));
                            r=session.next();
                        }
                        session.dispose();
                    } catch (Throwable e) {
                        Debug.report(e);
                    }
                    return false;
                }
                if (keyCode==SWT.F10){                    
                    try {
                        pif.stop();                       
                        pif.start();
                    } catch (IOException e) {
                        Debug.report(e);
                    } 
                    return false;
                }
                if (keyCode==SWT.F11){                    
                    try {
                        pif.start();
                    } catch (IOException e) {
                        Debug.report(e);
                    }
                    return false;
                }
                if (keyCode==SWT.F12){                    
                    try {
                        pif.stop();
                    } catch (IOException e) {
                      Debug.report(e);
                    }
                    return false;
                }
                return super.keyStrokeIntercepted(keyCode, keyChar);
            }
            };
        controller.setCompletionProvider(completionProvider);
        view.setController(controller);
        
        
        
        shell.open();
        shell.layout();
        while (!shell.isDisposed())
            if (!display.readAndDispatch())
                display.sleep();
    }
}