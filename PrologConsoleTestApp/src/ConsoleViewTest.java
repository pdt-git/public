/*
 */
import java.io.IOException;

import org.cs3.pl.console.ConsoleView;
import org.cs3.pl.console.DefaultConsoleController;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 */
public class ConsoleViewTest {
    public static void main(String[] args) throws IOException {
        Shell shell = new Shell();
        shell.setLayout(new FillLayout());
        shell.setSize(400, 500);
        Display display = shell.getDisplay();
        ConsoleView view = new ConsoleView();
        view.createPartControl(shell);
        PrologInterface pif = new PrologInterface();
        pif.addInitHook(new ConsultEngineDirHook());
        pif.addInitHook(new CreateServerThreadHook());
        pif.start();
        PrologSocketConsoleModel consoleModel = new PrologSocketConsoleModel();      
        view.setModel(consoleModel);
        PrologCompletionProvider completionProvider = new PrologCompletionProvider();
        completionProvider.setPrologInterface(pif);
        DefaultConsoleController controller = new DefaultConsoleController();
        controller.setCompletionProvider(completionProvider);
        view.setController(controller);
        
        
        
        shell.open();
        shell.layout();
        while (!shell.isDisposed())
            if (!display.readAndDispatch())
                display.sleep();
    }
}