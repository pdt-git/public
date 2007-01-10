package alice.tuprologx.runtime.tcp;
import java.io.Serializable;

public class NetMsg implements Serializable {
    public String methodName;

    public NetMsg(){
    }

    public NetMsg(String name) {
        methodName=name;
    }
}

