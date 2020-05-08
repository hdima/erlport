package erlport;

import java.io.*;
import java.util.*;
import java.lang.*;
import java.lang.reflect.*;
import java.nio.ByteBuffer;

import erlport.terms.*;

public class CLI {

    public static void main(final String[] args) {

        Options opts = new Options(args);

        Port port = new Port(opts);

        while(true) {
            try {
                Request req = port.read();
                //System.err.printf("Received: %s\n", req);
                try {
                    if (req.type == RequestType.CALL) {
                        Class<?> clazz = Class.forName(req.classname.value);
                        Class[] classArg = new Class[req.args.length];
                        for(int i=0; i<req.args.length; i++) {
                            classArg[i] = Object.class;
                        }
                        Method method = clazz.getMethod(req.functname.value, classArg);
                        Object result = method.invoke(null, req.args);

                        if (result == null) {
                            result = new Atom("ok");
                        }

                        // back to response
                        port.write(Response.success(req.requestId, result));
                    }
                } catch (Exception e) {
                    Binary errDesc = Utils.stringToBinary(Utils.getStackTrace(e));
                    System.err.println(errDesc);
                    port.write(Response.failure(req.requestId, errDesc));
                }
            } catch (EOFException e) {
                break;
            } catch (Exception e) {
                Binary errDesc = Utils.stringToBinary(Utils.getStackTrace(e));
                System.err.println(errDesc);
                try {
                    port.write(Response.stop(errDesc));
                } catch (Exception e2) {
                    Binary errDesc2 = Utils.stringToBinary(Utils.getStackTrace(e2));
                    System.err.println(errDesc2);
                }
                break;
            }
        }
    }
}
