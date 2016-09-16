package scl

import javax.servlet.http.{HttpServletRequest, HttpServletResponse, HttpServletRequestWrapper};
import java.net.{InetSocketAddress, InetAddress};
import org.eclipse.{jetty};
import scala.collection.JavaConversions._;

/// ipPort - port to listen, staticDir - directory with static content,
/// shutdownTime - exit timeout (sec) if no requests received (=1 - never exit)
/// callback - response callback function (returns true if request is served)
class ServerJetty(ipPort:Int, staticDir:String, shutdownTime:Int) {
    val _server = this;
    val server  = new jetty.server.Server(ipPort);
    
    val shutdownCounter = new java.util.concurrent.atomic.AtomicInteger(0);

    // start shutdown timeout timer
    if (shutdownTime > 0) {
        val shutdownTimer = new java.util.Timer() {
            scheduleAtFixedRate(new java.util.TimerTask() {
                def run = {
                    if (shutdownTime > 0) {
                        if (shutdownCounter.incrementAndGet == shutdownTime) {
println("Shutdown timeout !");
                            (new java.util.Timer).schedule(new java.util.TimerTask {
                                def run = _server.stop(0);
                            }, 1000)
                        }
                    }
                }
            }, 1000, 1000);
        }
    }
    
    // add static and dynamic content handlers
    server.setHandler(
        new jetty.server.handler.HandlerList {
            setHandlers(Array(
                //static resources
                new jetty.server.handler.ContextHandler {
                    setResourceBase(staticDir)
//                    setAliases(true)
                    setHandler(new jetty.server.handler.ResourceHandler)
                },
                //dynamic data
                new jetty.server.handler.DefaultHandler {
                    override def handle(target:String, baseRequest:jetty.server.Request, request:HttpServletRequest, response:HttpServletResponse):Unit = {
//						try {
                            shutdownCounter.set(0)
		                    if (!newHandler(target,baseRequest,request,response).handle)
		                        super.handle(target, baseRequest, request, response);
//                        } catch { case _:Exception => }
                    }
                }
            ))
        }
    )

    def newHandler(target:String, baseRequest:jetty.server.Request, request:HttpServletRequest, response:HttpServletResponse):HandlerJetty =
        new HandlerJetty(target, baseRequest, request, response);
    
    // start server
    def start:ServerJetty = {
        server.start;
        server.join;
        this
    }

    // stop server after delay (=0 to exit immediately)
    def stop(delay:Int):Unit = {
println("Stopping...")
        (new java.util.Timer).schedule( new java.util.TimerTask{
            def run = {
                server.stop
            }
        }, delay)
    }
    
    def isStarted = server.isStarted;
}

class HandlerJetty(target:String, baseRequest:jetty.server.Request, request:HttpServletRequest, response:HttpServletResponse) {
    var handled         = false; //true if response handled
    var contentType     = "application/json;charset=ISO-8859-1"; //response content type
    var contentEncoding = "ISO-8859-1";
    var responseStr     = ""; //response body

    def param(name:String):String = baseRequest.getParameter(name)
    def param(name:String, default:Object):String = if (baseRequest.getParameter(name) == null) default.toString else baseRequest.getParameter(name)
    
    def handle:Boolean = {
        //if success - return OK status
        if (handled) {
            baseRequest.setHandled(true);
            response.getWriter.write(responseStr)
            response.setContentType(contentType);
            response.setCharacterEncoding(contentEncoding);
            response.setStatus(HttpServletResponse.SC_OK);
        }
        handled
    }
}
