package scl

class ChannelSocketUDP extends Channel {
    var socket:java.net.DatagramSocket = null
    
    def open(name:String) = {
        try {
            val ns = name.split(":")
            socket = new java.net.DatagramSocket(if (ns.length == 2) ns(1).toInt else 8080, java.net.InetAddress.getByName(ns(0)))
        } catch { case _:Throwable => socket = null }
        opened
    }
    def opened = socket != null
    def close = { if (opened) { socket.close; socket = null } }
    
    def avail = if (opened) 0 else 0
    def read = {
        if (opened){
            Nil
        } else Nil
    }
    
    def write(bytes:Array[Byte]) = if (opened) {}
}
