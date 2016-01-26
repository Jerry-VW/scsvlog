package scl

class ChannelSocketTCP extends Channel {
    var socket:java.net.Socket = null
    
    def open(name:String) = {
        try {
            val ns = name.split(":")
            socket = new java.net.Socket(java.net.InetAddress.getByName(ns(0)), if (ns.length == 2) ns(1).toInt else 8080)
            _name  = name
        } catch { case _:Throwable => socket = null }
        opened
    }
    def opened = socket != null
    def close = { if (opened) { socket.close; socket = null } }
    
    def avail = if (opened) socket.getInputStream.available else 0
    def read = {
        if (opened && (avail > 0)){
            val ba = new Array[Byte](avail)
            socket.getInputStream.read(ba)
            ba
        } else Nil
    }
    
    def write(bytes:Array[Byte]) = if (opened) socket.getOutputStream.write(bytes)
}
