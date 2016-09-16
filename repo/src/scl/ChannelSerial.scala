package scl

import purejavacomm.{CommPort, CommPortIdentifier, SerialPort}

class ChannelSerial extends Channel {
    var port:SerialPort = null
    
    override def channels = ChannelSerial.channels

    def open(name:String) = {
        try {
            if (opened) close
            port = CommPortIdentifier.getPortIdentifier(name).open("SerialPJC@" + name, 100).asInstanceOf[SerialPort]
            _name = name
        } catch { case _:Exception => port = null }
        opened
    }
    def opened = port != null
    def close = { if (opened) { port.close; port = null } }
    
    def avail = if (opened) port.getInputStream.available else 0
    def read = {
        if (opened && (avail > 0)){
            val ba = new Array[Byte](avail)
            port.getInputStream.read(ba)
            ba
        } else Nil
    }
    
    def write(bytes:Array[Byte]) = if (opened) port.getOutputStream.write(bytes)
    
    override def props = List("baud","bits","parity","stops","rts","cts","dtr","dsr","dcd","ri")
    override def propSet(k:String, v:Any) = if (opened) try {
        k match {
            case "baud" => port.setSerialPortParams(v.toString.toInt, port.getDataBits, port.getStopBits, port.getParity)
            case "bits" => port.setSerialPortParams(port.getBaudRate,
                Map( 5 -> SerialPort.DATABITS_5, 6 -> SerialPort.DATABITS_6, 7 -> SerialPort.DATABITS_7, 8 -> SerialPort.DATABITS_8 )(v.toString.toInt),
                port.getStopBits, port.getParity)
            case "parity" => port.setSerialPortParams(port.getBaudRate, port.getDataBits, port.getStopBits,
                Map( "none" -> SerialPort.PARITY_NONE, "odd" -> SerialPort.PARITY_ODD, "even" -> SerialPort.PARITY_EVEN, "mark" -> SerialPort.PARITY_MARK, "space" -> SerialPort.PARITY_SPACE )(v.toString))
            case "stops" => port.setSerialPortParams(port.getBaudRate, port.getDataBits,
                Map( 1.0 -> SerialPort.STOPBITS_1, 1.5 -> SerialPort.STOPBITS_1_5, 2.0 -> SerialPort.STOPBITS_2 )(v.toString.toDouble),
                port.getParity)
            case "dtr" => port.setDTR(v.toString.toBoolean)
            case "rts" => port.setRTS(v.toString.toBoolean)
        }
    } catch { case _:Exception => }
    override def propGet(k:String, d:String):String = if (opened){
        k match {
            case "baud" => port.getBaudRate.toString
            case "bits" => (Map( SerialPort.DATABITS_5 -> "5", SerialPort.DATABITS_6 -> "6", SerialPort.DATABITS_7 -> "7", SerialPort.DATABITS_8 -> "8" ).getOrElse(port.getDataBits, "8"))
            case "parity" => (Map( SerialPort.PARITY_NONE -> "none", SerialPort.PARITY_ODD -> "odd", SerialPort.PARITY_EVEN -> "even", SerialPort.PARITY_MARK -> "mark", SerialPort.PARITY_SPACE -> "space" ).getOrElse(port.getParity, "none"))
            case "stops" => (Map( SerialPort.STOPBITS_1 -> "1.0", SerialPort.STOPBITS_1_5 -> "1.5", SerialPort.STOPBITS_2 -> "2.0" ).getOrElse(port.getStopBits, "1.0"))
            case "dtr" => port.isDTR.toString
            case "dsr" => port.isDSR.toString
            case "dcd" => port.isCD.toString
            case "rts" => port.isRTS.toString
            case "cts" => port.isCTS.toString
            case "ri" => port.isRI.toString
            case _ => d
        }
    } else d
}

object ChannelSerial {
    def channels = {
        val l = new collection.mutable.ArrayBuffer[String]
        val le = CommPortIdentifier.getPortIdentifiers
        while (le.hasMoreElements){
            val lp = le.nextElement.asInstanceOf[CommPortIdentifier]
            if ((lp.getPortType == CommPortIdentifier.PORT_SERIAL)&&(!lp.isCurrentlyOwned)) l += lp.getName
        }
        l
    }
}
