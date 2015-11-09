package scl

class KvIni(fn:String) extends Kv {
    
    override def load = {
        try {
            io.Source.fromFile(fn, "UTF-8").getLines.foreach { l =>
                val kv = l.split(" = ")
                if (kv.length == 2) map(kv(0)) = java.net.URLDecoder.decode(kv(1), "UTF8")
            }
            true
        } catch { case e:Throwable => false }
    }
    override def save = {
        try {
            java.nio.file.Files.write(
                (new java.io.File(fn)).toPath,
                (map.foldLeft("")((a,kv) =>
                    (a + kv._1 + " = " + java.net.URLEncoder.encode(kv._2, "UTF-8") + "\r\n"))).getBytes("UTF-8")
            )
            true
        } catch { case e:Throwable => false }
    }
}
