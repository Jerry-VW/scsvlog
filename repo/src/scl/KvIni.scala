package scl

class KvIni(var fn:String) extends Kv {
    def this() = this("")
    
    def fromString(s:String):Boolean = {
        s.split("\r\n").foreach { l =>
            val kv = l.split(" = ")
            if (kv.length == 2) map(kv(0)) = java.net.URLDecoder.decode(kv(1), "UTF8")
        }
        true
    }
    override def load = {
        try { fromString(scl.Files.readString(fn,"UTF-8"))
        } catch { case e:Exception => /*println("error loading INI " + fn);*/ false }
    }
    def load(name:String):Boolean = { fn = name; load }
    def load(f:java.io.File):Boolean = { fn = f.getCanonicalPath; load }

    override def toString:String = (map.keys.toArray.sorted.map { k => k + " = " + java.net.URLEncoder.encode(map(k), "UTF-8") }).mkString("\r\n")
    override def save = {
        try {
            scl.Files.writeString(fn, toString, "UTF-8")
            true
        } catch { case e:Exception => /*println("error saving INI " + fn);*/ false }
    }
    def save(name:String):Boolean = { fn = name; save }
    def save(f:java.io.File):Boolean = { fn = f.getCanonicalPath; save }
}
