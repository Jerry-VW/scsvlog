package scl

trait Channel {
    def channels:Seq[String] = Nil
    
    def open(name:String):Boolean
    def opened:Boolean
    def close
    
    def avail:Int
    def read:Seq[Byte]
    
    def write(bytes:Array[Byte])

    def props:Seq[String] = Nil
    def propSet(k:String, v:Any) = {}
    def propGet(k:String, d:String):String = ""
    def propGetI(k:String, d:Int):Int = if (propGet(k,"") == "") d else propGet(k,"").toInt
    def propGetD(k:String, d:Double):Double = if (propGet(k,"") == "") d else propGet(k,"").toDouble
    def propGetB(k:String, d:Boolean):Boolean = if (propGet(k,"") == "") d else propGet(k,"").toBoolean
}
