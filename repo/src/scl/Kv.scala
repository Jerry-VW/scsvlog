package scl

class Kv {
    val map = new collection.mutable.HashMap[String,String]

    def put(k:String, v:Any):Any = { map(k) = v.toString; v }

    def get(k:String, d:Any):String = map.getOrElse(k,d.toString)

    def getI(k:String, d:Int):Int = {
        val v = get(k, "")
        if (v == "") d else v.toInt
    }
    def getD(k:String, d:Double):Double = {
        val v = get(k, "")
        if (v == "") d else v.toDouble
    }
    def getB(k:String, d:Boolean):Boolean = {
        val v = get(k, "")
        if (v == "") d else v.toBoolean
    }
    
    def load:Boolean = true
    def save:Boolean = true
    
    load
}
