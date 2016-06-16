package scl

class Kv {
    val map = new collection.mutable.HashMap[String,String]

    def put(k:String, v:Any):Any = { map(k) = v.toString; v }
    def update(k:String, v:Any) = put(k,v)

    def get(k:String, d:String):String   = map.getOrElse(k,d)
    def get(k:String):String = get(k,"")
    def get(k:String, d:Int):Int         = { if (map.contains(k)){ try { map(k).toDouble.toInt } catch { case _:Exception => d }} else d }
    def get(k:String, d:Double):Double   = { if (map.contains(k)){ try { map(k).toDouble } catch { case _:Exception => d }} else d }
    def get(k:String, d:Boolean):Boolean = { if (map.contains(k)){ try { map(k).toBoolean } catch { case _:Exception => d }} else d }
    
    def apply(k:String) = get(k,"")
    def apply(k:String, d:String) = get(k,d)
    def apply(k:String, d:Int) = get(k,d)
    def apply(k:String, d:Double) = get(k,d)
    def apply(k:String, d:Boolean) = get(k,d)

    def load:Boolean = true
    def save:Boolean = true
    
    def clear = map.clear
    
    // copy all values to map
    def copyFrom(kv:Kv) = {
        map.clear
        for (k <- kv.map.keys) map(k) = kv(k)
    }
    
    load
}
