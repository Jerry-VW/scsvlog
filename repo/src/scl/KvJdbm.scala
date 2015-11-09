package scl

class KvJdbm(fn:String, dbn:String="db") extends Kv {
    val rm = jdbm.RecordManagerFactory.createRecordManager(fn)
    val htId = rm.getNamedObject(dbn)
    val ht = if (htId != 0) jdbm.htree.HTree.load(rm,htId) else jdbm.htree.HTree.createInstance(rm)
    if (htId == 0) rm.setNamedObject(dbn, ht.getRecid)
    
    override def put(k:String, v:Any):Any = { map(k) = v.toString; ht.put(k, v.toString); v }
    override def get(k:String, d:Any):String = {
        if (map.contains(k)) map(k)
        else {
            val v = ht.get(k).asInstanceOf[String]
            if (v == null) d.toString else {
                map(k) = v
                v
            }
        }
    }

    override def save:Boolean = { rm.commit; rm.close; true }
}
