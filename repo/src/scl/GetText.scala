package scl

trait GetText {
    def tr(s:String, pl:Int=1):String = GetText.tr(s,pl)
}

object GetText {
    val cat = new collection.mutable.HashMap[String, collection.mutable.HashMap[String, Array[Array[String]]]];

    val langDef = "en"
    var lang = langDef

    def tr(s:String, pl:Int=1 ):String = { try { cat(lang)(s)(1)(pl-1) } catch { case _:Throwable => s } }

    def langs:Seq[String] = langDef :: (for ((l,t) <- cat) yield l).toList
    def displayLangs:Seq[String] =
        (for (l <- langs) yield {
            val ls = l.split("_")
            val loc = if (ls.length == 1) new java.util.Locale(l) else new java.util.Locale(ls(0), ls(1))
            loc.getDisplayName(loc).capitalize;
        })
    
    def init(langIndex:Int, root:String="./locale", domain:String="default", category:String="MESSAGES") = {
        try {
            for (langDir <- new java.io.File(root).list if (new java.io.File(root + java.io.File.separator + langDir).isDirectory)) {
                try {
//                    println("Loading: " + (root + java.io.File.separator + langDir + java.io.File.separator + 
//                        "LC_" + category + java.io.File.separator + domain + ".mo"))
                    cat(langDir) = parseMO( scala.io.Source.fromFile(
                        root + java.io.File.separator + langDir + java.io.File.separator + 
                        "LC_" + category + java.io.File.separator + domain + ".mo",
                        "ISO-8859-1"
                        ).map(_.toByte ).toArray
                    )
                } catch { case _:Throwable => }
            }
            lang = langs(langIndex)
        } catch { case _:Throwable => }
    }
    
    /**
     * Parse .MO file contents
     * @param mo file contents in byte array
     * @return translations map
     */
    def parseMO( mo:Array[Byte] ):collection.mutable.HashMap[String, Array[Array[String]]] = {
        val lcat = new collection.mutable.HashMap[String, Array[Array[String]]];
        if (mo.length > 24) {
            // check magick for msb/lsb
            val magick = (((mo(0) & 0xff) * 256 + (mo(1) & 0xff)) * 256 + (mo(2) & 0xff)) * 256 + (mo(3) & 0xff);
            val msb = magick == 0x950412deL;
            
            if ((magick == 0x950412de) || (magick == 0xde120495)) {
                
                def u32( i:Int ):Int = if (msb)
                    ((((mo(i) & 0xff) * 256 + (mo(i+1) & 0xff)) * 256 + (mo(i+2) & 0xff)) * 256 + (mo(i+3) & 0xff));
                else
                    ((((mo(i+3) & 0xff) * 256 + (mo(i+2) & 0xff)) * 256 + (mo(i+1) & 0xff)) * 256 + (mo(i) & 0xff));
                
                val rev = u32(4); // revision
                val (revMaj, revMin) = (rev >> 16, rev % 65536); // major/minor revision
                // number of strings, offsets of original and translation strings tables
                val (sn, oto, ott) = (u32(8), u32(12), u32(16));

                
                if (sn > 1 && revMaj <= 1 && revMin <= 1) {
                    for (sc <- 1 to sn-1) { // process all strings
                        // original string(s) length, offset
                        val (osl, oso) = ( u32( oto + 8 * sc ), u32( oto + 8 * sc + 4 ) );
                        // translation string(s) length, offset
                        val (tsl, tso) = ( u32( ott + 8 * sc ), u32( ott + 8 * sc + 4 ) );
                        
                        if (osl > 0 && tsl > 0) {
                            // original string(s)
                            val os = mo.slice( oso, oso + osl + 1);
                            // extract all original forms
                            var (oss, ossp) = (List[String](), 0);
                            for (i <- 0 to os.length-1) {
                                if ( os(i) == 0 ) {
                                    oss = oss ::: new String(os.slice(ossp, i), "utf8") :: Nil;
                                    ossp = i + 1;
                                }
                            }
                            // translation string(s)
                            val ts = mo.slice( tso, tso + tsl + 1);
                            // extract all translation forms
                            var (tss, tssp) = (List[String](), 0);
                            for (i <- 0 to ts.length-1) {
                                if ( ts(i) == 0 ) {
                                    tss = tss ::: new String(ts.slice(tssp, i), "utf8") :: Nil;
                                    tssp = i + 1;
                                }
                            }
                            lcat( oss(0) ) = Array( oss.toArray, tss.toArray );
                        }
                    }
                }
                
            }
        }
        return lcat
    }
    
    /**
     * Parse .PO file
     * @param po sequence of strings from file
     * @return translations map
     */
    def parsePo( po:Seq[String] ):collection.mutable.HashMap[String, Array[String]] = {
        val lcat = new collection.mutable.HashMap[String, Array[String]];
/*        tranMap.clear // clear map
        localeDir = aLocaleDir
        domainName = aDomainName
        // search for translations
        try {
            for (langDir <- new File(localeDir).list()) {
                try {
                    val poFile = new File(localeDir + File.separator + langDir + File.separator + 
                        messagesDir + File.separator + domainName + ".po")
                    val moFile = new File(localeDir + File.separator + langDir + File.separator + 
                        messagesDir + File.separator + domainName + ".mo")
                    if (poFile.exists()) { // parse .po file
                        tranMap(langDir) = new HashMap[String, String]()
                        val fi = new FileInputStream( poFile )
                        val din = new Array[Byte](poFile.length().toInt)
                        fi.read( din ); fi.close()
                        val poStr = new String(din, poEncoding)
                        val poRe = "(?msu:msgid +\"(.*?)\".*?msgstr +\"(.*?)\")".r
                        for (m <- poRe.findAllIn( poStr ).matchData if ((m.groupCount == 2)&&(m.group(1).length > 0)))
                            tranMap(langDir)(m.group(1)) = m.group(2);
                    } else if (moFile.exists()) { // parse .mo file
                        
                    }
                } catch { case _=> if (logger != null) logger.log("Error loading localization directory " + langDir + ".") }
            }
        } catch { case _=> if (logger != null) logger.log("Error loading localization.") }
*/        
        return lcat;
    }
}
