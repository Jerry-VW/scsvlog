package scsvlog

object Config {
    val lafs = (javax.swing.UIManager.getInstalledLookAndFeels.toList.map( lf => lf.getClassName )) :::
        List("com.pagosoft.plaf.PgsLookAndFeel",
            "com.jtattoo.plaf.acryl.AcrylLookAndFeel","com.jtattoo.plaf.aero.AeroLookAndFeel","com.jtattoo.plaf.aluminium.AluminiumLookAndFeel",
            "com.jtattoo.plaf.bernstein.BernsteinLookAndFeel","com.jtattoo.plaf.fast.FastLookAndFeel","com.jtattoo.plaf.graphite.GraphiteLookAndFeel",
            "com.jtattoo.plaf.hifi.HiFiLookAndFeel","com.jtattoo.plaf.luna.LunaLookAndFeel","com.jtattoo.plaf.mcwin.McWinLookAndFeel",
            "com.jtattoo.plaf.mint.MintLookAndFeel","com.jtattoo.plaf.noire.NoireLookAndFeel","com.jtattoo.plaf.smart.SmartLookAndFeel",
            "com.jtattoo.plaf.texture.TextureLookAndFeel"
        )
    val lafsNames = lafs.map { ln => ln.split("\\.").last.replace("LookAndFeel","") }
    
    // load config
    val ini = new scl.KvIni("./scsvlog.ini")
//        val ini = new scl.KvJdbm("./scsvlog.db")
    
    // load localization
    scl.GetText.init(ini("lang",0))
    
    val title = "CSV log"

    // set L&F
    javax.swing.UIManager.setLookAndFeel(lafs(ini("laf",0)))

    val bauds = List(75,110,150,300,600,1200,1800,2400,3600,4800,7200,9600,14400,19200,28800,38400,57600,115200,128000,134400,
        161280,201600,230400,256000,268800,403200,460800,614400,806400,921600,1228800,2457600,3000000,6000000,12000000
    )
}
