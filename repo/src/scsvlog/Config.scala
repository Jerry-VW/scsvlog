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
}
