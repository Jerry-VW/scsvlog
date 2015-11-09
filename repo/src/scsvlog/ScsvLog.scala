package scsvlog

import java.awt
import scala.language.{reflectiveCalls,existentials}
import javax.swing.border.{TitledBorder,LineBorder,EtchedBorder}

object ScsvLog extends swing.SimpleSwingApplication with scl.GetText {
    val app = this

    def top = new swing.MainFrame { title = "CSV log"
        val top = this

        // load config
        val ini = new scl.KvIni("./scsvlog.ini")
//        val ini = new scl.KvJdbm("./scsvlog.db")
        
        // load localization
        scl.GetText.init(ini.getI("lang",0))
        
        // set L&F
        javax.swing.UIManager.setLookAndFeel(Config.lafs(ini.getI("laf",0)))

        var channel:scl.Channel = null
        val channelOpened = new java.util.concurrent.atomic.AtomicBoolean(false)

        val bauds = List(75,110,150,300,600,1200,1800,2400,3600,4800,7200,9600,14400,19200,28800,38400,57600,115200,128000,134400,
            161280,201600,230400,256000,268800,403200,460800,614400,806400,921600,1228800,2457600,3000000,6000000,12000000
        )
        
        var portOn:swing.CheckBox         = null
        var portPanel:swing.BoxPanel      = null
        var configPanel:swing.BoxPanel    = null
        var valuesPanel:swing.BoxPanel    = null
        var ipText:swing.TextField        = null
        var connectButton:swing.Button    = null
        var disconnectButton:swing.Button = null
        var statusText:swing.Label        = null
        var chart:scsvlog.Chart           = null
        var xTypeCombo:swing.ComboBox[String] = null
        var xDateFormatText:swing.TextField = null

        val channelsCount = 8
    
        object values {
            val labels = new collection.mutable.ArrayBuffer[swing.Label]
            val width = 10
            
            def setText(i:Int, v:Double) = { labels(i).text = ("%" + width + "s").format( if (v.isNaN) "-" else v.toString ) }
            def setAllText(v:Seq[Double]) = { for (i <- 0 until v.length) setText(i,v(i)) }
        }
    
        object colors {
            val available = List("red","magenta","orange","pink","green","blue","cyan","yellow","gray","lightGray","darkGray","black")
            val current = ini.get("colors","red,magenta,orange,green,blue,cyan,gray,black").split(",")
            def fromName(name:String):awt.Color = {
                if (name.startsWith("#")) awt.Color.decode(name)
                else classOf[awt.Color].getDeclaredField(name).get(null).asInstanceOf[awt.Color]
            }
            def toName(c:awt.Color) = "#" + Integer.toHexString(c.getRGB).substring(2)
            def set(i:Int, c:String):awt.Color = {
                current(i) = c
                values.labels(i).foreground = currentColor(i)
                currentColor(i)
            }
            def currentColor(i:Int) = fromName(current(i))
            
            val chooser = new swing.Dialog(top){
                val chooser  = this
                modal        = true
                visible      = false
                setLocationRelativeTo(top)
                val picker   = new swing.ColorChooser
                var selectedColor = "red"
                var callback: ( String => Unit ) = null
                contents = new swing.BoxPanel(swing.Orientation.Vertical){
                    contents ++= List(
                        picker
                        ,new swing.BoxPanel(swing.Orientation.Horizontal){
                            contents ++= List(
                                new swing.Button(new swing.Action("Ok"){
                                    def apply = { selectedColor = toName(picker.color); chooser.close; callback(selectedColor) }
                                })
                                ,new swing.Button(new swing.Action("Cancel"){ def apply = chooser.close })
                            )
                        }
                    )
                }
                def openColor(c:String, cb:String => Unit ){
                    selectedColor = c
                    picker.color = fromName(c)
                    callback = cb
                    open
                }
            }
        }
        
        // channel poll timer - receive/parse CSV lines
        val pollTimer = new java.util.Timer {
            var firstLine = true
            val readBuf = new collection.mutable.Queue[Byte]
            val lineBuf = new collection.mutable.ArrayBuffer[Byte]
            val lineNum = new java.util.concurrent.atomic.AtomicInteger(0)
            
            // process new line
            def processLine = { //println("RX: " + (new String(lineBuf.toArray, "UTF-8")));
                if (firstLine) firstLine = false;
                else {
                    val l = new String(lineBuf.toArray, "UTF-8")
                    if (l.startsWith("#")) statusText.text = "<html>" + l.substring(1) + "</html>"
                    else {
                        var x:Double = lineNum.getAndIncrement()
                        val ys = l.replaceAll(";","").replaceAll(",","").replaceAll("\r","").split("\\s+").toBuffer[String]
                        while ((ys.length > 0)&&(ys(0).length == 0)) ys.trimStart(1)
                        val y = (ys.map { _.toDouble })
                        if (y.length > 0){
                            if (ini.getI("xType",0) == 1){ x = y(0); y.trimStart(1) }
                            else if (ini.getI("xType",0) == 2) x = System.currentTimeMillis
                            while (y.length < channelsCount) y.append(Double.NaN)
                            values.setAllText(y)
                            if (ini.getB("chartOn",false)) chart.addPoints(x, y)
                        }
                    }
                }
            }
            scheduleAtFixedRate( new java.util.TimerTask {
                def run = {
                    if (channelOpened.get()){
                        try { readBuf ++= channel.read
                            var b = -1
                            while ((readBuf.length > 0)&&(b != '\n')){
                                b = readBuf.dequeue
                                if (b == '\n'){
                                    processLine
                                    lineBuf.clear
                                } else lineBuf += (b & 0xFF).toByte
                            }
                        } catch { case _:Throwable => disconnectButton.action.apply() }
                    } else { readBuf.clear; lineBuf.clear; lineNum.set(0) }
                }
            }, 50, 50)
        }

        contents = new swing.BoxPanel(swing.Orientation.Vertical){
            contents ++= List(
                new swing.BoxPanel(swing.Orientation.Horizontal){
                    border = new EtchedBorder
                    contents ++= List(
                        new swing.CheckBox { portOn = this
                            action = new swing.Action(tr("port")){ def apply() = portPanel.visible = selected }
                            selected = true
                        }
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("config")){ def apply() = configPanel.visible = ini.put("configOn", selected).asInstanceOf[Boolean] }
                            selected = ini.getB("configOn", false)
                        }
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("values")){ def apply() = valuesPanel.visible = ini.put("valuesOn", selected).asInstanceOf[Boolean] }
                            selected = ini.getB("valuesOn", false)
                        }
                        ,new swing.Label(" | ")
                        ,new swing.CheckBox {
                            action = new swing.Action("chart"){ def apply() = ini.put("chartOn", selected) }
                            selected = ini.getB("chartOn",false)
                        }
                        ,new swing.Button(new swing.Action(tr("reset")){ def apply = {
                            chart.clearPoints
                            pollTimer.lineNum.set(0)
                        }})
                        ,new swing.Button(new swing.Action("->PNG"){ def apply = chart.snapshotSave })
                        ,new swing.Label(" | ")
                        ,new swing.Label(" L&F: ")
                        ,new swing.ComboBox(Config.lafsNames){ maximumSize = preferredSize
                            selection.index = ini.getI("laf",0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) => ini.put("laf", selection.index) }
                        }
                        ,new swing.Label(" | ")
                        ,new swing.Label(" lang: ")
                        ,new swing.ComboBox( scl.GetText.displayLangs ){ maximumSize = preferredSize
                            selection.index = ini.getI("lang",0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) => ini.put("lang", selection.index) }
                        }
                        ,new swing.Label(" | ")
                        ,swing.Swing.HGlue
                        ,new swing.Button(new swing.Action(tr("connect")){ def apply = {
                            try {
                                if (channel != null){ channelOpened.set(false); channel.close; channel = null }
                                channel = ini.get("port","socketTCP") match {
                                    case "socketTCP" => new scl.ChannelSocketTCP{ open( ini.get("ip","127.0.0.1:9000") ) }
                                    case "socketUDP" => new scl.ChannelSocketUDP{ open( ini.get("ip","127.0.0.1:9000") ) }
                                    case p:String => new scl.ChannelSerial {
                                        open(p)
                                        propSet("baud",ini.get("baud",9600))
                                        propSet("bits",ini.get("bits",8))
                                        propSet("parity",ini.get("parity","none"))
                                        propSet("stops",ini.get("stops",1.0))
                                    }
                                }
                                chart.clearPoints
                                chart.xAxisFormat(ini.getI("xType",0) == 2, ini.get("xLabelDate","yyyy.MM.dd HH.mm.ss"))
                                
                                connectButton.visible    = false
                                disconnectButton.visible = true
                                portPanel.visible        = false
                                portOn.selected          = false
                                xTypeCombo.enabled       = false
                                xDateFormatText.enabled  = false
                                channelOpened.set(true)
                            } catch { case _:Throwable => }
                        }}){ connectButton = this }
                        ,new swing.Button(new swing.Action(tr("disconnect")){ def apply = {
                            channelOpened.set(false)
                            if (channel != null){ channel.close; channel = null }
                            connectButton.visible    = true
                            disconnectButton.visible = false
                            portOn.selected          = true
                            portPanel.visible        = true
                            xTypeCombo.enabled       = true
                            xDateFormatText.enabled  = true
                        }}){ disconnectButton = this; visible = false }
                    )
                }
                ,new swing.BoxPanel(swing.Orientation.Horizontal){ portPanel = this; visible = true
                    border = new TitledBorder( new EtchedBorder, tr("Port"), TitledBorder.LEFT, TitledBorder.TOP )
                    contents ++= List(
                        new swing.ComboBox(List("socketTCP","socketUDP") ::: scl.ChannelSerial.channels.sorted.toList){ maximumSize = preferredSize
                            selection.item = ini.get("port", "socketTCP")
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("port", selection.item)
                                ipText.visible = selection.item.startsWith("socket")
                                portPanel.revalidate
                            }
                        }
                        ,new swing.Label(" ")
                        ,new swing.TextField(18){ maximumSize = preferredSize; ipText  = this
                            visible = ini.get("port","socketTCP").startsWith("socket")
                            font = new awt.Font( "Monospaced", awt.Font.BOLD, font.getSize )
                            text    = ini.get("ip","127.0.0.1:9000")
                            listenTo(this)
                            reactions += { case swing.event.EditDone(_) => ini.put("ip", text) }
                            verifier = v => {
                                println( v.matches("^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$") )
                                v.matches("^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$")
                            }
                        }
                        ,new swing.Label(" | ")
                        ,new swing.Label(" baud:")
                        ,new swing.ComboBox(bauds){ maximumSize = preferredSize
                            makeEditable()(swing.ComboBox.intEditor)
                            selection.item = ini.getI("baud",9600)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("baud", selection.item)
                            }
                        }
                        ,new swing.Label(tr(" bits:"))
                        ,new swing.ComboBox(List(5,6,7,8)){ maximumSize = preferredSize
                            selection.item = ini.getI("bits",8)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("bits", selection.item)
                            }
                        }
                        ,new swing.Label(tr(" parity:"))
                        ,new swing.ComboBox(List("none","even","odd","mark","space")){ maximumSize = preferredSize
                            selection.item = ini.get("parity", "none")
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("parity", selection.item)
                            }
                        }
                        ,new swing.Label(tr(" stops:"))
                        ,new swing.ComboBox(List(1.0,1.5,2.0)){ maximumSize = preferredSize
                            selection.item = ini.getD("stops",1.0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("stops", selection.item)
                            }
                        }
                        ,swing.Swing.HGlue
                    )
                }
                ,new swing.BoxPanel(swing.Orientation.Vertical){ configPanel = this; visible = ini.getB("configOn", false); contents ++= List(
                    new swing.BoxPanel(swing.Orientation.Horizontal){ val generalConfigPanel = this; border = new EtchedBorder
                        contents ++= List(
                            new swing.Label(" x:")
                            ,new swing.TextField(5){ maximumSize = preferredSize
                                text = ini.get("xLabel","x")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("xLabel",text); chart.xName(text) }
                            }
                            ,new swing.Label(" ")
                            ,new swing.ComboBox(List("line №","1st col","date")){ maximumSize = preferredSize; xTypeCombo = this
                                selection.index = ini.getI("xType",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("xType",selection.index)
                                    xDateFormatText.visible = (selection.index == 2)
                                    generalConfigPanel.revalidate
                                }
                            }
                            ,new swing.Label(" ")
                            ,new swing.TextField(20){ maximumSize = preferredSize; visible = (ini.getI("xType",0) == 2); xDateFormatText = this
                                font = new awt.Font( "Monospaced", awt.Font.BOLD, font.getSize )
                                text = ini.get("xLabelDate","yyyy.MM.dd HH.mm.ss")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("xLabelDate",text) }
                            }
                            ,new swing.Label(tr(" limit: "))
                            ,new swing.ComboBox(List(0,100,500,1000,5000,10000,20000,50000,100000)){ maximumSize = preferredSize
                                makeEditable()(swing.ComboBox.intEditor)
                                selection.item = ini.getI("xLimit",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("xLimit", selection.item)
                                    chart.xLimit.set( selection.item )
                                }
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(" y:")
                            ,new swing.TextField(5){ maximumSize = preferredSize
                                text = ini.get("yLabel","y")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("yLabel",text); chart.yName(text) }
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(tr("window"))
                            ,new swing.Label(" x: ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winXmin",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winXmin", text);
                                    chart.rangeX(ini.getD("winXmin",0),ini.getD("winXmax",0))
                                }
                            }
                            ,new swing.Label(" .. ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winXmax",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winXmax", text);
                                    chart.rangeX(ini.getD("winXmin",0),ini.getD("winXmax",0))
                                }
                            }
                            ,new swing.Label(" y: ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winYmin",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winYmin", text);
                                    chart.rangeY(ini.getD("winYmin",0),ini.getD("winYmax",0))
                                }
                            }
                            ,new swing.Label(" .. ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winYmax",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winYmax", text);
                                    chart.rangeY(ini.getD("winYmin",0),ini.getD("winYmax",0))
                                }
                            }
                            ,swing.Swing.HGlue
                        )
                    }
                    ,new swing.GridPanel(5,channelsCount+1){ maximumSize = new swing.Dimension(Integer.MAX_VALUE,100)
                        border = new EtchedBorder
                        contents += new swing.Label(tr("show"))
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.CheckBox { maximumSize = preferredSize
                                action = new swing.Action(i.toString){
                                    def apply() = { ini.put("show"+i, selected); chart.traceShow(i-1,selected) }
                                }
                                selected = ini.getB("show"+i, false)
                        })
                        contents += new swing.Label(tr("name"))
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.TextField(8){
                                text = ini.get("name"+i,"Y"+i)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("name"+i, text); chart.traceName(i-1,text) }
                        })
                        contents += new swing.Label(tr("color"))
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.Button(""){ val _but = this; background = colors.currentColor(i-1);
                                action = new swing.Action(""){ def apply = {
                                    colors.chooser.openColor( colors.current(i-1), { c:String => _but.background = colors.set(i-1,c); chart.traceColor(i-1,colors.currentColor(i-1)) })
                            }}}
                        )
                        contents += new swing.Label(tr("width"))
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.FormattedTextField(java.text.NumberFormat.getNumberInstance){
                                text = ini.get("width"+i,1)
                                listenTo(this)
                                reactions += { case swing.event.ValueChanged(_) if (!this.hasFocus && text.length > 0 && editValid) =>
                                    ini.put("width"+i,text); chart.traceStyle(i-1,ini.getD("width"+i,1.0),ini.getI("style"+i,0))
                                }
                        })
                        contents += new swing.Label(tr("style"))
                        contents ++= (for (i <- 1 to channelsCount)
                            yield (new swing.ComboBox(List("───────","· · · ·","─ ─ ─ ─")){
                                selection.index = ini.getI("style"+i,0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("style"+i,selection.index)
                                    chart.traceStyle(i-1,ini.getD("width"+i,1.0),ini.getI("style"+i,0))
                                }
                            })
                        )
                    }
                ); border = new TitledBorder( new EtchedBorder, tr("Config"), TitledBorder.LEFT, TitledBorder.TOP )}
                ,new swing.BoxPanel(swing.Orientation.Horizontal){ valuesPanel = this; visible = ini.getB("valuesOn", false)
                    border = new EtchedBorder
                    for (i <- 0 until channelsCount){
                        val v = new swing.Label { font = new awt.Font("Monospace", awt.Font.BOLD, 30) }
                        contents += v
                        values.labels += v
                        values.setText(i,Double.NaN)
                        colors.set(i,colors.current(i))
                    }
                    contents += swing.Swing.HGlue
                }
                ,new scsvlog.Chart { top.chart = this; xName(ini.get("xLabel","x")); yName(ini.get("yLabel","y"));
                    for (i <- 1 to channelsCount){
                        addTrace(ini.get("name"+i,"Y"+i), colors.currentColor(i-1), ini.getB("show"+i,false),
                            ini.getD("width"+i,1.0), ini.getI("style"+i,0)
                        )
                    }
                    rangeX(ini.getD("winXmin",0),ini.getD("winXmax",0))
                    rangeY(ini.getD("winYmin",0),ini.getD("winYmax",0))
                    xLimit.set( ini.getI("xLimit",0) )
                }
                ,new swing.BoxPanel(swing.Orientation.Horizontal){
                    contents ++= List(
                        new swing.Label(""){ statusText = this; visible = false; font = new awt.Font("Monospace", awt.Font.BOLD, 18) }
                        ,swing.Swing.HGlue
                )}
            )
        }

        // restore window geometry
        minimumSize = new swing.Dimension( 800, 600 );
        bounds      = new swing.Rectangle( ini.getI("x",0), ini.getI("y",0), ini.getI("w",800), ini.getI("h",600) )
        preferredSize = new swing.Dimension( bounds.width, bounds.height )
        if (ini.getB("maximized", false)) maximize
        
        // save configuration on close
        override def closeOperation() {
            ini.put("x", bounds.x); ini.put("y", bounds.y);
            ini.put("w", bounds.width); ini.put("h", bounds.height);
            ini.put("maximized", maximized)
            ini.put("colors", colors.current.mkString(","))
            ini.save
            super.closeOperation()
        }
    }
}
