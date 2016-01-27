package scsvlog

import java.awt
import scala.language.{reflectiveCalls,existentials}
import javax.swing.border.{TitledBorder,LineBorder,EtchedBorder}
import java.util.concurrent.atomic
import scl.SwUtil
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}, org.eclipse.{jetty}

object ScsvLog extends swing.SimpleSwingApplication with scl.GetText {
    val app = this

    def top = new swing.MainFrame { title = Config.title
        val top                               = this
        val ini                               = Config.ini   

        var channel:scl.Channel                 = null
        val channelOpened                       = new atomic.AtomicBoolean(false)

        var portOn:swing.CheckBox               = null
        var portPanel:swing.BoxPanel            = null
        var serialPanel:swing.BoxPanel          = null
        var configPanel:swing.BoxPanel          = null
        var valuesPanel:swing.BoxPanel          = null
        var ipText:swing.TextField              = null
        var connectButton:swing.Button          = null
        var disconnectButton:swing.Button       = null
        var statusText:swing.Label              = null
        var chart:scl.Chart                     = null
        var xTypeCombo:swing.ComboBox[String]   = null
        var xDateFormatText:swing.TextField     = null
        var csvLoadButton:swing.Button          = null
        var csvSaveButton:swing.Button          = null
        
        val channelsCount                       = 10

        var serverPortCombo:swing.ComboBox[Int] = null
        var server:scl.ServerJetty              = null
        val serverData                          = new java.util.concurrent.atomic.AtomicReferenceArray[Double](channelsCount){
            for (i <- 0 until channelsCount) set(i, Double.NaN)
        }
    
        object values {
            val labels = new collection.mutable.ArrayBuffer[swing.Label]
            val width = 10
            
            def setText(i:Int, v:Double) = { labels(i).text = ("%" + width + "s").format( if (v.isNaN) "-" else v.toString ) }
            def setAllText(v:Seq[Double]) = { for (i <- 0 until v.length) setText(i,v(i)) }
        }
    
        object colors {
            val available = List("red","magenta","orange","pink","green","blue","cyan","yellow","gray","lightgray","darkgray","black","saddlebrown","tan")
            val current = ini.get("colors",available.mkString(",")).split(",")
            def set(i:Int, cs:String):awt.Color = {
                current(i) = cs
                val c = SwUtil.svgColor(cs)
                values.labels(i).foreground = c
                c
            }
            
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
                                    def apply = { selectedColor = SwUtil.svgName(picker.color); chooser.close; callback(selectedColor) }
                                })
                                ,new swing.Button(new swing.Action("Cancel"){ def apply = chooser.close })
                            )
                        }
                    )
                }
                def openColor(c:String, cb:String => Unit ){
                    selectedColor = c
                    picker.color = SwUtil.svgColor(c)
                    callback = cb
                    open
                }
            }
        }
        
        // CSV operations
        object csv {
            val lines  = new collection.mutable.ArrayBuffer[String]
            val dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")//"yyyy-MM-dd'T'HH:mm:ss.SSSZ")
            val saveDateFormatter = new java.text.SimpleDateFormat("yyMMdd_HHmm")
            val separator = ";"
            // save CSV dialog
            val dialog = new swing.FileChooser{
                fileFilter   = new javax.swing.filechooser.FileNameExtensionFilter("CSV file","csv")
                selectedFile = new java.io.File(ini.get("csvFile", "data.csv"))
            }
            def save = {
                dialog.title = tr("Save CSV...")
                dialog.selectedFile = new java.io.File(dialog.selectedFile.getParent + java.io.File.separator + saveDateFormatter.format(new java.util.Date) + ".csv")
                if (lines.nonEmpty && (dialog.showSaveDialog(null) == swing.FileChooser.Result.Approve)){
                    if ( !dialog.selectedFile.exists || (dialog.selectedFile.exists &&
                        (swing.Dialog.showConfirmation(null, tr("Replace ?"), tr("Confirm replace"), swing.Dialog.Options.YesNo, swing.Dialog.Message.Question) == swing.Dialog.Result.Yes))){
                        try {
                            java.nio.file.Files.write(dialog.selectedFile.toPath, lines.mkString("\r\n").getBytes("UTF-8"))
                        } catch { case _:Throwable => }
                        ini.put("csvFile", csv.dialog.selectedFile.getCanonicalPath )
                    }
                }
            }
            def load = {
                dialog.title = tr("Load CSV...")
                if (dialog.showOpenDialog(null) == swing.FileChooser.Result.Approve){
                    ini.put("csvFile", dialog.selectedFile.getCanonicalPath )
                    resetAll
                    try {
                        io.Source.fromFile(dialog.selectedFile, "UTF-8").getLines.foreach { l =>
                            val ls = l.split(separator)
                            if (ls.length > 2){
                                chart.addPoints(
                                    if (ls(0).contains(":")) dateFormatter.parse(ls(0)).getTime else ls(0).toDouble,
                                    ls.tail.map { _.toDouble }
                                )
                            }
                        }
                        top.title = Config.title + " : " + dialog.selectedFile.getCanonicalPath
                    } catch { case _:Throwable => }
                }
            }
        }
        
        // channel poll timer - receive/parse CSV lines
        val pollTimer = new java.util.Timer {
            
            val firstLine = new atomic.AtomicBoolean(true)
            val readBuf   = new collection.mutable.Queue[Byte]
            val lineBuf   = new collection.mutable.ArrayBuffer[Byte]
            val lineNum   = new atomic.AtomicInteger(0)
            val lineSkip  = new atomic.AtomicInteger(0)
            
            // process new line
            def processLine = {
                if (firstLine.get) firstLine.set(false)
                else {
                    val l = new String(lineBuf.toArray, "UTF-8")
                    if (l.startsWith("#")) statusText.text = "<html>" + l.substring(1) + "</html>"
                    else {
                        var x:Double = lineNum.getAndIncrement()
                        val ys = l.replaceAll(";","").replaceAll(",","").replaceAll("\r","").split("\\s+").toBuffer[String]
                        while ((ys.length > 0)&&(ys(0).length == 0)) ys.trimStart(1)
                        val y = (ys.map { _.toDouble }); //println(y.mkString(","))
                        for (i <- 0 until y.length if (!y(i).isNaN)){
                            y(i) += ini.getD("yAdd"+i,0)
                            serverData.lazySet(i, y(i))
                        }
                        if (y.length > 0){
                            if (ini.getI("xType",0) == 1){ x = y(0); y.trimStart(1) }
                            else if (ini.getI("xType",0) == 2) x = System.currentTimeMillis
                            while (y.length < channelsCount) y.append(Double.NaN)
                            values.setAllText(y)
                            if (lineSkip.get == 0){
                                lineSkip.set(ini.getI("xSkip",0))
                                if (ini.getB("chartOn",false)){
                                    chart.addPoints(x, y)
                                    csv.lines += (if (ini.getI("xType",0) == 2) csv.dateFormatter.format(new java.util.Date(x.toLong)) else x.toString) +
                                        csv.separator + y.mkString(csv.separator)
                                }
                            } else lineSkip.decrementAndGet
                        }
                    }
                }
            }
            scheduleAtFixedRate( new java.util.TimerTask {
                def run = {
                    if (channelOpened.get()){
                        try {
                            readBuf ++= channel.read
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

        // clear all data
        def resetAll = {
            chart.clearPoints
            chart.xAxisFormat(ini.getI("xType",0) == 2, ini.get("xLabelDate","yyyy.MM.dd HH.mm.ss"))
            pollTimer.lineNum.set(0)
            csv.lines.clear
        }
        
        contents = new swing.BoxPanel(swing.Orientation.Vertical){
            contents ++= List(
                new swing.BoxPanel(swing.Orientation.Horizontal){
                    border = new EtchedBorder
                    contents ++= List(
                        new swing.CheckBox { portOn = this
                            action = new swing.Action(tr("port")){ def apply() = portPanel.visible = selected }
                            selected = true
                            tooltip = tr("Show port configuration")
                        }
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("config")){ def apply() = configPanel.visible = ini.put("configOn", selected).asInstanceOf[Boolean] }
                            selected = ini.getB("configOn", false)
                            tooltip = tr("Show chart/channels configuration")
                        }
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("values")){ def apply() = valuesPanel.visible = ini.put("valuesOn", selected).asInstanceOf[Boolean] }
                            selected = ini.getB("valuesOn", false)
                            tooltip = tr("Show values")
                        }
                        ,new swing.Label(" | ")
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("chart")){ def apply() = ini.put("chartOn", selected) }
                            selected = ini.getB("chartOn",false)
                            tooltip = tr("Enable chart")
                        }
                        ,new swing.Button(new swing.Action(tr("reset")){ def apply = resetAll }){ tooltip = tr("Clear all data") }
                        ,new swing.Label(" | ")
                        ,new swing.Button(new swing.Action("->PNG"){ def apply = {
                            if (chart.snapshotSave(true)) ini.put("pngFile", chart.snapshotDialog.selectedFile.getCanonicalPath)
                        }}){ tooltip = tr("Save chart to PNG") }
                        ,new swing.Label(" | CSV ")
                        ,new swing.Button(new swing.Action(tr("save")){ def apply = { csv.save
                        }}){ tooltip = tr("Save data to CSV"); csvSaveButton = this }
                        ,new swing.Button(new swing.Action(tr("load")){ def apply = { csv.load
                        }}){ tooltip = tr("Load data from CSV"); csvLoadButton = this }
//                        ,new swing.CheckBox {
//                            action = new swing.Action(tr("auto")){ def apply() = ini.put("csvAuto", selected) }
//                            selected = false // ini.getB("csvAuto",false)
//                            tooltip = tr("Automatic periodic CSV save")
//                        }
                        ,new swing.Label(" | ")
                        ,new swing.Label(" L&F: ")
                        ,new swing.ComboBox(Config.lafsNames){ maximumSize = preferredSize
                            selection.index = ini.getI("laf",0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("laf", selection.index)
                                javax.swing.UIManager.setLookAndFeel(Config.lafs(selection.index))
                                javax.swing.SwingUtilities.updateComponentTreeUI(top.peer)
                            }
                            tooltip = tr("Select current Look&Feel")
                        }
                        ,new swing.Label(" | ")
                        ,new swing.Label(" lang: ")
                        ,new swing.ComboBox( scl.GetText.displayLangs ){ maximumSize = preferredSize
                            selection.index = ini.getI("lang",0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("lang", selection.index)
                            }
                            tooltip = tr("Select current language")
                        }
                        ,new swing.Label(" | ")
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("server")){ def apply() = {
                                ini.put("server", selected)
                                serverPortCombo.visible = selected
                            }}
                            selected = ini.getB("server",false)
                            tooltip = tr("Enable server")
                        }
                        ,new swing.ComboBox(List(80,8080,8090,9000)){ maximumSize = preferredSize
                            visible = ini.getB("server",false)
                            makeEditable()(swing.ComboBox.intEditor)
                            selection.item = ini.getI("serverPort",8090)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini.put("serverPort", selection.item)
                            }
                            tooltip = tr("Server port")
                            serverPortCombo = this
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
                                
                                if (channel.opened){
                                    top.title = Config.title + " : " + channel.name
                                    resetAll
                                    
                                    connectButton.visible    = false
                                    portPanel.visible        = false
                                    portOn.selected          = false
                                    xTypeCombo.enabled       = false
                                    xDateFormatText.enabled  = false
                                    disconnectButton.visible = true
                                    csvLoadButton.enabled    = false
                                    channelOpened.set(true)
                                }
                            } catch { case _:Throwable => }
                        }}){ connectButton = this; tooltip = tr("Connect to port") }
                        ,new swing.Button(new swing.Action(tr("disconnect")){ def apply = { println("disconnect...")
                            channelOpened.set(false)
                            if (channel != null){ channel.close; channel = null }
                            top.title                    = Config.title
                            connectButton.visible        = true
                            disconnectButton.visible     = false
                            portOn.selected              = true
                            portPanel.visible            = true
                            xTypeCombo.enabled           = true
                            xDateFormatText.enabled      = true
                            csvLoadButton.enabled        = true
                        }}){ disconnectButton = this; visible = false; tooltip = tr("Disconnect from port") }
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
                                serialPanel.visible = !ipText.visible
                                portPanel.revalidate
                            }
                            tooltip = tr("Select channel")
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
                            tooltip = tr("Socket IP address and port")
                        }
                        ,new swing.BoxPanel(swing.Orientation.Horizontal){ contents ++= List(
                            new swing.Label(tr(" baud:"))
                            ,new swing.ComboBox(Config.bauds){ maximumSize = preferredSize
                                makeEditable()(swing.ComboBox.intEditor)
                                selection.item = ini.getI("baud",9600)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("baud", selection.item)
                                }
                                tooltip = tr("Serial port baud rate")
                            }
                            ,new swing.Label(tr(" bits:"))
                            ,new swing.ComboBox(List(5,6,7,8)){ maximumSize = preferredSize
                                selection.item = ini.getI("bits",8)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("bits", selection.item)
                                }
                                tooltip = tr("Serial port data bits")
                            }
                            ,new swing.Label(tr(" parity:"))
                            ,new swing.ComboBox(List("none","even","odd","mark","space")){ maximumSize = preferredSize
                                selection.item = ini.get("parity", "none")
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("parity", selection.item)
                                }
                                tooltip = tr("Serial port parity")
                            }
                            ,new swing.Label(tr(" stops:"))
                            ,new swing.ComboBox(List(1.0,1.5,2.0)){ maximumSize = preferredSize
                                selection.item = ini.getD("stops",1.0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("stops", selection.item)
                                }
                                tooltip = tr("Serial port stop bits")
                            }
                        ); serialPanel = this; visible = !ini.get("port","socketTCP").startsWith("socket") }
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
                                tooltip = tr("X axis label")
                            }
                            ,new swing.Label(" ")
                            ,new swing.ComboBox(List(tr("line №"),tr("1st col"),tr("date"))){ maximumSize = preferredSize; xTypeCombo = this
                                selection.index = ini.getI("xType",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("xType",selection.index)
                                    xDateFormatText.visible = (selection.index == 2)
                                    generalConfigPanel.revalidate
                                }
                                tooltip = tr("X axis type")
                            }
                            ,new swing.Label(" ")
                            ,new swing.TextField(20){ maximumSize = preferredSize; visible = (ini.getI("xType",0) == 2); xDateFormatText = this
                                font = new awt.Font( "Monospaced", awt.Font.BOLD, font.getSize )
                                text = ini.get("xLabelDate","yyyy.MM.dd HH.mm.ss")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("xLabelDate",text) }
                                tooltip = tr("<html>X axis date/time format:<br>" +
                                    "<b>y</b> - year, <b>M</b> - month, <b>d</b> - date;<br>" +
                                    "<b>H</b> - hour, <b>m</b> - minute, <b>s</b> - second;<br>" +
                                    "<b>'single quotes'</b> - plain text.</html>")
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
                                tooltip = tr("Limit number of points")
                            }
                            ,new swing.Label(tr(" skip: "))
                            ,new swing.ComboBox(List(0,1,4,9,19,49,59,99,199,499,999)){ maximumSize = preferredSize
                                makeEditable()(swing.ComboBox.intEditor)
                                selection.item = ini.getI("xSkip",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("xSkip", selection.item)
                                }
                                tooltip = tr("Skip CSV lines")
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(" y:")
                            ,new swing.TextField(5){ maximumSize = preferredSize
                                text = ini.get("yLabel","y")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("yLabel",text); chart.yName(text) }
                                tooltip = tr("Y axis label")
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
                                tooltip = tr("X window minimum")
                            }
                            ,new swing.Label(" .. ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winXmax",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winXmax", text);
                                    chart.rangeX(ini.getD("winXmin",0),ini.getD("winXmax",0))
                                }
                                tooltip = tr("X window maximum")
                            }
                            ,new swing.Label(" y: ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winYmin",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winYmin", text);
                                    chart.rangeY(ini.getD("winYmin",0),ini.getD("winYmax",0))
                                }
                                tooltip = tr("Y window minimum")
                            }
                            ,new swing.Label(" .. ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini.get("winYmax",0)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("winYmax", text);
                                    chart.rangeY(ini.getD("winYmin",0),ini.getD("winYmax",0))
                                }
                                tooltip = tr("Y window maximum")
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(tr("grid: "))
                            ,new swing.CheckBox {
                                action = new swing.Action("x"){ def apply() = { ini.put("gridX", selected); chart.showGridX(selected) }}
                                selected = ini.getB("gridX",false)
                                tooltip = tr("Enable X grid")
                            }
                            ,new swing.CheckBox {
                                action = new swing.Action("y"){ def apply() = {
                                    ini.put("gridY", selected)
                                    chart.showGridY(selected)
                                    chart.showGridYRight(selected)
                                }}
                                selected = ini.getB("gridY",false)
                                tooltip = tr("Enable Y grid")
                            }
                            ,swing.Swing.HGlue
                        )
                    }
                    ,new swing.GridPanel(7,channelsCount+1){ maximumSize = new swing.Dimension(Integer.MAX_VALUE,150)
                        border = new EtchedBorder
                        contents += new swing.Label(tr("show")){ tooltip = tr("Trace visibility") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.CheckBox { maximumSize = preferredSize
                                action = new swing.Action(i.toString){
                                    def apply() = { ini.put("show"+i, selected); chart.traceShow(i-1,selected) }
                                }
                                selected = ini.getB("show"+i, false)
                        })
                        contents += new swing.Label(tr("name")){ tooltip = tr("Trace name") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.TextField(8){
                                text = ini.get("name"+i,"Y"+i)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini.put("name"+i, text); chart.traceName(i-1,text) }
                        })
                        contents += new swing.Label(tr("Y axis")){ tooltip = tr("Number of Y axis") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield (new swing.ComboBox(List(1,2)){
                                selection.index = ini.getI("yAxis"+i,0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini.put("yAxis"+i,selection.index)
                                }
                            })
                        )
                        contents += new swing.Label(tr("color")){ tooltip = tr("Trace color") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.Button(""){ val _but = this; background = SwUtil.svgColor(colors.current(i-1));
                                action = new swing.Action(""){ def apply = {
                                    colors.chooser.openColor( colors.current(i-1), { c:String => _but.background = colors.set(i-1,c); chart.traceColor(i-1,colors.current(i-1)) })
                            }}}
                        )
                        contents += new swing.Label(tr("width")){ tooltip = tr("Trace width") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.FormattedTextField(java.text.NumberFormat.getNumberInstance){
                                text = ini.get("width"+i,1)
                                listenTo(this)
                                reactions += { case swing.event.ValueChanged(_) if (!this.hasFocus && text.length > 0 && editValid) =>
                                    ini.put("width"+i,text); chart.traceStyle(i-1,ini.getD("width"+i,1.0),ini.getI("style"+i,0))
                                }
                        })
                        contents += new swing.Label(tr("style")){ tooltip = tr("Trace style") }
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
                        contents += new swing.Label("Δ"){ tooltip = tr("Value correction") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.FormattedTextField(java.text.NumberFormat.getNumberInstance){
                                text = ini.get("yAdd"+i,0)
                                listenTo(this)
                                reactions += { case swing.event.ValueChanged(_) if (!this.hasFocus && text.length > 0 && editValid) =>
                                    ini.put("yAdd"+i,text)
                                }
                        })
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
                ,new scl.Chart { top.chart = this; xName(ini.get("xLabel","x")); yName(ini.get("yLabel","y"));
                    addAxisRight(false)
                    for (i <- 1 to channelsCount){
                        addTrace(ini.get("name"+i,"Y"+i), colors.current(i-1), ini.getB("show"+i,false),
                            ini.getD("width"+i,1.0), ini.getI("style"+i,0), ini.getI("yAxis"+i,0) == 1
                        )
                        if (ini.getI("yAxis"+i,0) == 1) showAxisRight(true)
                    }
                    rangeX(ini.getD("winXmin",0),ini.getD("winXmax",0))
                    rangeY(ini.getD("winYmin",0),ini.getD("winYmax",0))
                    xLimit.set( ini.getI("xLimit",0) )
                    showGridX(ini.getB("gridX",false))
                    showGridY(ini.getB("gridY",false))
                    showGridYRight(ini.getB("gridY",false))
                    snapshotDialog.selectedFile = new java.io.File(ini.get("pngFile", "snapshot.png"))
                }
                ,new swing.BoxPanel(swing.Orientation.Horizontal){
                    contents ++= List(
                        new swing.Label(""){ statusText = this; visible = false; font = new awt.Font("Monospace", awt.Font.BOLD, 18) }
                        ,swing.Swing.HGlue
                )}
            )
        }

        // restore window geometry
        minimumSize   = new swing.Dimension( 800, 600 );
        bounds        = new swing.Rectangle( ini.getI("x",0), ini.getI("y",0), ini.getI("w",800), ini.getI("h",600) )
        preferredSize = new swing.Dimension( bounds.width, bounds.height )
        if (ini.getB("maximized", false)) maximize
        
        // start server
        if (ini.getB("server",false)){
            new java.lang.Thread {
                override def run = {
                    server = new scl.ServerJetty( ini.getI("serverPort",8090), "./static", 0 ){
                        override def newHandler(target:String, baseRequest:jetty.server.Request, request:HttpServletRequest, response:HttpServletResponse) =
                            new scl.HandlerJetty(target, baseRequest, request, response){
                            val _handler = this
                            override def handle:Boolean = {
                                try { target match {
                                    case "/values.json" =>
                                        response.setContentType("application/json")
                                        responseStr = "[" + (for (i <- 0 until channelsCount) yield serverData.get(i)).mkString(",") + "]"
                                        handled = true
                                }} catch { case _:Throwable => }
                                super.handle
                            }
                        }
                    }
                    server.start
                }
                start
            }
        }
        
        // save configuration on close
        override def closeOperation() {
            ini.put("x", bounds.x); ini.put("y", bounds.y);
            ini.put("w", bounds.width); ini.put("h", bounds.height);
            ini.put("maximized", maximized)
            ini.put("colors", colors.current.mkString(","))
            ini.save
            if (server != null) server.stop(0)
            super.closeOperation()
        }
    }
}
