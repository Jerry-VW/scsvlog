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
        val top                                 = this
        val ini                                 = Config.ini   

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
        
        val channelsCount                       = 20
        val channelsInRow                       = 10

        var serverPortCombo:swing.ComboBox[Int] = null
        var server:scl.ServerJetty              = null
        val serverData                          = new java.util.concurrent.atomic.AtomicReferenceArray[Double](channelsCount){
            for (i <- 0 until channelsCount) set(i, Double.NaN)
        }
    
        object values {
            val labels = new collection.mutable.ArrayBuffer[swing.Label]
            val width = 10
            
            def setText(i:Int, v:Double) = if (i < channelsCount){ labels(i).text = ("%" + width + "s").format( if (v.isNaN) "-" else v.toString ) }
            def setAllText(v:Seq[Double]) = swing.Swing.onEDT( { for (i <- 0 until v.length) setText(i,v(i)) } )
        }
    
        object colors {
            val available = Seq("red","magenta","orange","pink","green","blue","cyan","yellow","gray","lightgray","darkgray","black","saddlebrown","tan","brown","darkkhaki","beige","violet","darksalmon","indianred")
            val current = new collection.mutable.ArrayBuffer[String]{
                this.++=(available)
                val cl = ini("colors").split(",")
                for (i <- 0 until cl.length if (cl(i).nonEmpty)) this.update(i,cl(i))
            }
            def set(i:Int, cs:String):awt.Color = {
                current(i) = cs
                val c = SwUtil.svgColor(cs)
                values.labels(i).foreground = c
                c
            }
            def get(i:Int) = if (i < current.length) current(i) else if (i < available.length) available(i) else "black";
            
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
                                new swing.ComboBox(List("") ::: SwUtil.svgColors.keys.toList.sorted){ maximumSize = preferredSize
                                    listenTo(selection)
                                    reactions += { case swing.event.SelectionChanged(i) => if (selection.item != "") picker.color = SwUtil.svgColor(selection.item) }
                                    tooltip = tr("Standard color")
                                }
                                ,new swing.Label("  ")
                                ,new swing.Button(new swing.Action("Ok"){
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
                selectedFile = new java.io.File(ini("csvFile", "data.csv"))
            }
            def save = {
                dialog.title = tr("Save CSV...")
                dialog.selectedFile = new java.io.File(dialog.selectedFile.getParent + java.io.File.separator + saveDateFormatter.format(new java.util.Date) + ".csv")
                if (lines.nonEmpty && (dialog.showSaveDialog(null) == swing.FileChooser.Result.Approve)){
                    if ( !dialog.selectedFile.exists || (dialog.selectedFile.exists &&
                        (swing.Dialog.showConfirmation(null, tr("Replace ?"), tr("Confirm replace"), swing.Dialog.Options.YesNo, swing.Dialog.Message.Question) == swing.Dialog.Result.Yes))){
                        try {
                            java.nio.file.Files.write(dialog.selectedFile.toPath, lines.mkString("\r\n").getBytes("UTF-8"))
                        } catch { case _:Exception => }
                        ini("csvFile") = csv.dialog.selectedFile.getCanonicalPath
                    }
                }
            }
            def load = {
                dialog.title = tr("Load CSV...")
                if (dialog.showOpenDialog(null) == swing.FileChooser.Result.Approve){
                    ini("csvFile") = dialog.selectedFile.getCanonicalPath
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
                    } catch { case _:Exception => }
                }
            }
        }
        
        // channel poll timer - receive/parse CSV lines
        object poll {
            
            val firstLine = new atomic.AtomicBoolean(true)
            val readBuf   = new collection.mutable.Queue[Byte]
            val lineBuf   = new collection.mutable.ArrayBuffer[Byte]
            val lineNum   = new atomic.AtomicInteger(0)
            val lineSkip  = new atomic.AtomicInteger(0)
            val linesIn   = new java.util.concurrent.ConcurrentLinkedQueue[String]
            
            // process new line
            val lineTimer = new java.util.Timer { scheduleAtFixedRate( new java.util.TimerTask { def run = {
                if (!linesIn.isEmpty){
                    val l = linesIn.poll
                    if (firstLine.get) firstLine.set(false)
                    else try {
                        if (l.startsWith("#")){
                            statusText.text = "<html>" + l.substring(1).replace("\r","").replace("\n","") + "</html>"
                            if (!statusText.visible) statusText.visible = true
                        } else {
                            var x:Double = lineNum.getAndIncrement()
                            val ys = l.replace(";","").replace(",","").replace("\r","").replace("\n","").split("\\s+").toBuffer[String]
                            while ((ys.length > 0)&&(ys(0).length == 0)) ys.trimStart(1)
                            val y = (ys.map { ns =>
                                if (ns.startsWith("0x")) java.lang.Integer.parseInt(ns.substring(2), 16)
                                else if (ns.startsWith("0b")) java.lang.Integer.parseInt(ns.substring(2), 2)
                                else if (ns.startsWith("0o")) java.lang.Integer.parseInt(ns.substring(2), 8)
                                else ns.toDouble
                            })
                            // correction
                            for (i <- 0 until y.length if (!y(i).isNaN && !y(i).isInfinity)){
                                y(i) += ini("yAdd"+(i+1),0.0)
                                serverData.lazySet(i, y(i))
                            }
                            // add to graphs
                            if (y.length > 0){
                                if (ini("xType",0) == 1){ x = y(0); y.trimStart(1) }
                                else if (ini("xType",0) == 2) x = System.currentTimeMillis
                                while (y.length < channelsCount) y.append(Double.NaN)
                                values.setAllText(y)
                                if (lineSkip.get == 0){
                                    lineSkip.set(ini("xSkip",0))
                                    if (ini("chartOn",false)){
                                        swing.Swing.onEDT( chart.addPoints(x, y) )
                                        csv.lines += (if (ini("xType",0) == 2) csv.dateFormatter.format(new java.util.Date(x.toLong)) else x.toString) +
                                            csv.separator + y.mkString(csv.separator)
                                    }
                                } else lineSkip.decrementAndGet
                            }
                        }
                    } catch {
                        case _:Exception =>
                    }
                }
            }}, 10, 200)}
            
            val portTimer = new java.util.Timer { scheduleAtFixedRate( new java.util.TimerTask { def run = {
                if (channelOpened.get()){
                    try {
                        readBuf ++= channel.read
                        var b = -1
                        while ((readBuf.length > 0)&&(b != '\n')){
                            b = readBuf.dequeue
                            if (b == '\n'){
                                linesIn.add(new String(lineBuf.toArray, "UTF-8"))
                                lineBuf.clear
                            } else lineBuf += (b & 0xFF).toByte
                        }
                    } catch {
                        case _:java.io.IOException =>
                            if (!channel.name.startsWith("socket") && !channel.channels.contains(channel.name)){
                                readBuf.clear
                                linesIn.clear
                                disconnectButton.action.apply()
                            }
                        case _:Exception =>
                            lineBuf.clear
                    }
                } else { if (!readBuf.isEmpty){
                    readBuf.clear
                    lineBuf.clear
                    lineNum.set(0)
                }}
            }}, 10, 20)}
        }

        // clear all data
        def resetAll = {
            chart.clearPoints
            chart.xAxisFormat(ini("xType",0) == 2, ini("xLabelDate","yyyy.MM.dd HH.mm.ss"))
            poll.lineNum.set(0)
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
                            selected = ini("configOn", false)
                            tooltip = tr("Show chart/channels configuration")
                        }
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("values")){ def apply() = valuesPanel.visible = ini.put("valuesOn", selected).asInstanceOf[Boolean] }
                            selected = ini("valuesOn", false)
                            tooltip = tr("Show values")
                        }
                        ,new swing.Label(" | ")
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("chart")){ def apply() = ini.put("chartOn", selected) }
                            selected = ini("chartOn",false)
                            tooltip = tr("Enable chart")
                        }
                        ,new swing.Button(new swing.Action(tr("reset")){ def apply = resetAll }){ tooltip = tr("Clear all data") }
                        ,new swing.Label(" | ")
                        ,new swing.Button(new swing.Action(">"){ def apply = {
                            if (chart.snapshotSave(true)) ini("pngFile") = chart.snapshotDialog.selectedFile.getCanonicalPath
                        }}){ tooltip = tr("Save chart to PNG") }
                        ,new swing.Label(" PNG")
                        ,new swing.Label(" | ")
                        ,new swing.Button(new swing.Action(">"){ def apply = { csv.save
                        }}){ tooltip = tr("Save data to CSV"); csvSaveButton = this }
                        ,new swing.Label(" CSV ")
                        ,new swing.Button(new swing.Action(">"){ def apply = { csv.load
                        }}){ tooltip = tr("Load data from CSV"); csvLoadButton = this }
//                        ,new swing.CheckBox {
//                            action = new swing.Action(tr("auto")){ def apply() = ini.put("csvAuto", selected) }
//                            selected = false // ini("csvAuto",false)
//                            tooltip = tr("Automatic periodic CSV save")
//                        }
                        ,new swing.Label(" | ")
                        ,new swing.Label(" L&F: ")
                        ,new swing.ComboBox(Config.lafsNames){ maximumSize = preferredSize
                            selection.index = ini("laf",0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini("laf") = selection.index
                                javax.swing.UIManager.setLookAndFeel(Config.lafs(selection.index))
                                javax.swing.SwingUtilities.updateComponentTreeUI(top.peer)
                            }
                            tooltip = tr("Select current Look&Feel")
                        }
                        ,new swing.Label(" | ")
                        ,new swing.Label(" lang: ")
                        ,new swing.ComboBox( scl.GetText.displayLangs ){ maximumSize = preferredSize
                            selection.index = ini("lang",0)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini("lang") = selection.index
                            }
                            tooltip = tr("Select current language")
                        }
                        ,new swing.Label(" | ")
                        ,new swing.CheckBox {
                            action = new swing.Action(tr("server")){ def apply() = {
                                ini("server") = selected
                                serverPortCombo.visible = selected
                            }}
                            selected = ini("server",false)
                            tooltip = tr("Enable server")
                        }
                        ,new swing.ComboBox(List(80,8080,8090,9000)){ maximumSize = preferredSize
                            visible = ini("server",false)
                            makeEditable()(swing.ComboBox.intEditor)
                            selection.item = ini("serverPort",8090)
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini("serverPort") = selection.item
                            }
                            tooltip = tr("Server port")
                            serverPortCombo = this
                        }
                        ,new swing.Label(" | ")
                        ,swing.Swing.HGlue
                        ,new swing.Button(new swing.Action(tr("connect")){ def apply = {
                            try {
                                if (channel != null){ channelOpened.set(false); channel.close; channel = null }
                                channel = ini("port","socketTCP") match {
                                    case "socketTCP" => new scl.ChannelSocketTCP{ open( ini("ip","127.0.0.1:9000") ) }
                                    case "socketUDP" => new scl.ChannelSocketUDP{ open( ini("ip","127.0.0.1:9000") ) }
                                    case p:String => new scl.ChannelSerial {
                                        open(p)
                                        propSet("baud", ini("baud",9600))
                                        propSet("bits", ini("bits",8))
                                        propSet("parity", ini("parity","none"))
                                        propSet("stops", ini("stops",1.0))
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
                            } catch { case _:Exception => }
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
                            selection.item = ini("port", "socketTCP")
                            listenTo(selection)
                            reactions += { case swing.event.SelectionChanged(_) =>
                                ini("port") = selection.item
                                ipText.visible = selection.item.startsWith("socket")
                                serialPanel.visible = !ipText.visible
                                portPanel.revalidate
                            }
                            tooltip = tr("Select channel")
                        }
                        ,new swing.Label(" ")
                        ,new swing.TextField(18){ maximumSize = preferredSize; ipText  = this
                            visible = ini("port","socketTCP").startsWith("socket")
                            font = new awt.Font( "Monospaced", awt.Font.BOLD, font.getSize )
                            text    = ini("ip","127.0.0.1:9000")
                            listenTo(this)
                            reactions += { case swing.event.EditDone(_) => ini("ip") = text }
                            verifier = v => {
//                                println( v.matches("^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$") )
                                v.matches("^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$")
                            }
                            tooltip = tr("Socket IP address and port")
                        }
                        ,new swing.BoxPanel(swing.Orientation.Horizontal){ contents ++= List(
                            new swing.Label(tr(" baud:"))
                            ,new swing.ComboBox(Config.bauds){ maximumSize = preferredSize
                                makeEditable()(swing.ComboBox.intEditor)
                                selection.item = ini("baud",9600)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("baud") = selection.item
                                }
                                tooltip = tr("Serial port baud rate")
                            }
                            ,new swing.Label(tr(" bits:"))
                            ,new swing.ComboBox(List(5,6,7,8)){ maximumSize = preferredSize
                                selection.item = ini("bits",8)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("bits") = selection.item
                                }
                                tooltip = tr("Serial port data bits")
                            }
                            ,new swing.Label(tr(" parity:"))
                            ,new swing.ComboBox(List("none","even","odd","mark","space")){ maximumSize = preferredSize
                                selection.item = ini("parity", "none")
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("parity") = selection.item
                                }
                                tooltip = tr("Serial port parity")
                            }
                            ,new swing.Label(tr(" stops:"))
                            ,new swing.ComboBox(List(1.0,1.5,2.0)){ maximumSize = preferredSize
                                selection.item = ini("stops",1.0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("stops") = selection.item
                                }
                                tooltip = tr("Serial port stop bits")
                            }
                        ); serialPanel = this; visible = !ini("port","socketTCP").startsWith("socket") }
                        ,swing.Swing.HGlue
                    )
                }
                ,new swing.BoxPanel(swing.Orientation.Vertical){ configPanel = this; visible = ini("configOn", false); contents ++= List(
                    new swing.BoxPanel(swing.Orientation.Horizontal){ val generalConfigPanel = this; border = new EtchedBorder
                        contents ++= List(
                            new swing.Label(" x:")
                            ,new swing.TextField(5){ maximumSize = preferredSize
                                text = ini("xLabel","x")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("xLabel") = text; chart.xName(text) }
                                tooltip = tr("X axis label")
                            }
                            ,new swing.Label(" ")
                            ,new swing.ComboBox(List(tr("line №"),tr("1st col"),tr("date"))){ maximumSize = preferredSize; xTypeCombo = this
                                selection.index = ini("xType",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("xType") = selection.index
                                    xDateFormatText.visible = (selection.index == 2)
                                    generalConfigPanel.revalidate
                                }
                                tooltip = tr("X axis type")
                            }
                            ,new swing.Label(" ")
                            ,new swing.TextField(20){ maximumSize = preferredSize; visible = (ini("xType",0) == 2); xDateFormatText = this
                                font = new awt.Font( "Monospaced", awt.Font.BOLD, font.getSize )
                                text = ini("xLabelDate","yyyy.MM.dd HH.mm.ss")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("xLabelDate") = text }
                                tooltip = tr("<html>X axis date/time format:<br>" +
                                    "<b>y</b> - year, <b>M</b> - month, <b>d</b> - date;<br>" +
                                    "<b>H</b> - hour, <b>m</b> - minute, <b>s</b> - second;<br>" +
                                    "<b>'single quotes'</b> - plain text.</html>")
                            }
                            ,new swing.Label(tr(" limit: "))
                            ,new swing.ComboBox(List(0,100,500,1000,5000,10000,20000,50000,100000)){ maximumSize = preferredSize
                                makeEditable()(swing.ComboBox.intEditor)
                                selection.item = ini("xLimit",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("xLimit") = selection.item
                                    chart.xLimit.set( selection.item )
                                }
                                tooltip = tr("Limit number of points")
                            }
                            ,new swing.Label(tr(" skip: "))
                            ,new swing.ComboBox(List(0,1,4,9,19,49,59,99,199,499,999)){ maximumSize = preferredSize
                                makeEditable()(swing.ComboBox.intEditor)
                                selection.item = ini("xSkip",0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("xSkip") = selection.item
                                }
                                tooltip = tr("Skip CSV lines")
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(" y:")
                            ,new swing.TextField(5){ maximumSize = preferredSize
                                text = ini("yLabel","y")
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("yLabel") = text; chart.yName(text) }
                                tooltip = tr("Y axis label")
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(tr("window"))
                            ,new swing.Label(" x: ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini("winXmin",0).toString()
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("winXmin") = text;
                                    chart.rangeX(ini("winXmin",0),ini("winXmax",0))
                                }
                                tooltip = tr("X window minimum")
                            }
                            ,new swing.Label(" .. ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini("winXmax",0).toString()
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("winXmax") = text;
                                    chart.rangeX(ini("winXmin",0),ini("winXmax",0))
                                }
                                tooltip = tr("X window maximum")
                            }
                            ,new swing.Label(" y: ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini("winYmin",0).toString()
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("winYmin") = text;
                                    chart.rangeY(ini("winYmin",0),ini("winYmax",0))
                                }
                                tooltip = tr("Y window minimum")
                            }
                            ,new swing.Label(" .. ")
                            ,new swing.TextField(4){ maximumSize = preferredSize
                                text = ini("winYmax",0).toString()
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("winYmax") = text;
                                    chart.rangeY(ini("winYmin",0),ini("winYmax",0))
                                }
                                tooltip = tr("Y window maximum")
                            }
                            ,new swing.Label(" | ")
                            ,new swing.Label(tr("grid: "))
                            ,new swing.CheckBox {
                                action = new swing.Action("x"){ def apply() = { ini("gridX") = selected; chart.showGridX(selected) }}
                                selected = ini("gridX",false)
                                tooltip = tr("Enable X grid")
                            }
                            ,new swing.CheckBox {
                                action = new swing.Action("y"){ def apply() = {
                                    ini("gridY") = selected
                                    chart.showGridY(selected)
                                    chart.showGridYRight(selected)
                                }}
                                selected = ini("gridY",false)
                                tooltip = tr("Enable Y grid")
                            }
                            ,swing.Swing.HGlue
                        )
                    }
                    ,new swing.ScrollPane{ maximumSize = new swing.Dimension(Integer.MAX_VALUE,180); contents = new swing.GridPanel(7,channelsCount+1){
                        border = new EtchedBorder
                        contents += new swing.Label(tr("show")){ tooltip = tr("Trace visibility") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.CheckBox { maximumSize = preferredSize
                                action = new swing.Action(i.toString){
                                    def apply() = { ini("show"+i) = selected; chart.traceShow(i-1,selected) }
                                }
                                selected = ini("show"+i, false)
                                tooltip = tr("visibility of trace №%d").format(i)
                        })
                        contents += new swing.Label(tr("name")){ tooltip = tr("Trace name") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.TextField(8){
                                tooltip = tr("name of trace №%d").format(i)
                                text = ini("name"+i,"Y"+i)
                                listenTo(this)
                                reactions += { case swing.event.EditDone(_) => ini("name"+i) = text; chart.traceName(i-1,text) }
                        })
                        contents += new swing.Label(tr("Y axis")){ tooltip = tr("Number of Y axis") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield (new swing.ComboBox(List(1,2)){
                                tooltip = tr("axis of trace №%d").format(i)
                                selection.index = ini("yAxis"+i,0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("yAxis"+i) = selection.index
                                }
                            })
                        )
                        contents += new swing.Label(tr("color")){ tooltip = tr("Trace color") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.Button(""){ val _but = this; background = SwUtil.svgColor(colors.get(i-1));
                                action = new swing.Action(""){ def apply = {
                                    colors.chooser.openColor( colors.current(i-1), { c:String => _but.background = colors.set(i-1,c); chart.traceColor(i-1,colors.current(i-1)) })
                            } }; tooltip = tr("color of trace №%d").format(i) }
                        )
                        contents += new swing.Label(tr("width")){ tooltip = tr("Trace width") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.FormattedTextField(java.text.NumberFormat.getNumberInstance){
                                tooltip = tr("width of trace №%d").format(i)
                                text = ini("width"+i,1.0).toString()
                                listenTo(this)
                                reactions += { case swing.event.ValueChanged(_) if (!this.hasFocus && text.length > 0 && editValid) =>
                                    ini("width"+i) = text; chart.traceStyle(i-1,ini("width"+i,1.0),ini("style"+i,0))
                                }
                        })
                        contents += new swing.Label(tr("style")){ tooltip = tr("Trace style") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield (new swing.ComboBox(List("───────","· · · ·","─ ─ ─ ─")){
                                tooltip = tr("style of trace №%d").format(i)
                                selection.index = ini("style"+i,0)
                                listenTo(selection)
                                reactions += { case swing.event.SelectionChanged(_) =>
                                    ini("style"+i) = selection.index
                                    chart.traceStyle(i-1,ini("width"+i,1.0),ini("style"+i,0))
                                }
                            })
                        )
                        contents += new swing.Label("Δ"){ tooltip = tr("Value correction") }
                        contents ++= (for (i <- 1 to channelsCount)
                            yield new swing.FormattedTextField(java.text.NumberFormat.getNumberInstance){
                                tooltip = tr("delta of value №%d").format(i)
                                text = ini("yAdd"+i,0.0).toString()
                                listenTo(this)
                                reactions += { case swing.event.ValueChanged(_) if (!this.hasFocus && text.length > 0 && editValid) =>
                                    ini("yAdd"+i) = text
                                }
                        })
                    }}
                ); border = new TitledBorder( new EtchedBorder, tr("Config"), TitledBorder.LEFT, TitledBorder.TOP )}
                ,new swing.BoxPanel(swing.Orientation.Vertical){ valuesPanel = this; visible = ini("valuesOn", false)
                    border = new EtchedBorder
                    var ch = 0
                    for (row <- 0 until Math.ceil(channelsCount / channelsInRow.toDouble).toInt){
                        contents += new swing.BoxPanel(swing.Orientation.Horizontal){
                            contents += swing.Swing.HGlue
                            for (col <- 0 until channelsInRow if ((row*channelsInRow + col) < channelsCount)){
                                val v = new swing.Label {
                                    font = new awt.Font("Monospace", awt.Font.BOLD, 30)
                                    tooltip = tr("value №%d").format(row*channelsInRow + col + 1)
                                }
                                contents += v
                                values.labels += v
                                values.setText(row*channelsInRow + col,Double.NaN)
                                colors.set(row*channelsInRow + col,colors.get(row*channelsInRow + col))
                            }
                            contents += swing.Swing.HGlue
                        }
                    }
                }
                ,new scl.Chart { top.chart = this; xName(ini("xLabel","x")); yName(ini("yLabel","y"));
                    addAxisRight(false)
                    for (i <- 1 to channelsCount){
                        addTrace(ini("name"+i,"Y"+i), colors.current(i-1), ini("show"+i,false),
                            ini("width"+i,1.0), ini("style"+i,0), ini("yAxis"+i,0) == 1
                        )
                        if (ini("yAxis"+i,0) == 1) showAxisRight(true)
                    }
                    rangeX(ini("winXmin",0),ini("winXmax",0))
                    rangeY(ini("winYmin",0),ini("winYmax",0))
                    xLimit.set( ini("xLimit",0) )
                    showGridX(ini("gridX",false))
                    showGridY(ini("gridY",false))
                    showGridYRight(ini("gridY",false))
                    snapshotDialog.selectedFile = new java.io.File(ini("pngFile", "snapshot.png"))
                }
                ,new swing.BoxPanel(swing.Orientation.Horizontal){
                    contents ++= List(
                        new swing.Label(""){
                            statusText = this
                            visible = false
                            font = new awt.Font("Monospace", awt.Font.BOLD, 18)
                            horizontalAlignment = swing.Alignment.Left
                        }
                        ,swing.Swing.HGlue
                )}
            )
        }

        // restore window geometry
        minimumSize   = new swing.Dimension( 800, 600 );
        bounds        = new swing.Rectangle( ini("x",0), ini("y",0), ini("w",800), ini("h",600) )
        preferredSize = new swing.Dimension( bounds.width, bounds.height )
        if (ini("maximized", false)) maximize
        
        // start server
        if (ini("server",false)){
            new java.lang.Thread {
                override def run = {
                    server = new scl.ServerJetty( ini("serverPort",8090), "./static", 0 ){
                        override def newHandler(target:String, baseRequest:jetty.server.Request, request:HttpServletRequest, response:HttpServletResponse) =
                            new scl.HandlerJetty(target, baseRequest, request, response){
                            val _handler = this
                            override def handle:Boolean = {
                                try { target match {
                                    case "/values.json" =>
                                        response.setContentType("application/json")
                                        responseStr = "[" + (0 until channelsCount map(serverData.get(_))).mkString(",") + "]"
                                        handled = true
                                }} catch { case _:Exception => }
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
            ini("x") = bounds.x; ini("y") = bounds.y;
            ini("w") = bounds.width; ini("h") = bounds.height;
            ini("maximized") = maximized
            ini("colors") = colors.current.mkString(",")
            ini.save
            if (server != null) server.stop(0)
            super.closeOperation()
        }
    }
}

