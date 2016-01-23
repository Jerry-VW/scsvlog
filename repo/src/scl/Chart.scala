package scl

import info.monitorenter.gui.chart.{Chart2D,ZoomableChart,TracePoint2D,IAxis,IAxisScalePolicy,ITrace2D,rangepolicies,traces,axis,labelformatters}
import java.{awt}, javax.swing.filechooser.{FileNameExtensionFilter}
import scala.language.{reflectiveCalls}

class Chart extends swing.BoxPanel(swing.Orientation.Vertical) with scl.GetText {

    val _chart = new ZoomableChart {
        setAutoscrolls(true);
        setUseAntialiasing(true);
        setToolTipType( Chart2D.ToolTipType.DATAVALUES );
        setPaintLabels(true);
        
        var rpx:rangepolicies.ARangePolicy = new rangepolicies.RangePolicyUnbounded
        var rpy:rangepolicies.ARangePolicy = new rangepolicies.RangePolicyUnbounded
        
        addMouseListener( new awt.event.MouseListener {
            def mousePressed(e:awt.event.MouseEvent){}
            def mouseReleased(e:awt.event.MouseEvent){}
            def mouseEntered(e:awt.event.MouseEvent){}
            def mouseExited(e:awt.event.MouseEvent){}
            def mouseClicked(e:awt.event.MouseEvent){ if (e.getClickCount == 2){
                zoomAll()
                getAxisX.setRangePolicy(rpx)
                getAxisY.setRangePolicy(rpy)
            }}
        })
        
        addMouseWheelListener( new awt.event.MouseWheelListener {
            def mouseWheelMoved(e:awt.event.MouseWheelEvent){}
        })
    }
    
    var xLimit = new java.util.concurrent.atomic.AtomicInteger(0)
    var trace0:ITrace2D = null
    
    def tracesCount = _chart.getTraces.size()
    def addAxisRight(show:Boolean) = { _chart.addAxisYRight(new axis.AxisLinear); showAxisRight(show) }
    def showAxisLeft(show:Boolean) = _chart.getAxisY.setVisible(show)
    def showAxisRight(show:Boolean) = _chart.getAxesYRight.get(0).setVisible(show)
    def showGridX(show:Boolean) = _chart.getAxisX.setPaintGrid(show)
    def showGridY(show:Boolean) = _chart.getAxisY.setPaintGrid(show)
    def showGridYRight(show:Boolean) = _chart.getAxesYRight.get(0).setPaintGrid(show)
    def addTrace(name:String, color:String, show:Boolean, width:Double, style:Int, yAxisRight:Boolean=false){
        _chart.addTrace(new traces.Trace2DSorted, _chart.getAxisX, if (yAxisRight) _chart.getAxesYRight.get(0) else _chart.getAxisY)
        val i = tracesCount-1
        traceName(i, name)
        traceColor(i, SwUtil.svgColor(color))
        traceShow(i, show)
        traceStyle(i, width, style)
    }
    def trace(i:Int):ITrace2D = (_chart.getTraces().toArray())(i).asInstanceOf[ITrace2D]
    def pointsCount = { if (trace0 == null) trace0 = trace(0); trace0.getSize }
    def addPoints(x:Double, y:Seq[Double]) = {
        for (i <- 0 until y.length) trace(i).addPoint(x, y(i))
        // remove excess points
        if (xLimit.get > 0) for (t <- _chart.getTraces().toArray()){
            while (t.asInstanceOf[ITrace2D].getSize() > xLimit.get) t.asInstanceOf[ITrace2D].removePoint(
                t.asInstanceOf[ITrace2D].iterator().next()
            )
        }
    }
    def clearPoints = for (i <- 0 until tracesCount) trace(i).removeAllPoints
    def traceName(i:Int, name:String) = trace(i).setName(name)
    def traceColor(i:Int, color:awt.Color) = trace(i).setColor(color)
    def traceColor(i:Int, color:String) = trace(i).setColor(SwUtil.svgColor(color))
    def traceShow(i:Int, show:Boolean) = trace(i).setVisible(show)
    def traceStyle(i:Int, width:Double, style:Int) = {
        if (style == 0) trace(i).setStroke(new awt.BasicStroke(width.toFloat))
        else trace(i).setStroke(new awt.BasicStroke(width.toFloat,
            awt.BasicStroke.CAP_BUTT, awt.BasicStroke.JOIN_MITER, 1.0f,
            List( Array(0.0f), Array(1.0f,2.0f), Array(5.0f,5.0f) )(style), 0.0f
        ))
    }
    
    def xName(name:String) = _chart.getAxisX().getAxisTitle().setTitle(name)
    def yName(name:String) = _chart.getAxisY().getAxisTitle().setTitle(name)
    def yNameRight(name:String) = _chart.getAxesYRight.get(0).getAxisTitle().setTitle(name)
    
    def rangeX(min:Double, max:Double) = {
        _chart.rpx = if (min >= max) (new rangepolicies.RangePolicyUnbounded) else
            (new rangepolicies.RangePolicyMinimumViewport(new info.monitorenter.util.Range(min,max)))
        _chart.getAxisX.setRangePolicy( _chart.rpx )
    }
    def rangeY(min:Double, max:Double) = {
        _chart.rpy = if (min >= max) (new rangepolicies.RangePolicyUnbounded) else 
            (new rangepolicies.RangePolicyMinimumViewport(new info.monitorenter.util.Range(min,max)))
        _chart.getAxisY.setRangePolicy( _chart.rpy )
    }

    // set format for X axis
    def xAxisFormat(date:Boolean, format:String) = _chart.getAxisX.setFormatter( if (date)
        new labelformatters.LabelFormatterDate(new java.text.SimpleDateFormat(format))
        else new labelformatters.LabelFormatterSimple
    )
    
    def requestRepaint(request:Boolean) = _chart.setRequestedRepaint(request)
    
    // save snapshot dialog
    val snapshotDialog = new swing.FileChooser{
        title        = "Save as PNG..."
        fileFilter   = new FileNameExtensionFilter("PNG file","png")
        selectedFile = new java.io.File(".")
    }
    // save snapshot button
    def snapshotSave = {
        if (snapshotDialog.showSaveDialog(null) == swing.FileChooser.Result.Approve){
            try { javax.imageio.ImageIO.write( _chart.snapShot, "png", snapshotDialog.selectedFile )
            } catch { case _:Throwable => }
        }
    }
    peer.add(_chart)
}
