package scl

import java.awt

class SwHsep(h:Int=10) extends swing.Separator(swing.Orientation.Horizontal) {
    maximumSize = new swing.Dimension(Integer.MAX_VALUE, h)
}

object SwUtil {
    val svgColors = Map("black"->"#000000","navy"->"#000080","darkblue"->"#00008B","mediumblue"->"#0000CD","blue"->"#0000FF",
        "darkgreen"->"#006400","green"->"#008000","teal"->"#008080","darkcyan"->"#008B8B","deepskyblue"->"#00BFFF","darkturquoise"->"#00CED1",
        "mediumspringgreen"->"#00FA9A","lime"->"#00FF00","springgreen"->"#00FF7F","cyan"->"#00FFFF","aqua"->"#00FFFF","midnightblue"->"#191970",
        "dodgerblue"->"#1E90FF","lightseagreen"->"#20B2AA","forestgreen"->"#228B22","seagreen"->"#2E8B57","darkslategray"->"#2F4F4F",
        "darkslategrey"->"#2F4F4F","limegreen"->"#32CD32","mediumseagreen"->"#3CB371","turquoise"->"#40E0D0","royalblue"->"#4169E1",
        "steelblue"->"#4682B4","darkslateblue"->"#483D8B","mediumturquoise"->"#48D1CC","indigo"->"#4B0082","darkolivegreen"->"#556B2F",
        "cadetblue"->"#5F9EA0","cornflowerblue"->"#6495ED","mediumaquamarine"->"#66CDAA","dimgrey"->"#696969","dimgray"->"#696969",
        "slateblue"->"#6A5ACD","olivedrab"->"#6B8E23","slategrey"->"#708090","slategray"->"#708090","lightslategray"->"#778899",
        "lightslategrey"->"#778899","mediumslateblue"->"#7B68EE","lawngreen"->"#7CFC00","chartreuse"->"#7FFF00","aquamarine"->"#7FFFD4",
        "maroon"->"#800000","purple"->"#800080","olive"->"#808000","gray"->"#808080","grey"->"#808080","skyblue"->"#87CEEB",
        "lightskyblue"->"#87CEFA","blueviolet"->"#8A2BE2","darkred"->"#8B0000","darkmagenta"->"#8B008B","saddlebrown"->"#8B4513",
        "darkseagreen"->"#8FBC8F","lightgreen"->"#90EE90","mediumpurple"->"#9370DB","darkviolet"->"#9400D3","palegreen"->"#98FB98",
        "darkorchid"->"#9932CC","yellowgreen"->"#9ACD32","sienna"->"#A0522D","brown"->"#A52A2A","darkgray"->"#A9A9A9","darkgrey"->"#A9A9A9",
        "lightblue"->"#ADD8E6","greenyellow"->"#ADFF2F","paleturquoise"->"#AFEEEE","lightsteelblue"->"#B0C4DE","powderblue"->"#B0E0E6",
        "firebrick"->"#B22222","darkgoldenrod"->"#B8860B","mediumorchid"->"#BA55D3","rosybrown"->"#BC8F8F","darkkhaki"->"#BDB76B",
        "silver"->"#C0C0C0","mediumvioletred"->"#C71585","indianred"->"#CD5C5C","peru"->"#CD853F","chocolate"->"#D2691E","tan"->"#D2B48C",
        "lightgray"->"#D3D3D3","lightgrey"->"#D3D3D3","thistle"->"#D8BFD8","orchid"->"#DA70D6","goldenrod"->"#DAA520","palevioletred"->"#DB7093",
        "crimson"->"#DC143C","gainsboro"->"#DCDCDC","plum"->"#DDA0DD","burlywood"->"#DEB887","lightcyan"->"#E0FFFF","lavender"->"#E6E6FA",
        "darksalmon"->"#E9967A","violet"->"#EE82EE","palegoldenrod"->"#EEE8AA","lightcoral"->"#F08080","khaki"->"#F0E68C","aliceblue"->"#F0F8FF",
        "honeydew"->"#F0FFF0","azure"->"#F0FFFF","sandybrown"->"#F4A460","wheat"->"#F5DEB3","beige"->"#F5F5DC","whitesmoke"->"#F5F5F5",
        "mintcream"->"#F5FFFA","ghostwhite"->"#F8F8FF","salmon"->"#FA8072","antiquewhite"->"#FAEBD7","linen"->"#FAF0E6","lightgoldenrodyellow"->"#FAFAD2",
        "oldlace"->"#FDF5E6","red"->"#FF0000","fuchsia"->"#FF00FF","magenta"->"#FF00FF","deeppink"->"#FF1493","orangered"->"#FF4500",
        "tomato"->"#FF6347","hotpink"->"#FF69B4","coral"->"#FF7F50","darkorange"->"#FF8C00","lightsalmon"->"#FFA07A","orange"->"#FFA500",
        "lightpink"->"#FFB6C1","pink"->"#FFC0CB","gold"->"#FFD700","peachpuff"->"#FFDAB9","navajowhite"->"#FFDEAD","moccasin"->"#FFE4B5",
        "bisque"->"#FFE4C4","mistyrose"->"#FFE4E1","blanchedalmond"->"#FFEBCD","papayawhip"->"#FFEFD5","lavenderblush"->"#FFF0F5","seashell"->"#FFF5EE",
        "cornsilk"->"#FFF8DC","lemonchiffon"->"#FFFACD","floralwhite"->"#FFFAF0","snow"->"#FFFAFA","yellow"->"#FFFF00","lightyellow"->"#FFFFE0", 
        "ivory"->"#FFFFF0","white"->"#FFFFFF"
    )
    val svgNames = svgColors.keys
    def svgColor(c:String, d:String="black") = java.awt.Color.decode(if (c.startsWith("#")) c else svgColors.getOrElse(c, svgColors.getOrElse(d, svgColors("black"))))
    def svgName(c:java.awt.Color) = "#" + Integer.toHexString(c.getRGB).substring(2)


    // polygon bounding rectangle
    def polyBounds(poly:Seq[(Int,Int)]):Seq[(Int,Int)] = {
        var xMin = Int.MaxValue; var xMax = Int.MinValue
        var yMin = Int.MaxValue; var yMax = Int.MinValue
        for (v <- poly){
            xMin = Math.min(xMin, v._1); xMax = Math.max(xMax, v._1)
            yMin = Math.min(yMin, v._2); yMax = Math.max(yMax, v._2)
        }
        Array((xMin,yMin), (xMax,yMax))
    }
    // bounding rectangle
    def polyRect(poly:Seq[(Int,Int)]):swing.Rectangle = {
        val bounds = polyBounds(poly)
        new swing.Rectangle(bounds(0)._1,bounds(0)._2, bounds(1)._1-bounds(0)._1,bounds(1)._2-bounds(0)._2)
    }

    // check if point in polygon
    def pointInPoly(x:Int,y:Int, poly:Seq[(Int,Int)]):Boolean = {
        var ip = false
        if (poly.length > 2){
            var i = 0; var j = poly.length-1
            while (i < poly.length){
                if (((poly(i)._2 > y) != (poly(j)._2 > y)) &&
                    (x < (poly(j)._1-poly(i)._1) * (y-poly(i)._2) / (poly(j)._2-poly(i)._2) + poly(i)._1))
                        ip = !ip;
                j = i; i += 1
            }
        }
        ip
    }
    // scale polygon from view coordinates to real imagecoordinates
    def polyScaled(poly:Seq[(Int,Int)], scaleX:Double,scaleY:Double):Seq[(Int,Int)] =
        for (v <- poly) yield ( (v._1 * scaleX).toInt, (v._2 * scaleY).toInt );

    // check if polygon is rectangle
    def polyIsRect(poly:Seq[(Int,Int)]):Boolean =
        ((poly.length == 4)&&(poly(0)._2 == poly(1)._2)&&(poly(1)._1 == poly(2)._1)&&(poly(2)._2 == poly(3)._2)&&(poly(3)._1 == poly(0)._1));

    // calculate polygon area
    def polyRectArea(poly:Seq[(Int,Int)]):Int = if (poly.length < 3) 0 else {
        val r = polyRect(poly)
        r.width * r.height
    }
    
    // create 4-vertex poly from 2-vertex rectangle
    def rectToPoly(rect:Seq[(Int,Int)]):Seq[(Int,Int)] = if (rect.length != 2) Nil else {
        val x = Math.min(rect(0)._1,rect(1)._1); val y = Math.min(rect(0)._2,rect(1)._2)
        val w = Math.abs(rect(1)._1 - rect(0)._1); val h = Math.abs(rect(1)._2 - rect(0)._2)
        Seq((x,y),(x+w,y),(x+w,y+h),(x,y+h))
    }
    
    // get image in polygon
    def cropImage(poly:Seq[(Int,Int)], im:awt.image.BufferedImage, rotation:Double):awt.image.BufferedImage = {
        if ((poly.length > 2)&&(im != null)){
            val bounds = polyBounds(poly)
            val fill = awt.Color.WHITE.getRGB
            val crop = cloneImage(im.getSubimage(bounds(0)._1,bounds(0)._2, bounds(1)._1-bounds(0)._1+1, bounds(1)._2-bounds(0)._2+1))
            // clear out-of-border dots in non-rectangular polygon
            if (!polyIsRect(poly)) for (y <- 0 until (bounds(1)._2 - bounds(0)._2))
                for (x <- 0 until (bounds(1)._1 - bounds(0)._1)) try {
                    if (!pointInPoly(x+bounds(0)._1,y+bounds(0)._2, poly)) crop.setRGB(x,y, fill)
                } catch { case e:Exception => println(s"error at ${x}:${y}"); throw e }
            if (rotation != 0.0){ rotateImage(crop, rotation)
//println("angle: "+rotation)
//println(s"${crop.getWidth} x ${crop.getHeight}")
//                val t = new awt.geom.AffineTransform
//                t.rotate(rotation * Math.PI / 180.0, crop.getWidth / 2, crop.getHeight / 2)
//                (new awt.image.AffineTransformOp(t,
//                    awt.image.AffineTransformOp.TYPE_BILINEAR)).filter(crop,null)
            } else crop
        } else null
    }
        // convert Image to BufferedImage
    def toBufferedImage(im:awt.Image):awt.image.BufferedImage = if (im != null){
        val nim = new awt.image.BufferedImage(im.getWidth(null), im.getHeight(null), awt.image.BufferedImage.TYPE_INT_ARGB)
        val g = nim.getGraphics
        nim.getGraphics.drawImage(im, 0,0, null);
        nim.getGraphics.dispose
        nim
    } else null
    // fit image to width/height
    def fitImage(im:awt.image.BufferedImage, w:Int, h:Int):awt.image.BufferedImage = {
        var nw = w; var nh = h
        if (im.getHeight.toDouble / im.getWidth >= h.toDouble / w) nw = (im.getWidth.toDouble * nh / im.getHeight).toInt
        else nh = (im.getHeight.toDouble * nw / im.getWidth.toDouble).toInt
        toBufferedImage(im.getScaledInstance(nw, nh, awt.Image.SCALE_AREA_AVERAGING))
    }
    // clone image
    def cloneImage(im:awt.image.BufferedImage):awt.image.BufferedImage =
        new awt.image.BufferedImage(im.getColorModel, im.copyData(im.getRaster.createCompatibleWritableRaster), im.getColorModel.isAlphaPremultiplied, null);

    // rotate image
    def rotateImage(im:awt.image.BufferedImage, angle:Double):awt.image.BufferedImage = {
        val w = im.getWidth; val h = im.getHeight
        if (angle == 0.0) im
        else if ((angle == 90.0)||(angle == -90.0)){
            val io = new awt.image.BufferedImage(h,w, im.getType)
            if (angle == 90.0) for (y <- 0 until h; x <- 0 until w) io.setRGB(h-y-1,x, im.getRGB(x,y))
            else for (y <- 0 until h; x <- 0 until w) io.setRGB(y,w-x-1, im.getRGB(x,y))
            io
        } else if (angle == 180.0){
            val io = new awt.image.BufferedImage(w,h, im.getType)
            for (y <- 0 until h; x <- 0 until w) io.setRGB(x,h-y-1, im.getRGB(x,y))
            io
        } else im
    }
}
