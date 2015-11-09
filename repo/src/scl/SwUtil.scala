package scl

class SwHsep(h:Int=10) extends swing.Separator(swing.Orientation.Horizontal) {
    maximumSize = new swing.Dimension(Integer.MAX_VALUE, h)
}
