package scl

import java.io.File

object Files {
    def file(f:String) = new java.io.File(f)
    def exists(f:String) = file(f).exists()
    
    // create multiple dirs
    def mkDirs(dirs:File*):Unit = dirs foreach { dir => dir.mkdir() };
    // clear multiple dirs
    def clearDirs(dirs:File*) = dirs foreach { clearDir( _ , false ) }
    
    // copy file
    def copy(src:File, dest:File):Unit = java.nio.file.Files.copy(src.toPath, dest.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    def copy(src:String, dest:String):Unit = copy(new File(src), new File(dest))
    
    // create single dir
    def mkdir(d:File) = if (!d.exists()) java.nio.file.Files.createDirectory(d.toPath)
    // recursive delete dir
    def clearDir(d:File, delRoot:Boolean) = if (d.exists){
        val root = d.toPath
        java.nio.file.Files.walkFileTree(d.toPath(), new java.nio.file.SimpleFileVisitor[java.nio.file.Path]{
            override def visitFile(f:java.nio.file.Path, attrs:java.nio.file.attribute.BasicFileAttributes) = {
                try { java.nio.file.Files.delete(f)
                } catch { case e:Exception => }
                java.nio.file.FileVisitResult.CONTINUE
            }
            override def postVisitDirectory(f:java.nio.file.Path, e:java.io.IOException) = {
                if (delRoot || (!delRoot && (f != root))) try { java.nio.file.Files.delete(f)
                } catch { case e:Exception => }
                java.nio.file.FileVisitResult.CONTINUE
            }
        })
    }
    
    // read to array of bytes
    def readBytes(f:File):Array[Byte] = java.nio.file.Files.readAllBytes(f.toPath)
    // read text file
    def readString(f:File, enc:String):String = new String(java.nio.file.Files.readAllBytes(f.toPath), enc)
    def readString(fn:String, enc:String):String = readString(new File(fn),enc)
    // write text file
    def writeString(f:File, s:String, enc:String):Unit = java.nio.file.Files.write(f.toPath, s.getBytes(enc))
    def writeString(fn:String, s:String, enc:String):Unit = writeString(new File(fn),s,enc)
    def writeBytes(f:File, bytes:Array[Byte]):Unit = java.nio.file.Files.write(f.toPath, bytes)
    def writeBytes(fn:String, bytes:Array[Byte]):Unit = writeBytes(new File(fn), bytes)
}
