package models

import java.io.File

import play.api.libs.Files.TemporaryFile

class TemporaryFileBuilder {
 def createFile(filename:String)=new File(filename)
 def createTemporaryFile(f:File)=TemporaryFile(f)
}
