package models

import java.io.File

import play.api.libs.Files.TemporaryFile

class TemporaryFileBuilder {
 def createTemporaryFile(f:File)=TemporaryFile(f)
}
