package models

import java.io.OutputStream
import java.util.zip.ZipOutputStream

class ZipOutputStreamBuilder {
  def createZipOutputStream(os:OutputStream)=new ZipOutputStream(os)
}
