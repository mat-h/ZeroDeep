package data.mnist

class LabelLoader(fileName: String) extends LoadOnce(fileName) {
  type T = Byte
  def magic = 2049
  
  def readMeta = {}
  def readOnce: T = stream.readByte
}