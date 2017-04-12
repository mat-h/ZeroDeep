package data.mnist

import breeze.linalg._

class ImageLoader(fileName: String) extends LoadOnce(fileName) {
  type T = DenseMatrix[Int]
  def magic = 2051
  
  var width: Int = 0
  var height: Int = 0
  def readMeta = {
    width = stream.readInt()
    height = stream.readInt()
    println(s"info: $count * $width * $height")
  }
  
  def readOnce: T = {
    val m = DenseMatrix.zeros[Int](height, width)

    for (y <- 0 until height; x <- 0 until width)
      m(y, x) = stream.readUnsignedByte()

    m
  }
}