package data.mnist

import breeze.linalg._

class ImageLoader(fileName: String) extends LoadOnce(fileName) {
  type T = DenseMatrix[Int]
  def magic = 2051
  
  def readMeta = {
    if (width == 0 && height == 0) {
      width_=(stream.readInt)
      height_=(stream.readInt)
      println(s"ImageSize: $count * $width * $height")
    }
  }

  def readOnce: T = {
    val m = DenseMatrix.zeros[Int](height * width, 1)

    for (y <- 0 until height * width)
      m(y, 0) = stream.readUnsignedByte()

    m
  }
}