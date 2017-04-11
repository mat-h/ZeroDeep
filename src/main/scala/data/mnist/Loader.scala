package data.mnist

import scala.io.Source
import scala.io.Codec
import java.io._
import java.util.zip.GZIPInputStream
import breeze.linalg._

object Loader extends App {

  val key_file = Map(
    "train_img" -> "train-images-idx3-ubyte.gz",
    "train_label" -> "train-labels-idx1-ubyte.gz",
    "test_img" -> "t10k-images-idx3-ubyte.gz",
    "test_label" -> "t10k-labels-idx1-ubyte.gz")

  // key_file.values.map(download(_))
  // key_file.values.map(read(_))
  val images = read("train-images-idx3-ubyte.gz")
  
  images.take(1).foreach(println)

  def download(fileName: String) {
    val target = s"http://yann.lecun.com/exdb/mnist/$fileName"
    println(target)

    val binary = Source.fromURL(target, "ISO8859-1")

    val file = new File(s"data/$fileName")

    val os = new FileOutputStream(file)

    val br = new BufferedReader(binary.reader())

    var b: Int = 0
    while (b != -1) {
      b = br.read()
      os.write(b.toByte)
    }

    os.close()
    br.close()
  }
  
  def read(fileName: String) = readImages(0, s"data/$fileName")
  
  def readImages(ind: Int, path: String): Stream[DenseMatrix[Int]] = {
    println(s"reading $path")
    
    val stream = new DataInputStream(new GZIPInputStream(new FileInputStream(path)))
    assert(stream.readInt() == 2051, "Wrong MNIST label stream magic")
    
    val count = stream.readInt()
    val width = stream.readInt()
    val height = stream.readInt()
    
    println(s"info: $count * $width * $height")

    def readImage(): DenseMatrix[Int] = {
      val m = DenseMatrix.zeros[Int](height, width)
  
      for (y <- 0 until height; x <- 0 until width)
        m(y, x) = stream.readUnsignedByte()
  
      m
    }
    
    if (ind >= count)
      Stream.empty
    else
      Stream.cons(readImage(), readImages(ind + 1, path))
  }

}