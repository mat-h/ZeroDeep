package data.mnist

import java.io._
import scala.io.Source
import scala.io.Codec
import breeze.linalg._

object Loader {

  val train_img = (new ImageLoader("train-images-idx3-ubyte.gz")).readAll(0)
  val train_label = (new LabelLoader("train-labels-idx1-ubyte.gz")).readAll(0)
  val test_img = (new ImageLoader("t10k-images-idx3-ubyte.gz")).readAll(0)
  val test_label = (new LabelLoader("t10k-labels-idx1-ubyte.gz")).readAll(0)
  
  def download(fileName: String) {
    val target = s"http://yann.lecun.com/exdb/mnist/$fileName"
    println(target)

    val binary = Source.fromURL(target, "ISO8859-1")

    val br = new BufferedReader(binary.reader())

    val file = new File(s"data/$fileName")

    val os = new FileOutputStream(file)
    
    var b: Int = 0
    while (b != -1) {
      b = br.read()
      os.write(b.toByte)
    }

    os.close()
    br.close()
  }

}