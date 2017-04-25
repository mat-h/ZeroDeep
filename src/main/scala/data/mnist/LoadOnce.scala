package data.mnist

import java.io._
import java.util.zip.GZIPInputStream

abstract class LoadOnce(val fileName: String) {
  type T
  def magic: Int
  val path = s"data/$fileName"
  if (! new File(path).exists) Loader.download(fileName)

  val stream = new DataInputStream(new GZIPInputStream(new FileInputStream(path)))

  assert(stream.readInt == magic, "Wrong MNIST label stream magic")

  var count = stream.readInt
  var width: Int = 0
  var height: Int = 0
  readMeta()

  def readAll(index: Int): Stream[T] = {
    if (index >= count)
      Stream.empty
    else
      Stream.cons(readOnce, readAll(index + 1))
  }
  protected def readMeta(): Unit
  protected def readOnce(): T
}