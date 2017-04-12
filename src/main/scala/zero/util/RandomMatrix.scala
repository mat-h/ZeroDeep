package zero.util

import breeze.linalg._
import java.util.Random

object RandomMatrix {
  val r = new Random(System.currentTimeMillis())
  
  def create(rows: Int, cols: Int) = {
    var matrix = randomVector(cols)
    for (i <- 0 until rows-1)
      matrix = DenseMatrix.vertcat(matrix, randomVector(cols))
    matrix
  }
  
  def randomVector(l: Int) = DenseMatrix((0 until l).map(_ => r.nextDouble))
  
}