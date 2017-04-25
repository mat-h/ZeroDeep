package zero.util

import breeze.linalg._
import java.util.Random

object Matrix {
  val r = new Random(System.currentTimeMillis())

  def randomMatrix(rows: Int, cols: Int) = {
    var matrix = randomVector(cols)
    for (i <- 0 until rows - 1)
      matrix = DenseMatrix.vertcat(matrix, randomVector(cols))
    matrix
  }

  def randomVector(l: Int) = DenseMatrix((0 until l).map(_ => r.nextDouble))

  private def _0s = List.fill(_: Int)(0.0)

  def unitVec(dim: Int, i: Int) = new DenseVector[Double](_0s(i) ::: 1.0 :: _0s(dim - i + 1) toArray) toDenseMatrix

  private def oneHotMatrix(shape: Tuple2[Int, Int], idx: Tuple2[Int, Int]) = {
    val _0s = CSCMatrix.zeros[Double](shape._1, shape._2)
    _0s(idx._1 to idx._1, idx._2 to idx._2) := 1.0
    _0s
  }

  def variationMatrix(r: Int, c: Int) = {
    println(r + " x " + c)
    (for (i <- 0 until r; j <- 0 until c) yield (i, j))
      .map(oneHotMatrix((r, c), _))
  }
}