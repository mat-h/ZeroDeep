package zero.functions

import breeze.linalg.DenseMatrix
import scala.math.exp
import scala.math.max

trait MatrixFun {
  def apply(x: Double): Double
  def apply(xs: List[Double]): List[Double] = xs.map(this(_))
  def apply(mat: DenseMatrix[Double]): DenseMatrix[Double] = mat.map(this(_))
}

object Step extends MatrixFun {
  def apply(x: Double): Double = if (x < 0) 0 else 1
}

object ReLU extends MatrixFun {
  def apply(x: Double): Double = max(0, x)
}

object Sigmoid extends MatrixFun {
  def apply(x: Double): Double = 1 / (1 + exp(-x))
}

object Id extends MatrixFun {
  def apply(x: Double): Double = x
}

object SoftMax {
  def apply(xs: DenseMatrix[Double]): DenseMatrix[Double] =
    xs.map(_ - xs.toArray.max)
      .map(exp)
      .map(_ / xs.map(exp).sum)
}