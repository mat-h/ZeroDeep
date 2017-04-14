package zero.functions

import breeze.linalg._

object Loss {
  def mean_squared(y: DenseMatrix[Double], t: DenseMatrix[Double]) = 
    (y.toArray zip t.toArray)
      .map(p => Math.pow(p._1 - p._2, 2))
      .sum * 0.5
  
  def cross_entropy(y: DenseMatrix[Double], t: DenseMatrix[Double]) = 
    (y.toArray zip t.toArray)
      .map(p => p._2 * Math.log(p._1 + 1e-7))
      .sum * -1
}