package zero.functions

object Loss {
  def mean_squared(y: Seq[Double], t: Seq[Double]): Double = 0.5* (y zip t).map(p => Math.pow(p._1 - p._2, 2)).sum
  def cross_entropy(y: Seq[Double], t: Seq[Double]): Double = -1 *(y zip t).map(p => p._2 * Math.log(p._1 + 1e-7)).sum
}