package zero.perceptron.gate

trait Gate {
  def w1: Double
  def w2: Double
  def bias: Double
  def apply(x1: Int, x2: Int) = {
    val coef = List(w1, w2, bias)
    if (coef.zip(List(x1, x2, -1)).map(v => v._1*v._2).sum < 0) 0
    else 1
  }
}