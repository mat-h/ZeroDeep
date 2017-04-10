package zero.perceptron.gate

object Xor {
  def apply(x1: Int, x2: Int) = {
    val s1 = Nand(x1, x2)
    val s2 = Or(x1, x2)
    And(s1, s2)
  }
}