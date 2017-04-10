package zero.neuralnet

import breeze.linalg._

import zero.functions._

object TwoLayer {
  val X = DenseMatrix(1.0,2.0)
  val W1 = DenseMatrix((1.0,3.0,5.0), (2.0,4.0,6.0))
  val B1 = DenseMatrix(1.0,2.0,3.0)
  println(X.t * W1 + B1.t)
  
  val Z1 = Sigmoid(X.t * W1 + B1.t)
  val W2 = DenseMatrix((1.0,3.0), (2.0,4.0), (3.0,5.0))
  val B2 = DenseMatrix(1.0,2.0)
  println(Z1 * W2 + B2.t)
  
  val Z2 = Sigmoid(Z1 * W2 + B2.t)
  val W3 = DenseMatrix((1.0, 3.0), (2.0, 4.0))
  val B3 = DenseMatrix(1.0, 3.0)
  println(Z2 * W3 + B3.t)
  
  Id(Z2 * W3 + B3.t)  
}