package zero.neuralnet

import breeze.linalg._

import zero.functions._
import zero.util.RandomMatrix

object TwoLayer {

  val W1 = RandomMatrix.create(2, 3)
  val B1 = RandomMatrix.create(1, 3)
  val W2 = RandomMatrix.create(3, 2)
  val B2 = RandomMatrix.create(1, 2)
  

  def predict(X: DenseMatrix[Double]): DenseMatrix[Double] = {
    assert(X.rows == 2, "Illegal input size")

    val Z1 = Sigmoid(X.t * W1 + B1)
    Id(Z1 * W2 + B2)

    //    val Z2 = Sigmoid(Z1 * W2 + B2.t)
    //    val W3 = DenseMatrix((1.0, 3.0), (2.0, 4.0))
    //    val B3 = DenseMatrix(1.0, 3.0)
    //    println(Z2 * W3 + B3.t)

    //    Id(Z2 * W3 + B3.t)
  }

  def loss(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) =
    Loss.cross_entropy(predict(input).toArray.toSeq, teacher.toArray.toSeq)
    
  def accuracy(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {}
  def gradient(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {}
}