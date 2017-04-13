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
  }

  def loss(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) =
    Loss.cross_entropy(predict(input.t).toArray.toSeq, teacher.toArray.toSeq)
    
  def accuracy(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {}
  def gradient(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {
    val f = (x:DenseMatrix[Double]) => loss(x, teacher)
    val h = 1e-4
    val zeroes = List.fill(input.size)(0.0)
    val dx_vectors = (0 until input.size).map(i => {
      new DenseMatrix[Double](1, zeroes.take(i) ::: h :: zeroes.drop(i+1) toArray, 0)
    })
    
    dx_vectors.map(dx => {
      (f(input + dx) - f(input - dx))/ (2 * h)
    })
  }
}