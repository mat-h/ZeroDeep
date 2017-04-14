package zero.neuralnet

import breeze.linalg._

import zero.functions._
import zero.util.Matrix.{randomMatrix, unitVec}

class TwoLayer(val shape: List[Int] = List(2,3,2)) {

  require(shape.length == 3)
  
  val W1 = randomMatrix(shape(0), shape(1))
  val B1 = randomMatrix(1, shape(1))
  val W2 = randomMatrix(shape(1), shape(2))
  val B2 = randomMatrix(1, shape(2))
  
  val dim = shape(0)
    
  def predict(input: DenseMatrix[Double]): DenseMatrix[Double] = {
    assert(input.rows == dim, "Illegal input size")

    val Z1 = Sigmoid(input.t * W1 + B1)
    Id(Z1 * W2 + B2)
  }

  def loss(teacher: DenseMatrix[Double])(input: DenseMatrix[Double]) =
    Loss.cross_entropy(predict(input.t), teacher)
    
  def accuracy(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {}
  
  def gradient(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {
    val h = 1e-4
    val f = loss(teacher) _

    (0 until dim)
      .map(unitVec(dim, _) * h)
      .map(dx => (f(input + dx) - f(input - dx)) / (2 * h))
  }
}