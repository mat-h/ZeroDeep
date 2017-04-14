package zero.neuralnet

import breeze.linalg._

import zero.functions._
import zero.util.Matrix.{ randomMatrix, unitVec, partialMatrices }

class TwoLayer(val shape: List[Int] = List(2, 3, 2)) {

  require(shape.length == 3)

  var W1 = randomMatrix(shape(0), shape(1))
  var B1 = randomMatrix(1, shape(1))
  var W2 = randomMatrix(shape(1), shape(2))
  var B2 = randomMatrix(1, shape(2))

  val dim = shape(0)

  def predict(input: DenseMatrix[Double]): DenseMatrix[Double] = {
    assert(input.rows == dim, "Illegal input size")

    val Z1 = Sigmoid(input.t * W1 + B1)
    SoftMax(Z1 * W2 + B2)
  }

  def loss(teacher: DenseMatrix[Double])(input: DenseMatrix[Double]) =
    lossWith(this)(teacher)(input)

  def lossWith(model: TwoLayer)(teacher: DenseMatrix[Double])(input: DenseMatrix[Double]) =
    Loss.cross_entropy(model.predict(input.t), teacher)

  type Overrider = TwoLayer => Unit
  def lossUsing(teacher: DenseMatrix[Double], input: DenseMatrix[Double])(o: Overrider) = {
    val model = copy
    o(model)
    lossWith(model)(teacher)(input)
  }

  def copy = {
    val result = new TwoLayer(shape)
    result.W1 = W1
    result.B1 = B1
    result.W2 = W2
    result.B2 = B2
    result
  }

  def accuracy(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) =
    (if (argmax(predict(input.t))._2 == argmax(teacher)._2) 1 else 0)

  def gradient(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {
    val h = 1e-4
    val f = lossUsing(teacher, input) _

    Map("W1" ->
      partialMatrices(W1)
      .map(_ :*= h)
      .map(dx => {
        (f(m => m.W1 = m.W1 + dx) - f(m => m.W1 = m.W1 - dx)) / (2 * h)
      }),

      "W2" ->
        partialMatrices(W2)
        .map(_ :*= h)
        .map(dx => {
          (f(m => m.W2 = m.W2 + dx) - f(m => m.W2 = m.W2 - dx)) / (2 * h)
        }),

      "B1" ->
        partialMatrices(B1)
        .map(_ :*= h)
        .map(dx => {
          (f(m => m.B1 = m.B1 + dx) - f(m => m.B1 = m.B1 - dx)) / (2 * h)
        }),

      "B2" ->
        partialMatrices(B2)
        .map(_ :*= h)
        .map(dx => {
          (f(m => m.B2 = m.B2 + dx) - f(m => m.B2 = m.B2 - dx)) / (2 * h)
        }))
  }
}