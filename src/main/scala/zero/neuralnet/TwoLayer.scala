package zero.neuralnet

import breeze.linalg._
import java.io.PrintWriter

import zero.functions._
import zero.minibatch.Sampler
import zero.util.Matrix.{ randomMatrix, unitVec, variationMatrix }
import zero.minibatch.Sampler

class TwoLayer(val shape: List[Int] = List(2, 3, 2)) {

  require(shape.length == 3)

  var W1 = randomMatrix(shape(0), shape(1))
  var B1 = randomMatrix(1, shape(1))
  var W2 = randomMatrix(shape(1), shape(2))
  var B2 = randomMatrix(1, shape(2))

  val dim = shape(0)

  def predict(input: DenseMatrix[Double]): DenseMatrix[Double] = {
    val r = input.rows
    assert(r == dim, s"Illegal input size $r != $dim")

    val Z1 = Sigmoid(input.t * W1 + B1)
    SoftMax(Z1 * W2 + B2)
  }

  def loss(teacher: DenseMatrix[Double])(input: DenseMatrix[Double]) =
    lossWith(this)(teacher)(input)

  def lossWith(model: TwoLayer)(teacher: DenseMatrix[Double])(input: DenseMatrix[Double]) =
    Loss.cross_entropy(model.predict(input), teacher)

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

  def argmax(seq: Seq[Any]) = seq.zipWithIndex.maxBy(_._2)._1

  def accuracy(input: Seq[DenseMatrix[Double]], teacher: Seq[Double]) =
    if (argmax(input.map(predict)) == argmax(teacher)) 1 else 0

  var grad: Map[String, Seq[Double]] = Map()

  def gradient(input: DenseMatrix[Double], teacher: DenseMatrix[Double]) = {
    val h = 1e-4
    val f = lossUsing(teacher, input) _
    
    assert(input.rows > 1)

    type Getter = TwoLayer => DenseMatrix[Double]
    type Setter = TwoLayer => (DenseMatrix[Double] => Unit)
    
    def update(param: Getter)(setter: Setter) = {
      def F(_w: DenseMatrix[Double]): Double = f(setter(_)(_w))

      val in = param(this)
      println(in.toString() + ", " + in.rows + " x " + in.cols)
    
      variationMatrix(in.rows, in.cols)
      .map(_ :*= h)
      .map(dx => (F(param(this) + dx.toDense) - F(param(this) - dx.toDense)) / (2 * h))
    }

    grad = Map(
      "W1" -> update(_.W1)(_.W1_=),
      "W2" -> update(_.W2)(_.W2_=),
      "B1" -> update(_.B1)(_.B1_=),
      "B2" -> update(_.B2)(_.B2_=)
    )

  }

  def dump = {
    println(s"W1 = $W1")
    println(s"B1 = $B1")
    println(s"W2 = $W2")
    println(s"B2 = $B2")
  }
}

object TwoLayer {
  def train(train_data: Seq[(DenseMatrix[Double], Int)]) = {
    val sampler = new Sampler(train_data)
    val miniBatch = sampler take (100)

    val logFile = new PrintWriter("log/loss.txt")

    val m = new TwoLayer(List(784, 50, 10))

    def trainOnce =
      miniBatch.map(p => {
        val teacher = unitVec(10, p._2)
        m.gradient(p._1, teacher)
        def convert(seq: Option[Seq[Double]], rows: Int) = (seq: @unchecked) match { case Some(g) => new DenseMatrix(rows, g.toArray, 0) }
        convert(m.grad.get("W1"), m.W1.rows)
        convert(m.grad.get("W2"), m.W2.rows)
        convert(m.grad.get("B1"), m.B1.rows)
        convert(m.grad.get("B2"), m.B2.rows)
        logFile.write(m.loss(teacher)(p._1).toString)
      })

    for (i <- 0 to 1000) trainOnce
  }
}