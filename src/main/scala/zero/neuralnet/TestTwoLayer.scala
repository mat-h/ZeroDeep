package zero.neuralnet

object TestTwoLayer extends App {
  import data.mnist.Loader

  import breeze.linalg._

  import zero.neuralnet._

  TwoLayer.train(Loader.train_img.toSeq.map(convert(_, Double)) zip Loader.train_label.toSeq.map(_.toInt))

}