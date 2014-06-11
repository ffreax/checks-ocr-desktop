package ua.pp.chuprin.checksocr

import javax.imageio.ImageIO
import java.io.File
import java.awt.{Rectangle, Color}
import scala.runtime.RichInt
import java.awt.image.{BufferedImage, Raster}

object Application extends App {
  val check = AtbRecognizer.recognize(ImageIO.read(new File("атб2.png")))

  check.products.foreach((product) => println(product.name + "\t - \t" + product.price))
  println("sum: " + check.sum)
}
