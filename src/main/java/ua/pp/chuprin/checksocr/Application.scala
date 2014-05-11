package ua.pp.chuprin.checksocr

import javax.imageio.ImageIO
import java.io.File
import java.awt.{Rectangle, Color}
import scala.runtime.RichInt
import java.awt.image.{BufferedImage, Raster}

object Application extends App {
  val check = AtbRecognizer.recognize(ImageIO.read(new File("d:/master/study/10-semester/распознавание образов/чеки/атб2.png")))
  println(check.products.length)
}
