package ua.pp.chuprin.checksocr

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import scala.collection.mutable.ArrayBuffer
import ua.pp.chuprin.checksocr.fontcache.AtbFontCache
import scala.math.abs

object AtbRecognizer extends Recognizer {

  val characterWidth = 20

  val recognitionTable = AtbFontCache.buildRecognitionTable();

  override def recognize(image: BufferedImage): Check = {
    val images = Recognizer.splitLines(image)
    val name = new StringBuilder
    val products = new ArrayBuffer[Product]
    for(i <- 4 until images.length - 10) {
      val namePrice: (BufferedImage, BufferedImage) = explodeNamePrice(images(i))
      name ++= recognizeProductLine(namePrice._1)
      if(!isEmpty(namePrice._2)) {
        products += new Product(name.toString, recognizePrice(namePrice._2))
        name.clear
      }
    }
    val totalPrice = readPrice(images(images.length - 10))

    // TODO move
    var number = 0;
    images.foreach((image) => {
      ImageIO.write(image, "png", new File("generated/lines/" + number.toString + ".png"))
      number += 1;
    })

    new Check(products.toList, totalPrice)
  }


  private def explodeNamePrice(image: BufferedImage) = {
    (
      image.getSubimage(0, 0, (image.getWidth * 0.75).toInt, image.getHeight ),
      image.getSubimage((image.getWidth * 0.75).toInt, 0, image.getWidth - (image.getWidth * 0.75).toInt, image.getHeight )
    )
  }

  private def recognizeProductLine(image: BufferedImage) = {
    var number = 0
    var charImage = Recognizer.crop(image.getSubimage(number * characterWidth, 0, characterWidth, image.getHeight))

    val line = new StringBuilder
    var empty = 0;
    while(empty < 2 && charImage != null) {
      if(isEmpty(charImage)) {
        empty += 1
        line += ' '
      } else {
        empty = 0
        line += recognizeChar(charImage)
      }

      number += 1
      if((number + 1) * characterWidth < image.getWidth) {
        charImage = Recognizer.crop(image.getSubimage(number * characterWidth, 0, characterWidth, image.getHeight))
      } else  {
        charImage = null
      }
    }

    line
  }


  def recognizeChar(charImage: BufferedImage): Char = {
    var minError = Double.PositiveInfinity
    var resultChar = '_'
    for ((char, data) <- recognitionTable) {
      val error = calcError(charImage, data);
      if (error < minError) {
        minError = error
        resultChar = char
      }
    }

    resultChar
  }

  def calcError(image: BufferedImage, data: Array[Double]) = {
    val gray = Recognizer.gray(image)
    if(gray.length == data.length) {
      var error = 0.0
      for(i <- 0 until gray.length) {
        error += abs(gray(i) - data(i))
      }
      error
    } else {
      Double.PositiveInfinity
    }
  }

  private def recognizePrice(image: BufferedImage): Int = {
    0
  }

  private def readPrice(image: BufferedImage) = {
    0
  }

  private def isEmpty(image: BufferedImage) = {
    Recognizer.leftBound(image) == image.getWidth
  }

}
