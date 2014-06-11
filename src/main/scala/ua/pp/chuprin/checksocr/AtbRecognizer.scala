package ua.pp.chuprin.checksocr

import java.awt.image.{AffineTransformOp, BufferedImage}
import javax.imageio.ImageIO
import java.io.File
import scala.collection.mutable.ArrayBuffer
import ua.pp.chuprin.checksocr.fontcache.AtbFontCache
import scala.math.abs
import scala.util.control.Breaks._
import ua.pp.chuprin.checksocr.Recognizer
import java.awt.geom.AffineTransform
import java.awt.Graphics2D

object AtbRecognizer extends Recognizer {

  val CHARACTER_WIDTH = 20
  val CHECK_WIDTH = 737

  val recognitionTable = AtbFontCache.buildRecognitionTable();

  def getScaled(image: BufferedImage) = {
    val left = Recognizer.leftBound(image)
    val right = Recognizer.rightBound(image)
    val actualWidth = right - left + 1

    val scale = (actualWidth + 0.0f) / CHECK_WIDTH
    scaleToBuffered(image, Math.round(image.getWidth / scale), Math.round(image.getHeight / scale))
  }
  
  def scaleToBuffered(image: BufferedImage, width : Int, height : Int) = {
    toBuffered(image.getScaledInstance(width, height, java.awt.Image.SCALE_DEFAULT), width, height);
  }

  override def recognize(image: BufferedImage) = {
    val rotatedImage = rotate(image)
    ImageIO.write(rotatedImage, "png", new File("generated/rotated.png"))

    val scaledImage = getScaled(rotatedImage)
    ImageIO.write(scaledImage, "png", new File("generated/scaled.png"))

    val images = Recognizer.splitLines(scaledImage)
    val name = new StringBuilder
    val products = new ArrayBuffer[Goods]
    for(i <- 4 until images.length - 10) {
      val namePrice = explodeNamePrice(images(i))
      name ++= recognizeProductLine(namePrice._1)
      if(!isEmpty(namePrice._2)) {
        products += new Goods(name.toString, recognizePrice(namePrice._2, 3))
        name.clear
      }
    }
    val totalPrice = readPrice(images(images.length - 10))

    var number = 0;
    images.foreach((image) => {
      ImageIO.write(image, "png", new File("generated/lines/" + number.toString + ".png"))
      number += 1;
    })

    new Check(products.toList, totalPrice)
  }

  private def rotate(image: BufferedImage) = {
    val at = new AffineTransform();

    val radians = DeSkew.skewRadians(image)
    at.rotate(radians, image.getWidth / 2, image.getHeight / 2);

    val op = new AffineTransformOp(at, AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
    op.filter(image, null);
  }

  private def explodeNamePrice(image: BufferedImage) = {
    (
      image.getSubimage(0, 0, (image.getWidth * 0.75).toInt, image.getHeight ),
      image.getSubimage((image.getWidth * 0.75).toInt, 0, image.getWidth - (image.getWidth * 0.75).toInt, image.getHeight )
    )
  }

  private def recognizeProductLine(image: BufferedImage) = {
    var number = 0
    var charImage = Recognizer.crop(image.getSubimage(number * CHARACTER_WIDTH, 0, CHARACTER_WIDTH, image.getHeight))

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
      if((number + 1) * CHARACTER_WIDTH < image.getWidth) {
        charImage = Recognizer.crop(image.getSubimage(number * CHARACTER_WIDTH, 0, CHARACTER_WIDTH, image.getHeight))
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

  private def recognizePrice(image: BufferedImage, padding: Int) = {
    var x = image.getWidth - CHARACTER_WIDTH * padding;
    var result = recognizeNumber(Recognizer.crop(image.getSubimage(x, 0, CHARACTER_WIDTH, image.getHeight))) * 0.01
    x -= CHARACTER_WIDTH
    result += recognizeNumber(Recognizer.crop(image.getSubimage(x, 0, CHARACTER_WIDTH, image.getHeight))) * 0.1
    x -= CHARACTER_WIDTH * 2

    var rank = 0
    var char = image.getSubimage(x, 0, CHARACTER_WIDTH, image.getHeight);
    breakable {
      while (true) {
        result += recognizeNumber(Recognizer.crop(char)) * Math.round(Math.pow(10, rank))

        rank += 1
        x -= CHARACTER_WIDTH
        if(x > 0) {
          char = image.getSubimage(x, 0, CHARACTER_WIDTH, image.getHeight)
          if (isEmpty(char)) {
            break
          }
        } else {
          break
        }
      }
    }

    result
  }

  private def readPrice(image: BufferedImage) = {
    val width = Math.round(image.getWidth / 4.0f * 3)
    val height = Math.round(image.getHeight / 4.0f * 3)

    recognizePrice(scaleToBuffered(image, width, height), 1)
  }

  private def toBuffered(image : java.awt.Image, width : Int, height : Int) = {
    val buffered = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for(i <- 0 until buffered.getWidth) {
      for(j <- 0 until buffered.getHeight) {
        buffered.setRGB(i, j, 0xFFFFFFFF)
      }
    }

    buffered.getGraphics().drawImage(image, 0, 0, null);

    buffered
  }

  private def recognizeNumber(image : BufferedImage) = {
    val result = recognizeChar(image) - '0'
    if(result < 0 || result > 9) {
      5
    } else {
      result
    }
  }

  private def isEmpty(image: BufferedImage) = {
    Recognizer.leftBound(image) == image.getWidth
  }
}
