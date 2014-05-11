package ua.pp.chuprin.checksocr

import java.awt.image.BufferedImage
import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

trait Recognizer {
  def recognize(image : BufferedImage) : Check

  class Check(val products : List[Any], val sum : Float) {
  }

  class Product(val name : String, val price : Int) {
  }
}

object Recognizer {
  def isWhite(rgbValue : Int) = {
    val color = explode(rgbValue)

    val inWhiteRange = inRange(255, 250) _

    inWhiteRange(color._2) && inWhiteRange(color._3) && inWhiteRange(color._4)
  }

  def explode(rgbValue: Int) = {
    ((rgbValue >> 24) & 0xFF, (rgbValue >> 16) & 0xFF, (rgbValue >> 8) & 0xFF, (rgbValue) & 0xFF)
  }

  private def inRange (max : Int, min : Int) (value : Int) = {
    value <= max && value >= min
  }

  def splitLines(image: BufferedImage, cropStart : Boolean = true, cropEnd : Boolean = true): List[BufferedImage] = {
    var start = 0
    if(cropStart) {
      start = leftBound(image)
    }

    var end = image.getWidth - 1
    if(cropEnd) {
      end = rightBound(image)
    }

    if(end > start) {
      var heigth = 0
      val lines = ArrayBuffer[BufferedImage]()
      for(y <- 0 until image.getHeight) {
        var whiteCount : Int = 0
        for(x <- 0 until image.getWidth) {
          if (isWhite(image.getRGB(x, y))) whiteCount += 1
        }
        if (whiteCount == image.getWidth) { // white line
          if(heigth > 10) {
            lines += image.getSubimage(start, y - heigth, end - start, heigth)
          }
          heigth = 0
        } else {
          heigth += 1
        }
      }

      lines.toList
    } else {
      List.empty
    }
  }

  def leftBound(image: BufferedImage) : Int = {
    for(x <- 0 until image.getWidth) {
      var whiteCount : Int = 0
      for (y <- 0 until image.getHeight) {
        if (isWhite(image.getRGB(x, y))) whiteCount += 1
      }
      if (whiteCount < image.getHeight) { // black column
        return x
      }
    }

    image.getWidth
  }

  def topBound(image: BufferedImage) : Int = {
    for (y <- 0 until image.getHeight) {
      var whiteCount : Int = 0
      for(x <- 0 until image.getWidth) {
        if (isWhite(image.getRGB(x, y))) whiteCount += 1
      }
      if (whiteCount < image.getWidth) { // black column
        return y
      }
    }

    image.getHeight
  }

  def rightBound(image: BufferedImage) : Int = {
    var x = image.getWidth - 1;
    while(x >= 0) {
      var whiteCount : Int = 0
      for (y <- 0 until image.getHeight) {
        if (isWhite(image.getRGB(x, y))) whiteCount += 1
      }
      if (whiteCount < image.getHeight) { // black column
        return x
      }
      x -= 1
    }

    0
  }

  def bottomBound(image: BufferedImage) : Int = {
    var y = image.getHeight - 1
    while (y >= 0) {
      var whiteCount : Int = 0
      for(x <- 0 until image.getWidth) {
        if (isWhite(image.getRGB(x, y))) whiteCount += 1
      }
      if (whiteCount < image.getWidth) { // black column
        return y
      }
      y -= 1
    }

    0
  }

  def crop(image: BufferedImage) = {
    val left = leftBound(image)
    val top = topBound(image)

    if(left == image.getWidth || top == image.getHeight)
      image.getSubimage(0, 0, 1, 1)
    else
      image.getSubimage(left, top, rightBound(image) - left + 1, bottomBound(image) - top + 1)
  }

  def gray(image: BufferedImage) = {
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, AtbRecognizer.characterWidth).
      map(Recognizer.explode _).
      map(color => color._2 * 0.229 + color._3 * 0.587 + color._4 * 0.114)
  }
}
