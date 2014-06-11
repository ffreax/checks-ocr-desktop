package ua.pp.chuprin.checksocr.fontcache

import ua.pp.chuprin.checksocr.{AtbRecognizer, Recognizer}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

object AtbFontCache extends App {

  def buildRecognitionTable() = {
    val content = Array("""!"#$%&'()*+,-./0123456789:;<=>?@[\]^_`{|}~""",
      """ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz""",
      """АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯІЇабвгдеёжзийклмнопрстуфхцчшщъыьэюяії""")

    val lines = Recognizer.splitLines(ImageIO.read(new File("src/main/resources/ua/pp/chuprin/checksocr/atb.png")), true, false)

    assert(lines.length == content.length)

    val recognitionTable = scala.collection.mutable.Map[Char, Array[Double]]()
    for(i <- 0 until lines.length) {
      val line = lines(i)
      for(j <- 0 until content(i).length) {

        val cropped = Recognizer.crop(line.getSubimage(j * AtbRecognizer.CHARACTER_WIDTH, 0, AtbRecognizer.CHARACTER_WIDTH, line.getHeight))
        ImageIO.write(cropped, "png", new File("generated/chars/" + i + "_" + j + ".png"))

        recognitionTable(content(i).charAt(j)) = Recognizer.gray(cropped)
      }
    }

    recognitionTable
  }
}
