
import common._

import scala.reflect.macros.whitebox

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var totalRed: Int = 0;
    var totalGreen: Int = 0;
    var totalBlue: Int = 0;
    var totalAlpha: Int = 0;
    var count = 0;
    for (dx <- -radius to radius;
         dy <- -radius to radius) {

      val posX: Int = x + dx
      val posY: Int = y + dy
      if (posX < src.width && posX >= 0 && posY >= 0 && y + dy < src.height) {
        totalRed += red(element(src, posX, y + dy));
        totalGreen += green(element(src, posX, y + dy));
        totalBlue += blue(element(src, posX, y + dy));
      //  println("alpha: " + element(src, posX, y + dy) + " (" + posX + "," + (y + dy) + ")")
        totalAlpha += alpha(element(src, posX, y + dy));
        count += 1;
      }
    }
    rgba(totalRed / count, totalGreen / count, totalBlue / count, totalAlpha / count);
  }

  def element(src: Img, x: Int, y: Int): RGBA = {
    src(clamp(x, 0, src.width - 1), clamp(y, 0, src.height - 1))
  }
}
