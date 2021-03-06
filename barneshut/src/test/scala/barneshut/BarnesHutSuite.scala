package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"centerX ${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"centerY ${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"mass ${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"massX ${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"massY ${quad.massY} should be 26f")
    assert(quad.total == 1, s"total ${quad.total} should be 1")
  }

  test("Fork with 4 empty quadrants") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"centerX ${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"centerY ${quad.centerY} should be 30f")
    assert(quad.mass ~= 0f, s"mass ${quad.mass} should be 0f")
    assert(quad.massX ~= 20f, s"massX ${quad.massX} should be 18f")
    assert(quad.massY ~= 30f, s"massY ${quad.massY} should be 26f")
    assert(quad.total == 0, s"total ${quad.total} should be 0")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"centerX $centerX should be 51f")
        assert(centerY == 46.3f, s"centerY $centerY should be 46.3f")
        assert(size == 5f, s"size $size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Leaf.insert(b) should return a new Fork if size > minimumSize") {
    val quad = Leaf(20f, 30f, 10f, List())
    val b = new Body(3f, 17f, 27f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.centerX == 17.5f, s"centerX ${nw.centerX} should be 17.5f")
        assert(nw.centerY == 27.5f, s"centerY ${nw.centerY} should be 27.5f")
        assert(nw.size.toFloat == 5f, s"size ${nw.size} should be 5f")
      case _ =>
        fail(s"Leaf.insert() should have returned a Fork, was $inserted")
    }
  }

  test("'insert' should work correctly on a leaf with center (1,1) and size 2") {
    val quad = Leaf(1f, 1f, 2f, List(new Body(5.0f, 0.4f, 0.4f, 0.1f, 0.1f)))
    val b = new Body(5.0f, 1.4f, 0.4f, 0.1f, 0.1f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) => {
        nw match {
          case Leaf(centerX, centerY, size, bodies) =>
            assert(nw.centerX == 0.5f, s"centerX ${nw.centerX} should be 0.5f")
            assert(nw.centerY == 0.5f, s"centerY ${nw.centerY} should be 0.5f")
            assert(nw.size.toFloat == 1f, s"size ${nw.size} should be 1f")
            assert(nw.total == 1, s"total ${nw.total} should be 1")
          case _ => fail(s"nw should be a leaf, was $nw")
        }
      }
      case _ =>
        fail(s"Leaf.insert() should have returned a Fork, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  test("'Simulator.updateBoundaries' with a body at (25,47)") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1f
    boundaries.minY = 0f
    boundaries.maxX = 99f
    boundaries.maxY = 100f

    val updateBoundaries: Boundaries = new Simulator(null, null).updateBoundaries(boundaries, body)
    assert(updateBoundaries.minX == 1f, s"minX ${updateBoundaries.minX} should be 1f")
    assert(updateBoundaries.maxX == 99f, s"maxX ${updateBoundaries.maxX} should be 99f")
    assert(updateBoundaries.minY == 0f, s"minY ${updateBoundaries.minY} should be 0f")
    assert(updateBoundaries.maxY == 100f, s"maxY ${updateBoundaries.maxY} should be 100f")
  }

}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }

}

