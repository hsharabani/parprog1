package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 0") {
    val output = new Array[Float](0)
    lineOfSight(Array[Float](), output)
    assert(output.toList == List())
  }

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements with big values at start") {
    val res = upsweepSequential(Array[Float](0f, 8f, 2f, 9f), 1, 4)
    assert(res == 8f)
  }

  test("upsweepSequential should correctly handle the chunk 2 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 8f, 2f, 9f), 2, 4)
    assert(res == 3f)
  }

  test("upsweep should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, 2)
    assert(res.maxPrevious == 4f)
  }

  test("upsweep should correctly handle the chunk 1 until 4 of an array of 4 elements threshold 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, 1)
    assert(res.maxPrevious == 4f)
  }

  test("upsweep should correctly handle the chunk 3 until 4 of an array of 4 elements") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f), 3, 4, 2)
    assert(res.maxPrevious == 3f)
  }

  test("upsweep should correctly handle the chunk 1 until 17 of an array of 17 elements") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 0f, 1f, 8f, 9f, 0f, 1f, 8f, 9f, 0f, 1f, 8f, 9f, 10f), 1, 17, 1)
    assert(res.maxPrevious == 4f)
    println()
  }

  test("parLineOfSight should correctly handle a 17 element array when the starting angle is zero") {
    val output = new Array[Float](17)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 0f, 1f, 8f, 9f, 0f, 1f, 8f, 9f, 0f, 1f, 8f, 9f, 10f), output, 1)
    assert(output.toList == List(0f, 1f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f))
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("parLineOfSight should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 2)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
}

