// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

package basics

object ClassesAndTraits {
  class MutablePoint(var x: Double, var y: Double) {
    def move(dx: Double, dy: Double): Unit = {
      x = x + dx
      y = y + dy
    }
    override def toString: String =
      s"($x, $y)"
  }

  sealed trait Shape extends Located with Bounded

  sealed trait Shape3D extends Shape with Located3D with Bounded3D

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
  }

  final case class Rectangle(leftX: Double, bottomY: Double, length: Double, height: Double) extends Shape {
    override def minX: Double = leftX
    override def maxX: Double = leftX + length
    override def minY: Double = bottomY
    override def maxY: Double = bottomY + height
    override def x: Double = leftX
    override def y: Double = bottomY
  }

  final case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape {
    override def minX: Double = Set(point1, point2, point3).map(_.minX).min
    override def maxX: Double = Set(point1, point2, point3).map(_.maxX).max
    override def minY: Double = Set(point1, point2, point3).map(_.minY).min
    override def maxY: Double = Set(point1, point2, point3).map(_.maxY).max
    override def x: Double = minX
    override def y: Double = minY
  }

  final case class Square(leftX: Double, bottomY: Double, length: Double) extends Shape {
    override def minX: Double = leftX
    override def maxX: Double = leftX + length
    override def minY: Double = bottomY
    override def maxY: Double = bottomY + length
    override def x: Double = leftX
    override def y: Double = bottomY
  }

  final case class Cube(pointX: Point3D, pointY: Point3D, pointZ: Point3D) extends Shape3D {
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???
  }

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).max
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  def describe(x: Shape): String = x match {
    case Point(x, y) => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius) => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
    case Rectangle(leftX, bottomY, lenght, height) => s"Rectangle(leftX = $leftX, bottomY = $bottomY, lenght = $lenght"
      s", height = $height"
    case Triangle(point1, point2, point3) => s"Triangle(" + describe(point1) + ", " + describe(point2) + ", "  +
      describe(point3) + " )"
    case Square(leftX, bottomY, length) => s"Square(leftX = $leftX, bottomY = $bottomY, lenght = $length)"
  }

  def area(x: Shape): Double = x match {
    case Point(x, y) => 0
    case Circle(centerX, centerY, radius) => Math.PI * radius * radius
    case Rectangle(leftX, bottomY, length, height) => length * height
    case Square(leftX, bottomY, length) => length * length
    case Triangle(point1, point2, point3) =>
      Math.abs((point1.x * (point2.y - point3.y) + point2.x * (point3.y - point1.y) +
        point3.x * (point1.y - point2.y)) / 2)
  }

  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded =
      ClassesAndTraits.minimumBoundingRectangle(objects)
  }
}
