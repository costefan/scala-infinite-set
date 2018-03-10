import scala.annotation.tailrec

type Set = Int => Boolean


def contains(s: Set, elem: Int): Boolean = s(elem)

def singletonSet2(elem: Int): Set = {
  def innerFunction(given: Int) =
    if (elem == given) true
    else false

  innerFunction
}

def singletonSet(elem: Int): Set =
  (given: Int) => given == elem



val a = singletonSet(3)
val b = singletonSet(2)

contains(a, 1)
a(2)
a(1)

def union2(s: Set, t: Set): Set = {
  def innerFunc(given: Int) =
    if (s(given) || t(given)) true
    else false

  innerFunc
}


def union(s: Set, t: Set): Set =
  (given: Int) => s(given) || t(given)


val anotherUnionSet = union(a, b)

anotherUnionSet(2)
anotherUnionSet(1)
anotherUnionSet(3)

def intersect(s: Set, t: Set): Set =
  (given: Int) => s(given) && b(given)

intersect(a, b)(3)

def diff(s: Set, t: Set): Set =
  (given: Int) =>  !(s(given) && t(given))

diff(a, b)(2)
diff(a, b)(2)

def filter(s: Set, p: Int => Boolean): Set =
  (given: Int) => s(given) && p(given)

val xFilter = (g: Int) => g > 4

filter(a, xFilter)(2)

filter(union(a, b), xFilter)(2)

val filter = (a: Int) => a < 3

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > 1000) true
    else if (s(a) && !p(a)) false
    else iter(a + 1)
  }
  iter(-1000)

}


forall(union(a, b), xFilter)
val notFilter = filter

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

println("Here exists output")
val xFilter2 = (g: Int) => g > 4

exists(union(a, b), xFilter2)

def map1(s: Set, f: Int => Int): Set = {
  def inner(elem: Int): Boolean = {
    if (s(f(elem))) {
      println(elem)
      println(f(elem))
      println(s(f(elem)))
      true
    }
    else false
  }

  inner
}

def map(s: Set, f: Int => Int): Set =  (elem: Int) => exists(s,
  (el2) => f(el2) == elem)


  map(union(a, b), (g: Int) => g + 1)(0)
  map(union(a, b), (g: Int) => g + 1)(1)
  map(union(a, b), (g: Int) => g + 1)(2)
  map(union(a, b), (g: Int) => g + 1)(3)