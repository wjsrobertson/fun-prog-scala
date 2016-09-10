sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 3.25
// Write a function size that counts the number of nodes (leaves and branches) in a
// tree.
def size[A](tree: Tree[A]): Int = tree match {
  case Leaf(a) => 1
  case Branch(a, b) => 1 + size(a) + size(b)
}
size(Leaf(1))
size(Branch(Leaf(1), Leaf(2)))

// Exercise 3.26
// Write a function maximum that returns the maximum element in a Tree[Int]
def max(tree: Tree[Int]): Int = {
  def maxAcc(tree: Tree[Int], m: Int): Int = tree match {
    case Leaf(a) => Math.max(a, m)
    case Branch(a, b) => Math.max(maxAcc(a, m), maxAcc(b, m))
  }

  maxAcc(tree, 0)
}
max(Branch(Leaf(1), Leaf(2)))

// Exercise 3.27
// Write a function depth that returns the maximum path length from the root
// of a tree to any leaf.
def depth(tree: Tree[Int]): Int = {
  def depthAcc(tree: Tree[Int], m: Int): Int = tree match {
    case Leaf(a) => 1
    case Branch(a, b) => 1 + Math.max(depth(a), depth(b))
  }

  depthAcc(tree, 0)
}
depth(Branch(Leaf(1), Leaf(2)))
depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))

// Exercise 3.28
// Write a function map, analogous to the method of the same name on List,
// that modifies each element in a tree with a given function.
def map[A, B](tree: Tree[A])(f: (A) => A): Tree[A] = tree match {
  case Leaf(a) => Leaf(f(a))
  case Branch(a, b) => Branch(map(a)(f), map(b)(f))
}
map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1)

// Exercise 3.29
// Generalize size, maximum, depth, and map, writing a new function
// fold that abstracts over their similarities. Reimplement them in terms of
// this more general function.
def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
  case Leaf(a) => f(a, z)
  case Branch(a, b) => fold(b, fold(a, z)(f))(f)
}

def size2(tree: Tree[Int]) = fold(tree, 0)( (a,b) => b+1 )
size2(Branch(Leaf(1), Leaf(9)))

def max2(tree: Tree[Int]) = fold(tree, 0)( (a,b) => Math.max(a,b) )
max2(Branch(Leaf(1), Leaf(9)))

def depth2(tree: Tree[Int]) = ??? //fold(tree, 0)( (a,b) => b+1 )

def map2[A,B](tree: Tree[A])(f: (A) => B): Tree[B] = ???