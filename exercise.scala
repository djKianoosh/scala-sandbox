
// Start with simple addition and subtraction functions
def succ(n: Int) = n + 1
def pred(n: Int) = n - 1

def add(x: Int, y: Int): Int = {
    if (x == 0) y 
    else if (y == 0) x
    else add (succ(x), pred(y))
}

def safeAdd(x: Int, y: Int): Int = {
  if (x>=y) add(y,x)
    else add (x,y)
}

def sum(list: List[Int]): Int = {
  list.foldLeft(0) {(a,b) => add(a,b)}
}
/* or  list foreach {i => sum = add(sum, i)} */


def length[A] (list: List[A]): Int = {
  list.foldLeft(0) {
    (a,b) => succ(a)
  }
}

// Vince
def sumString(list: List[Int]): String = {
  list.foldLeft("") {
    (str, i) => 
      if (str=="")
         i.toString
      else 
        str + ", " + i
  }
}

def concat2(l: List[String]): String = {
  ("" /: l) {_+_}
}

// Kia
def concatWith(delim: String, list: List[Int]): String = {
  list.foldLeft("") {
    (str, i) => 
      if (str=="")
         i.toString
      else 
        concat2(List(str, delim, i.toString))
  }
}

// Matt
def intersection(firstList: List[Int], secondList: List[Int]): List[Int] = {
  firstList.intersect(secondList)
}

