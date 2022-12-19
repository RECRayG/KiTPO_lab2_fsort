package com.lab2.scala.doublyLinkedList

import com.lab2.java.doublyLinkedList.DoWith
import com.lab2.java.doublyLinkedList.Comparator
import com.lab2.java.doublyLinkedList.DoublyLinkedList

import scala.::


class MyLinkedList[T] extends DoublyLinkedList[T] {
  private var head: Node = _
  private var tail: Node = _
  private var length: Int = 0
  private var nde: Int = 0

  def addM(data: T): Unit = {
    if (head == null) {
      head = new Node(Some(data))
      tail = head
      length += 1
      return
    }
    val newTail = new Node(Some(data))
    newTail.prev = tail
    tail.next = newTail
    tail = newTail
    length += 1
  }

  def getM(index: Int): T = getNode(index).data.get

  def forEach(action: T => Unit): Unit = {
    var tmp = head
    for (_ <- 0 until length) {
      action(tmp.data.get)
      tmp = tmp.next
    }
  }

  def sizeM: Int = length

  def removeM(index: Int): Unit = {
    val tmp = getNode(index)
    if (tmp == head) {
      tmp.prev = null
      head = tmp.next
    } else {
      if (tmp.prev != null) {
        tmp.prev.next = tmp.next
        tmp.prev = null
      }
    }

    if (tmp == tail) {
      tmp.next = null
      tail = tmp.prev
    } else {
      if (tmp.next != null) {
        tmp.next.prev = tmp.prev
        tmp.next = null
      }
    }

    tmp.next = null
    tmp.prev = null
    tmp.data = null
    length -= 1
  }

  def removeAllM(): Unit = {
    if(head != null) {
      var tmp = getNode(0)
      while({tmp.next != null}) {
        removeM(0)
        tmp = getNode(0)
      }
      removeM(0)
    }
  }

  def addM(data: T, index: Int): Unit = {
    val tmp = getNode(index)
    val newNode = new Node(Some(data))
    if (tmp != null) {
      tmp.prev.next = newNode
      newNode.prev = tmp.prev
    }
    else {
      head = newNode
    }
    newNode.next = tmp
    tmp.prev = newNode
    length += 1
  }

  def forEachM(a: DoWith[T]): Unit = {
    var tmp = head
    for (_ <- 0 until length) {
      a.doWith(tmp.data.get)
      tmp = tmp.next
    }
  }

  def insert(x: T, xs: List[T]): List[T] =
    if(xs.isEmpty) x :: xs else xs.head :: insert(x, xs.tail)

  def copy(): List[T] = {
    var lst = List[T]()
    var temp = this.head
    while (temp != null) {
      lst = insert(temp.data.get, lst)
      temp = temp.next
    }
    lst
  }

  def copyBack(xs: List[T]): List[T] = {
    if(!xs.isEmpty) {
      this.addM(xs.head)
      copyBack(xs.tail)
    }
    xs
  }

  def sort(comparator: Comparator[T]): Unit = {
    var lst = List[T]()
    lst = copy()
    lst = mergeSort(comparator, lst)
    this.removeAllM()
    copyBack(lst)
  }


  def mergeSort(comparator: Comparator[T], xs: List[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (comparator.compare(x,y) < 0) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

      val (left, right) = xs splitAt (n)
      merge(mergeSort(comparator,left), mergeSort(comparator,right))
    }
  }

//  def msort(xs: MyLinkedList[T], comparator: Comparator[T]): MyLinkedList[T] = {
//    val n = xs.length / 2
//    if (n == 0) xs
//    else {
//      def merge(xs: MyLinkedList[T], ys: MyLinkedList[T], comparator: Comparator[T]) = ???
//      val (fst, snd) = xs splitAt n
//      merge(msort(fst,comparator), msort(snd,comparator), comparator)
//    }
//  }

//  def merge(xs: MyLinkedList[T], ys: MyLinkedList[T]) =
//    xs match {
//      case Nil =>
//        ys
//      case x :: xs1 =>
//        ys match {
//          case Nil =>
//            xs
//          case y :: ys1 =>
//            if (x < y) x :: merge(xs1, ys)
//            else y :: merge(xs, ys1)
//        }
//    }

//  def msort[T](less: (MyLinkedList[T], MyLinkedList[T]) => Boolean)(xs: List[MyLinkedList[T]]): List[MyLinkedList[T]] = {
//    def merge(xs1: List[MyLinkedList[T]], xs2: List[MyLinkedList[T]]): List[MyLinkedList[T]] =
//      if (xs1.isEmpty) xs2
//      else if (xs2.isEmpty) xs1
//      else if (less(xs1.head, xs2.head)) xs1.head :: merge(xs1.tail, xs2)
//      else xs2.head :: merge(xs1, xs2.tail)
//
//    nde = nde / 2
//    val n = nde
//    if (n == 0) xs
//    else merge(msort(less)(xs take n), msort(less)(xs drop n))
//  }
//
//  def <(y: MyLinkedList[T], comparator: Comparator[T]): Boolean = {
//    if(comparator.compare(this.head.data.get,y.head.data.get) < 0)
//      true
//    else
//    if (comparator.compare(this.head.data.get, y.head.data.get) > 0)
//      false
//    else
//    false
//  }



//  def mergeSort(comparator: Comparator[T], xs: MyLinkedList[T]): MyLinkedList[T] = {
//    //    def merge(xs1: MyLinkedList[T], xs2: MyLinkedList[T]): MyLinkedList[T] =
//    //      if (xs1.head.data.isEmpty) xs2
//    //      else if (xs2.head.data.isEmpty) xs1
//    //      else xs2.head :: merge(xs1, xs2)
//
//    val n = xs.length / 2
//    if (xs.length <= 1) xs
//    else {
//      def merge(comparator: Comparator[T], xs: MyLinkedList[T], ys: MyLinkedList[T]): MyLinkedList[T] =
//        (xs, ys) match {
//          case (null, ys) => ys
//          case (xs, null) => xs
//          case (xs1, ys1) =>
//            if (comparator.compare(xs1.head.data.get, ys1.head.data.get) < 0) xs1 :: merge(comparator,xs1, ys)
//            else ys1 :: merge(comparator,xs, ys1)
//        }
//
//      val left = xs take (n,xs)
//      val right = xs drop (n,xs)
//      merge(comparator, mergeSort(comparator, left), mergeSort(comparator, right))
//    }
//  }
//
//  def ::(x: MyLinkedList[T]): MyLinkedList[T] = {
//    var xh = x.head
//    var pr = 0
//    while (xh != null) {
//      this.add(xh.data.get, pr)
//      xh = xh.next
//      pr = pr + 1
//    }
//    this
//  }
//
//  object pushBegin {
//    def unapply(value: MyLinkedList[T]): Option[(MyLinkedList[T], MyLinkedList[T])] = {
//      //value.add(value.head.data.get, 0)
//      this.unapply(value)
//    }
//  }
//
//
//  def take(n: Int, h: MyLinkedList[T]): MyLinkedList[T] = {
//    var count = 0;
//    var fast = h.head
//    val lst = new MyLinkedList[T]()
//    while (count < n) {
//      if(fast != null) {
//        lst.add(fast.data.get)
//        fast = fast.next
//      }
//      count = count + 1
//    }
//    lst
//  }
//
//  def drop(n: Int, h: MyLinkedList[T]): MyLinkedList[T] = {
//    var count = 0;
//    var fast = h.head
//    val lst = new MyLinkedList[T]()
//    while (count < n) {
//      if (fast != null) {
//        fast = fast.next
//      }
//      count = count + 1
//    }
//
//    while (fast != null) {
//      lst.add(fast.data.get)
//      fast = fast.next
//    }
//
//    lst
//  }





//  def mergeSort(comparator: Comparator[T], xs: List[T]): List[T] = {
//    val n = xs.length / 2
//    if (n == 0) xs
//    else {
//      def merge(xs: List[T], ys: List[T]): List[T] =
//        (xs, ys) match {
//          case (Nil, ys) => ys
//          case (xs, Nil) => xs
//          case (x :: xs1, y :: ys1) =>
//            if (comparator.compare(x,y) < 0) x :: merge(xs1, ys)
//            else y :: merge(xs, ys1)
//        }
//
//      val (left, right) = xs splitAt (n)
//      merge(mergeSort(comparator, left), mergeSort(comparator, right))
//    }
//  }

//  private def mergeSort(h: Node, comparator: Comparator[T]): Node = {
//    if (h == null || h.next == null) {
//      return h
//    }
//
//    val middle = getMiddle(h)
//    val middleNext = middle.next
//
//    middle.next = null
//
//    val left = mergeSort(h, comparator)
//    val right = mergeSort(middleNext, comparator)
//
//    merge(left, right, comparator)
//  }
//
//  private def merge(head11: Node, head22: Node, comparator: Comparator[T]) = {
//    var left = head11
//    var right = head22
//    val merged = new Node(None)
//    var temp = merged
//    while ( {
//      left != null && right != null
//    }) {
//      if (comparator.compare(left.data.get, right.data.get) < 0) {
//        temp.next = left
//        left.prev = temp
//        left = left.next
//      }
//      else {
//        temp.next = right
//        right.prev = temp
//        right = right.next
//      }
//      temp = temp.next
//    }
//    while ( {
//      left != null
//    }) {
//      temp.next = left
//      left.prev = temp
//      left = left.next
//      temp = temp.next
//    }
//    while ( {
//      right != null
//    }) {
//      temp.next = right
//      right.prev = temp
//      right = right.next
//      temp = temp.next
//      this.tail = temp
//    }
//    merged.next
//  }

  private def getMiddle(h: Node) = {
    var fast = h.next
    var slow = h
    while (fast != null) {
      fast = fast.next
      if (fast != null) {
        slow = slow.next
        fast = fast.next
      }
    }
    slow
  }

  private def getNode(index: Int): Node = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException()
    var tmp = head
    for (_ <- 0 until index) {
      tmp = tmp.next
    }
    tmp
  }

  class Node(var data: Option[T]) {
    var next: Node = _
    var prev: Node = _
  }
}