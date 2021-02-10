/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import scala.annotation.tailrec
import scala.util.Try

object P00 {
  def flatten[X](xss: List[List[X]]): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], wss: List[List[X]]): List[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {

  @scala.annotation.tailrec
  def last[X](xs: List[X]): X = xs match{
    case h::t => if (t!=List.empty) last(t) else h
    case Nil => xs.head
  } // TO BE IMPLEMENTED
}

object P02 {

  @scala.annotation.tailrec
  def penultimate[X](xs: List[X]): X = xs match {
    case h::t => if (t.tail!=List.empty) penultimate(t) else h
    case _ => xs.head
  }// TO BE IMPLEMENTED
}

object P03 {

  @scala.annotation.tailrec
  def kth[X](k: Int, xs: List[X]): X = ??? // TO BE IMPLEMENTED
}

object P04 {

  def length[X](xs: List[X]): Int = {
    @tailrec
    def inner(result: Int, _xs: List[X]): Int = {
      _xs match {
        case Nil => result
        case _ :: t => inner(result + 1, t)
      }
    }

    inner(0, xs)
  }
}

object P05 {

  def reverse[X](xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    case h::t => reverse(t):+h
    case _ => xs
  }
}

object P06 {

  // inefficient solution
  def isPalindrome[X](xs: List[X]): Boolean = {
    if(xs.size>1)
      {
        if(xs.head==xs.last)
        {
            isPalindrome(xs.tail.dropRight(1))
        }
        return false
      }
    true
  } // TO BE IMPLEMENTED
}

object P07 {

  type ListAny = List[Any]

  def flatten(xs: ListAny): ListAny =xs match {
    // TO BE IMPLEMENTED
    case List() => List()
    case y::ys => y match {
      case k::ks => flatten(List(k)) ::: flatten(ks) ::: flatten(ys)
      case _ => y :: flatten(ys)
    }
  }
}

object P08 {

  def compress[X](xs: List[X]): List[X] = xs match {
    // TO BE IMPLEMENTED
    case h::t if t.size>0 => if(h!=t.head) h +: compress(t) else h+:compress(t.tail)
    case _ => xs
  }
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
    def _pack[X](left:List[List[X]],right:List[X]): List[List[X]] = right match{
      case h::t if(left.size==0 || h!=left.last.head) => _pack(left:::List(List(h)),t)
      case h::t => _pack(left.init:::List(left.last:::List(h)),t)
      case _ => left
    }
    _pack(List(),xs)
  }
}

object P10 {

  def encode[X](xs: List[X]): List[(Int, X)] = ??? // TO BE IMPLEMENTED
}

object P11 {

  def encodeModified[X](xs: List[X]): List[Any] = ??? // TO BE IMPLEMENTED
}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] = ??? // TO BE IMPLEMENTED
}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P14 {

  def duplicate[X](xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}
