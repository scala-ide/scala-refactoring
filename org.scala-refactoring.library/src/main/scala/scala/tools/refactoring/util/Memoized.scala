/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package util

object Memoized {

  /**
   *
   * Create a function that memoizes its results in a WeakHashMap.
   *
   * Note that memoization is tricky if the objects that
   * are memoized are mutable and the function being memoized
   * returns a value that somehow depends on this mutable property.
   *
   * For example, if we memoize a function that filters trees
   * based on their position, and later modify the position of
   * a tree, the memoized function will return the wrong value.
   *
   * So in order to make it safe, the mkKey function can be used
   * to provide a better key by including the mutable value.
   *
   * @param mkKey A function that creates the key.
   * @param toMem The function we want to memoize.
   */
  def on[X, Y, Z](mkKey: X => Y)(toMem: X => Z): X => Z = {

    val cache = new java.util.WeakHashMap[Y, Z]

    { (x: X) =>
      val k = mkKey(x)
      if(cache.containsKey(k)) {
        val n = cache.get(k)
        if(n == null) {
          toMem(x)
        } else {
          n
        }
      } else {
        val n = toMem(x)
        cache.put(k, n)
        n
      }
    }
  }

  def apply[X, Z](toMem: X => Z): X => Z = {

    val cache = new java.util.WeakHashMap[X, Z]

    { (x: X) =>
      if(cache.containsKey(x)) {
        val n = cache.get(x)
        if(n == null) {
          toMem(x)
        } else {
          n
        }
      } else {
        val n = toMem(x)
        cache.put(x, n)
        n
      }
    }
  }
}

