package com.agilogy.srdb.types

import java.sql.PreparedStatement

/**
 * A writer capable of setting one or more parameters in a `PreparedStatement` to pass an instance of `T`
 * @tparam T The Scala type representing the parameter or parameters to set in the `PreparedStatement`
 * @group API
 */
trait DbWriter[T] {

  self =>

  val length: Int

  def set(ps: PreparedStatement, value: T): Unit = set(ps, 1, value)

  def set(ps: PreparedStatement, pos: Int, value: T): Unit

}
