package com.agilogy.srdb.types

case class ColumnWriteException(pos: Int, causedBy: Throwable) extends RuntimeException
