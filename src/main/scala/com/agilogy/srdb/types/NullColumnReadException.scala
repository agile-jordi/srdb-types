package com.agilogy.srdb.types

import java.sql.ResultSet

import scala.util.Try
import scala.util.control.NonFatal

/** @group API */
case class NullColumnReadException(columnName: String, availableColumnNames: Option[Seq[(String, Any)]]) extends RuntimeException {
  override def getMessage: String = {
    lazy val columnNamesMsg = availableColumnNames
      .map(_.mkString("Available columns are ", ", ", "."))
      .getOrElse("Available columns could not be determined.")
    s"Null found reading column supposedly not null column $columnName. $columnNamesMsg"
  }
}

object NullColumnReadException {

  private[types] def apply(columnName: String, rs: ResultSet): NullColumnReadException = {
    val availableColumnNames = Try {
      val md = rs.getMetaData
      for {
        i <- 1.to(md.getColumnCount)
      } yield {
        val columnName = md.getColumnName(i)
        columnName -> rs.getObject(i)
      }
    }.toOption
    NullColumnReadException(columnName, availableColumnNames)
  }

}

