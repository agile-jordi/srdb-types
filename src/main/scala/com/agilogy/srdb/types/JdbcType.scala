package com.agilogy.srdb.types

import java.sql

/**
 * A type according to the JDBC specification
 *
 * This class is only used because JDBC, until 3.0, uses `Int`s  stead of typed instances
 *
 * @param code The JDBC `Int` code of the JDBC type
 * @group API
 */
sealed abstract class JdbcType(val code: Int, val name: String)

/** @group API */
object JdbcType {
  case object TinyInt extends JdbcType(sql.Types.TINYINT, "TINYINT")
  case object SmallInt extends JdbcType(sql.Types.SMALLINT, "SMALLINT")
  case object Integer extends JdbcType(sql.Types.INTEGER, "INTEGER")
  case object BigInt extends JdbcType(sql.Types.BIGINT, "BIGINT")

  case object Boolean extends JdbcType(sql.Types.BOOLEAN, "BOOLEAN")

  case object Varchar extends JdbcType(sql.Types.VARCHAR, "VARCHAR")
  case object Char extends JdbcType(sql.Types.CHAR, "CHAR")
  case object LongVarchar extends JdbcType(sql.Types.LONGNVARCHAR, "LONGNVARCHAR")

  case object Date extends JdbcType(sql.Types.DATE, "DATE")
  case object Time extends JdbcType(sql.Types.TIME, "TIME")
  // TODO: Java8 includes a sql.Types.TIMESTAMPTZ constant. Should we use it?
  case object TimestampTZ extends JdbcType(sql.Types.TIMESTAMP, "TIMESTAMPTZ")
  //  case object TimestampTZ extends JdbcType(sql.Types.TIMESTAMP_WITH_TIMEZONE, "TIMESTAMPTZ")

  case object Numeric extends JdbcType(sql.Types.NUMERIC, "NUMERIC")
  case object Decimal extends JdbcType(sql.Types.DECIMAL, "DECIMAL")
  case object Float extends JdbcType(sql.Types.FLOAT, "FLOAT")
  case object Double extends JdbcType(sql.Types.DOUBLE, "DOUBLE")

  case object Array extends JdbcType(sql.Types.ARRAY, "ARRAY")

  case object Other extends JdbcType(sql.Types.OTHER, "OTHER")

}

