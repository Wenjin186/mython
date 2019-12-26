package edu.colorado.csci3155.mython

case class TypeError(msg: String) extends Exception
case class UnknownIdentifierError(msg: String) extends Exception
case class RunTimeError(msg: String) extends Exception