package com.github.kczulko.chapter12

import java.util.Date

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {
  def validBirthdate(date: String): Validation[String, Date] = {
    val dateFormat = "yyyy-MM-dd"
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(date))
    } catch {
      case _: Throwable => Failure(s"Birthdate must be in the form $dateFormat")
    }
  }

  def validPhone(phone: String): Validation[String, String] = {
    if (phone.matches("[0-9]{10}"))
      Success(phone)
    else
      Failure("Phone number must have 10 digits")
  }

  def validName(name: String): Validation[String,String] = {
    if (name.isEmpty)
      Failure("Name is empty")
    else
      Success(name)
  }

  def validate(name: String, phone: String, birthdate: String): Validation[String, WebForm] =
    Applicatives.validationApp.map3(
      WebForm.validName(name), WebForm.validBirthdate(birthdate), WebForm.validPhone(phone)
    )(WebForm(_,_,_))
}
